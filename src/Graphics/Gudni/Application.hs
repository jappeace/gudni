{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts #-}
module Graphics.Gudni.Application
  ( runApplication
  , Model(..)
  , ScreenMode(..)
  , SimpleTime
  )
where
-- This must be compiled with the -threaded option

import Foreign.C
import Foreign.C.String
import Foreign

import Linear

import Debug.Trace
import System.Environment
import Control.DeepSeq

import Data.Char
import Data.Maybe(fromMaybe)

import System.Exit (exitSuccess)
import System.Directory
import System.FilePath
import System.Timeout
import System.Clock
import System.Mem

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Monad.Random
import Control.Exception (evaluate)

import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.InterfaceSDL
import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.ScreenMode

import Graphics.Gudni.OpenCL.Setup
import Graphics.Gudni.OpenCL.KernelLibrary
import Graphics.Gudni.OpenCL.CallKernels

import Graphics.Gudni.Raster.Types

import Graphics.Gudni.Raster.Constants (rANDOMFIELDsIZE)
import Graphics.Gudni.OpenCL.EmbeddedOpenCLSource
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.Job
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.RandomField

import qualified Data.Vector.Storable as VS
import System.Info

type SimpleTime = Double

class Model s where
  -- | Construct a Scene from the state of type `s`
  constructScene  :: s -> String -> GlyphMonad IO (Scene Int)
  -- | Update the state based on the elapsed time and a list of inputs
  updateModelState :: Monad m => Int -> SimpleTime -> [Input (Point2 PixelSpace)] -> s -> m s
  -- | Set the initial display to FullScreen or a specific window size in pixels.
  screenSize       :: s -> ScreenMode
  -- | Determine if the application will enter the event loop.
  -- for debugging purposes you can set this to False and render on frame and quit.
  shouldLoop       :: s -> Bool
  -- | Path to the Truetype font file that is initially loaded.
  fontFile         :: s -> IO String
  -- | Bitmap texture data provided from the state for the rendered scene.
  providePictureData :: s -> IO (VS.Vector Word8, [PictureMemoryReference])
  -- | Do something with the output of the rasterizer.
  handleOutput :: s -> DrawTarget -> StateT InterfaceState IO s

data ApplicationState s = AppState
    { -- | The state maintained specific to the interface type.
      _appBackend       :: InterfaceState
      -- | Structure for marking time.
    , _appTimeKeeper    :: TimeKeeper
      -- | The start time of the application.
    , _appStartTime     :: TimeSpec
      -- | Constructor used to store the OpenCL state, compiled kernels and device metadata.
    , _appOpenCLLibrary :: OpenCLKernelLibrary
      -- | A string representing information about the app. Usually timing data and other stuff for display.
    , _appStatus        :: String
     -- | The number of event loop cycles that have commenced from starting.
    , _appCycle         :: Int
     -- | Polymorphic type defined by the client program. Represents the state of the client application.
    , _appState         :: s
    }
makeLenses ''ApplicationState

-- | Monad Stack for the event loop.
type ApplicationMonad s = StateT (ApplicationState s) (GeometryMonad (GlyphMonad IO))

runApplicationMonad :: ApplicationState s -> ApplicationMonad s a -> GeometryMonad (GlyphMonad IO) a
runApplicationMonad = flip evalStateT

-- | Initializes openCL, frontend interface, timekeeper, randomfield data and returns the initial `ApplicationState`
setupApplication :: Model s => s -> IO (ApplicationState s)
setupApplication state  =
  do  -- Setup OpenCL state and kernels.
      openCLLibrary <- setupOpenCL False False openCLSourceWithDefines
      -- Initialize the backend state.
      backendState <- startInterface (screenSize state)
      -- Start the timeKeeper
      timeKeeper <- startTimeKeeper
      startTime <- getTime Realtime
      return $ AppState backendState timeKeeper startTime openCLLibrary "No Status" 0 state

-- | Closes the interface.
closeApplication :: ApplicationMonad s ()
closeApplication = withIO appBackend closeInterface

-- | Initialize the ApplicationMonad stack and enter the event loop.
runApplication :: (Show s, Model s) => s -> IO ()
runApplication state =
    do  -- Initialize the application and get the initial state.
        appState <- setupApplication state
        -- Start the glyph monad.
        runGlyphMonad $
            do  -- Load a font file.
                mFontFile <- liftIO $ fontFile state
                -- Add the font file to the glyph monad.
                addFont mFontFile
                -- Generate a random field for the stochastic aliasing of the rasterizer.
                randomField <- liftIO $ makeRandomField rANDOMFIELDsIZE
                -- Run the geometry serialization monad.
                runGeometryMonad randomField $
                    -- Run the application monad.
                    runApplicationMonad appState $
                        do  -- start the event loop.
                            loop
                            -- when the loop exits close the application.
                            closeApplication

-- | Convert a `Timespec` to the `SimpleTime` (a double in seconds from application start)
toSimpleTime :: TimeSpec -> SimpleTime
toSimpleTime timeSpec = (fromIntegral . toNanoSecs $ timeSpec) / 1000000000

-- | Get the time elapsed from starting the event loop.
getElapsedTime :: ApplicationMonad s SimpleTime
getElapsedTime =
  do  startTime   <- use appStartTime
      currentTime <- liftIO $ getTime Realtime
      return $ toSimpleTime $ currentTime `diffTimeSpec` startTime

-- | Debug message from the ApplicationMonad
appMessage :: String -> ApplicationMonad s ()
appMessage = liftIO . putStrLn

-- | Call a function f in IO that uses the application state as it's first argument.
overState f =
  do  state <- use appState
      state' <- liftIO $ f state
      appState .= state'

-- | Initialize the timekeeper
restartAppTimer :: ApplicationMonad s ()
restartAppTimer = appTimeKeeper <~ liftIO startTimeKeeper

-- | First phase of event loop.
beginCycle :: ApplicationMonad s ()
beginCycle =
    do  restartAppTimer

-- | Update the model state and generate a shape tree, marking time along the way.
processState :: (Show s, Model s) => SimpleTime -> [Input (Point2 PixelSpace)] -> ApplicationMonad s (Scene Int)
processState elapsedTime inputs =
    do  frame <- fromIntegral <$> use appCycle
        overState $ updateModelState frame elapsedTime inputs
        status <- use appStatus
        markAppTime "Advance State"
        state <- use appState
        if null inputs
        then appMessage $ show state
        else appMessage $ show state ++ show inputs

        --shapeTree <- lift . evalRandIO $ fuzz 5000
        shapeTree <- lift . lift $ constructScene state status
        --appMessage $ "ShapeTree " ++ show shapeTree
        --lift . putStrLn $ textForm
        markAppTime "Build State"
        return shapeTree

-- | Prepare and render the shapetree to a bitmap via the OpenCL kernel.
drawFrame :: (Model s) => CInt -> Scene Int -> ApplicationMonad s DrawTarget
drawFrame frame scene =
    do  --appMessage "ResetJob"
        library <- use appOpenCLLibrary
        target <- withIO appBackend (prepareTarget (clUseGLInterop library))
        appS <- use appState
        (pictData, pictureMemoryReferences) <- liftIO $ providePictureData appS
        let canvasSize = P (targetArea target)
        lift (geoCanvasSize .= (fromIntegral <$> canvasSize))
        lift (geoTileTree .= buildTileTree (fromIntegral <$> canvasSize))
        markAppTime "Build TileTree"
        substanceState <- lift ( execSubstanceMonad pictureMemoryReferences $
                                 buildOverScene scene)
        --liftIO $ evaluate $ rnf (substances, boundedShapedEnclosures, substanceState)
        markAppTime "Traverse ShapeTree"
        geometryState <- lift $ get
        --liftIO $ putStrLn $ "TileTree " ++ show (geometryState ^. geoTileTree)
        let rasterParams = RasterParams library
                                        pictData
                                        target
                                        geometryState
                                        substanceState
        appMessage "===================== rasterStart ====================="
        jobs <- lift $ buildRasterJobs rasterParams
        markAppTime "Build Raster Jobs"
        lift $ queueRasterJobs frame rasterParams jobs
        appMessage "===================== rasterDone ====================="
        markAppTime "Rasterize Threads"
        lift resetGeometryMonad
        liftIO $ freeRasterJobs jobs
        --liftIO $ threadDelay 3000000
        return target

-- Final phase of the event loop.
endCycle :: SimpleTime -> ApplicationMonad s ()
endCycle elapsedTime =
    do  tk         <- use appTimeKeeper
        cycleCount <- use appCycle
        let status = showTimes "Loop Cycle" True tk
                   -- ++ show job
                   ++ "------ Cycle: "
                   ++ show cycleCount
                   ++ "\n ------ Elapsed Time: "
                   ++ showFlFixed' 2 1 elapsedTime ++ "\n"
        appStatus .= status
        when (os == "darwin") $ appMessage status
        appCycle += 1

-- Mark a time in the status.
markAppTime :: String -> ApplicationMonad s ()
markAppTime message =
    do  tk  <- use appTimeKeeper
        tk' <- liftIO $ markTime tk message ()
        appTimeKeeper .= tk'

-- | Determine if a particular input should quit the application.
isQuit :: Input a -> Bool
isQuit input =
    case input of
        InputWindow WindowClosed -> True
        InputKey Pressed (KeyModifier _ _ _ True) (KeyLetter LetterQ) -> True
        InputKey Pressed _ (KeyCommand CommandQuit) -> True
        _ -> False

-- | Cycle through the event loop.
loop :: (Show s, Model s) => ApplicationMonad s ()
loop  =
  do  --appMessage "checkInputs"
      inputs <- withIO appBackend checkInputs
      unless (any isQuit inputs) $
          do  elapsedTime <- getElapsedTime
              beginCycle
              scene <- processState elapsedTime inputs
              frame <- fromIntegral <$> use appCycle
              target <- drawFrame frame scene
              state <- use appState
              state' <- withIO appBackend $ handleOutput state target
              appState .= state'
              markAppTime "Raster Frame"
              endCycle elapsedTime
              liftIO performMinorGC -- the idea here is that if we perform garbage collection
                                    -- on each frame we'll get a more consistent frame rate.
              continue <- shouldLoop <$> use appState
              when continue loop
