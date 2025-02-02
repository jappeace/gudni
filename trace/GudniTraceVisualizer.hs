{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GudniTraceVisualizer
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure

import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Fuzzy
import Graphics.Gudni.Util.Draw

import Data.Word
import Data.List
import Data.Maybe
import Data.Bits

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse

import Codec.Picture

import GHC.Exts

data SliceHs = SliceHs
    { sStart  :: Int
    , sLength :: Int
    } deriving (Read, Show)

data ShapeHs = ShapeHs
    { shapeTag :: Int
    , shapeSlice :: SliceHs
    } deriving (Read, Show)

data ColorHs = ColorHs
  { redChan   :: Float
  , greenChan :: Float
  , blueChan  :: Float
  , alphaChan :: Float
  } deriving (Read, Show)

data ThresholdHs = ThresholdHs
  { thrIndex         :: Int
  , thrTop           :: Float
  , thrBottom        :: Float
  , thrLeft          :: Float
  , thrRight         :: Float
  , thrPositiveSlope :: Int
  , thrTopPersist    :: Int
  , thrBottomPersist :: Int
  , thrShapeIndex    :: Int
  } deriving (Read, Show)

data Action
    = ListStart
    | TileInfoHs
        { tileBox :: (Int, Int, Int, Int)
        , tileHDepth :: Int
        , tileVDepth :: Int
        , tileShapeSlice :: SliceHs
        }
    | ColorStateHs
        { csBackgroundColor :: ColorHs
        , csPictureData :: Int
        , csPictureRefs :: Int
        , csIsConstant :: Int
        , absolutePosition :: (Float, Float)
        }
    | ThresholdStateHs
        { thresholds         :: [ThresholdHs]
        --, thresholdStart     :: Int
        --, numThresholds      :: Int
        , renderStart        :: (Float, Float)
        , renderEnd          :: (Float, Float)
        --, needHorizontalPass :: Int
        --, inHorizontalPass   :: Int
        --, thresholdWasAdded  :: Int
        --, slotThresholdCount :: Int
        --, addThresholdCount  :: Int
        }
    | ShapeStateHs
        { shapeBits    :: Int
        , shapeIndices :: [(Int,Int)]
        , shapeStack   :: [Word32]
        }
    | ParseStateHs
       { currentThreshold :: Int
       --, numActive :: Int
       , sectionStart :: (Float, Float)
       , sectionEnd   :: (Float, Float)
       --, pixelY :: Float
       , accColorArea :: (Float, Float, Float, Float, Float, Float, Float, Float)
       --, sectionCount :: Int
       --, frameNumber  :: Int
       --, buildCount   :: Int
       --, randomFieldCursor :: Int
       --, randomField :: Int
       }
    | TileStateHs
       { tileShapeStart :: Int
       , tileNumShapes :: Int
       , tileSize :: Int
       , bitmapSize :: (Int, Int)
       , threadDelta :: (Int, Int)
       , tileIndex :: Int
       , intHeight :: Int
       , floatHeight :: Float
       , threadUnique :: Int
       , column :: Int
       }
    | TraversalHs
       { travLeftControl :: (Float, Float, Float, Float)
       , travRight :: (Float, Float)
       , travXPos :: Float
       , travIndex :: Int
       }
    deriving (Read, Show)

data ActionState = ActionState
   { tsTileInfo       :: Maybe Action
   , tsColorState     :: Maybe Action
   , tsThresholdState :: Maybe Action
   , tsShapeState     :: Maybe Action
   , tsParseState     :: Maybe Action
   , tsTileStateHs    :: Maybe Action
   , tsTraversal      :: Maybe Action
   } deriving (Show)

initActionState :: ActionState
initActionState = ActionState
   { tsTileInfo       = Nothing
   , tsColorState     = Nothing
   , tsThresholdState = Nothing
   , tsShapeState     = Nothing
   , tsParseState     = Nothing
   , tsTileStateHs    = Nothing
   , tsTraversal      = Nothing
   }

updateActionState :: ActionState -> Action -> ActionState
updateActionState state action =
  case action of
      ListStart        {} -> state
      TileInfoHs       {} -> state {tsTileInfo       = Just action}
      ColorStateHs     {} -> state {tsColorState     = Just action}
      ThresholdStateHs {} -> state {tsThresholdState = Just action}
      ShapeStateHs     {} -> state {tsShapeState     = Just action}
      ParseStateHs     {} -> state {tsParseState     = Just action}
      TileStateHs      {} -> state {tsTileStateHs    = Just action}
      TraversalHs      {} -> state {tsTraversal      = Just action}

data TraceState = TraceState
  { _stateScale       :: SubSpace
  , _stateDelta       :: Point2 SubSpace
  , _stateCircleDelta :: Point2 SubSpace
  , _stateAngle       :: Angle  SubSpace
  , _statePaused      :: Bool
  , _stateSpeed       :: SubSpace
  , _statePace        :: SubSpace
  , _stateLastTime    :: SimpleTime
  , _stateDirection   :: Bool
  , _statePlayhead    :: SubSpace
  , _stateCursor      :: Point2 PixelSpace
  , _stateStep        :: Int
  , _stateSelectPass  :: Int
  , _stateActions     :: [Action]
  }
makeLenses ''TraceState

instance Show TraceState where
  show state = "Scale: " ++ show (state ^. stateScale) ++ " Delta: " ++ show (state ^. stateDelta)

initialModel actions =
    TraceState
    { _stateScale       = 10
    , _stateDelta       = Point2 0 0
    , _stateCircleDelta = Point2 0 0
    , _stateAngle       = 0 @@ turn-- quarterTurn
    , _statePaused      = True
    , _stateSpeed       = 0.01
    , _statePace        = 100
    , _stateLastTime    = 0
    , _stateDirection   = True
    , _statePlayhead    = 0
    , _stateCursor      = Point2 18 6 :: Point2 PixelSpace
    , _stateStep        = 0
    , _stateSelectPass  = 0
    , _stateActions     = actions
    }

tuple2Point :: (Float, Float) -> Point2 SubSpace
tuple2Point (x,y) = Point2 (SubSpace x) (SubSpace y)

colorAreaToColor :: (Float, Float, Float, Float, Float, Float, Float, Float)
                 -> (Color, Float)
colorAreaToColor (r,g,b,a,area,_,_,_) = (rgbaColor r g b a, area)

buildThresholdHs threshold =
  let
  -- thrIndex         threshold -- :: Int
  top    = SubSpace . realToFrac . thrTop    $ threshold -- :: Float
  bottom = SubSpace . realToFrac . thrBottom $ threshold -- :: Float
  left   = SubSpace . realToFrac . thrLeft   $ threshold -- :: Float
  right  = SubSpace . realToFrac . thrRight  $ threshold -- :: Float
  slope  = thrPositiveSlope threshold -- :: Int
  -- thrTopPersist    threshold -- :: Int
  -- thrBottomPersist threshold -- :: Int
  -- thrShapeIndex    threshold -- :: Int
  adjustedStroke = if thrTopPersist threshold == 1 || thrBottomPersist threshold == 1
                   then 0.025
                   else 0.0125
  (leftY, rightY) = if (thrPositiveSlope threshold == 1)
                    then (top, bottom)
                    else (bottom, top)

  startPoint = Point2 left  leftY
  endPoint   = Point2 right rightY
  shapeIndex = thrShapeIndex threshold
  color      = black  -- if shapeIndex == 0
                      -- then colorModifier $ colors !! shapeIndex
                      -- else transparent 0.01 $ black
  in  overlap [solid color $ line adjustedStroke startPoint endPoint
              --,tTranslate (Point2 (DSpace . realToFrac . thrLeft  $ threshold) (DSpace . realToFrac . thrTop $ threshold)) .
              -- SLeaf (Left $ transparent 0.25 $ light blue) 0 $
              -- rectangle (Point2 ((DSpace . realToFrac . thrRight  $ threshold) - (DSpace . realToFrac . thrLeft $ threshold))
              --                   ((DSpace . realToFrac . thrBottom $ threshold) - (DSpace . realToFrac . thrTop  $ threshold))
              --           )
              ]

buildTileInfoHs :: Action -> ShapeTree Int
buildTileInfoHs action = undefined
  -- tileBox :: (Int, Int, Int, Int)
  -- tileHDepth :: Int
  -- tileVDepth :: Int
  -- tileShapeSlice :: Slice

buildColorStateHs :: Action -> ShapeTree Int
buildColorStateHs action = undefined
   -- csBackgroundColor action
   -- csPictureData     action
   -- csPictureRefs     action
   -- csIsConstant      action
   -- absolutePosition  action

buildThresholdStateHs :: Action -> ShapeTree Int
buildThresholdStateHs action =
    let
        ts     = thresholds     action -- [ThresholdHs]
    --  tStart = thresholdStart action -- Int
    --  tNum   = numThresholds  action -- Int
    --  renderStart        action -- (Float, Float)
    --  renderEnd          action -- (Float, Float)
    --  needHorizontalPass action -- Int
    --  inHorizontalPass   action -- Int
    --  thresholdWasAdded  action -- Int
    --  slotThresholdCount action -- Int
    --  addThresholdCount  action -- Int
    in  overlap $ map buildThresholdHs ts


buildShapeStateHs :: Action -> ShapeTree Int
buildShapeStateHs action = undefined
    -- shapeBits    action -- Int
    -- shapeIndices action -- [(Int,Int)]
    -- shapeStack   action -- [Word32]

buildParseStateHs :: Action -> ShapeTree Int
buildParseStateHs action =
    let
    --  currentThreshold  action -- Int
    --  numActive         action -- Int
    start = tuple2Point $ sectionStart action -- (Float, Float)
    end   = tuple2Point $ sectionEnd   action -- (Float, Float)
    --  pixelY            action -- Float
    (color, area) = colorAreaToColor $  accColorArea action -- (Float, Float, Float, Float, Float, Float, Float, Float)
    --  sectionCount      action -- Int
    --  frameNumber       action -- Int
    --  buildCount        action -- Int
    --  randomFieldCursor action -- Int
    --  randomField       action -- Int
    in sTranslate start . solid color . rectangle $ end - start

buildTileStateHs :: Action -> ShapeTree Int
buildTileStateHs action = undefined
    -- tileShapeStart -- Int
    -- tileNumShapes  -- Int
    -- tileSize       -- Int
    -- bitmapSize     -- (Int, Int)
    -- threadDelta    -- (Int, Int)
    -- tileIndex      -- Int
    -- intHeight      -- Int
    -- floatHeight    -- Float
    -- threadUnique   -- Int
    -- column         -- Int

buildTraversalHs :: Action -> ShapeTree Int
buildTraversalHs action = undefined
    -- travLeftControl -- (Float, Float, Float, Float)
    -- travRight       -- (Float, Float)
    -- travXPos        -- Float
    -- travIndex       -- Int

buildActionState :: ActionState -> ShapeTree Int
buildActionState state = overlap . catMaybes $
    [ fmap buildTileInfoHs       (tsTileInfo       state)
    , fmap buildColorStateHs     (tsColorState     state)
    , fmap buildThresholdStateHs (tsThresholdState state)
    , fmap buildShapeStateHs     (tsShapeState     state)
    , fmap buildParseStateHs     (tsParseState     state)
    , fmap buildTileStateHs      (tsTileStateHs    state)
    , fmap buildTraversalHs      (tsTraversal      state)
    --, Just . solid blue . rectangle $ Point2 10 10
    ]

instance Model TraceState where
    screenSize state = --FullScreen
        Window $ Point2 1024
                        1024
    shouldLoop _ = True
    fontFile state = fromMaybe "Times New Roman.ttf" <$> listToMaybe . filter (isInfixOf "Times New Roman.ttf") <$> fontLibrary
    updateModelState frame elapsedTime inputs state =
        flip execStateT state $
            do  mapM_ processInput inputs
                lastTime <- use stateLastTime
                stateLastTime .= elapsedTime
                speed <- use stateSpeed
                whenM(not <$> use statePaused) $
                    do  direction <- use stateDirection
                        let f = if direction then (+) else (-)
                            timeDelta = elapsedTime - lastTime
                            dt = realToFrac timeDelta * realToFrac speed
                        statePlayhead %= (`f` dt)
    constructScene state status =
        do  statusGlyphs <- mapM glyphString $ lines status
            let tree = transformFromState state $ constructFromState state
                statusTree = statusDisplay state statusGlyphs
                withStatus = if True  then overlap [statusTree, tree] else tree
            return $ Scene (light gray) withStatus
    providePictureData state = noPictures

statusDisplay :: TraceState -> [[Glyph SubSpace]] -> ShapeTree Int
statusDisplay state status =
    sTranslateXY 3100 2000 .
    sScale 24 .
    solid (redish $ dark gray) .
    paraGrid 1 $
    status

transformFromState :: TraceState -> ShapeTree Int -> ShapeTree Int
transformFromState state =
  let sc    = view stateScale state
      delta = view stateDelta state
      angle = view stateAngle state
  in  sTranslate delta .
      sRotate angle .
      sScale sc

constructFromState :: TraceState -> ShapeTree Int
constructFromState state =
  let steps   = view stateStep  state
      actions = take (steps + 2) $ view stateActions state
  in  buildActionState $ foldl updateActionState initActionState actions

processInput :: Monad m => Input (Point2 PixelSpace) -> StateT TraceState m ()
processInput input =
    case input of
        (InputKey Pressed _ inputKeyboard) ->
            do  speed <- use stateSpeed
                pace  <- use statePace
                case inputKeyboard of
                    KeySymbol SymbolSpace -> statePaused %= not
                    KeyArrow ArrowUp      -> stateSpeed *=  1.25
                    KeyArrow ArrowDown    -> stateSpeed //= 1.25
                    KeyLetter LetterW   -> stateDelta %= (^+^ Point2   0    (-pace))
                    KeyLetter LetterS   -> stateDelta %= (^+^ Point2   0      pace )
                    KeyLetter LetterA   -> stateDelta %= (^+^ Point2 (-pace)  0    )
                    KeyLetter LetterD   -> stateDelta %= (^+^ Point2   pace   0    )
                    KeyLetter LetterY   -> stateDirection %= not
                    KeySymbol SymbolRightBracket -> stateScale *=  1.1
                    KeySymbol SymbolLeftBracket  -> stateScale //= 1.1
                    KeySymbol SymbolComma  -> whenM (uses stateStep (> 0 {-arbitrary-})) $ stateStep -= 1
                    KeySymbol SymbolPeriod -> whenM (uses stateStep (< 1000)) $ stateStep += 1
                    KeyLetter LetterR   -> stateAngle %= normalizeAngle . (^+^ (speed @@ turn))
                    KeyLetter LetterT   -> stateAngle %= normalizeAngle . (^-^ (speed @@ turn))
                    -- KeyArrow ArrowRight -> whenM (uses stateCurrentTest (< (length tests - 1))) $ stateCurrentTest += 1
                    -- KeyArrow ArrowLeft  -> whenM (uses stateCurrentTest (> 0)) $ stateCurrentTest -= 1
                    _                   -> return ()
        (InputMouse detection modifier clicks positionInfo) ->
            case detection of
              Pressed -> stateCursor .= positionInfo
              _ -> return ()
        _ -> return ()


main :: IO ()
main = do actions <- read <$> readFile "trace/trace02.txt"
          runApplication (initialModel actions :: TraceState)
