{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.Setup
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for setting up an OpenCL platform to run the rasterizer kernel.
-- Including macro definitions that are implanted into OpenCL source code.

module Graphics.Gudni.OpenCL.Setup
  ( setupOpenCL
  )
where

import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.Interface.GLInterop
import Graphics.Gudni.OpenCL.CppDefines

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Util.Debug

import Control.Parallel.OpenCL
import Control.Lens
import CLUtil.KernelArgs
import CLUtil
import CLUtil.Initialization

--import Graphics.Gudni.OpenCL.DeviceQuery

import Data.List
import Data.Maybe
import Data.Foldable
import Control.Exception

import qualified Data.ByteString.Char8 as BS

instance OpenCLSource BS.ByteString where
  prepSource = BS.unpack

-- | List of definition pragmas to be added to the beggining of the Kernels.cl file.
cppDefines :: RasterSpec -> [CppDefinition]
cppDefines spec =
  [Cpp "STOCHASTIC_FACTOR"                 (CppFloat sTOCHASTICfACTOR                )
  ,Cpp "RANDOMFIELDSIZE"                   (CppInt   rANDOMFIELDsIZE                 )
  ,Cpp "MAXTHRESHOLDS"                     (CppInt   $ spec ^. specMaxThresholds   )
  ,Cpp "MAXSHAPE"                          (CppInt   $ spec ^. specMaxShapes       )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_BITMASK"    (CppHex64 sHAPETAGsUBSTANCEtYPEbITmASK    )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_SOLIDCOLOR" (CppHex64 sHAPETAGsUBSTANCEtYPEsOLIDcOLOR )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_PICTURE"    (CppHex64 sHAPETAGsUBSTANCEtYPEpICTURE    )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_SHIFT"      (CppInt   sHAPETAGsUBSTANCETYPEsHIFT      )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_BITMASK"     (CppHex64 sHAPETAGcOMPOUNDtYPEbITmASK     )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_CONTINUE"    (CppHex64 sHAPETAGcOMPOUNDtYPEcONTINUE    )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_ADD"         (CppHex64 sHAPETAGcOMPOUNDtYPEaDD         )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_SUBTRACT"    (CppHex64 sHAPETAGcOMPOUNDtYPEsUBTRACT    )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_SHIFT"       (CppInt   sHAPETAGcOMPOUNDtYPEsHIFT       )
  ,Cpp "SHAPETAG_SUBSTANCEID_BITMASK"      (CppHex64 sHAPETAGsUBSTANCEIDbITMASK      )
  --,Cpp "DEBUG_OUTPUT"                      (CppNothing) -- uncomment this to turn on simple debugging output
  --,Cpp "DEBUG_TRACE"                       (CppNothing) -- uncomment this to turn on parsable debugging output
  ]

-- | Embedded source with implanted definition pragmas.
addDefinesToSource :: RasterSpec -> BS.ByteString -> String
addDefinesToSource spec src = appendCppDefines sOURCEfILEpADDING (cppDefines spec) (BS.unpack src)

-- | This function determines the basic paramters of the rasterizer based on
determineRasterSpec :: CLDeviceID -> IO RasterSpec
determineRasterSpec device =
  do  computeUnits  <- clGetDeviceMaxComputeUnits       device
      maxGroupSize  <- clGetDeviceMaxWorkGroupSize      device
      localMemSize  <- clGetDeviceLocalMemSize          device
      maxBufferSize <- clGetDeviceMaxConstantBufferSize device
      globalMemSize <- clGetDeviceGlobalMemSize         device
      return RasterSpec { _specMaxTileSize     = fromIntegral maxGroupSize
                        , _specThreadsPerTile  = fromIntegral maxGroupSize
                        , _specMaxTilesPerCall = fromIntegral maxGroupSize
                        , _specMaxThresholds   = mAXtHRESHOLDS
                        , _specMaxShapes       = mAXsHAPE
                        }

-- | Order Devices based on the number of compute units
orderGPU :: CLDeviceDetail -> CLDeviceDetail -> Ordering
orderGPU gpu1 gpu2 = compare (gpu2 ^. clDeviceMaxComputeUnits) (gpu1 ^. clDeviceMaxComputeUnits)

-- | Determine if the Device Name of a particular device contains a specific string.
deviceNameContains :: String -> CLDeviceDetail -> Bool
deviceNameContains selector deviceInfo = isInfixOf selector (deviceInfo ^. clDeviceName)

-- | Select a device the best device among a set of qualified.
deviceSelect :: (CLDeviceDetail -> Bool) -> (CLDeviceDetail -> CLDeviceDetail -> Ordering) -> [CLDeviceDetail] -> Maybe CLDeviceDetail
deviceSelect qualified order details = listToMaybe . sortBy order . filter qualified $ details

-- |Load a program from an OpenCLSource using a string listing the build options and a previously initialized
-- 'OpenCLState' The returned function may be used to create
-- executable kernels from the loaded program.
loadProgramWOptions' :: (OpenCLSource s) => [CLBuildOption] -> OpenCLState -> s -> IO (String -> IO CLKernel)
loadProgramWOptions' options state src =
  do  program <- clCreateProgramWithSource (clContext state) $ prepSource src
      catch (clBuildProgram program [clDevice state] $ formOptions options) $ \(x :: SomeException) -> do
            errorLog <- clGetProgramBuildLog program $ clDevice state
            putStrLn errorLog 
            throwIO x
      return $ clCreateKernel program

-- Translate a CLBuildOption to a string.
formOption :: CLBuildOption -> String
formOption option = case option of
  -- | -------- Preprocessor Options ----------
  -- | Predefine name as a macro, with definition 1.
  -- | -D name=definition or -D name
  -- The contents of definition are tokenized and processed as if they appeared during translation phase three in a
  --  `#define' directive. In particular, the definition will be truncated by embedded newline characters.
  CLDefine name mDef -> "-D " ++ name ++ maybe "" ('=':) mDef
  -- | -I dir
  -- | Add the directory dir to the list of directories to be searched for header files.
  CLIncludeDir directory -> "-I dir " ++ directory
  -- | ---------- Math Intrinsics Options--------
  -- | These options control compiler behavior regarding floating-point arithmetic.
  -- | These options trade off between speed and correctness.
  -- | Treat double precision floating-point constant as single precision constant.
  CLSinglePrecisionConstant -> "-cl-single-precision-constant"
  -- | This option controls how single precision and double precision denormalized numbers are handled.
  -- | If specified as a build option, the single precision denormalized numbers may be flushed to zero and if
  -- | the optional extension for double precision is supported, double precision denormalized numbers may also be flushed to zero.
  -- | This is intended to be a performance hint and the OpenCL compiler can choose not to flush denorms to zero if the device supports
  -- | single precision (or double precision) denormalized numbers.
  -- | This option is ignored for single precision numbers if the device does not support single precision denormalized numbers i.e.
  -- | CL_FP_DENORM bit is not set in CL_DEVICE_SINGLE_FP_CONFIG.
  -- | This option is ignored for double precision numbers if the device does not support double precision or if it does support
  -- | double precison but CL_FP_DENORM bit is not set in CL_DEVICE_DOUBLE_FP_CONFIG.
  -- | This flag only applies for scalar and vector single precision floating-point variables and computations on these floating-point variables inside a program. It does not apply to reading from or writing to image objects.
  CLDenormsAreZero -> "-cl-denorms-are-zero"
  -- | ----------- Optimization Options -------------
  -- | These options control various sorts of optimizations. Turning on optimization flags makes the compiler attempt to improve the performance and/or code size at the expense of compilation time and possibly the ability to debug the program.
  -- | This option disables all optimizations. The default is optimizations are enabled.
  CLOptDisable -> "-cl-opt-disable"
  -- | This option allows the compiler to assume the strictest aliasing rules.
  CLStrictAliasing -> "-cl-strict-aliasing"
  -- | The following options control compiler behavior regarding floating-point arithmetic. These options trade off between performance and correctness and must be specifically enabled. These options are not turned on by default since it can result in incorrect output for programs which depend on an exact implementation of IEEE 754 rules/specifications for math functions.
  -- | Allow a * b + c to be replaced by a mad. The mad computes a * b + c with reduced accuracy. For example, some OpenCL devices implement mad as truncate the result of a * b before adding it to c.
  CLMadEnable -> "-cl-mad-enable"
  -- | Allow optimizations for floating-point arithmetic that ignore the signedness of zero. IEEE 754 arithmetic specifies the behavior of distinct +0.0 and -0.0 values, which then prohibits simplification of expressions such as x+0.0 or 0.0*x (even with -clfinite-math only). This option implies that the sign of a zero result isn't significant.
  CLNoSignedZeros -> "-cl-no-signed-zeros"
  -- | Allow optimizations for floating-point arithmetic that (a) assume that arguments and results are valid, (b) may violate IEEE 754 standard and (c) may violate the OpenCL numerical compliance requirements as defined in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5. This option includes the -cl-no-signed-zeros and -cl-mad-enable options.
  CLUnsafeMathOptimizations -> "-cl-unsafe-math-optimizations"
  -- | Allow optimizations for floating-point arithmetic that assume that arguments and results are not NaNs or ±∞. This option may violate the OpenCL numerical compliance requirements defined in in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5.
  CLFiniteMathOnly -> "-cl-finite-math-only"
  -- | Sets the optimization options -cl-finite-math-only and -cl-unsafe-math-optimizations.
  -- | This allows optimizations for floating-point arithmetic that may violate the IEEE 754 standard and the OpenCL numerical compliance requirements defined in the specification in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5. This option causes the preprocessor macro __FAST_RELAXED_MATH__ to be defined in the OpenCL rogram.
  CLFastRelaxedMath -> "-cl-fast-relaxed-math"
  -- | Options to Request or Suppress Warnings
  -- | Warnings are diagnostic messages that report constructions which are not inherently erroneous but which are risky or suggest there may have been an error. The following languageindependent options do not enable specific warnings but control the kinds of diagnostics produced by the OpenCL compiler.
  -- | Inhibit all warning messages.
  CLInhibitWarning -> "-w"
  -- | Make all warnings into errors.
  CLWarningIntoError -> "-Werror"


-- Translate a list of buildOptions
formOptions :: [CLBuildOption] -> String
formOptions = intercalate " " . map formOption


-- | Create a Rasterizer by setting up an OpenCL device.
setupOpenCL :: Bool -> Bool -> BS.ByteString -> IO Rasterizer
setupOpenCL enableProfiling useCLGLInterop src =
  do
      putStrLn $ "Initializing OpenCL device."
      -- Create Detail Records for every available device.
      details <- queryOpenCL CL_DEVICE_TYPE_GPU
      -- List all platforms and all devices.
      mapM (putStrLn . show) details
      -- Filter function for qualified devices to select.
      --let deviceFilter = deviceNameContains "Iris Pro"  -- select the first Iris Pro GPU
      --let deviceFilter = deviceNameContains "Vega"        -- select the first AMD GPU
      let deviceFilter = const True -- all GPUs qualify
      -- Try to select the best qualified device.
      case deviceSelect deviceFilter orderGPU details of
        Nothing -> error "No GPU qualifies based on filter criteria."
        Just detail ->
          do  state <- if useCLGLInterop
                       then initFromGL CL_DEVICE_TYPE_ALL -- initialize an OpenCL state for the device that is available for CL-GL-Interop
                       else clStateInit (detail ^. clDeviceDeviceId) -- initialize an OpenCL state for the first GPU that meets the criteria.
              putStrLn $ "Finished initializing OpenCL device."
              putStrLn $ dumpDeviceDetail detail
              -- Compiler options for OpenCL compiler.
              let options = [ CLFastRelaxedMath
                            , CLStrictAliasing
                            --, CLWarningIntoError
                            ]
              -- Get metadata from the openCL device.
              let device = clDevice state
              rasterSpec <- determineRasterSpec device
              let modifiedSrc = addDefinesToSource rasterSpec src
              putStrLn $ modifiedSrc 
              -- Compile the source.
              putStrLn $ "Starting OpenCL kernel compile"
              program <- loadProgramWOptions' options state modifiedSrc

              putStrLn $ "Finished OpenCL kernel compile"
              -- get the rasterizer kernel.
              rasterKernel <- program "multiTileRaster"

              -- Return a Library constructor with relevant information about the device for the rasterizer.
              return Rasterizer { _rasterClState  = state
                                , _rasterClKernel = rasterKernel
                                , _rasterUseGLInterop = useCLGLInterop
                                , _rasterSpec = rasterSpec
                                }
