
module Profiling.CUDA.Metrics
    ( cuptiMetricsInit
    , cuptiMetricsEnable
    , cuptiMetricsDisable
    , cuptiGetMetrics
    ) where

import Control.Monad (when, forM)

import Data.List (intercalate)
import Data.Word

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import GHC.IO.Exception       ( IOException(..), IOErrorType(..) )

foreign import ccall unsafe "cupti_metrics_init"
    c_cupti_metrics_init :: CString -> IO ()
foreign import ccall unsafe "cupti_metrics_enable"
    c_cupti_metrics_enable :: IO CInt
foreign import ccall unsafe "cupti_metrics_disable"
    c_cupti_metrics_disable :: IO ()
foreign import ccall unsafe "cupti_get_metrics_count"
    c_cupti_get_metrics_count :: IO CInt
foreign import ccall unsafe "cupti_get_metric"
    c_cupti_get_metric :: CInt -> IO Word64

foreign import ccall unsafe "cuptiGetResultString"
    c_cuptiGetResultString :: CInt -> Ptr CString -> IO ()

-- | Initialises CUPTI metrics with a list of metrics to
-- calculate. Due to hardware limits and our design, only certain
-- metrics and metric combinations will work correctly here. Things to
-- keep in mind:
--
--  * Some metrics will cause CUDA to generate instrumentation code.
--    This will obviously reduce performance. According to my tests,
--    this can be quite significant, so don't be surprised.
--
--  * This is hands-off-profiling, so we cannot work with any metrics
--    combination that would require us to do multiple passes. This
--    rules out having too many active at the same time.
--
--  * Furthermore, this rules out instrumentation alongside hardware
--    counters. This makes sense, as instrumentation would likely
--    have an influence on hardware counters?
--
cuptiMetricsInit :: [String] -> IO ()
cuptiMetricsInit metrics =
  withCString (intercalate "," metrics) c_cupti_metrics_init

-- | Enables CUDA metrics monitoring. Note that metrics will only work
-- for contexts that were created *after* this function was called!
cuptiMetricsEnable :: IO ()
cuptiMetricsEnable = do
    res <- c_cupti_metrics_enable
    when (res /= 0) $ do
        alloca $ \pstr -> do
            c_cuptiGetResultString res pstr
            resStr <- peekCString =<< peek pstr
            ioError IOError { ioe_handle   = Nothing
                            , ioe_type     = IllegalOperation
                            , ioe_location = "cuptiMetricsEnable"
                            , ioe_description = resStr
                            , ioe_errno    = Nothing
                            , ioe_filename = Nothing
                            }

-- | Disables CUDA activity monitoring
cuptiMetricsDisable :: IO ()
cuptiMetricsDisable = c_cupti_metrics_disable

-- | Gets list of metric values
cuptiGetMetrics :: IO [Word64]
cuptiGetMetrics = do
    count <- c_cupti_get_metrics_count
    forM [0..count-1] c_cupti_get_metric
