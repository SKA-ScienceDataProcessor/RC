
module Profiling.CUDA.Activity
    ( cuptiEnable, cuptiDisable, cuptiFlush

    , CUptiMemLoc(..)
    , cuptiGetMemsetTime, cuptiGetMemcpyTime
    , cuptiGetKernelTime, cuptiGetOverheadTime
    , cuptiGetMemsetBytes, cuptiGetMemcpyBytes
    , cuptiGetMemcpyTimeTo, cuptiGetMemcpyBytesTo
    ) where

import Control.Monad (when)

import Data.Word

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import GHC.IO.Exception       ( IOException(..), IOErrorType(..) )

foreign import ccall unsafe "cupti_enable"
    c_cupti_enable :: IO CInt
foreign import ccall unsafe "cupti_disable"
    c_cupti_disable :: IO ()

foreign import ccall unsafe "cupti_getMemsetTime"
    c_cupti_getMemsetTime :: IO Word64
foreign import ccall unsafe "cupti_getMemcpyTime"
    c_cupti_getMemcpyTime :: CInt -> CInt -> IO Word64
foreign import ccall unsafe "cupti_getKernelTime"
    c_cupti_getKernelTime :: IO Word64
foreign import ccall unsafe "cupti_getOverheadTime"
    c_cupti_getOverheadTime :: IO Word64
--foreign import ccall unsafe "cupti_getDroppedRecords"
--    c_cupti_getDroppedRecords :: IO CInt
foreign import ccall unsafe "cupti_getMemsetBytes"
    c_cupti_getMemsetBytes :: IO Word64
foreign import ccall unsafe "cupti_getMemcpyBytes"
    c_cupti_getMemcpyBytes :: CInt -> CInt -> IO Word64

foreign import ccall unsafe "cuptiActivityFlushAll"
    c_cuptiActivityFlushAll :: Word32 -> IO CInt
foreign import ccall unsafe "cuptiGetResultString"
    c_cuptiGetResultString :: CInt -> Ptr CString -> IO ()

-- | Enables CUDA activity monitoring, starting the counters.
cuptiEnable :: IO ()
cuptiEnable = do
    res <- c_cupti_enable
    when (res /= 0) $ do
        alloca $ \pstr -> do
            c_cuptiGetResultString res pstr
            resStr <- peekCString =<< peek pstr
            ioError IOError { ioe_handle   = Nothing
                            , ioe_type     = IllegalOperation
                            , ioe_location = "cuptiActivityEnable"
                            , ioe_description = resStr
                            , ioe_errno    = Nothing
                            , ioe_filename = Nothing
                            }
-- | Disables CUDA activity monitoring
cuptiDisable :: IO ()
cuptiDisable = c_cupti_disable

-- | Flushes CUDA performance counters
cuptiFlush :: IO ()
cuptiFlush = do
    res <- c_cuptiActivityFlushAll 0
    when (res /= 0) $ do
        alloca $ \pstr -> do
            c_cuptiGetResultString res pstr
            resStr <- peekCString =<< peek pstr
            ioError IOError { ioe_handle   = Nothing
                            , ioe_type     = IllegalOperation
                            , ioe_location = "cuptiActivityFlushAll"
                            , ioe_description = resStr
                            , ioe_errno    = Nothing
                            , ioe_filename = Nothing
                            }

-- | Queries times. The return values are the sum of times spend on
-- certain actions. Note that with parallel execution, this might
-- advance faster than the clock.
cuptiGetMemsetTime, cuptiGetKernelTime, cuptiGetOverheadTime, cuptiGetMemsetBytes :: IO Integer
cuptiGetMemsetTime = fmap fromIntegral c_cupti_getMemsetTime
cuptiGetKernelTime = fmap fromIntegral c_cupti_getKernelTime
cuptiGetOverheadTime = fmap fromIntegral c_cupti_getOverheadTime
cuptiGetMemsetBytes = fmap fromIntegral c_cupti_getMemsetBytes

-- | A memory location for memory transfers
data CUptiMemLoc
    = CUptiHost
    | CUptiDevice
    | CUptiArray
    deriving (Eq, Enum)

allLocs :: [CUptiMemLoc]
allLocs = [(CUptiHost)..(CUptiArray)]

cuptiGetMemcpyTime :: CUptiMemLoc -> CUptiMemLoc -> IO Integer
cuptiGetMemcpyTime from to = fmap fromIntegral $
    c_cupti_getMemcpyTime (fromIntegral (fromEnum from)) (fromIntegral (fromEnum to))
cuptiGetMemcpyBytes :: CUptiMemLoc -> CUptiMemLoc -> IO Integer
cuptiGetMemcpyBytes from to = fmap fromIntegral $
    c_cupti_getMemcpyBytes (fromIntegral (fromEnum from)) (fromIntegral (fromEnum to))

cuptiGetMemcpyTimeTo :: CUptiMemLoc -> IO Integer
cuptiGetMemcpyTimeTo to = sum `fmap` mapM (flip cuptiGetMemcpyTime to) allLocs
cuptiGetMemcpyBytesTo :: CUptiMemLoc -> IO Integer
cuptiGetMemcpyBytesTo to = sum `fmap` mapM (flip cuptiGetMemcpyBytes to) allLocs
