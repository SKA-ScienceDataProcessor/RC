module Binner where

import Foreign
import Foreign.C
import Data.Complex
import Text.Printf

import OskarBinReader

-- In fact we it returns CInt, but we use Int to not clutter the code
foreign import ccall "bin"  cbin  :: CString -> CInt -> CInt -> CDouble -> CDouble -> Ptr (Complex Double) -> Ptr CDouble -> IO Int
foreign import ccall "binm" cbinm :: CString -> CInt -> CInt -> CDouble -> CDouble -> Ptr (Complex Double) -> Ptr CDouble -> IO Int

bin :: String -> Bool -> TaskData -> IO ()
bin prefix isHalfGCF td = throwIf_ (/= 0) (printf "Error %d while binning...") mkbin
  where
    binner = if isHalfGCF then cbinm else cbin
    fi = fromIntegral
    mkbin = withCString prefix $ \cpre -> binner cpre (fi $ tdChannels td) (fi $ tdPoints td) scale (tdWstep td) (tdVisibilies td) (tdUVWs td)
    -- FIXME: It is copypasted from GPUGridder.hs.
    -- Make the single definition!
    scale = (2048 - 124 - 1) / (tdMaxx td) -- 124 max hsupp
