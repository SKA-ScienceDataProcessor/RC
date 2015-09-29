{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C
import Foreign.Ptr
import Data.Int

import Halide.BufferT
import Halide.Types
import Halide.Marshal

import qualified Data.Vector.Storable as S



main = main1

main1 :: IO ()
main1 = do
  let off  = 110
      size = 20
      dim  = (off,size) :. Z
  f  <- call kern_generate_f dim
  g  <- call kern_generate_f dim
  pp <- call kern_dotp       dim f g
  a  <- call kern_sum        ()  pp
  print $ toVector f
  print $ toVector g
  print $ toVector pp
  print (a :: Scalar Float)
  return ()
  --
  
main2 :: IO ()
main2 = do
  let off  = 110 :: Int32
      sz   = 20  :: Int32
      dim  = (off,sz) :. Z
      size = (Extent $ wrapDimensions dim)
  --
  f  <- eraseTypes kern_generate_f []    size
  g  <- eraseTypes kern_generate_f []    size
  pp <- eraseTypes kern_dotp       [f,g] size
  a  <- eraseTypes kern_sum        [pp]  Unit
  --
  case a of
    Number n -> print n
    _        -> error "Oops!"
  -- g  <- call kern_generate_f dim
  -- pp <- call kern_dotp       dim f g
  -- a  <- call kern_sum        ()  pp
  -- print $ toVector f
  -- print $ toVector g
  -- print $ toVector pp
  -- print (a :: Scalar Float)
  return ()

  {-
main2 :: IO ()
main2 = do
  let off  = 0
      size = 20
      dim  = (off,size) :. Z
  -- Generate f
  f <- allocArray dim :: IO (Array Dim1 Float)
  do buf_f <- wrapArray f
     withBufferT buf_f $ \p -> 
       check =<< c_kern_generate_f p
  -- Generate g
  g <- allocArray dim :: IO (Array Dim1 Float)
  do buf_g <- wrapArray g
     withBufferT buf_g $ \p ->
       check =<< c_kern_generate_f p
  -- Generate pointwise product
  pp <- allocArray dim :: IO (Array Dim1 Float)
  do buf_f  <- wrapArray f
     buf_g  <- wrapArray g
     buf_pp <- wrapArray pp
     withBufferT buf_f    $ \p_f  ->
       withBufferT buf_g  $ \p_g  ->
       withBufferT buf_pp $ \p_pp ->
       check =<< c_kern_dotp p_f p_g p_pp
  -- Sum
  a <- do buf_pp <- wrapArray pp
          withBufferT buf_pp $ \p_pp  -> do
            (a,err) <- withScalarResult $ \p_sum ->
              c_kern_sum p_pp off size p_sum
            check err
            return a
  print $ toVector f
  print $ toVector g
  print $ toVector pp
  print (a :: Scalar Float)
  return ()
-}  


check :: CInt -> IO ()
check 0 = return ()
check n = error $ "Error calling Halide: " ++ show n


toVector :: S.Storable a => Array Dim1 a -> S.Vector a
toVector (Array ((_,n):.Z) ptr) = S.unsafeFromForeignPtr0 ptr (fromIntegral n)


----------------------------------------------------------------
-- FFI declarations
----------------------------------------------------------------

{-
foreign import ccall "kern_generate_f"
  c_kern_generate_f :: KernelCSig '[] (Array Dim1 Float)

foreign import ccall "kern_dotp"
  c_kern_dotp :: KernelCSig '[Array Dim1 Float, Array Dim1 Float] (Array Dim1 Float)

foreign import ccall "kern_sum"
  c_kern_sum :: KernelCSig '[Array Dim1 Float, Scalar Int32, Scalar Int32] (Scalar Float)
-}


foreign import ccall "kern_generate_f"
  kern_generate_f :: Kernel '[] (Array Dim1 Float)

foreign import ccall "kern_dotp"
  kern_dotp :: Kernel '[Array Dim1 Float, Array Dim1 Float] (Array Dim1 Float)

foreign import ccall "kern_sum"
  kern_sum :: Kernel '[Array Dim1 Float] (Scalar Float)
