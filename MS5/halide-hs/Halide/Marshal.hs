{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Marshalling of data between haskell and Halide kernels
module Halide.Marshal where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Vector.HFixed.Class  (Fn,Fun(..),Arity,curryFun,uncurryFun)
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable

import Halide.BufferT
import Halide.Types


----------------------------------------------------------------
-- Type guided marshalling
----------------------------------------------------------------

-- | Type class for scalars known by Halide
class Storable a => HalideScalar a where

instance HalideScalar Int32
instance HalideScalar Float
instance HalideScalar Double


-- | Call halide kernel and automatically marshal all parameters
call :: forall xs a. (MarshalParams xs, MarshalResult a)
     => Kernel xs a  -- ^ Kernel to call
     -> Extent a     -- ^ Size of result. Must be passed automatically
     -> Fn xs (IO a)
call kern size
  = unFun (go <$> marshalParams (unwrapK kern) :: Fun xs (IO a))
  where
    go act = do
      cont <- act
      marshalResult cont size

unwrapK :: Kernel xs a -> Fun (KernelCParams xs) (Ptr BufferT -> IO CInt)
unwrapK (Kernel f) = Fun f



-- | Type class for allocating buffers for results of Halide kernels
class MarshalResult a where
  marshalResult :: (Ptr BufferT -> IO CInt) -> Extent a -> IO a

instance HalideScalar a => MarshalResult (Scalar a) where
  marshalResult cont () = do
    (a,n) <- withScalarResult cont
    when (n /= 0) $ error "Could not call Halide kernel!"
    return a

instance (HalideScalar a, MarshalArray dim) => MarshalResult (Array dim a) where
  marshalResult cont dim = do
    arr <- allocArray dim
    buf <- wrapArray  arr
    n   <- withBufferT buf $ \pbuf ->
      cont pbuf
    when (n /= 0) $ error "Could not call Halide kernel!"
    return arr



-- | Marshal all parameters for function
marshalParams :: MarshalParams xs => Fun (KernelCParams xs) a -> Fun xs (IO a)
marshalParams fun = fmap ($ fun) <$> marshalParamsF


-- | Type class for marshalling of parameters for halide kernels
class Arity xs => MarshalParams xs where
  marshalParamsF :: Fun xs (IO (Fun (KernelCParams xs) a -> a))

instance MarshalParams '[] where
  marshalParamsF = Fun $ return unFun
         
instance (HalideScalar x, MarshalParams xs) => MarshalParams (Scalar x ': xs) where
  marshalParamsF = uncurryFun $ \(Scalar x) ->
    do contIO <- marshalParamsF
       return $ do
         cont <- contIO
         return $ \f -> cont (curryFun f x)
                                                              
instance (HalideScalar x, MarshalArray dim, MarshalParams xs) => MarshalParams (Array dim x ': xs) where
  marshalParamsF = uncurryFun $ \arr ->
    do contIO <- marshalParamsF
       return $ do
         cont <- contIO
         buf  <- wrapArray arr
         withBufferT buf $ \pbuf ->
           return $ \f -> cont (curryFun f pbuf)



----------------------------------------------------------------
-- Manual marshaling of arrays
----------------------------------------------------------------

-- | Type class for marshalling array of different dimensions
class MarshalArray dim where
  -- | Copy array information to the buffer_t for passing
  wrapArray  :: HalideScalar a => Array dim a -> IO BufferT
  -- | Allocate new array. Memory content is not initialized
  allocArray :: HalideScalar a => dim -> IO (Array dim a)
  allocArray dim = do
    arr <- mallocForeignPtrArray $ nOfElements dim
    return $ Array dim arr
  -- | Full array size
  nOfElements :: dim -> Int


-- | Allocate array for halide kernel which returns scalar value
withScalarResult :: forall a b. HalideScalar a => (Ptr BufferT -> IO b) -> IO (Scalar a,b)
withScalarResult action = do
  buf <- newBufferT
  setElemSize buf $ sizeOf (undefined :: a)
  setBufferStride  buf 1 0 0 0
  setBufferExtents buf 1 0 0 0
  alloca $ \ptr -> do
    setHostPtr buf (castPtr ptr)
    r <- withBufferT buf action
    a <- peek ptr
    return (Scalar a, r)

instance MarshalArray ((Int32,Int32) :. Z) where
  wrapArray (Array ((off,size) :. Z) arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf    1 0 0 0
    setBufferMin     buf  off 0 0 0
    setBufferExtents buf size 0 0 0
    withForeignPtr arr $ setHostPtr buf . castPtr
    return buf
  nOfElements ((_,n) :. Z) = fromIntegral n

sizeOfVal :: forall p a. HalideScalar a => p a -> Int
sizeOfVal _ = sizeOf (undefined :: a)
