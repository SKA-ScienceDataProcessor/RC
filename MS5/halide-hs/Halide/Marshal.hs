{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Marshalling of data between haskell and Halide kernels
module Halide.Marshal (
    -- * Hihg level API
    call
  , eraseTypes  
    -- * Type classes
  , HalideScalar(..)
  , MarshalArray(..)
  , withScalarResult
  , MarshalResult(..)
  , MarshalParams(..)
  ) where

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
-- Wrappers for calling kernels
----------------------------------------------------------------

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


eraseTypes
  :: forall xs a. (MarshalParams xs, MarshalResult a)
  => Kernel xs a
  -> [Box]                      -- ^ Parameters
  -> Box                        -- ^ Extent of result
  -> IO Box                     -- ^ Result
eraseTypes kern param size
  = fmap wrapResult
  $ dynamicParams (Fun (call kern ext) :: Fun xs (IO a)) param
  where
    ext = case unwrapExtent ([] :: [a]) size of
            Just x  -> x
            Nothing -> error "Bad data for result's extent"


unwrapK :: Kernel xs a -> Fun (KernelCParams xs) (Ptr BufferT -> IO CInt)
unwrapK (Kernel f) = Fun f




----------------------------------------------------------------
-- Type guided marshalling
----------------------------------------------------------------

-- | Type class for scalars known by Halide
class (Num a, Storable a) => HalideScalar a where
  wrapScalar   :: a -> ScalarVal
  unwrapScalar :: ScalarVal -> Maybe a
  
instance HalideScalar Int32 where
  wrapScalar   = Int32
  unwrapScalar (Int32 i) = Just i
  unwrapScalar _         = Nothing
instance HalideScalar Float where
  wrapScalar = Float
  unwrapScalar (Float i) = Just i
  unwrapScalar _         = Nothing
instance HalideScalar Double where
  wrapScalar = Double
  unwrapScalar (Double i) = Just i
  unwrapScalar _          = Nothing

unwrapScalarBox :: HalideScalar a => Box -> Maybe a
unwrapScalarBox (Number a) = unwrapScalar a
unwrapScalarBox _          = Nothing

unwrapScalarArray
  :: forall dim a. (HalideScalar a, MarshalArray dim)
  => Box -> Maybe (Array dim a)
unwrapScalarArray (SomeArray ty dim ptr) = do
  _ :: a <- unwrapScalar     ty
  d      <- unwrapDimensions dim
  return $ Array d (castForeignPtr ptr)
unwrapScalarArray _ = Nothing



-- | Type class for allocating buffers for results of Halide kernels
class MarshalResult a where
  marshalResult :: (Ptr BufferT -> IO CInt) -> Extent a -> IO a
  wrapResult    :: a -> Box
  unwrapExtent  :: p a -> Box -> Maybe (Extent a)

instance HalideScalar a => MarshalResult (Scalar a) where
  marshalResult cont () = do
    (a,n) <- withScalarResult cont
    when (n /= 0) $ error "Could not call Halide kernel!"
    return a
  wrapResult (Scalar a) = Number $ wrapScalar a
  unwrapExtent _ Unit = Just ()
  unwrapExtent _ _    = Nothing

instance (HalideScalar a, MarshalArray dim) => MarshalResult (Array dim a) where
  marshalResult cont dim = do
    arr <- allocArray dim
    buf <- allocBufferT  arr
    n   <- withBufferT buf $ \pbuf ->
      cont pbuf
    when (n /= 0) $ error "Could not call Halide kernel!"
    return arr
  wrapResult (Array dim p) =
    SomeArray (wrapScalar (0 :: a)) (wrapDimensions dim) (castForeignPtr p)
  unwrapExtent _ (Extent dim) = unwrapDimensions dim
  unwrapExtent _ _            = Nothing

-- | Marshal all parameters for function
marshalParams :: MarshalParams xs => Fun (KernelCParams xs) a -> Fun xs (IO a)
marshalParams fun = fmap ($ fun) <$> marshalParamsF


-- | Type class for marshalling of parameters for halide kernels
class Arity xs => MarshalParams xs where
  -- | Wrap all parameters for calling halide
  marshalParamsF :: Fun xs (IO (Fun (KernelCParams xs) a -> a))
  -- | Convert function with statically known types to 
  dynamicParams :: Fun xs a -> [Box] -> a

instance MarshalParams '[] where
  marshalParamsF = Fun $ return unFun
  dynamicParams f [] = unFun f
  dynamicParams _ _  = error "Too many parameters!"

instance (HalideScalar x, MarshalParams xs) => MarshalParams (Scalar x ': xs) where
  marshalParamsF = uncurryFun $ \(Scalar x) ->
    do contIO <- marshalParamsF
       return $ do
         cont <- contIO
         return $ \f -> cont (curryFun f x)
  dynamicParams _   [] = error "Too few parameters!"
  dynamicParams fun (dyn:rest) =
    case unwrapScalarBox dyn of
      Just n  -> dynamicParams (curryFun fun (Scalar n)) rest
      Nothing -> error "Type mismatch!"

instance (HalideScalar x, MarshalArray dim, MarshalParams xs) => MarshalParams (Array dim x ': xs) where
  marshalParamsF = uncurryFun $ \arr ->
    do contIO <- marshalParamsF
       return $ do
         cont <- contIO
         buf  <- allocBufferT arr
         withBufferT buf $ \pbuf ->
           return $ \f -> cont (curryFun f pbuf)
  dynamicParams _   [] = error "Too few parameters!"
  dynamicParams fun (dyn:rest) =
    case unwrapScalarArray dyn of
      Just arr -> dynamicParams (curryFun fun arr) rest
      Nothing  -> error "Type mismatch!"



----------------------------------------------------------------
-- Manual marshaling of arrays
----------------------------------------------------------------

-- | Type class for marshalling array of different dimensions
class MarshalArray dim where
  -- | Copy array information to the buffer_t for passing
  allocBufferT  :: HalideScalar a => Array dim a -> IO BufferT
  -- | Allocate new array. Memory content is not initialized
  allocArray :: HalideScalar a => dim -> IO (Array dim a)
  allocArray dim = do
    arr <- mallocForeignPtrArray $ nOfElements dim
    return $ Array dim arr
  -- | Full array size
  nOfElements :: dim -> Int
  -- | Unwrap dimensions of an array
  unwrapDimensions :: [(Int32,Int32)] -> Maybe dim
  wrapDimensions   :: dim -> [(Int32,Int32)]


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
  allocBufferT (Array ((off,size) :. Z) arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf    1 0 0 0
    setBufferMin     buf  off 0 0 0
    setBufferExtents buf size 0 0 0
    withForeignPtr arr $ setHostPtr buf . castPtr
    return buf
  nOfElements ((_,n) :. Z) = fromIntegral n
  unwrapDimensions [n] = Just (n :. Z)
  unwrapDimensions _   = Nothing
  wrapDimensions (n :. Z) = [n]


sizeOfVal :: forall p a. HalideScalar a => p a -> Int
sizeOfVal _ = sizeOf (undefined :: a)
