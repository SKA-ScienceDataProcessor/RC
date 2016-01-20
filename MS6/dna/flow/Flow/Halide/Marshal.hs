{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP                 #-}
-- |
-- Marshalling of data between haskell and Halide kernels
module Flow.Halide.Marshal (
    -- * High level API
    call
  , callWrite
  , eraseTypes
  , arr2scalar
  , scalar2arr
    -- * Type classes
  , HalideScalar(..)
  , MarshalArray(..)
  , withScalarResult
  , MarshalResult(..)
  , MarshalParams(..)
  , HalideCanBind(..)
  ) where

import Control.Monad
import Data.Int
import Data.Vector.HFixed.Class  (Fn,Fun(..),Arity,curryFun,uncurryFun)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Flow.Halide.BufferT
import Flow.Halide.Types
import Flow.Vector
import Flow.Internal

----------------------------------------------------------------
-- Wrappers for calling kernels
----------------------------------------------------------------

-- | Call halide kernel and automatically marshal all parameters
call :: forall xs a. (MarshalParams xs, MarshalResult a)
     => HalideKernel xs a  -- ^ Kernel to call
     -> Extent a           -- ^ Size of result. Must be passed automatically
     -> Fn xs (IO a)
call kern size
  = unFun (go <$> marshalParams (unwrapK kern) :: Fun xs (IO a))
  where
    go act = do
      cont <- act
      marshalNewResult cont size

-- | Call halide kernel, initialising the output buffer with data from the
-- last given parameter.
callWrite :: forall xs a. (MarshalParams xs, MarshalResult a)
          => HalideKernel xs a -- ^ Kernel to call
          -> Fn xs (a -> IO a)
callWrite kern
  = unFun (go <$> marshalParams (unwrapK kern) :: Fun xs (a -> IO a))
  where
    go act ret = do
      cont <- act
      marshalResult cont ret

eraseTypes
  :: forall xs a. (MarshalParams xs, MarshalResult a)
  => HalideKernel xs a
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


unwrapK :: HalideKernel xs a -> Fun (KernelCParams xs) (Ptr BufferT -> IO CInt)
unwrapK (HalideKernel f) = Fun f

-- | Type class for adding constant parameters to a kernel. Meant for
-- configuration.
class HalideCanBind x kern where
  type HalideBind x kern
  halideBind :: HalideBind x kern -> x -> kern
instance HalideScalar x => HalideCanBind x (HalideKernel xs a) where
  type HalideBind x (HalideKernel xs a) = HalideKernel (Scalar x ': xs) a
  halideBind (HalideKernel f) x = HalideKernel (f x)

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
  return $ Array d (castVector ptr)
unwrapScalarArray _ = Nothing



-- | Type class for allocating buffers for results of Halide kernels
class MarshalResult a where
  marshalNewResult :: (Ptr BufferT -> IO CInt) -> Extent a -> IO a
  marshalResult    :: (Ptr BufferT -> IO CInt) -> a -> IO a
  wrapResult       :: a -> Box
  unwrapExtent     :: p a -> Box -> Maybe (Extent a)

instance HalideScalar a => MarshalResult (Scalar a) where
  marshalNewResult cont () = do
    (a,n) <- withScalarResult cont
    when (n /= 0) $ error "Could not call Halide kernel!"
    return a
  marshalResult cont _ = marshalNewResult cont ()
  wrapResult (Scalar a) = Number $ wrapScalar a
  unwrapExtent _ Unit = Just ()
  unwrapExtent _ _    = Nothing

instance (HalideScalar a, MarshalArray dim) => MarshalResult (Array dim a) where
  marshalNewResult cont dim = do
    arr <- allocArray dim
    marshalResult cont arr
  marshalResult cont arr = do
    buf <- allocBufferT  arr
    n   <- withBufferT buf $ \pbuf ->
      cont pbuf
    when (n /= 0) $ error "Could not call Halide kernel!"
    return arr
  wrapResult (Array dim p) =
    SomeArray (wrapScalar (0 :: a)) (wrapDimensions dim) (castVector p)
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

instance (HalideScalar x, MarshalArray ds, MarshalParams xs) =>
         MarshalParams (Array ds x ': xs) where
  marshalParamsF :: forall a. Fun (Array ds x ': xs) (IO (Fun (KernelCParams (Array ds x ': xs)) a -> a))
  marshalParamsF = uncurryFun $ \arr ->
    do contIO <- marshalParamsF :: Fun xs (IO (Fun (KernelCParams xs) a -> a))
       return $ case isScalar (undefined :: ds) of
         IsScalar -> do
           cont <- contIO
           Scalar x <- arr2scalar arr
           return $ \f -> cont (curryFun f x)
         NotScalar -> do
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
    arr <- allocCVector (nOfElements dim)
    return $ Array dim arr
  -- | Full array size
  nOfElements :: dim -> Int
  -- | Unwrap dimensions of an array
  unwrapDimensions :: [(Int32,Int32)] -> Maybe dim
  wrapDimensions   :: dim -> [(Int32,Int32)]
  -- | Case split for scalarity
  isScalar :: dim -> IsScalar dim

data IsScalar dim where
  IsScalar :: IsScalar Z
  NotScalar :: IsScalar (d0 :. d1)

-- | Convert scalar to 0-dimensional array
scalar2arr :: HalideScalar a => Scalar a -> IO (Array Z a)
scalar2arr (Scalar a) = do
  arr@(Array Z v) <- allocArray Z
  pokeVector v 0 a
  return arr

-- | Convert 0-dimensional array to scalar
arr2scalar :: HalideScalar a => Array Z a -> IO (Scalar a)
arr2scalar (Array Z v) =
  Scalar <$> peekVector v 0

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

instance MarshalArray Dim0 where
  allocBufferT (Array Z arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf 1 0 0 0
    setBufferMin     buf 1 0 0 0
    setBufferExtents buf 1 0 0 0
    case arr of
      CVector _ p -> setHostPtr buf (castPtr p)
#ifdef CUDA
      HostVector   _ (HostPtr p) -> setHostPtr   buf (castPtr p)
      DeviceVector _ p -> setDevicePtr buf (castPtr p)
#endif
    return buf
  nOfElements Z = 1
  unwrapDimensions [] = Just Z
  unwrapDimensions _  = Nothing
  wrapDimensions Z = []
  isScalar _ = IsScalar

instance MarshalArray Dim1 where
  allocBufferT (Array ((off,size) :. Z) arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf    1 0 0 0
    setBufferMin     buf  off 0 0 0
    setBufferExtents buf size 0 0 0
    case arr of
      CVector _ p -> setHostPtr buf (castPtr p)
#ifdef CUDA
      HostVector   _ (HostPtr p) -> setHostPtr   buf (castPtr p)
      DeviceVector _ p -> setDevicePtr buf (castPtr p)
#endif
    return buf
  nOfElements ((_,n) :. Z) = fromIntegral n
  unwrapDimensions [n] = Just (n :. Z)
  unwrapDimensions _   = Nothing
  wrapDimensions (n :. Z) = [n]
  isScalar _ = NotScalar

instance MarshalArray Dim2 where
  allocBufferT (Array ((off1,size1) :. (off, size) :. Z) arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf    1  size 0 0
    setBufferMin     buf  off  off1 0 0
    setBufferExtents buf size size1 0 0
    case arr of
      CVector _ p -> setHostPtr buf (castPtr p)
#ifdef CUDA
      HostVector   _ (HostPtr p) -> setHostPtr   buf (castPtr p)
      DeviceVector _ p -> setDevicePtr buf (castPtr p)
#endif
    return buf
  nOfElements ((_,n) :. (_,m) :. Z) = fromIntegral n * fromIntegral m
  unwrapDimensions [n,m] = Just (n :. m :. Z)
  unwrapDimensions _   = Nothing
  wrapDimensions (n :. m :. Z) = [n,m]
  isScalar _ = NotScalar

instance MarshalArray Dim3 where
  allocBufferT (Array ((off2,size2) :. (off1, size1) :. (off, size) :. Z) arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf    1  size (size*size1) 0
    setBufferMin     buf  off  off1         off2 0
    setBufferExtents buf size size1        size2 0
    case arr of
      CVector _ p -> setHostPtr buf (castPtr p)
#ifdef CUDA
      HostVector   _ (HostPtr p) -> setHostPtr   buf (castPtr p)
      DeviceVector _ p -> setDevicePtr buf (castPtr p)
#endif
    return buf
  nOfElements ((_,n) :. (_,m) :. (_, o) :. Z)
    = fromIntegral n * fromIntegral m * fromIntegral o
  unwrapDimensions [n,m,o] = Just (n :. m :. o :. Z)
  unwrapDimensions _   = Nothing
  wrapDimensions (n :. m :. o :. Z) = [n,m,o]
  isScalar _ = NotScalar

instance MarshalArray Dim4 where
  allocBufferT (Array ((off3,size3) :. (off2, size2) :. (off1, size1) :. (off, size) :. Z) arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf    1  size (size*size1) (size*size1*size2)
    setBufferMin     buf  off  off1         off2              off3
    setBufferExtents buf size size1        size2             size3
    case arr of
      CVector _ p -> setHostPtr buf (castPtr p)
#ifdef CUDA
      HostVector   _ (HostPtr p) -> setHostPtr   buf (castPtr p)
      DeviceVector _ p -> setDevicePtr buf (castPtr p)
#endif
    return buf
  nOfElements ((_,n) :. (_,m) :. (_, o) :. (_, p) :. Z)
    = fromIntegral n * fromIntegral m * fromIntegral o * fromIntegral p
  unwrapDimensions [n,m,o,p] = Just (n :. m :. o :. p :. Z)
  unwrapDimensions _   = Nothing
  wrapDimensions (n :. m :. o :. p :. Z) = [n,m,o,p]
  isScalar _ = NotScalar


sizeOfVal :: forall p a. HalideScalar a => p a -> Int
sizeOfVal _ = sizeOf (undefined :: a)
