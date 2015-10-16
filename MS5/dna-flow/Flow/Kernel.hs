{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Flow.Kernel
  ( DataRepr(..), ReprAccess(..)
  , NoRepr(..)
  , rangeKernel0, rangeKernel1
  , VectorRepr(..)
  , vecKernel0, vecKernel1, vecKernel2, vecKernel3
  , HalideRepr(..), DynHalideRepr(..), HalideReprClass(..)
  , Dim0, Dim1, Dim2, dim0, dim1, dim2
  , halideKernel0, halideKernel1, halideKernel2, halideKernel3
  -- * reexports (for FFI)
  , CInt(..), HalideKernel(..)
  ) where

import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.Vector.HFixed.Class ( Fn )

import Flow.Internal
import Flow.Builder
import Flow.Vector

import Flow.Halide.Marshal
import Flow.Halide.Types

import Foreign.C ( CInt(..) )

-- | No representation: Either don't produce anything (= nobody can use
-- result) or don't care about input (= accept any input).
data NoRepr a = NoRepr
  deriving Typeable
instance Typeable a => Show (NoRepr a) where
  show _ = "nothing [" ++ show (typeOf (undefined :: a)) ++ "]"
instance Typeable a => DataRepr (NoRepr a) where
  type ReprType (NoRepr a) = a
  reprNop _ = True
  reprAccess _ = ReadAccess
  reprCompatible _ _ = True

-- | Vector representation: A variable-sized "Vector" with @val@
-- elements, representing abstract data of type @abs@. This is
-- essentially our low-level representation, so outside of the value
-- type check this will add no safety.
data VectorRepr val abs = VectorRepr ReprAccess
  deriving Typeable
instance (Typeable val, Typeable abs) => Show (VectorRepr val abs) where
  show _ = show (typeOf (undefined :: val)) ++ " vector [" ++ show (typeOf (undefined :: abs)) ++ "]"
instance (Typeable val, Typeable abs) => DataRepr (VectorRepr val abs) where
  type ReprType (VectorRepr val abs) = abs
  reprNop _ = False
  reprAccess (VectorRepr acc)  = acc
  reprCompatible _ _ = True

rangeKernel0 :: DataRepr r
             => String -> r -> (Int -> Int -> IO (Vector a))
             -> Kernel (ReprType r)
rangeKernel0 name retRep code = kernel name Z retRep code'
  where code' _ [] = fail $ "kernel " ++ show name ++ ": Received wrong number of domains!"
        code' _ ds | RangeDomain (Range low high) <- last ds
                   = castVector <$> code low high

rangeKernel1 :: (DataRepr r, DataRepr r0)
             => String -> r0 -> r -> (Int -> Int -> Vector () -> IO (Vector a))
             -> Flow (ReprType r0) -> Kernel (ReprType r)
rangeKernel1 name repr0 retRep code = kernel name (repr0 :. Z) retRep code'
  where code' _   [] = fail $ "kernel " ++ show name ++ ": Received wrong number of domains!"
        code' [v] ds | RangeDomain (Range low high) <- last ds
                     = castVector <$> code low high (fst v)
        code' _   _  = fail $ "kernel " ++ show name ++ ": Received wrong number of arguments!"

vecKernel0 :: (Typeable val, Typeable abs)
           => String -> VectorRepr val abs -> IO (Vector val) -> Kernel abs
vecKernel0 name rrepr code = kernel name Z rrepr $ \_ _ -> castVector <$> code

vecKernel1 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val abs
           -> (Vector val0 -> IO (Vector val))
           -> Flow abs0 -> Kernel abs
vecKernel1 name repr0 rrepr code = kernel name (repr0 :. Z) rrepr $ \vs ds -> case (vs, ds) of
  ([vec], []) -> castVector <$> code (castVector $ fst vec)
  (_    , []) -> fail "vecKernel1: Received wrong number of input buffers!"
  (_    , _)  -> fail "vecKernel1: Received wrong number of domains!"

vecKernel2 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0, Typeable val1, Typeable abs1)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val1 abs1 -> VectorRepr val abs
           -> (Vector val0 -> Vector val1 -> IO (Vector val))
           -> Flow abs0 -> Flow abs1 -> Kernel abs
vecKernel2 name repr0 repr1 rrepr code = kernel name (repr0 :. repr1 :. Z) rrepr $ \case
  [vec,vec1] -> \case
    []     -> castVector <$> code (castVector (fst vec)) (castVector (fst vec1))
    _other -> fail "vecKernel2: Called for wrong number of domains!"
  _other     -> fail "vecKernel2: Received wrong number of input buffers!"

vecKernel3 :: ( Typeable val, Typeable abs, Typeable val0, Typeable abs0
              , Typeable val1, Typeable abs1, Typeable val2, Typeable abs2)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val1 abs1 -> VectorRepr val2 abs2 -> VectorRepr val abs
           -> (Vector val0 -> Vector val1 -> Vector val2 -> IO (Vector val))
           -> Flow abs0 -> Flow abs1 -> Flow abs2 -> Kernel abs
vecKernel3 name repr0 repr1 repr2 rrepr code = kernel name (repr0 :. repr1 :. repr2 :. Z) rrepr $ \case
  [vec,vec1,vec2] -> \case
    []     -> castVector <$> code (castVector (fst vec)) (castVector (fst vec1)) (castVector (fst vec2))
    _other -> fail "vecKernel3: Called for wrong number of domains!"
  _other          -> fail "vecKernel3: Received wrong number of input buffers!"

-- | Halide array of statically known (!) size. Scalar type is @val@,
-- dimensionality is given by @dim@, and @abs@ identifies the abstract
-- data type.
data HalideRepr dim val abs = HalideRepr dim
  deriving Typeable
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim) =>
         Show (HalideRepr dim val abs) where
  showsPrec _ (HalideRepr dim)
    = shows (typeOf (undefined :: val)) . showString " halide vector "
    . shows dim . showString " [" . shows (typeOf (undefined :: abs)) . showString "]"
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim, Eq dim) =>
         DataRepr (HalideRepr dim val abs) where
  type ReprType (HalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (HalideRepr _) = ReadAccess
  reprCompatible (HalideRepr d0) (HalideRepr d1) = d0 == d1

-- | Halide array of size given by a domain
data DynHalideRepr val abs = DynHalideRepr (DomainHandle Range)
  deriving Typeable
instance (Typeable val, Typeable abs, HalideScalar val) =>
         Show (DynHalideRepr val abs) where
  showsPrec _ (DynHalideRepr dim)
    = shows (typeOf (undefined :: val)) . showString " halide vector "
    . shows dim . showString " [" . shows (typeOf (undefined :: abs)) . showString "]"
instance (Typeable val, Typeable abs, HalideScalar val) =>
         DataRepr (DynHalideRepr val abs) where
  type ReprType (DynHalideRepr val abs) = abs
  reprNop _ = False
  reprAccess (DynHalideRepr _) = ReadAccess
  reprCompatible (DynHalideRepr d0) (DynHalideRepr d1)
    = d0 `dhIsParent` d1
  reprDomain (DynHalideRepr d) = [dhId d]
  reprMerge _ dvs [RangeDomain (Range low high)] = do
    out <- allocCVector (high - low) :: IO (Vector val)
    -- Populate vector. The caller should have made sure that the
    -- ranges actually cover the full vector.
    forM_ dvs $ \([RangeDomain (Range l h)], v) -> do
      forM_ [l..h-1] $ \i -> do
        pokeVector out (i-low) =<< peekVector (castVector v) (i-l)
    return $ Just $ castVector out
  reprMerge r _   doms = error $
    "reprMerge: Unexpected number of domains for " ++ show r ++ ": " ++ show (length doms)

type family KernelParams (reprs :: [*]) :: [*]
type instance KernelParams '[] = '[]
type instance KernelParams (repr ': reprs) = HalrParam repr ': KernelParams reprs

-- | Halide data representation. Instances of this type class can be
-- used with "halideKernel" and friends.
class (DataRepr r, HalideScalar (HalrVal r), MarshalArray (HalrDim r)) =>
      HalideReprClass r where

  -- | Halide dimension type. If this is "Z" we have a scalar,
  -- otherwise an array of the appropriate dimension.
  type HalrDim r

  -- | Value types of the array / scalar
  type HalrVal r

  -- | Get *concrete* dimensions of the Halide data
  -- representation. This might depend on the domain.
  halrDim :: r -> [Domain] -> HalrDim r

  -- | Type of Halide function that produces this data representation
  type HalideFun (xs :: [*]) r

  -- | Produce our result. Depending on data representation, this
  -- might pass extra data.
  halrCall :: forall xs. MarshalParams (KernelParams xs)
           => r -> Proxy xs
           -> HalideFun xs r -> [Domain]
           -> Fn (KernelParams xs) (IO (HalrParam r))

type HalrParam r = Array (HalrDim r) (HalrVal r)

instance ( Typeable dim, MarshalArray dim, Show dim, Eq dim
         , Typeable val, HalideScalar val
         , Typeable abs
         ) =>
         HalideReprClass (HalideRepr dim val abs) where
  type HalrDim (HalideRepr dim val abs) = dim
  type HalrVal (HalideRepr dim val abs) = val
  type HalideFun xs (HalideRepr dim val abs)
    = HalideKernel (KernelParams xs) (Array dim val)
  halrDim (HalideRepr d) _ = d
  halrCall r _ fun ds = call fun (halrDim r ds)

instance ( Typeable val, HalideScalar val
         , Typeable abs
         ) =>
         HalideReprClass (DynHalideRepr val abs) where
  type HalrDim (DynHalideRepr val abs) = Dim1
  type HalrVal (DynHalideRepr val abs) = val
  type HalideFun xs (DynHalideRepr val abs)
    = HalideKernel (KernelParams xs) (Array Dim1 val)
  halrDim _ [RangeDomain (Range low high)]
    = dim1 (fromIntegral low) (fromIntegral $ high - low)
  halrDim r doms
    = error $ "halrDim: Unexpected number of domains for " ++ show r ++ ": " ++ show (length doms)
  halrCall r _ fun doms
    = call fun (halrDim r doms)

halideKernel0 :: HalideReprClass rr
              => String
              -> rr
              -> HalideFun '[] rr
              -> Kernel (ReprType rr)
halideKernel0 name retR code = kernel name Z retR $ \_ ds -> do
  vecR <- halrCall retR (Proxy :: Proxy '[]) code ds
  return $ castVector $ arrayBuffer vecR

halideKernel1 :: forall rr r0. (HalideReprClass rr, HalideReprClass r0)
              => String
              -> r0 -> rr
              -> HalideFun '[r0] rr
              -> Flow (ReprType r0) -> Kernel (ReprType rr)
halideKernel1 name rep0 repR code = kernel name (rep0 :. Z) repR code'
  where code' [(v0,d0)] ds = do
         vecR <- halrCall repR (Proxy :: Proxy '[r0]) code ds
                          (Array (halrDim rep0 d0) (castVector v0))
         return $ castVector $ arrayBuffer vecR
        code' _ _ = fail "halideKernel1: Received wrong number of input buffers!"

halideKernel2 :: forall rr r0 r1. (HalideReprClass rr, HalideReprClass r0, HalideReprClass r1)
              => String
              -> r0 -> r1 -> rr
              -> HalideFun '[r0, r1] rr
              -> Flow (ReprType r0) -> Flow (ReprType r1) -> Kernel (ReprType rr)
halideKernel2 name rep0 rep1 repR code = kernel name (rep0 :. rep1 :. Z) repR code'
  where code' [(v0,d0), (v1,d1)] ds = do
         vecR <- halrCall repR (Proxy :: Proxy '[r0, r1]) code ds
                          (Array (halrDim rep0 d0) (castVector v0))
                          (Array (halrDim rep1 d1) (castVector v1))
         return $ castVector $ arrayBuffer vecR
        code' _ _ = fail "halideKernel2: Received wrong number of input buffers!"

halideKernel3 :: forall rr r0 r1 r2. (HalideReprClass rr, HalideReprClass r0,
                                      HalideReprClass r1, HalideReprClass r2)
              => String
              -> r0 -> r1 -> r2 -> rr
              -> HalideFun '[r0, r1, r2] rr
              -> Flow (ReprType r0) -> Flow (ReprType r1) -> Flow (ReprType r2) -> Kernel (ReprType rr)
halideKernel3 name rep0 rep1 rep2 repR code = kernel name (rep0 :. rep1 :. rep2 :. Z) repR code'
  where code' [(v0,d0), (v1,d1), (v2, d2)] ds = do
         vecR <- halrCall repR (Proxy :: Proxy '[r0, r1, r2]) code ds
                          (Array (halrDim rep0 d0) (castVector v0))
                          (Array (halrDim rep1 d1) (castVector v1))
                          (Array (halrDim rep2 d2) (castVector v2))
         return $ castVector $ arrayBuffer vecR
        code' _ _ = fail "halideKernel3: Received wrong number of input buffers!"
