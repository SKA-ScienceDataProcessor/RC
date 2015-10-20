{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO: Figure out how to lift this, see below.
{-# LANGUAGE UndecidableInstances #-}

module Flow.Halide
  ( -- * Data representation
    HalideRepr, DynHalideRepr, HalideReprClass(..)
  , halideRepr, dynHalideRepr
    -- * Halide extents
  , Dim, Dim0, Dim1, Dim2, Dim3, Dim4
  , dim0, dim1, (:.)(..), Z(..), nOfElements
    -- * Kernel wrappers
  , halideKernel0, halideKernel1, halideKernel2, halideKernel3
  , halideKernel1Write, halideKernel2Write
  , halideBind, HalideBind
    -- * reexports (for FFI)
  , CInt(..), HalideKernel(..)
  ) where

import Control.Monad

import Data.Typeable
import Data.Vector.HFixed.Class ( Fn )

import Flow.Internal
import Flow.Builder
import Flow.Vector

import Flow.Halide.Marshal
import Flow.Halide.Types

import Foreign.C ( CInt(..) )

-- | Halide array of statically known (!) size. Scalar type is @val@,
-- dimensionality is given by @dim@, and @abs@ identifies the abstract
-- data type.
data HalideRepr dim val abs = HalideRepr ReprAccess dim
  deriving Typeable
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim) =>
         Show (HalideRepr dim val abs) where
  showsPrec _ (HalideRepr _ dim)
    = shows (typeOf (undefined :: val)) . showString " halide vector "
    . shows dim . showString " [" . shows (typeOf (undefined :: abs)) . showString "]"
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim, Eq dim) =>
         DataRepr (HalideRepr dim val abs) where
  type ReprType (HalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (HalideRepr acc _) = acc
  reprCompatible (HalideRepr _ d0) (HalideRepr _ d1) = d0 == d1

-- | Constructor function for "HalideRepr". Returns a data
-- representation with "ReadAccess".
halideRepr :: dim -> HalideRepr dim val abs
halideRepr = HalideRepr ReadAccess

-- | One-dimensional Halide array of size given by a domain
data DynHalideRepr dim val abs = DynHalideRepr ReprAccess dim (DomainHandle Range)
  deriving Typeable
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim) =>
         Show (DynHalideRepr dim val abs) where
  showsPrec _ (DynHalideRepr _ dim dom)
    = shows (typeOf (undefined :: val)) . showString " halide vector "
    . shows dim . showString ", " . shows dom
    . showString " [" . shows (typeOf (undefined :: abs)) . showString "]"
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim, Eq dim) =>
         DataRepr (DynHalideRepr dim val abs) where
  type ReprType (DynHalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (DynHalideRepr acc _ _) = acc
  reprDomain (DynHalideRepr _ _ d) = [dhId d]
  reprCompatible (DynHalideRepr _ ex0 d0) (DynHalideRepr _ ex1 d1)
    = ex0 == ex1 && d0 `dhIsParent` d1
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

-- | Constructor function for "DynHalideRepr". Returns a data
-- representation with "ReadAccess".
dynHalideRepr :: dim -> DomainHandle Range -> DynHalideRepr dim val abs
dynHalideRepr = DynHalideRepr ReadAccess

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

  -- | Change data representation into a writeable one.
  halrWrite :: r -> r

  -- | Type of Halide function that produces this data representation
  type HalideFun (xs :: [*]) r

  -- | Produce our result. Depending on data representation, this
  -- might pass extra data.
  halrCall :: forall xs. MarshalParams (KernelParams xs)
           => r -> Proxy xs
           -> HalideFun xs r -> [Domain]
           -> Fn (KernelParams xs) (IO (HalrParam r))
  halrCallWrite :: forall xs. MarshalParams (KernelParams xs)
                => r -> Proxy xs
                -> HalideFun xs r
                -> Fn (KernelParams xs) (HalrParam r -> IO (HalrParam r))

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
  halrDim (HalideRepr _ d) _ = d
  halrWrite (HalideRepr _ d) = HalideRepr WriteAccess d
  halrCall      r _ fun ds = call fun (halrDim r ds)
  halrCallWrite _ _ fun    = callWrite fun

-- Here is where we need undecideable instance, regrettably
instance ( Typeable dim, MarshalArray (Dim :. dim), Show dim, Eq dim
         , Typeable val, HalideScalar val
         , Typeable abs
         ) =>
         HalideReprClass (DynHalideRepr dim val abs) where
  type HalrDim (DynHalideRepr dim val abs) = Dim :. dim
  type HalrVal (DynHalideRepr dim val abs) = val
  type HalideFun xs (DynHalideRepr dim val abs)
    = HalideKernel (KernelParams xs) (Array (Dim :. dim) val)
  halrDim (DynHalideRepr _ dim _) [RangeDomain (Range low high)]
    = (fromIntegral low, fromIntegral $ high - low) :. dim
  halrDim r doms
    = error $ "halrDim: Unexpected number of domains for " ++ show r ++ ": " ++ show (length doms)
  halrWrite (DynHalideRepr _ dim dh)
    = DynHalideRepr WriteAccess dim dh
  halrCall r _ fun doms
    = call fun (halrDim r doms)
  halrCallWrite _ _ fun
    = callWrite fun

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

halideKernel1Write
  :: forall rr r0. (HalideReprClass rr, HalideReprClass r0)
  => String
  -> r0 -> rr
  -> HalideFun '[r0] rr
  -> Flow (ReprType r0) -> Flow (ReprType rr) -> Kernel (ReprType rr)
halideKernel1Write name rep0 repR code = kernel name (rep0 :. (halrWrite repR) :. Z) repR code'
  where code' [(v0,d0), (v1,d1)] ds = do
         -- Should hold by construction.
         when (ds /= d1) $
           fail $ "halideKernel1Write: Domain mismatch between parameter and return value!"
         vecR <- halrCallWrite repR (Proxy :: Proxy '[r0]) code
                               (Array (halrDim rep0 d0) (castVector v0))
                               (Array (halrDim repR d1) (castVector v1))
         return $ castVector $ arrayBuffer vecR
        code' _ _ = fail "halideKernel1Write: Received wrong number of input buffers!"

halideKernel2Write
  :: forall rr r0 r1. (HalideReprClass rr, HalideReprClass r0, HalideReprClass r1)
  => String
  -> r0 -> r1 -> rr
  -> HalideFun '[r0, r1] rr
  -> Flow (ReprType r0) -> Flow (ReprType r1) -> Flow (ReprType rr) -> Kernel (ReprType rr)
halideKernel2Write name rep0 rep1 repR code = kernel name (rep0 :. rep1 :. (halrWrite repR) :. Z) repR code'
  where code' [(v0,d0),(v1,d1),(v2,d2)] ds = do
         -- Should hold by construction.
         when (ds /= d2) $
           fail $ "halideKernel2Write: Domain mismatch between parameter and return value!"
         vecR <- halrCallWrite repR (Proxy :: Proxy '[r0, r1]) code
                               (Array (halrDim rep0 d0) (castVector v0))
                               (Array (halrDim rep1 d1) (castVector v1))
                               (Array (halrDim repR d2) (castVector v2))
         return $ castVector $ arrayBuffer vecR
        code' _ _ = fail "halideKernel2Write: Received wrong number of input buffers!"
