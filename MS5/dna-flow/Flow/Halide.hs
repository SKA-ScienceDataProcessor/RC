{-# LANGUAGE ConstraintKinds #-}
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
    HalideRepr, DynHalideRepr, BinHalideRepr, HalideReprClass(..)
  , halideRepr, dynHalideRepr, binHalideRepr
    -- * Halide extents
  , Dim, Dim0, Dim1, Dim2, Dim3, Dim4
  , dim0, dim1, (:.)(..), Z(..), nOfElements
    -- * Kernel wrappers
  , halideKernel0, halideKernel1, halideKernel2, halideKernel3
  , halideKernel1Write, halideKernel2Write
  , halideBind, HalideBind
  , halideDump, halideTextDump2D
    -- * reexports (for FFI)
  , CInt(..), HalideKernel(..)
  ) where

import Control.Monad

import qualified Data.Map as Map
import Data.Typeable
import Data.Vector.HFixed.Class ( Fn )

import Flow.Internal
import Flow.Builder
import Flow.Vector
import Flow.Kernel

import Flow.Halide.Marshal
import Flow.Halide.Types

import Foreign.C ( CInt(..) )
import Foreign.Storable ( sizeOf )

import System.IO

-- | Common context of all halide data representations: Everything must be
-- typeable so we can "cast" data representations for the type
-- check. For the same reason we also need an "Eq" instance for
-- comparing data dimensions and a "Show" message to provide a helpful
-- error message.  Finally, we must obviously be able to marshal
-- arrays with the given dimensions "dim" and underlying values "val".
type HalideCtx dim val abs =
  ( Typeable dim, MarshalArray dim, Show dim, Eq dim
  , Typeable val, HalideScalar val
  , Typeable abs
  )

-- | Halide array of statically known (!) size. Scalar type is @val@,
-- dimensionality is given by @dim@, and @abs@ identifies the abstract
-- data type.
data HalideRepr dim val abs = HalideRepr ReprAccess dim
  deriving Typeable
instance HalideCtx dim val abs => Show (HalideRepr dim val abs) where
  showsPrec _ (HalideRepr _ dim)
    = shows (typeOf (undefined :: abs)) . showString " as halide vector " .
      shows (typeOf (undefined :: val)) . shows dim
instance HalideCtx dim val abs => DataRepr (HalideRepr dim val abs) where
  type ReprType (HalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (HalideRepr acc _) = acc
  reprCompatible (HalideRepr _ d0) (HalideRepr _ d1) = d0 == d1
  reprSize r ds = Just $ nOfElements (halrDim r ds) * sizeOf (undefined :: val)

-- | Constructor function for "HalideRepr". Returns a data
-- representation with "ReadAccess".
halideRepr :: dim -> HalideRepr dim val abs
halideRepr = HalideRepr ReadAccess

-- | Halide array where the size in one dimension is given by a domain
data DynHalideRepr dim val abs = DynHalideRepr ReprAccess dim (Domain Range)
  deriving Typeable
instance HalideCtx dim val abs => Show (DynHalideRepr dim val abs) where
  showsPrec _ (DynHalideRepr _ dim dom)
    = shows (typeOf (undefined :: abs)) . showString " as halide vector " .
      shows (typeOf (undefined :: val)) . ('[':) . shows dom . (',':) . tail . shows dim
instance (HalideCtx dim val abs, MarshalArray (Dim :. dim))
         => DataRepr (DynHalideRepr dim val abs) where
  type ReprType (DynHalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (DynHalideRepr acc _ _) = acc
  reprDomain (DynHalideRepr _ _ d) = [dhId d]
  reprCompatible (DynHalideRepr _ ex0 d0) (DynHalideRepr _ ex1 d1)
    = ex0 == ex1 && d0 `dhIsParent` d1
  reprMerge _ dvs [RangeRegion _ (Range low high)] = do
    out <- allocCVector (high - low) :: IO (Vector val)
    -- Populate vector. The caller should have made sure that the
    -- ranges actually cover the full vector.
    forM_ (Map.toList dvs) $ \([RangeRegion _ (Range l h)], v) -> do
      forM_ [l..h-1] $ \i -> do
        pokeVector out (i-low) =<< peekVector (castVector v) (i-l)
    return $ Just $ castVector out
  reprMerge r _   doms = error $
    "reprMerge: Unexpected number/types of domains for " ++ show r ++ ": " ++ show doms
  reprSize r ds = Just $ nOfElements (halrDim r ds) * sizeOf (undefined :: val)

-- | Halide array where the size in one dimension is given by a bin domain
data BinHalideRepr dim val abs = BinHalideRepr ReprAccess dim (Domain Bins)
  deriving Typeable
instance HalideCtx dim val abs => Show (BinHalideRepr dim val abs) where
  showsPrec _ (BinHalideRepr _ dim dom)
    = shows (typeOf (undefined :: abs)) . showString " as halide vector " .
      shows (typeOf (undefined :: val)) . ('[':) . shows dom . (',':) . tail . shows dim
instance (HalideCtx dim val abs, MarshalArray (Dim :. dim))
         => DataRepr (BinHalideRepr dim val abs) where
  type ReprType (BinHalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (BinHalideRepr acc _ _) = acc
  reprDomain (BinHalideRepr _ _ d) = [dhId d]
  reprCompatible (BinHalideRepr _ ex0 d0) (BinHalideRepr _ ex1 d1)
    = ex0 == ex1 && d0 `dhIsParent` d1
  reprMerge _ _ [BinRegion _ _bins] =
    -- TODO
    fail "reprMerge not implemented yet on BinHalideRepr!"
  reprMerge r _   doms = error $
    "reprMerge: Unexpected number/types of domains for " ++ show r ++ ": " ++ show doms
  reprSize r ds = Just $ nOfElements (halrDim r ds) * sizeOf (undefined :: val)

-- | Constructor function for "DynHalideRepr". Returns a data
-- representation with "ReadAccess".
dynHalideRepr :: dim -> Domain Range -> DynHalideRepr dim val abs
dynHalideRepr = DynHalideRepr ReadAccess

-- | Constructor function for "BinHalideRepr". Returns a data
-- representation with "ReadAccess".
binHalideRepr :: dim -> Domain Bins -> BinHalideRepr dim val abs
binHalideRepr = BinHalideRepr ReadAccess

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
  halrDim :: r -> RegionBox -> HalrDim r

  -- | Change data representation into a writeable one.
  halrWrite :: r -> r

  -- | Type of Halide function that produces this data representation
  type HalideFun (xs :: [*]) r

  -- | Produce our result. Depending on data representation, this
  -- might pass extra data.
  halrCall :: forall xs. MarshalParams (KernelParams xs)
           => r -> Proxy xs
           -> HalideFun xs r -> RegionBox
           -> Fn (KernelParams xs) (IO (HalrParam r))
  halrCallWrite :: forall xs. MarshalParams (KernelParams xs)
                => r -> Proxy xs
                -> HalideFun xs r
                -> Fn (KernelParams xs) (HalrParam r -> IO (HalrParam r))

type HalrParam r = Array (HalrDim r) (HalrVal r)

instance HalideCtx dim val abs => HalideReprClass (HalideRepr dim val abs) where
  type HalrDim (HalideRepr dim val abs) = dim
  type HalrVal (HalideRepr dim val abs) = val
  type HalideFun xs (HalideRepr dim val abs)
    = HalideKernel (KernelParams xs) (Array dim val)
  halrDim (HalideRepr _ d) _ = d
  halrWrite (HalideRepr _ d) = HalideRepr WriteAccess d
  halrCall      r _ fun ds = call fun (halrDim r ds)
  halrCallWrite _ _ fun    = callWrite fun

-- Here is where we need undecideable instance, regrettably
instance (HalideCtx dim val abs, MarshalArray (Dim :. dim)) => HalideReprClass (DynHalideRepr dim val abs) where
  type HalrDim (DynHalideRepr dim val abs) = Dim :. dim
  type HalrVal (DynHalideRepr dim val abs) = val
  type HalideFun xs (DynHalideRepr dim val abs)
    = HalideKernel (KernelParams xs) (Array (Dim :. dim) val)
  halrDim (DynHalideRepr _ dim _) [RangeRegion _ (Range low high)]
    = (fromIntegral low, fromIntegral $ high - low) :. dim
  halrDim r doms
    = error $ "halrDim: Unexpected number/types of domains for " ++ show r ++ ": " ++ show doms
  halrWrite (DynHalideRepr _ dim dh)
    = DynHalideRepr WriteAccess dim dh
  halrCall r _ fun doms
    = call fun (halrDim r doms)
  halrCallWrite _ _ fun
    = callWrite fun

instance (HalideCtx dim val abs, MarshalArray (Dim :. dim)) =>
         HalideReprClass (BinHalideRepr dim val abs) where
  type HalrDim (BinHalideRepr dim val abs) = Dim :. dim
  type HalrVal (BinHalideRepr dim val abs) = val
  type HalideFun xs (BinHalideRepr dim val abs)
    = HalideKernel (KernelParams xs) (Array (Dim :. dim) val)
  halrDim (BinHalideRepr _ dim _) [BinRegion _ (Bins binMap)]
    | [size] <- Map.elems binMap
    = (0, fromIntegral size) :. dim
    -- Note that in contrast to DynHalideRepr, we cannot communicate
    -- the array transformation to Halide here (after all, it is
    -- decidedly non-linear). Instead we base every bin at index 0 -
    -- a minor inconsistency?
  halrDim r doms
    = error $ "halrDim: Unexpected number/types of domains for " ++ show r ++ ": " ++ show doms
  halrWrite (BinHalideRepr _ dim dh)
    = BinHalideRepr WriteAccess dim dh
  halrCall r _ fun doms
    = call fun (halrDim r doms)
  halrCallWrite _ _ fun
    = callWrite fun

instance (Typeable dom, Typeable rep, HalideReprClass rep) =>
         HalideReprClass (RegionRepr dom rep) where
  type HalrDim (RegionRepr dom rep) = HalrDim rep
  type HalrVal (RegionRepr dom rep) = HalrVal rep
  type HalideFun xs (RegionRepr dom rep) = HalideFun xs rep
  halrDim (RegionRepr _ rep) ds = halrDim rep ds
  halrWrite (RegionRepr dh rep) = RegionRepr dh (halrWrite rep)
  halrCall rep _ _ []
    = error $ "Not enough domains passed to halrCall for " ++ show rep ++ "!"
  halrCall (RegionRepr _ rep) xs fun (_:doms)
    = halrCall rep xs fun doms
  halrCallWrite (RegionRepr _ rep) xs fun
    = halrCallWrite rep xs fun

halideKernel0 :: HalideReprClass rr
              => String
              -> rr
              -> HalideFun '[] rr
              -> Kernel (ReprType rr)
halideKernel0 name retR code = mergingKernel name Z retR $ \_ ds -> do
  vecR <- halrCall retR (Proxy :: Proxy '[]) code ds
  return $ castVector $ arrayBuffer vecR

halideKernel1 :: forall rr r0. (HalideReprClass rr, HalideReprClass r0)
              => String
              -> r0 -> rr
              -> HalideFun '[r0] rr
              -> Flow (ReprType r0) -> Kernel (ReprType rr)
halideKernel1 name rep0 repR code = mergingKernel name (rep0 :. Z) repR code'
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
halideKernel2 name rep0 rep1 repR code = mergingKernel name (rep0 :. rep1 :. Z) repR code'
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
halideKernel3 name rep0 rep1 rep2 repR code = mergingKernel name (rep0 :. rep1 :. rep2 :. Z) repR code'
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
halideKernel1Write name rep0 repR code = mergingKernel name (rep0 :. (halrWrite repR) :. Z) repR code'
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
halideKernel2Write name rep0 rep1 repR code = mergingKernel name (rep0 :. rep1 :. (halrWrite repR) :. Z) repR code'
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

-- | Simple kernel that dumps the contents of a channel with Halide
-- data representation to a file.
halideDump :: forall r. HalideReprClass r => r -> FilePath -> Flow (ReprType r) -> Kernel ()
halideDump rep file = mergingKernel (show (typeOf (undefined :: ReprType r)) ++ "-writer")
                                    (rep :. Z) NoRepr $ \[(v,doms)] _ -> do
  let v' = castVector v :: Vector (HalrVal r)
      size = nOfElements $ halrDim rep doms
  dumpVector' v' 0 size file
  return nullVector

-- | Simple kernel that dumps the contents of a channel with Halide
-- data representation to a text file
halideTextDump2D :: forall r. (HalideReprClass r, HalrDim r ~ Dim2, Show (HalrVal r))
                 => r -> FilePath -> Flow (ReprType r) -> Kernel ()
halideTextDump2D rep file = mergingKernel (show (typeOf (undefined :: ReprType r)) ++ "-text-writer")
                                          (rep :. Z) NoRepr $ \[(v,doms)] _ ->
  withFile file WriteMode $ \h -> do
    let v' = castVector v :: Vector (HalrVal r)
        ((_,hgt) :. (_, wdt) :. Z) = halrDim rep doms
    forM_ [0..hgt-1] $ \y -> do
      vals <- forM [0..wdt-1] $ \x -> peekVector v' (fromIntegral $ y*wdt+x)
      hPutStrLn h $ show vals
    return nullVector