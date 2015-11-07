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
type DynHalideRepr dim val abs = RangeRepr (HalideRepr dim val abs)

-- | Halide array where the size in one dimension is given by a bin domain
type BinHalideRepr dim val abs = BinRepr (HalideRepr dim val abs)

-- | Constructor function for "DynHalideRepr". Returns a data
-- representation with "ReadAccess".
dynHalideRepr :: dim -> Domain Range -> DynHalideRepr dim val abs
dynHalideRepr dim dom = RangeRepr dom (HalideRepr ReadAccess dim)

-- | Constructor function for "BinHalideRepr". Returns a data
-- representation with "ReadAccess".
binHalideRepr :: dim -> Domain Bins -> BinHalideRepr dim val abs
binHalideRepr dim dom = BinRepr dom (HalideRepr ReadAccess dim)

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

instance (Typeable dom, Typeable rep, HalideReprClass rep) =>
         HalideReprClass (RegionRepr dom rep) where
  type HalrDim (RegionRepr dom rep) = HalrDim rep
  type HalrVal (RegionRepr dom rep) = HalrVal rep
  type HalideFun xs (RegionRepr dom rep) = HalideFun xs rep
  halrDim (RegionRepr _ rep) ds = halrDim rep ds
  halrWrite (RegionRepr dh rep) = RegionRepr dh (halrWrite rep)
  halrCall rep _ _ []
    = error $ "Not enough domains passed to halrCall for " ++ show rep ++ "!"
  halrCall      (RegionRepr _ rep) xs fun (_:doms) = halrCall rep xs fun doms
  halrCallWrite (RegionRepr _ rep) xs fun          = halrCallWrite rep xs fun

-- Here is where we need undecideable instance, regrettably
instance (HalideReprClass rep, MarshalArray (Dim :. HalrDim rep)) =>
         HalideReprClass (RangeRepr rep) where
  type HalrDim (RangeRepr rep) = Dim :. HalrDim rep
  type HalrVal (RangeRepr rep) = HalrVal rep
  type HalideFun xs (RangeRepr rep)
    = HalideKernel (KernelParams xs) (Array (Dim :. HalrDim rep) (HalrVal rep))
  halrDim (RangeRepr _ rep) ((RangeRegion _ (Range low high)):rbox)
    = (fromIntegral low, fromIntegral $ high - low) :. halrDim rep rbox
  halrDim r doms
     = error $ "halrDim: number/types of domains for " ++ show r ++ ": " ++ show doms
  halrWrite (RangeRepr dh rep) = RangeRepr dh (halrWrite rep)
  halrCall      r _ fun doms = call fun (halrDim r doms)
  halrCallWrite _ _ fun      = callWrite fun

instance (HalideReprClass rep, MarshalArray (Dim :. HalrDim rep)) =>
         HalideReprClass (BinRepr rep) where
  type HalrDim (BinRepr rep) = Dim :. HalrDim rep
  type HalrVal (BinRepr rep) = HalrVal rep
  type HalideFun xs (BinRepr rep)
    = HalideKernel (KernelParams xs) (Array (Dim :. HalrDim rep) (HalrVal rep))
  halrDim (BinRepr _ rep) ((BinRegion _ (Bins binMap)):ds)
    = (0, fromIntegral $ sum $ Map.elems binMap) :. halrDim rep ds
    -- Note that in contrast to RangeRepr, we cannot communicate
    -- the array transformation to Halide here (after all, it is
    -- decidedly non-linear). Instead we base every bin at index 0 -
    -- a minor inconsistency?
  halrDim r doms
    = error $ "halrDim: Unexpected number/types of domains for " ++ show r ++ ": " ++ show doms
  halrWrite (BinRepr dh rep) = BinRepr dh (halrWrite rep)
  halrCall      r _ fun doms = call fun (halrDim r doms)
  halrCallWrite _ _ fun      = callWrite fun

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
halideKernel1Write name rep0 repR code = foldingKernel name (rep0 :. (halrWrite repR) :. Z) code'
  where code' [(v0,d0)] v1 ds = do
         vecR <- halrCallWrite repR (Proxy :: Proxy '[r0]) code
                               (Array (halrDim rep0 d0) (castVector v0))
                               (Array (halrDim repR ds) (castVector v1))
         return $ castVector $ arrayBuffer vecR
        code' _ _ _ = fail "halideKernel1Write: Received wrong number of input buffers!"

halideKernel2Write
  :: forall rr r0 r1. (HalideReprClass rr, HalideReprClass r0, HalideReprClass r1)
  => String
  -> r0 -> r1 -> rr
  -> HalideFun '[r0, r1] rr
  -> Flow (ReprType r0) -> Flow (ReprType r1) -> Flow (ReprType rr) -> Kernel (ReprType rr)
halideKernel2Write name rep0 rep1 repR code = foldingKernel name (rep0 :. rep1 :. (halrWrite repR) :. Z) code'
  where code' [(v0,d0),(v1,d1)] v2 ds = do
         vecR <- halrCallWrite repR (Proxy :: Proxy '[r0, r1]) code
                               (Array (halrDim rep0 d0) (castVector v0))
                               (Array (halrDim rep1 d1) (castVector v1))
                               (Array (halrDim repR ds) (castVector v2))
         return $ castVector $ arrayBuffer vecR
        code' _ _ _ = fail "halideKernel2Write: Received wrong number of input buffers!"

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
