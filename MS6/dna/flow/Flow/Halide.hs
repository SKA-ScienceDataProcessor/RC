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
    HalideReprClass(..)
  , HalideRepr, DynHalideRepr, BinHalideRepr, MarginRepr
  , halideRepr, dynHalideRepr, binHalideRepr, marginRepr
    -- * Halide extents
  , Dim, Dim0, Dim1, Dim2, Dim3, Dim4
  , dim0, dim1, (:.)(..), Z(..), nOfElements
    -- * Kernel wrappers
  , halideKernel0, halideKernel1, halideKernel2, halideKernel3
  , halideKernel1Write, halideKernel2Write
  , halideBind, HalideBind
  , halidePrint, halideDump, halideReadDump
  , halideTextDump2D, halideTextDump4D
    -- * reexports (for FFI)
  , CInt(..), HalideKernel(..)
  ) where

import Control.Monad

import qualified Data.Map as Map
import Data.Typeable
import Data.Vector.HFixed.Class ( Fn )

import Flow.Internal
import Flow.Vector
import Flow.Kernel
import Flow.Domain

import Flow.Halide.Marshal
import Flow.Halide.Types

import Foreign.C ( CInt(..) )
import Foreign.Storable ( sizeOf )

import System.IO
import Data.List ( intercalate )
import Text.Printf ( printf )

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
  showsPrec _ = reprShows []
instance HalideCtx dim val abs => DataRepr (HalideRepr dim val abs) where
  type ReprType (HalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (HalideRepr acc _) = acc
  reprCompatible (HalideRepr _ d0) (HalideRepr _ d1) = d0 == d1
  reprSize r ds = Just $ nOfElements (halrDim r ds) * sizeOf (undefined :: val)
  reprShowsName (HalideRepr _ _) =
    showString "halide vector " . shows (typeOf (undefined :: val))
  reprShows ds r@(HalideRepr _ dim) = reprShowsDefault (showDims ++ ds) r
   where showsDim (m,ext) = shows m $ (':':) $ shows (m+ext) ""
         showDims = map showsDim $ reverse $ wrapDimensions dim

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

instance (HalideReprClass rep, MarshalArray (Dim :. HalrDim rep)) =>
         HalideReprClass (RegionRepr Range rep) where
  type HalrDim (RegionRepr Range rep) = Dim :. HalrDim rep
  type HalrVal (RegionRepr Range rep) = HalrVal rep
  type HalideFun xs (RegionRepr Range rep)
    = HalideKernel (KernelParams xs) (Array (Dim :. HalrDim rep) (HalrVal rep))
  halrDim (RegionRepr _ rep) ((RangeRegion _ (Range low _)):rbox)
    = (fromIntegral low, 1) :. halrDim rep rbox
    -- Note that this is also fudging the facts quite a bit - the
    -- region size is clearly 1, but we can still set the minimum
    -- field however we want, so we can use "min" in order to pass the
    -- region to Halide. This is obviously - again - slightly hacky
    -- and the kernel needs to expect it.
  halrDim r doms
     = error $ "halrDim: number/types of domains for " ++ show r ++ ": " ++ show doms
  halrWrite (RegionRepr dom rep) = RegionRepr dom (halrWrite rep)
  halrCall      r _ fun doms = call fun (halrDim r doms)
  halrCallWrite _ _ fun      = callWrite fun

instance (HalideReprClass rep, MarshalArray (HalrDim rep)) =>
         HalideReprClass (RegionRepr Bins rep) where
  type HalrDim (RegionRepr Bins rep) = HalrDim rep
  type HalrVal (RegionRepr Bins rep) = HalrVal rep
  type HalideFun xs (RegionRepr Bins rep)
    = HalideKernel (KernelParams xs) (Array (HalrDim rep) (HalrVal rep))
  halrDim (RegionRepr _ rep) (_:rbox)
    = halrDim rep rbox
  halrDim r doms
    = error $ "halrDim: number/types of domains for " ++ show r ++ ": " ++ show doms
  halrWrite (RegionRepr dom rep) = RegionRepr dom (halrWrite rep)
  halrCall      r _ fun doms = call fun (halrDim r doms)
  halrCallWrite _ _ fun      = callWrite fun

-- Here is where we need an undecideable instance, regrettably
instance (HalideReprClass rep, MarshalArray (Dim :. HalrDim rep)) =>
         HalideReprClass (ArrayRepr rep) where
  type HalrDim (ArrayRepr rep) = Dim :. HalrDim rep
  type HalrVal (ArrayRepr rep) = HalrVal rep
  type HalideFun xs (ArrayRepr rep)
    = HalideKernel (KernelParams xs) (Array (Dim :. HalrDim rep) (HalrVal rep))
  halrDim (ArrayRepr (l, h) rep) rbox
    = (fromIntegral l, fromIntegral (h-l)) :. halrDim rep rbox
  halrWrite (ArrayRepr dh rep) = ArrayRepr dh (halrWrite rep)
  halrCall      r _ fun doms = call fun (halrDim r doms)
  halrCallWrite _ _ fun      = callWrite fun

-- Here is where we need an undecideable instance, regrettably
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
    = (0, fromIntegral $ sum $ map (sum . Map.elems) $ Map.elems binMap) :. halrDim rep ds
    -- Note that in contrast to RangeRepr, we cannot communicate
    -- the array transformation to Halide here (after all, it is
    -- decidedly non-linear). Instead we base every bin at index 0 -
    -- a minor inconsistency?
  halrDim r doms
    = error $ "halrDim: Unexpected number/types of domains for " ++ show r ++ ": " ++ show doms
  halrWrite (BinRepr dh rep) = BinRepr dh (halrWrite rep)
  halrCall      r _ fun doms = call fun (halrDim r doms)
  halrCallWrite _ _ fun      = callWrite fun

instance (HalideReprClass rep, MarshalArray (Dim :. HalrDim rep)) =>
         HalideReprClass (MarginRepr rep) where
  type HalrDim (MarginRepr rep) = HalrDim (RangeRepr rep)
  type HalrVal (MarginRepr rep) = HalrVal (RangeRepr rep)
  type HalideFun xs (MarginRepr rep)
    = HalideKernel (KernelParams xs) (Array (HalrDim (RangeRepr rep)) (HalrVal rep))
  halrDim (MarginRepr ov rep) rbox
    = let (off, ext) :. dims = halrDim rep rbox
       in (off-fromIntegral ov, ext+2*fromIntegral ov) :. dims
  halrWrite (MarginRepr dh rep) = MarginRepr dh (halrWrite rep)
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

-- | Simple kernel that shows the contents of a buffer as text
halidePrint :: forall r. (HalideReprClass r, Show (HalrVal r))
            => r -> String -> Flow (ReprType r) -> Kernel ()
halidePrint rep caption = mergingKernel (show (typeOf (undefined :: ReprType r)) ++ "-printer")
                                        (rep :. Z) NoRepr $ \[(v,rbox)] _ -> do
  let v' = castVector v :: Vector (HalrVal r)
      size = nOfElements $ halrDim rep rbox
  elems <- unmakeVector v' 0 size
  putStrLn $ caption ++ " (" ++ show rep ++ ", region box " ++ show rbox ++ "): " ++ show elems
  return nullVector

-- | Make file name unique for cases where we have a non-trivial
-- region box
dumpFileName :: FilePath -> RegionBox -> FilePath
dumpFileName file rbox
  | null rbox = file
  | otherwise = file ++ '-': rboxName
  where rboxName = intercalate "_" (map regName rbox)
        regName (RangeRegion _ (Range low high))
          = printf "%04d-%04d" low high
        regName binr@BinRegion{}
          = intercalate "_" $ map binName $ regionBins binr
        binName (RegionBinDesc low high _) = printf "%010.2f-%010.2f" low high

-- | Simple kernel that dumps the contents of a channel with Halide
-- data representation to a file.
halideDump :: forall r. HalideReprClass r => r -> FilePath -> Flow (ReprType r)
           -> Kernel ()
halideDump rep file
  = mappingKernel (show (typeOf (undefined :: ReprType r)) ++ "-writer")
                  (rep :. Z) NoRepr $ \[vs] _ -> do
  forM_ (Map.assocs vs) $ \(rbox, v) -> do
    -- Cast vector and get size as given by Halide data representation
    let v' = castVector v :: Vector (HalrVal r)
        size = nOfElements $ halrDim rep rbox
    -- Write buffer to file
    dumpVector' v' 0 size (dumpFileName file rbox)
  -- No result
  return nullVector

-- | Simple kernel that reads in data in the format produced by
-- 'halideDump'.
halideReadDump :: forall r. HalideReprClass r => r -> FilePath -> Kernel (ReprType r)
halideReadDump rep file
  = kernel (show (typeOf (undefined :: ReprType r)) ++ "-reader")
           Z rep $ \_ rboxes -> do
  vs <- allocReturns allocCVector rep rboxes
  forM vs $ \(rbox, v) -> do
    -- Cast vector and get size as given by Halide data representation
    let size = nOfElements $ halrDim rep rbox
    -- Read buffer from file
    readVector (v :: Vector (HalrVal r)) (dumpFileName file rbox) size
    return $ castVector v

-- | Simple kernel that dumps the contents of a channel with Halide
-- data representation to a text file
halideTextDump2D :: forall r. (HalideReprClass r, HalrDim r ~ Dim2, Show (HalrVal r))
                 => r -> FilePath -> Flow (ReprType r) -> Kernel ()
halideTextDump2D rep file = mappingKernel (show (typeOf (undefined :: ReprType r)) ++ "-text-writer")
                                          (rep :. Z) NoRepr $ \[vs] _ -> do
  forM_ (Map.assocs vs) $ \(rbox, v) ->
   withFile (dumpFileName file rbox) WriteMode $ \h -> do
    let v' = castVector v :: Vector (HalrVal r)
        ((_,hgt) :. (_, wdt) :. Z) = halrDim rep rbox
    forM_ [0..min 1000 hgt-1] $ \y -> do
      vals <- forM [0..wdt-1] $ \x -> peekVector v' (fromIntegral $ y*wdt+x)
      hPutStrLn h $ show vals
  return nullVector

-- | Simple kernel that dumps the contents of a channel with Halide
-- data representation to a text file
halideTextDump4D :: forall r. (HalideReprClass r, HalrDim r ~ Dim4, Show (HalrVal r))
                  => r -> FilePath -> Flow (ReprType r) -> Kernel ()
halideTextDump4D rep file = mappingKernel (show (typeOf (undefined :: ReprType r)) ++ "-text-writer")
                                          (rep :. Z) NoRepr $ \[vs] _ -> do
  forM_ (Map.assocs vs) $ \(rbox, vec) ->
   withFile (dumpFileName file rbox) WriteMode $ \h -> do
    let vec' = castVector vec :: Vector (HalrVal r)
        ((_,vext) :. (_,uext) :. (_,hgt) :. (_, wdt) :. Z) = halrDim rep rbox
    forM_ [0..vext-1] $ \v -> forM_ [0..uext-1] $ \u -> forM_ [0..min 1000 hgt-1] $ \y -> do
      let ix x = fromIntegral $ v*uext*wdt*hgt+u*wdt*hgt+y*wdt+x
      vals <- forM [0..wdt-1] $ \x -> peekVector vec' (ix x)
      hPutStrLn h $ show vals
  return nullVector
