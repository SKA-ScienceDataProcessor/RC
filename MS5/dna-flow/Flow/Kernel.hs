{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Flow.Kernel
  ( DataRepr(..), ReprAccess(..)
  , NoRepr(..)
  , RegionRepr(..), RangeRepr(..), BinRepr(..)
  , mappingKernel, mergingKernel, foldingKernel
  , rangeKernel0, rangeKernel1
  , VectorRepr(..)
  , vecKernel0, vecKernel1, vecKernel2, vecKernel3
  ) where

import Control.Applicative
import Control.Monad

import Data.Function
import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Typeable

import Flow.Internal
import Flow.Builder
import Flow.Vector

-- | No representation: Either don't produce anything (= nobody can use
-- result) or don't care about input (= accept any input).
data NoRepr a = NoRepr
  deriving Typeable
instance Typeable a => Show (NoRepr a) where
  showsPrec _ _ = showString "nothing [" . shows (typeOf (undefined :: a)) . (']':)
instance Typeable a => DataRepr (NoRepr a) where
  type ReprType (NoRepr a) = a
  reprNop _ = True
  reprAccess _ = ReadAccess
  reprCompatible _ _ = True

-- | Per-region representation: Does not change data representation,
-- but distributes data so that we have one data object per
-- region. Every region is supposed to correspond to exactly one
-- buffer in the underlying data representation.
data RegionRepr dom rep where
  RegionRepr :: DataRepr rep => Domain dom -> rep -> RegionRepr dom rep
 deriving Typeable
instance (Show (Domain dom), Show rep) => Show (RegionRepr dom rep) where
  showsPrec _ (RegionRepr dh rep)
    = shows rep . showString " over " . shows dh
instance (DataRepr rep, Typeable dom, Typeable rep) => DataRepr (RegionRepr dom rep) where
  type ReprType (RegionRepr dom rep) = ReprType rep
  reprNop (RegionRepr _ rep) = reprNop rep
  reprAccess (RegionRepr _ rep) = reprAccess rep
  reprCompatible (RegionRepr dh0 rep0) (RegionRepr dh1 rep1)
    = (dh0 == dh1 || Just dh1 == dhParent dh0) && rep0 `reprCompatible` rep1
  reprDomain (RegionRepr dh rep) = dhId dh : reprDomain rep
  reprMerge _ _ _ = fail "reprMerge for region repr undefined!"
  reprSize (RegionRepr _ rep) (_:ds) = reprSize rep ds
  reprSize rep                _      = fail $ "Not enough domains passed to reprSize for " ++ show rep ++ "!"

-- | Per-range representation: Similar to "RegionRepr", but instead of
-- one object, the data is a vector with the range's size. The given
-- data representation describes the value layout.
data RangeRepr rep = RangeRepr (Domain Range) rep
  deriving Typeable
instance Show rep => Show (RangeRepr rep) where
  showsPrec _ (RangeRepr rep dom)
    = shows rep . ('[':) . shows dom . (']':)
instance DataRepr rep => DataRepr (RangeRepr rep) where
  type ReprType (RangeRepr rep) = ReprType rep
  reprNop _ = False
  reprAccess (RangeRepr _ rep) = reprAccess rep
  reprDomain (RangeRepr d rep) = dhId d : reprDomain rep
  reprCompatible (RangeRepr d0 rep0) (RangeRepr d1 rep1)
    = d0 `dhIsParent` d1 && reprCompatible rep0 rep1
  reprMergeCopy = rangeMergeCopy
  reprSize (RangeRepr _ rep) (RangeRegion _ (Range l h):ds)
    = fmap (* (h-l)) (reprSize rep ds)
  reprSize rep                _      = fail $ "Not enough domains passed to reprSize for " ++ show rep ++ "!"

rangeMergeCopy :: DataRepr rep => RangeRepr rep -> RegionData -> RegionBox -> Vector Int8 -> Int -> IO ()
rangeMergeCopy (RangeRepr _ rep) rd ((RangeRegion _ (Range low _)):ds) outv outoff
  | Just subSize <- reprSize rep ds
  = do -- Go through top-level regions
       forM_ (groupBy ((==) `on` head . fst) $ Map.toList rd) $ \grp -> do
         -- Get the sub-range to calculate the offset. Must be the
         -- same for all region boxes in the group per groupBy
         -- definition. Then construct the local region data.
         let RangeRegion _ (Range l h) = head (fst (head grp))
             rd' = Map.fromList $ map (\(rbox, v) -> (tail rbox, v)) grp
         -- Call rangeMergeCopy recursively
         forM_ [l-low..h-low-1] $ \i ->
           reprMergeCopy rep rd' ds outv (outoff + subSize * i)
rangeMergeCopy r _ rbox _ _ = fail $
  "reprMerge: Unexpected number/types of domains for " ++ show r ++ ": " ++ show rbox

-- | Per-bin representation: Similar to "RangeRepr", but instead of using a
-- range, the vector size is given by the size associated with a
-- bin. The data representation again describes the element layout.
data BinRepr rep = BinRepr (Domain Bins) rep
  deriving Typeable
instance Show rep => Show (BinRepr rep) where
  showsPrec _ (BinRepr rep dom)
    = shows rep . ('[':) . shows dom . (']':)
instance DataRepr rep => DataRepr (BinRepr rep) where
  type ReprType (BinRepr rep) = ReprType rep
  reprNop (BinRepr _ rep) = reprNop rep
  reprAccess (BinRepr _ rep) = reprAccess rep
  reprDomain (BinRepr d rep) = dhId d : reprDomain rep
  reprCompatible (BinRepr d0 rep0) (BinRepr d1 rep1)
    = d0 `dhIsParent` d1 && reprCompatible rep0 rep1
  reprMerge _ _ [BinRegion _ _bins] =
    -- TODO
    fail "reprMerge not implemented yet on BinRepr!"
  reprMerge r _   doms = error $
    "reprMerge: Unexpected number/types of domains for " ++ show r ++ ": " ++ show doms
  reprSize (BinRepr _ rep) ((BinRegion _ (Bins bins)):ds)
    = fmap (* (sum $ Map.elems bins)) (reprSize rep ds)
  reprSize rep _ = fail $ "Not enough domains passed to reprSize for " ++ show rep ++ "!"

-- | Vector representation: A variable-sized "Vector" with @val@
-- elements, representing abstract data of type @abs@. This is
-- essentially our low-level representation, so outside of the value
-- type check this will add no safety.
data VectorRepr val abs = VectorRepr ReprAccess
  deriving Typeable
instance (Typeable val, Typeable abs) => Show (VectorRepr val abs) where
  showsPrec _ _ = shows (typeOf (undefined :: abs)) . showString " vector " .
                  shows (typeOf (undefined :: val)) . showString "[]"
instance (Typeable val, Typeable abs) => DataRepr (VectorRepr val abs) where
  type ReprType (VectorRepr val abs) = abs
  reprNop _ = False
  reprAccess (VectorRepr acc)  = acc
  reprCompatible _ _ = True

-- | Code implementing a kernel for a single output domain. See
-- "mapKernel".
type MappingKernelCode = [RegionData] -> RegionBox -> IO (Vector ())

-- | Kernel producing output seperately by region. Parameters
-- that correspond to the output domain are limited to the region
-- being worked on. So in effect, this implements a "map".
mappingKernel :: (DataRepr r, IsReprs rs, IsReprKern (ReprType r) rs)
          => String -> rs -> r -> MappingKernelCode -> ReprKernFun (ReprType r) rs
mappingKernel name parReprs retRep code = kernel name parReprs retRep $ \pars ->
  mapM $ \reg -> do
    let limit = limitToRegion ("mappingKernel " ++ name) (reprDomain retRep) reg
    code (map limit (zipReprs parReprs pars)) reg

-- | Zip kernel parameters with their expected data
-- representations. Especially filter out NOOP-parameters, as they
-- will not get passed.
zipReprs :: IsReprs rs => rs -> [RegionData] -> [(ReprI, RegionData)]
zipReprs parReprs pars = zip reprs pars
  where reprs = filter (not . isNoReprI) $ toReprsI parReprs

-- | Limit all incoming parameters that are split in the domain of the
-- given region to *only* have data for the given region.
limitToRegion :: String -> [DomainId] -> RegionBox -> (ReprI, RegionData) -> RegionData
limitToRegion name dom rbox (parRi, par)
  | repriDomain parRi /= dom
  = par
  | Just par' <- Map.lookup rbox par
  = Map.singleton rbox par'
  | otherwise
  = error $ name ++ " impossible: Asked to produce output for region " ++ show rbox ++
            " but this region was not found for parameter " ++ show parRi ++ "!"

-- | Code implementing a kernel for a single output domain and single parameter
-- regions. See "mergedKernel".
type MergingKernelCode = [(Vector (), RegionBox)] -> RegionBox -> IO (Vector ())

-- | Kernel that not only works only on a single region of the output domain at
-- a time, but also expects parameters to be single-region. This only
-- works if we can meaningfully merge the parameters (as in "reprMerge").
mergingKernel :: (DataRepr r, IsReprs rs, IsReprKern (ReprType r) rs)
             => String -> rs -> r -> MergingKernelCode -> ReprKernFun (ReprType r) rs
mergingKernel name parReprs retRep code = mappingKernel name parReprs retRep $ \pars reg -> do
  let merge :: (ReprI, RegionData) -> IO (Vector (), RegionBox)
      merge (ReprI parR, par)
        -- Parameters with just a single region can be passed directly
        -- to the kernel
        | [(rbox, v)] <- Map.toList par = return (v, rbox)
        -- Otherwise we attempt to merge all available regions
        -- together and produce a combined vector for the associated
        -- data.
        | Just merged <- regionMerge $ Map.keys par
        = do mvec <- reprMerge parR par merged
             case mvec of
               Just vec -> return (vec, merged)
               Nothing  -> fail $ "mergingKernel " ++ name ++ ": Failed to merge regions " ++
                                  show (Map.keys par) ++ " into region " ++ show merged ++
                                  " size" ++ show (reprSize parR merged) ++ "!"
        | otherwise
        = fail $ "mergingKernel " ++ name ++ ": Attempted to merge regions " ++
                 show (Map.keys par) ++ " for " ++ show parR ++ "! This is not (yet?) supported..."
  -- Pair parameters with their data representations. Noop-parameters
  -- are not passed.
  mergedPars <- mapM merge $ zipReprs parReprs pars
  code mergedPars reg

-- | Code implementing a fold operation for a single input and output
-- domain. See "foldingKernel".
type FoldingKernelCode = [(Vector (), RegionBox)] -> Vector() -> RegionBox -> IO (Vector ())

-- | Data family for getting the last (fold) parameter data
-- representation of a list. As folding kernels iterate over this,
-- this must also be the return data representation.
class DataRepr (FoldPar xs) => HasFoldPar xs where
  type FoldPar xs :: *
  foldPar :: xs -> FoldPar xs
instance DataRepr x => HasFoldPar (x :. Z) where
  type FoldPar (x :. Z) = x
  foldPar (x :. Z) = x
instance HasFoldPar (x :. xs) => HasFoldPar (y :. (x :. xs)) where
  type FoldPar (y :. (x :. xs)) = FoldPar (x :. xs)
  foldPar (_ :. (x :. xs)) = foldPar (x :. xs)

-- | Kernel producing output by folding over all regions in the input
-- data (while mapping over the output data). This is an alternative
-- to "mergingKernel", but only works for kernels that we can
-- meaningfully apply to data multiple times.
foldingKernel :: (HasFoldPar rs, IsReprs rs, IsReprKern (ReprType (FoldPar rs)) rs)
              => String -> rs -> FoldingKernelCode -> ReprKernFun (ReprType (FoldPar rs)) rs
foldingKernel name parReprs code = mappingKernel name parReprs (foldPar parReprs) $ \pars reg -> do

  -- Get parameter sets we want to pass
  let iter :: [(ReprI, RegionData)] -> [[(Vector (), RegionBox)]]
      iter [] = [[]]
      iter ((parRi, par):rest)
        | [(rbox, v)] <- Map.toList par
        = map ((v, rbox):) (iter rest)
        | otherwise
        = do (rbox, parv) <- Map.assocs par
             let limit = limitToRegion ("foldingKernel " ++ name) (repriDomain parRi) rbox
                 rest' = zip (map fst rest) $ map limit rest
             map ((parv, rbox):) (iter rest')
      iterPars = iter $ take (length pars - 1) $ zipReprs parReprs pars

  -- Call kernel code for every iteration
  let fpar0 | [(_rbox, fpar0')] <- Map.toList (last pars)
                        = fpar0'
            | otherwise = error $ "foldingKernel " ++ name ++ " impossible: Multiple regions for folding parameter!"
  (\c -> foldM c fpar0 iterPars) $ \fpar pars' ->
    code pars' fpar reg

rangeKernel0 :: DataRepr r
             => String -> r -> (Int -> Int -> IO (Vector a))
             -> Kernel (ReprType r)
rangeKernel0 name retRep code = mergingKernel name Z retRep code'
  where code' _ [] = fail $ "kernel " ++ show name ++ ": Received wrong number of domains!"
        code' _ ds | RangeRegion _ (Range low high) <- last ds
                   = castVector <$> code low high
        code' _ _  = fail $ "kernel " ++ show name ++ ": Received wrong types of domains!"

rangeKernel1 :: (DataRepr r, DataRepr r0)
             => String -> r0 -> r -> (Int -> Int -> Vector () -> IO (Vector a))
             -> Flow (ReprType r0) -> Kernel (ReprType r)
rangeKernel1 name repr0 retRep code = mergingKernel name (repr0 :. Z) retRep code'
  where code' _   [] = fail $ "kernel " ++ show name ++ ": Received wrong number of domains!"
        code' [v] ds | RangeRegion _ (Range low high) <- last ds
                     = castVector <$> code low high (fst v)
        code' _   _  = fail $ "kernel " ++ show name ++ ": Received wrong number of arguments or wrong types of domains!"

vecKernel0 :: (Typeable val, Typeable abs)
           => String -> VectorRepr val abs -> IO (Vector val) -> Kernel abs
vecKernel0 name rrepr code = mergingKernel name Z rrepr $ \_ _ -> castVector <$> code

vecKernel1 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val abs
           -> (Vector val0 -> IO (Vector val))
           -> Flow abs0 -> Kernel abs
vecKernel1 name repr0 rrepr code = mergingKernel name (repr0 :. Z) rrepr $ \vs ds -> case (vs, ds) of
  ([vec], []) -> castVector <$> code (castVector $ fst vec)
  (_    , []) -> fail "vecKernel1: Received wrong number of input buffers!"
  (_    , _)  -> fail "vecKernel1: Received wrong number of domains!"

vecKernel2 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0, Typeable val1, Typeable abs1)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val1 abs1 -> VectorRepr val abs
           -> (Vector val0 -> Vector val1 -> IO (Vector val))
           -> Flow abs0 -> Flow abs1 -> Kernel abs
vecKernel2 name repr0 repr1 rrepr code = mergingKernel name (repr0 :. repr1 :. Z) rrepr $ \case
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
vecKernel3 name repr0 repr1 repr2 rrepr code = mergingKernel name (repr0 :. repr1 :. repr2 :. Z) rrepr $ \case
  [vec,vec1,vec2] -> \case
    []     -> castVector <$> code (castVector (fst vec)) (castVector (fst vec1)) (castVector (fst vec2))
    _other -> fail "vecKernel3: Called for wrong number of domains!"
  _other          -> fail "vecKernel3: Received wrong number of input buffers!"
