{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Flow.Kernel
  ( -- * Data representation
    DataRepr(..), ReprAccess(..), NoRepr(..)
    -- ** Distributed
  , RegionRepr(..), ArrayRepr(..), RangeRepr(..), BinRepr(..), MarginRepr(..)
  , marginRepr
    -- * Kernel types
    -- ** Primitive
  , KernelCode, RegionData, kernel
  , MappingKernelCode, mappingKernel
  , MergingKernelCode, mergingKernel
  , FoldingKernelCode, foldingKernel
  , allocReturns
    -- ** Combinator
  , regionKernel, IsKernelDef
    -- ** Range
  , rangeKernel0, rangeKernel1
    -- ** Vector
  , VectorRepr(..)
  , vecKernel0, vecKernel1, vecKernel2, vecKernel3
  -- * Support
  , Z(..), (:.)(..)
  ) where

import Control.Monad

import Data.Function
import Data.Int
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import Data.Typeable

import Flow.Internal
import Flow.Builder
import Flow.Vector

import Foreign.Storable

-- | No representation: Either don't produce anything (= nobody can use
-- result) or don't care about input (= accept any input).
--
-- This data representation is special insofar that it allows kernels
-- to declare that a data dependency is not going to get used. For
-- example, this might be useful when the 'Strategy' wants to easily
-- switch between 'Kernel's with different input data requirements.
data NoRepr a = NoRepr
  deriving Typeable
instance Typeable a => Show (NoRepr a) where
  showsPrec _ = reprShows []
instance Typeable a => DataRepr (NoRepr a) where
  type ReprType (NoRepr a) = a
  reprNop _ = True
  reprAccess _ = ReadAccess
  reprCompatible _ _ = True
  reprShowsName _ = showString "nothing"

-- | Per-region representation combinator. The underlying representation remains
-- the same no matter the concrete shape of the 'Domain'. This means
-- that for a split 'Domain' every 'Region' will have data of the same
-- layout as the un-split data.
--
-- The most natural example here would be a "sum" kind of 'Flow':
-- Clearly the natural distributed representation is going to be a
-- number of partial sums, which have exactly the same data
-- representation as the overall sum.
--
-- Note that this even useful when we are not actually planning to
-- merge: If a 'Flow' represents a list of some sort, we can easily
-- represent this as a region data representation using a 'Domain'
-- split into one 'Region' per list entry. For example, if we want to
-- simply run an existing kernel a number of times, we can easily do
-- this using 'regionKernel', which adds 'RegionRepr' on all inputs
-- and outputs. If we 'distribute' this sort of 'Kernel' sequentially,
-- we would effectively change the list into a stream.
data RegionRepr dom rep = RegionRepr (Domain dom) rep
 deriving Typeable
instance (DataRepr rep, Typeable dom) => Show (RegionRepr dom rep) where
  showsPrec _ = reprShows []
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
  reprShows ds (RegionRepr dh r) = reprShows (d:ds) r
    where d = "regs(" ++ show dh ++ ")"

-- | The data is represented as a const-size vector of elements according to the
-- underlying data representation. No distribution possible.
data ArrayRepr rep = ArrayRepr (Int, Int) rep
  deriving Typeable
instance DataRepr rep => Show (ArrayRepr rep) where
  showsPrec _ = reprShows []
instance DataRepr rep => DataRepr (ArrayRepr rep) where
  type ReprType (ArrayRepr rep) = ReprType rep
  reprNop _ = False
  reprAccess (ArrayRepr _ rep) = reprAccess rep
  reprDomain (ArrayRepr _ rep) = reprDomain rep
  reprCompatible (ArrayRepr s0 rep0) (ArrayRepr s1 rep1)
    = s0 == s1 && reprCompatible rep0 rep1
  reprSize (ArrayRepr (l, h) r) ds  = fmap (*(h-l)) (reprSize r ds)
  reprShows ds (ArrayRepr (l, h) r) = reprShows (d:ds) r
    where d = show l ++ ":" ++ show h

-- | The data is represented as a vector of elements according to the
-- underlying data representation and size of the region's
-- 'Range'. This directly corresponds to an array that is distributed
-- in non-overlapping chunks.
--
-- (We even support automatic merging when the 'Domain's of input and
-- output data representations only differ in their split
-- degree. However, it's not quite clear yet whether this is really
-- the direction we want to go.)
data RangeRepr rep = RangeRepr (Domain Range) rep
  deriving Typeable
instance DataRepr rep => Show (RangeRepr rep) where
  showsPrec _ = reprShows []
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
  reprShows ds (RangeRepr dh r) = reprShows (d:ds) r
    where d = show dh

rangeMergeCopy :: DataRepr rep => RangeRepr rep -> RegionBox
               -> RegionData -> Int
               -> Vector Int8 -> Int -> IO ()
rangeMergeCopy rrep@(RangeRepr _ rep) rbox rd inoff outv outoff
  | ((RangeRegion _ (Range low _)):ds) <- rbox
  , Just subSize <- reprSize rep ds
  = do -- Go through top-level regions
       forM_ (groupBy ((==) `on` head . fst) $ Map.toList rd) $ \grp -> do
         -- Get the sub-range to calculate the offset. Must be the
         -- same for all region boxes in the group per groupBy
         -- definition. Then construct the local region data.
         let RangeRegion _ (Range l h) = head (fst (head grp))
             rd' = Map.fromList $ map (\(rb, v) -> (tail rb, v)) grp
         -- Call rangeMergeCopy recursively
         forM_ [l-low..h-low-1] $ \i ->
           reprMergeCopy rep ds rd' (inoff + subSize * (i-(l-low))) outv (outoff + subSize * i)
  | otherwise
  = fail $ "reprMerge: Unexpected number/types of domains for " ++ show rrep ++ ": " ++ show rbox

-- | Per-bin representation: Similar to 'RangeRepr', but the vector size
-- is given by the size sum of the 'Region''s bins. As before, the
-- nested data representation describes the element layout.
data BinRepr rep = BinRepr (Domain Bins) rep
  deriving Typeable
instance DataRepr rep => Show (BinRepr rep) where
  showsPrec _ = reprShows []
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
    = fmap (* (sum $ map (sum . Map.elems) $ Map.elems bins)) (reprSize rep ds)
  reprSize rep _ = fail $ "Not enough domains passed to reprSize for " ++ show rep ++ "!"
  reprShows ds (BinRepr dh r) = reprShows (d:ds) r
    where d = show dh

-- | Like 'RangeRepr', but every 'Region' is seen as being a constant
-- number of elements larger than it would normally be. This
-- corresponds to /overlapping/ arrays.
--
-- As usual, this can be useful in a number of situations. On the
-- input side, a kernel might need a certain element environment in
-- order to do its work. For example, a simple 3x3 bloom filter might
-- elect to ask for its input to be split in a way that allows it to
-- look 1 pixel into every direction at any time. We can also use the
-- same trick on the output data, where presumably a later merge step
-- would have to restore consistency in the overlapped regions.
--
-- Note that the margin applies to /every/ region, including the
-- primitive un-split one, so if you say
--
--   > dom <- makeRangeDomain 0 100
--   > bind bla $ kernel "kern" Z (marginRepr 10 elemRep) ...
--
-- The kernel is expected to produce a vector of 120 elements
-- satisfying @elemRep@!

data MarginRepr rep = MarginRepr Int (RangeRepr rep)
  deriving Typeable
instance DataRepr rep => Show (MarginRepr rep) where
  showsPrec _ = reprShows []
instance DataRepr rep => DataRepr (MarginRepr rep) where
  type ReprType (MarginRepr rep) = ReprType rep
  reprNop (MarginRepr _ rep) = reprNop rep
  reprAccess (MarginRepr _ rep) = reprAccess rep
  reprCompatible (MarginRepr ov0 rep0) (MarginRepr ov1 rep1)
    = ov0 == ov1 && rep0 `reprCompatible` rep1
  reprDomain (MarginRepr _ rep) = reprDomain rep
  reprMerge _ _ _ = fail "reprMerge for region repr undefined!"
  reprSize (MarginRepr ov rrep@(RangeRepr _ rep)) rds@(_:ds)
    | Just size <- reprSize rrep rds
    , Just ovSize <- reprSize rep ds
    = Just $ size + 2 * ov * ovSize
  reprSize _ _ = Nothing
  reprShows ds (MarginRepr ov (RangeRepr dh r)) = reprShows (d:ds) r
    where d = show ov ++ "+" ++ show dh ++ "+" ++ show ov

-- | Constructor function for "MarginRepr".
marginRepr :: DataRepr rep => Domain Range -> Int -> rep -> MarginRepr rep
marginRepr dom ov = MarginRepr ov . RangeRepr dom

-- | Vector representation: A variable-sized "Vector" with @val@
-- elements, representing abstract data of type @abs@. This is
-- essentially our low-level representation, so outside of the value
-- type check this will add no safety.
data VectorRepr val abs = VectorRepr ReprAccess
  deriving Typeable
instance (Typeable val, Typeable abs) => Show (VectorRepr val abs) where
  showsPrec _ = reprShows []
instance (Typeable val, Typeable abs) => DataRepr (VectorRepr val abs) where
  type ReprType (VectorRepr val abs) = abs
  reprNop _ = False
  reprAccess (VectorRepr acc)  = acc
  reprCompatible _ _ = True
  reprShowsName _ = shows (typeOf (undefined :: val)) . showString " vector"

-- | Code implementing a kernel for a single output domain. See
-- 'mappingKernel'.
type MappingKernelCode = [RegionData] -> RegionBox -> IO (Vector ())

-- | Kernel producing output seperately by region. Parameters
-- that correspond to the output domain are limited to the region
-- being worked on. So in effect, this implements a "map".
mappingKernel :: (DataRepr r, IsReprs rs, IsReprKern (ReprType r) rs)
          => String -> rs -> r -> MappingKernelCode -> ReprKernFun (ReprType r) rs
mappingKernel name parReprs retRep code = kernel name parReprs retRep $ \pars ->
  mapM $ \reg -> do
    let limit = limitToRegion ("mappingKernel " ++ name) (reprDomain retRep) reg
    code (map limit (zipReprs (toReprsI parReprs) pars)) reg

-- | Zip kernel parameters with their expected data
-- representations. Especially filter out NOOP-parameters, as they
-- will not get passed.
zipReprs :: [ReprI] -> [RegionData] -> [(ReprI, RegionData)]
zipReprs parReprs pars = zip reprs pars
  where reprs = filter (not . isNoReprI) parReprs

-- | Limit all incoming parameters that are split in a domain of the
-- given region box to *only* have data for region in question.
limitToRegion :: String -> [DomainId] -> RegionBox -> (ReprI, RegionData) -> RegionData
limitToRegion name dom rbox (parRi, par)
  -- No domains in common? Nothing to do
  | not $ any (flip IM.member rboxMap) (repriDomain parRi)
  = par
  -- Same domains? Then we should be able to look up the region
  -- box. This is a fairly common case, so it is a good idea to
  -- optimise it.
  | repriDomain parRi == dom, Just par' <- Map.lookup rbox par
  = Map.singleton rbox par'
  -- No regions left after filtering?
  | Map.null par_filtered
  = error $ name ++ " impossible: Asked to produce output for region " ++ show rbox ++
            " but this region was not found for parameter with representation " ++ show parRi ++ "!" ++
            " Got regions: " ++ show (Map.keys par)
  | otherwise
  = par_filtered
 where rboxMap = IM.fromList $ map (\r -> (dhiId $ regionDomain r, r)) rbox
       inRBox reg = case IM.lookup (dhiId $ regionDomain reg) rboxMap of
         Just reg' -> reg == reg'
         Nothing   -> True
       par_filtered = Map.filterWithKey (\prbox _ -> all inRBox prbox) par

-- | Code implementing a kernel for a single output domain and single parameter
-- regions. See 'mergingKernel'.
type MergingKernelCode = [(Vector (), RegionBox)] -> RegionBox -> IO (Vector ())

-- | Kernel that not only works only on a single region of the output domain at
-- a time, but also expects parameters to be single-region. This only
-- works if we can meaningfully merge the parameters (as in 'reprMerge').
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
  mergedPars <- mapM merge $ zipReprs (toReprsI parReprs) pars
  code mergedPars reg

-- | Code implementing a fold operation for a single input and output
-- domain. See 'foldingKernel'.
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
      iterPars = iter $ take (length pars - 1) $ zipReprs (toReprsI parReprs) pars

  -- Call kernel code for every iteration
  let fpar0 | [(_rbox, fpar0')] <- Map.toList (last pars)
                        = fpar0'
            | otherwise = error $ "foldingKernel " ++ name ++ " impossible: Multiple regions for folding parameter!"
  (\c -> foldM c fpar0 iterPars) $ \fpar pars' ->
    code pars' fpar reg

-- | Kernel modificator to have it work on a all regions in an
-- isolated fashion. This is roughly the same ideas as with
-- @mappingKernel@, but here we impose this behaviour on top of an
-- existing kernel (which might not be a mapping kernel itself).
regionKernel :: (Typeable d, IsKernelDef kf) => Domain d -> kf -> kf
regionKernel dom = mapKernelDef $ \(Kernel name khints code parReprs retRepr) ->
  let addReg (fl, ReprI rep) = (fl, ReprI $ RegionRepr dom rep)
      parReprs' = map addReg parReprs
      retRepr' = RegionRepr dom retRepr
      trans :: ([RegionData] -> [[Region]] -> a) -> [RegionData] -> [[Region]] -> [a]
      trans tr pars boxes =
        -- Go through regions sorted by top-level boxes
        let boxGroups = groupBy ((==) `on` head) boxes
            zpars = zipReprs (map snd parReprs') pars
            act boxes' =
                  let reg = head $ head boxes'
                      limit = limitToRegion ("regionKernel " ++ name) [dhId dom] [reg]
                      pars' = map limit zpars
                      pars'' = map (Map.mapKeysMonotonic tail) pars'
                  in tr pars'' (map tail boxes')
        in map act boxGroups
      code' pars boxes = fmap concat . sequence $ trans code pars boxes
      khints' pars boxes = concat $ trans khints pars boxes
      --
   in Kernel name khints' code' parReprs' retRepr'

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

-- | Helper to automatically allocate output buffers according to data
-- representation produced and region boxes requested. Only works for
-- data representations that support 'reprSize'.
allocReturns :: forall rep a. (DataRepr rep, Storable a)
             => (Int -> IO (Vector a)) -> rep -> [RegionBox] -> IO [(RegionBox, Vector a)]
allocReturns alloc rep = mapM $ \rbox -> case reprSize rep rbox of
  Just size  | let elemSize = sizeOf (undefined :: a)
             , size `mod` elemSize == 0
             -> fmap ((,) rbox) $ alloc (size `div` elemSize)
  other -> fail $ "allocReturnBufs: Could not automatically allocate return data for " ++
                  "data representation " ++ show rep ++ " size " ++ show other ++ "!"
