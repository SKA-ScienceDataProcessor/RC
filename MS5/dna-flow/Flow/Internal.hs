{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, ScopedTypeVariables,
             TypeOperators, DeriveDataTypeable, TypeSynonymInstances,
             FlexibleInstances, FlexibleContexts
 #-}

module Flow.Internal where

import Control.Monad.State.Strict

import Data.Function ( on )
import Data.Hashable
import Data.Int
import Data.List     ( sort, groupBy )
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import Data.Typeable

import Flow.Vector

-- | Simple type-level lists. TODO: Import from an external library?
-- There are too many to choose from...
data Z = Z
  deriving (Typeable, Eq)
data (:.) a b = (:.) a b
  deriving (Typeable, Eq)
infixr :.

type Dim = (Int32,Int32)

-- | Dimensions defined using the type-level lists.
type Dim0 = Z
type Dim1 = Dim :. Dim0
type Dim2 = Dim :. Dim1
type Dim3 = Dim :. Dim2
type Dim4 = Dim :. Dim3
dim0 :: Dim0
dim0 = Z
dim1 :: Dim -> Dim1
dim1 e = e :. Z
dim2 :: Dim -> Dim -> Dim2
dim2 e0 e1 = e0 :. e1 :. Z

instance Show Dim0 where
  showsPrec _ Z = showString "[]"
instance Show Dim1 where
  showsPrec _ ((m,ext) :. _) =
    ('[':) . shows m . (':':) . shows (m+ext) . (']':)
instance Show (Dim :. dim) => Show (Dim :. Dim :. dim) where
  showsPrec _ ((m,ext) :. ds) =
    ('[':) . shows m . (':':) . shows (m+ext) . (',':) . tail . shows ds

-- | Internal representation of a data flow.
data FlowI = FlowI
  { flHash    :: {-# UNPACK #-} !Int
  -- ^ Flow hash, for quick equality checks
  , flName    :: String
  -- ^ Flow name. Used for profiling as well a disambiguating flow
  -- with the same type.
  , flDepends :: [FlowI]
  -- ^ Data flow dependencies of this flow
  , flWildcard :: Maybe Int
  -- ^ If set, this flow is a wildcard used in rule matching (internal)
  }
  deriving (Eq)

instance Hashable FlowI where
  i `hashWithSalt` (FlowI h _ _ _) = i `hashWithSalt` h
instance Show FlowI where
  showsPrec _ (FlowI _ n _  Just{})  = showString "*" . showString n
  showsPrec _ (FlowI _ n ds Nothing) = showString n . shows ds

-- | Flow type. Carries an type tag to identifiy the abstract data
-- carried.
newtype Flow a = Flow FlowI
  deriving Show

-- | Creates a wildcard flow (internal use only)
wildFlow :: Int -> Flow a
wildFlow i = Flow $ FlowI i ("wild" ++ show i) [] (Just i)

-- | Smart constructor to create a new abstract flow, taking care of
-- correct hashing
mkFlow :: String -> [FlowI] -> Flow a
mkFlow name fis = Flow $ FlowI (hash (name, fis, noWild)) name fis noWild
  where noWild = Nothing :: Maybe Int

-- | Kernel frontend representation
data Kernel a where
  Kernel :: DataRepr r
         => String -> KernelCode
         -> [(FlowI, ReprI)] -> r
         -> Kernel (ReprType r)

type KernelId = Int

-- | Kernel backend representation
data KernelBind = KernelBind
  { kernId   :: KernelId   -- ^ Unique number identifiying this kernel
  , kernFlow :: FlowI      -- ^ Implemented flow
  , kernName :: String     -- ^ Name
  , kernRepr :: ReprI      -- ^ Data representation produced
  , kernDeps :: [KernelDep]-- ^ Kernel dependencies
  , kernCode :: KernelCode -- ^ Code to execute the kernel
  , kernReprCheck :: ReprI -> Bool
    -- ^ Check whether a sink data representation is compatible with
    -- the data we produce
  }

kernDomain :: KernelBind -> [DomainId]
kernDomain KernelBind{kernRepr=ReprI r} = reprDomain r

-- | Kernel dependency
data KernelDep = KernelDep
  { kdepId :: KernelId
  , kdepRepr :: ReprI
  }

instance Show KernelDep where
  showsPrec _ kdep =
    shows (kdepRepr kdep) . showString " from kernel " . shows (kdepId kdep)

-- | Extracts domain from kernel dependency
kdepDomain :: KernelDep -> [DomainId]
kdepDomain (KernelDep {kdepRepr=ReprI rep}) = reprDomain rep

-- | Extracts access from kernel dependency
kdepAccess :: KernelDep -> ReprAccess
kdepAccess (KernelDep {kdepRepr=ReprI rep}) = reprAccess rep

-- | Region box. Refers to the intersection of regions in
-- zero or more distinct (!) domains.
type RegionBox = [Region]

-- | Region box data. This maps a number of region boxes (assumed to
-- be belonging to the same domain) to associated data.
type RegionData = Map.Map RegionBox (Vector ())

-- | Code implementing a kernel. Takes a number of parameters as
-- "RegionData" and produces data for a given region box(es).
type KernelCode = [RegionData] -> [RegionBox] -> IO [Vector ()]

instance Show KernelBind where
  showsPrec _ (KernelBind kid kflow kname krepr kdeps _ _)
    = showString "Kernel " . shows kid . showString ":" . showString kname
      . showString " implementing " . showString (flName kflow)
      . showString " producing " . shows krepr
      . showString " using " . shows kdeps

type DomainId = Int
data Domain a = Domain
  { dhId    :: DomainId
    -- | Produce a new domain handle that is split up @n@ times more
  , dhSplit  :: Int -> Strategy (Domain a)
    -- | Creates the domain from given data. This is how you construct
    -- root domains (those without parents).
  , dhCreate :: RegionData -> IO Region
    -- | Splits out regions from the parent domain. This is used for
    -- constructing sub-domains.
  , dhRegion :: Region -> IO [Region]
    -- | Get domain this one was derived from. Not set for root domains.
  , dhParent :: Maybe (Domain a)
    -- | Check whether a a region of this domain is permissible in the
    -- context of a given region box
  , dhFilterBox :: RegionBox -> Region -> Maybe Region
  }

instance forall a. Typeable a => Show (Domain a) where
  showsPrec _ dh = shows (typeOf (undefined :: a)) . showString " domain " . shows (dhId dh)
instance Eq (Domain a) where
  (==) = (==) `on` dhId
instance Ord (Domain a) where
  (<=) = (<=) `on` dhId
  compare = compare `on` dhId

dhIsParent :: Domain a -> Domain a -> Bool
dhIsParent dh dh1
  | dh == dh1                = True
  | Just dh' <- dhParent dh  = dhIsParent dh' dh1
  | otherwise                = False

data DomainI where
  DomainI :: forall a. Domain a -> DomainI

dhiId :: DomainI -> DomainId
dhiId (DomainI di) = dhId di

instance Eq DomainI where
  di0 == di1  = dhiId di0 == dhiId di1

-- | Domains are just ranges for now. It is *very* likely that we are
-- going to have to generalise this in some way.
data Region = RangeRegion (Domain Range) Range
            | BinRegion (Domain Bins) Bins
  deriving Typeable

instance Show Region where
  showsPrec _ (RangeRegion dom r) = showString "Region<" . shows (dhId dom) . ('>':) . shows r
  showsPrec _ (BinRegion dom bins) = showString "Region<" . shows (dhId dom) . ('>':) . shows bins

-- These instances are important because we are going to use regions
-- in order to index maps (often in the form of
-- "RegionBox"). Therefore we must especially make sure that regions
-- that only differ in terms of "meta-data" are equal!
-- TODO: ugly
instance Ord Region where
  compare (RangeRegion d0 r0) (RangeRegion d1 r1)
    = compare d0 d1 `mappend` compare r0 r1
  compare (BinRegion d0 (Bins bins0)) (BinRegion d1 (Bins bins1))
    = compare d0 d1 `mappend` mconcat (zipWith compare (Map.keys bins0) (Map.keys bins1))
  compare RangeRegion{} BinRegion{} = LT
  compare BinRegion{} RangeRegion{} = GT
instance Eq Region where
  r0 == r1  = compare r0 r1 == EQ

data Range = Range Int Int
  deriving (Typeable, Eq, Ord)
instance Show Range where
  showsPrec _ (Range low high) = shows low . (':':) . shows high
data Bins = Bins (Map.Map (Double, Double) (Map.Map RegionBox Int))
  deriving (Typeable, Eq, Ord)
instance Show Bins where
  showsPrec _ (Bins bins) = showString "Bins" . flip (foldr f) (Map.toList bins)
    where f ((low, high), m) = (' ':) . shows low . (':':) . shows high . shows (Map.elems m)

-- | Checks whether the second domain is a subset of the first
domainSubset :: Region -> Region -> Bool
domainSubset (RangeRegion d0 (Range low0 high0)) (RangeRegion d1 (Range low1 high1))
  = d0 == d1 && low0 <= low1 && high0 >= high1
domainSubset (BinRegion d0 (Bins bins0)) (BinRegion d1 (Bins bins1))
  = d0 == d1 && all (`Map.member` bins1) (Map.keys bins0)
domainSubset _ _
  = False

-- | Merges a number of region boxes, if possible. All region boxes must
-- use the same domains and especially have the same dimensionality!
--
-- TODO: Support for something else besides "RangeRegion"
regionMerge :: [RegionBox] -> Maybe RegionBox
regionMerge []   = Nothing
regionMerge [[]] = Just []
regionMerge boxes
  | Just merged' <- merged
  , (RangeRegion d (Range l _):ds_head) <- head merged'
  , and $ zipWith no_gaps merged' (tail merged')
  , (RangeRegion _ (Range _ h):ds_last) <- last merged'
  , ds_head == ds_last
  = Just (RangeRegion d (Range l h):ds_head)
  | otherwise
  = Nothing
 where grouped = groupBy ((==) `on` head) (sort boxes)
       merged = mapM merge grouped
       merge :: [RegionBox] -> Maybe RegionBox
       merge boxGrp = fmap (head (head boxGrp) :) $ regionMerge $ map tail boxGrp
       no_gaps (RangeRegion d0 (Range _ h):ds0) (RangeRegion d1 (Range l _):ds1)
         = d0 == d1 && h == l && ds0 == ds1
       no_gaps _ _ = error "domainMerge: Mixed domain types...?"

regionDomain :: Region -> DomainI
regionDomain (RangeRegion d _) = DomainI d
regionDomain (BinRegion d _)   = DomainI d

type StratMap = HM.HashMap FlowI KernelBind
type Strategy a = State StratState a

-- | State while constructing a strategy
data StratState = StratState
  { ssKernelId :: {-# UNPACK #-}!KernelId -- ^ Next fresh kernel ID
  , ssDomainId :: {-# UNPACK #-}!DomainId -- ^ Next fresh domain ID
  , ssMap      :: StratMap          -- ^ Kernels currently in scope
  , ssRules    :: [StratRule]       -- ^ Currently active rules
  , ssSteps    :: [Step]            -- ^ Scheduling steps in current scope so far
  }
initStratState :: StratState
initStratState = StratState 0 0 HM.empty [] []

-- | Make a new kernel ID
freshKernelId :: Strategy KernelId
freshKernelId = state $ \ss -> (ssKernelId ss, ss {ssKernelId = 1 + ssKernelId ss})

-- | Make a fresh domain ID
freshDomainId :: Strategy DomainId
freshDomainId = state $ \ss -> (ssDomainId ss, ss {ssDomainId = 1 + ssDomainId ss})

-- | Add a step to the strategy
addStep :: Step -> Strategy ()
addStep step =  modify $ \ss -> ss { ssSteps = step : ssSteps ss }

class (Show r, Typeable r, Typeable (ReprType r)) => DataRepr r where
  type ReprType r
  -- | Does the representation contain no data? This means that we are
  -- going to ignore it.
  reprNop :: r -> Bool
  reprNop _ = False
  reprAccess :: r -> ReprAccess
  reprCompatible :: r -> r -> Bool
  reprCompatible _ _ = True
  reprDomain :: r -> [DomainId]
  reprDomain _ = []
  reprMerge :: r -> RegionData -> RegionBox -> IO (Maybe (Vector ()))
  reprMerge r rd rb
    | Just size <- reprSize r rb
    = do v <- allocCVector size :: IO (Vector Int8)
         reprMergeCopy r rb rd 0 v 0
         return $ Just $ castVector v
    | otherwise
    = return Nothing
  reprMergeCopy :: r -> RegionBox     -- ^ Region box to fill
                -> RegionData -> Int  -- ^ Input data & offset
                -> Vector Int8 -> Int -- ^ Output vector & offset
                -> IO ()
  reprMergeCopy r [] rd inoff outv outoff
    | Just size <- reprSize r [],
      Just inv <- Map.lookup [] rd
    = copyVector outv outoff (castVector inv) inoff size
  reprMergeCopy _ _  _  _     _    _ = fail "reprMerge unimplemented!"
  reprSize :: r -> RegionBox -> Maybe Int
  reprSize _ _ = Nothing

-- | Who has ownership of the data representation?
data ReprAccess
  = ReadAccess
    -- ^ Caller has ownership. Kernel should not change the buffer.
  | WriteAccess
    -- ^ Callee has ownership. Kernel can change the buffer, and must
    -- free it if it is not returned. Might require framework to
    -- duplicate the buffer.
  deriving (Show, Eq)

data ReprI where
  ReprI :: forall r. DataRepr r => r -> ReprI
instance Show ReprI where
  showsPrec p (ReprI r) = showsPrec p r

isNoReprI :: ReprI -> Bool
isNoReprI (ReprI repr) = reprNop repr
repriDomain :: ReprI -> [DomainId]
repriDomain (ReprI repr) = reprDomain repr

-- | Resource scheduling policy
data Schedule
  = SeqSchedule
  | ParSchedule
  deriving (Show, Eq)

-- | A strategy rule, explaining how to implement certain data flow patterns
newtype StratRule = StratRule (FlowI -> Maybe (Strategy ()))

-- | Schedule step
data Step where
  DomainStep :: Typeable a => Maybe KernelId -> Domain a -> Step
    -- ^ Create a new domain. Might depend on data produced by a kernel.
  KernelStep :: KernelBind -> Step
    -- ^ Execute a kernel for the given domain(s)
  DistributeStep :: Typeable a => Domain a -> Schedule -> [Step] -> Step
    -- ^ Distribute steps across the given domain using the given
    -- scheduling policy.

-- | List all kernels used in schedule
stepsToKernels :: [Step] -> [KernelBind]
stepsToKernels = concatMap go
  where go (KernelStep kb)            = [kb]
        go (DistributeStep _ _ steps) = concatMap go steps
        go _other                     = []
