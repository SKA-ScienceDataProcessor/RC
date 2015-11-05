{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, ScopedTypeVariables,
             TypeOperators, DeriveDataTypeable, TypeSynonymInstances,
             FlexibleInstances, FlexibleContexts
 #-}

module Flow.Internal where

import Control.Monad.State.Strict

import Data.Function ( on )
import Data.Hashable
import Data.Int
import Data.List     ( sort )
import qualified Data.Map as Map
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

type KernelId = Int

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
  , dhCreate :: Maybe (Vector ()) -> IO Region
    -- | Splits out regions from the parent domain. This is used for
    -- constructing sub-domains.
  , dhRegion :: Region -> IO [Region]
    -- | Get domain this one was derived from. Not set for root domains.
  , dhParent :: Maybe (Domain a)
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

-- | Domains are just ranges for now. It is *very* likely that we are
-- going to have to generalise this in some way.
data Region = RangeRegion (Domain Range) Range
            | BinRegion (Domain Bins) Bins
  deriving (Typeable, Eq, Ord, Show)

data Range = Range Int Int
  deriving (Typeable, Eq, Ord, Show)
data Bins = Bins (Map.Map (Double, Double) Int)
  deriving (Typeable, Eq, Ord, Show)

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
-- TODO: Ugly. Write properly
regionMerge :: [RegionBox] -> Maybe RegionBox
regionMerge [] = Just []
regionMerge dss
  | not $ all ((==1) . length) dss
  = error "domainMerge: Not implemented yet for domain combinations!"
  | RangeRegion d (Range l _) <- head ds
  , and $ zipWith no_gaps ds (tail ds)
  , RangeRegion _ (Range _ h) <- last ds
  = Just [RangeRegion d $ Range l h]
  | otherwise
  = Nothing
 where ds = sort $ map head dss
       no_gaps (RangeRegion d0 (Range _ h)) (RangeRegion d1 (Range l _))
         = d0 == d1 && h == l
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
  reprMerge _ _ _ = return Nothing
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
  SplitStep :: Typeable a => Domain a -> [Step] -> Step
    -- ^ Split a domain into regions
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
