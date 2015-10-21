{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, ScopedTypeVariables,
             TypeOperators, DeriveDataTypeable #-}

module Flow.Internal where

import Control.Monad.State.Strict

import Data.Function ( on )
import Data.List     ( sort )
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Typeable

import Flow.Vector

-- | Simple type-level lists. TODO: Import from an external library?
-- There are too many to choose from...
data Z = Z
  deriving (Show, Typeable, Eq)
data (:.) a b = (:.) a b
  deriving (Show, Typeable, Eq)
infixr :.

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
  deriving Show

-- | Extracts domain from kernel dependency
kdepDomain :: KernelDep -> [DomainId]
kdepDomain (KernelDep {kdepRepr=ReprI rep}) = reprDomain rep

-- | Extracts access from kernel dependency
kdepAccess :: KernelDep -> ReprAccess
kdepAccess (KernelDep {kdepRepr=ReprI rep}) = reprAccess rep

-- | Code implementing a kernel
type KernelCode = [(Vector (), [Domain])] -> [Domain] -> IO (Vector ())

instance Show KernelBind where
  showsPrec _ (KernelBind kid kflow kname krepr kdeps _ _)
    = showString "Kernel " . shows kid . showString ":" . showString kname
      . showString " implementing " . showString (flName kflow)
      . showString " producing " . shows krepr
      . showString " using " . shows kdeps

type DomainId = Int
data DomainHandle a = DomainHandle
  { dhId    :: DomainId
    -- | Number of regions this domain is split into
  , dhSize  :: Int
    -- | Produce a new domain handle that is split up @n@ times more
  , dhSplit  :: Int -> Strategy (DomainHandle a)
    -- | Domain this was derived from
  , dhParent :: Maybe (DomainHandle a)
    -- | Creates a new region for this handle
  , dhCreate :: IO Domain
    -- | Splits a region of the parent domain
  , dhRegion :: Domain -> IO [Domain]
  }

instance forall a. Typeable a => Show (DomainHandle a) where
  showsPrec _ dh = showString "Domain " . shows (dhId dh) . showString " [" .
                   shows (typeOf (undefined :: a)) . showString "]"
instance Eq (DomainHandle a) where
  (==) = (==) `on` dhId

dhIsParent :: DomainHandle a -> DomainHandle a -> Bool
dhIsParent dh dh1
  | dh == dh1                = True
  | Just dh' <- dhParent dh  = dhIsParent dh' dh1
  | otherwise                = False

-- | Domains are just ranges for now. It is *very* likely that we are
-- going to have to generalise this in some way.
data Domain = RangeDomain Range
  deriving (Typeable, Eq, Ord, Show)

data Range = Range Int Int
  deriving (Typeable, Eq, Ord, Show)

-- | Checks whether the second domain is a subset of the first
domainSubset :: Domain -> Domain -> Bool
domainSubset (RangeDomain (Range low0 high0)) (RangeDomain (Range low1 high1))
  = low0 <= low1 && high0 >= high1

-- | Merges a number of domains, if possible. All domains must have
-- the same length!
--
-- TODO: Ugly. Write properly
domainMerge :: [[Domain]] -> Maybe [Domain]
domainMerge [] = Just []
domainMerge dss
  | not $ all ((==1) . length) dss
  = error "domainMerge: Not implemented yet for domain combinations!"
  | and $ zipWith no_gaps ds (tail ds)
  , RangeDomain (Range l _) <- head ds
  , RangeDomain (Range _ h) <- last ds
  = Just [RangeDomain $ Range l h]
  | otherwise
  = Nothing
 where ds = sort $ map head dss
       no_gaps (RangeDomain (Range _ h)) (RangeDomain (Range l _))
         = h == l

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

class (Show r, Typeable r) => DataRepr r where
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
  reprMerge :: r -> [([Domain], Vector ())] -> [Domain] -> IO (Maybe (Vector ()))
  reprMerge _ _ _ = return Nothing
  reprSize :: r -> [Domain] -> Maybe Int
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

-- | Resource scheduling policy
data Schedule
  = SeqSchedule
  | ParSchedule
  deriving (Show, Eq)

-- | A strategy rule, explaining how to implement certain data flow patterns
newtype StratRule = StratRule (FlowI -> Maybe (Strategy ()))

-- | Schedule step
data Step where
  DomainStep :: Typeable a => DomainHandle a -> Step
    -- ^ Create a new domain
  SplitStep :: Typeable a => DomainHandle a -> [Step] -> Step
    -- ^ Split a domain into regions
  KernelStep :: KernelBind -> Step
    -- ^ Execute a kernel for the given domain(s)
  DistributeStep :: Typeable a => DomainHandle a -> Schedule -> [Step] -> Step
    -- ^ Distribute steps across the given domain using the given
    -- scheduling policy.

-- | List all kernels used in schedule
stepsToKernels :: [Step] -> [KernelBind]
stepsToKernels = concatMap go
  where go (KernelStep kb)            = [kb]
        go (DistributeStep _ _ steps) = concatMap go steps
        go _other                     = []
