{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, ScopedTypeVariables #-}

module Strategy.Internal where

import Control.Monad.State.Strict

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Typeable
import qualified Data.ByteString as BS

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
  , kernDeps :: [KernelId] -- ^ Kernel dependencies
  , kernCode :: KernelCode -- ^ Code to execute the kernel
  , kernReprCheck :: ReprI -> Bool
    -- ^ Check whether a sink data representation is compatible with
    -- the data we produce
  }

-- | Code implementing a kernel
type KernelCode = [BS.ByteString] -> IO BS.ByteString

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
    -- | Get indexed region
  , dhRegion :: Int -> a
    -- | Produce a new domain handle that is split up @n@ times more
  , dhSplit :: Int -> Strategy (DomainHandle a)
  }

instance forall a. Typeable a => Show (DomainHandle a) where
  showsPrec _ dh = showString "Domain " . shows (dhId dh) . showString " [" .
                   shows (typeOf (undefined :: a)) . showString "]"

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
  type RPar r
  -- | Does the representation contain no data? This means that we are
  -- going to ignore it.
  reprNop :: r -> Bool
  reprNop _ = False
  reprAccess :: r -> ReprAccess
  reprCompatible :: r -> r -> Bool
  reprCompatible _ _ = True

-- | Who has ownership of the data representation?
data ReprAccess
  = ReadAccess
    -- ^ Caller has ownership. Kernel should not change the buffer.
  | WriteAccess
    -- ^ Callee has ownership. Kernel can change the buffer, and must
    -- free it if it is not returned. Might require framework to
    -- duplicate the buffer.

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
  deriving Show

-- | A strategy rule, explaining how to implement certain data flow patterns
newtype StratRule = StratRule (FlowI -> Maybe (Strategy ()))

-- | Schedule step
data Step where
  DomainStep :: Typeable a => DomainHandle a -> Step
    -- ^ Create a new domain
  SplitStep :: Typeable a => DomainHandle a -> DomainHandle a -> [Step] -> Step
    -- ^ Split a domain into regions
  KernelStep :: [DomainId] -> KernelBind -> Step
    -- ^ Execute a kernel for the given domain(s)
  DistributeStep :: Typeable a => DomainHandle a -> Schedule -> [Step] -> Step
    -- ^ Distribute steps across the given domain using the given
    -- scheduling policy.

-- | List all kernels used in schedule
stepsToKernels :: [Step] -> [KernelBind]
stepsToKernels = concatMap go
  where go (KernelStep _ kb)          = [kb]
        go (DistributeStep _ _ steps) = concatMap go steps
        go _other                     = []
