{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, ScopedTypeVariables,
             TypeOperators, DeriveDataTypeable, TypeSynonymInstances,
             FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving
 #-}

module Flow.Internal where

import Control.Monad.State.Strict

import Data.Binary hiding (get, put)
import qualified Data.Binary as B
import Data.Function ( on )
import Data.Hashable
import Data.Int
import qualified Data.IntMap as IM
import Data.List     ( sort, groupBy )
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Typeable

import DNA (ProfileHint)
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

-- | Abstract flow type, standing for a node in the global data flow.
-- This represents some data the program is going to produce during
-- its execution. For example, a "Flow" might stand for the entirety
-- of all images produced by a gridding process.
--
-- Note that this data might end up being distributed, and that we
-- might chose different encodings for the data depending on program
-- stage. We especially do not promise that the data will be available
-- in its entirety at any given point in time -- we might for example
-- decide to sequentially distribute ("stream") the flow data
-- representation.
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
         => String
         -> [ProfileHint]
         -> KernelCode
         -> [(FlowI, ReprI)]
         -> r
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
  , kernHints :: [ProfileHint]
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
-- 'RegionData' and produces data for given 'RegionBox'(es).
type KernelCode = [RegionData] -> [RegionBox] -> IO [Vector ()]

instance Show KernelBind where
  showsPrec _ (KernelBind kid kflow kname krepr kdeps _ _ _)
    = showString "Kernel " . shows kid . showString ":" . showString kname
      . showString " implementing " . showString (flName kflow)
      . showString " producing " . shows krepr
      . showString " using " . shows kdeps

type DomainId = Int

-- | Handle to a domain, or more precisely a distinct 'Region' set of
-- the domain. The type parameter @a@ names the locality assumption
-- associated with the domain, which decides the 'Region' type
-- (see 'Range' or 'Bins').
data Domain a = Domain
  { dhId    :: DomainId
    -- | Produce a new domain handle that is split up @n@ times more
  , dhSplit  :: Int -> Strategy (Domain a)
    -- | Creates the root region for this domain using given
    -- data. This is the region constructor for root domains (those
    -- without parents)
  , dhCreate :: RegionData -> IO Region
    -- | Splits out regions from the parent region. This is used for
    -- non-root domains.
  , dhRegion :: Region -> IO [Region]
    -- | Get domain this one was derived from. Not set for root domains.
  , dhParent :: Maybe (Domain a)
    -- | Check whether a a region of this domain is permissible in the
    -- context of a given region box
  , dhFilterBox :: RegionBox -> Region -> Maybe Region
    -- | Restrict a list of regions of a certain domain to the
    -- regions that lie within a region of a parent domain.
  , dhRestrict :: Region -> [Region] -> [Region]
    -- | Write a region of this domain
  , dhPutRegion :: Region -> Put
    -- | Read a region of this domain
  , dhGetRegion :: GetContext -> Get Region
  }

instance Typeable a => Show (Domain a) where
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
  DomainI :: forall a. Typeable a => Domain a -> DomainI

dhiId :: DomainI -> DomainId
dhiId (DomainI di) = dhId di

instance Eq DomainI where
  di0 == di1  = dhiId di0 == dhiId di1
instance Show DomainI where
  show (DomainI dom) = show dom

-- | A region of a 'Domain'. Every region is uniquely associated with
-- its domain as well as guaranteed to have no overlap with other
-- regions of the same 'Domain' region set.
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

-- | Range between two integer values of the form @[low,high[@. This type
-- is also used to characterise 'Range' 'Domain's.
data Range = Range Int Int
  deriving (Typeable, Eq, Ord)
instance Show Range where
  showsPrec _ (Range low high) = shows low . (':':) . shows high

-- | Bins grouping together indices according to a certain data
-- property @f@. Each bin corresponds to a @Double@ value range of
-- @f@.
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
newtype Strategy a = Strategy { unStrategy :: State StratState a }
  deriving (Monad, Applicative, Functor)

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
freshKernelId = Strategy $ state $ \ss -> (ssKernelId ss, ss {ssKernelId = 1 + ssKernelId ss})

-- | Make a fresh domain ID
freshDomainId :: Strategy DomainId
freshDomainId = Strategy $ state $ \ss -> (ssDomainId ss, ss {ssDomainId = 1 + ssDomainId ss})

-- | Add a step to the strategy
addStep :: Step -> Strategy ()
addStep step = Strategy $ modify $ \ss -> ss { ssSteps = step : ssSteps ss }

-- | Run "Strategy" monad to convert it into a series of steps
runStrategy :: Strategy () -> [Step]
runStrategy strat = reverse $ ssSteps $ execState (unStrategy strat) initStratState

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

-- | Resource scheduling policy. When splitting up work, this decides
-- how we arrange the individual pieces of work in relation to each other.
data Schedule
  = SeqSchedule -- ^ Sequentialise work: Loop over all 'Region's.
  | ParSchedule -- ^ Parallelise work: Allocate nodes and give each one 'Region' to work on.
  deriving (Show, Eq)

-- | A strategy rule, explaining how to implement certain data flow patterns
newtype StratRule = StratRule (FlowI -> Maybe (Strategy ()))

-- | Schedule step
data Step where

  DomainStep :: Typeable a => Maybe KernelId -> Domain a -> Step
    -- Create a new domain. There are two main cases here:
    --
    --  1. "dhParent" is not set: Then this is a root domain with
    --  exactly one region attached to it.
    --
    --    do bla <- makeDomain reg  -- => { bla -> [reg] }
    --
    --  2. Otherwise this is a split domain, which corresponds to
    --  multiple regions at the same time:
    --
    --    do bla <- makeDomain reg        -- => { bla -> [reg] }
    --       blub <- makeSplitDomain bla  -- => { bla -> [reg] ,
    --                                            blub -> [reg0, reg1, reg2...] }
    --
    --  with - presumably - "reg = reg0+reg1+reg2+..." for some sort
    --  of sensible definition. This is however not enforced. [And I
    --  actually see no reason why a split domain couldn't be a root
    --  domain, but it's probably future work to see whether this
    --  makes sense.]
    --
    --  In either case, runtime data can be used to inform domain
    --  creation, given as a woefully untyped buffer. This can be used
    --  both to determine region data (for example read the number of
    --  visibilities from a file) as well as determine the number of
    --  split regions that are created (for example to dynamically
    --  determinine the amount of parallelism we want).
    --
    --  The split structure of the domain governs two things, which
    --  are meant to be kept in a synchronised fashion during runtime:
    --
    --   1. Data distribution. If the data representation mentions a
    --      split domain, this means that the data is meant to be
    --      maintained split up into buffers accordingly.
    --
    --   2. Work distribution: Using "DistributeStep" below we can
    --      choose to also align control flow with it
    --
    --  The underlying assumption is locality: If two pieces of data
    --  or computation relate to the same domain, they are related
    --  exactly if they also relate to the same region. Otherwise we
    --  will hide them from each other. This is what allows this to
    --  distribute this effectively.

  KernelStep :: KernelBind -> Step
    -- Execute a kernel for the given domain. The produced data
    -- format is defined by "kernRepr". This especially means that
    -- this is expected to produce one buffer per locally visible
    -- region box that fits the domain combination given by
    -- "reprDomain . kernRepr".
    --
    -- The locality property from above means that all parameters get
    -- "selected" by the output regions: If a parameter domain is the
    -- same as a return domain, the parameter is restricted to only
    -- the domains that we want to produce output for. See
    -- "DistributeStep" for how this works in the context of
    -- distribution.

  RecoverStep :: KernelBind -> KernelId -> Step
    -- Execute a kernel for all regions that the kernel with the
    -- given ID failed to produce - because of crashes, for example.
    --
    -- The result of a recover step will be a region that is
    -- guaranteed to be complete. Therefore the program can not
    -- terminate because of missing data produced by the designated
    -- Kernel.

  DistributeStep :: Typeable a => Domain a -> Schedule -> [Step] -> Step
    -- Distribute steps across the given domain using the given
    -- scheduling policy. This means that we execute the given steps
    -- as many times as the mentioned domain has regions, restricting
    -- the "visibility" of the domain to just the region in question
    -- [TODO: Pretty sure we could generalise this to more than one
    -- region depending on "Schedule". But let's roll with it for
    -- simplicity.]
    --
    -- Visibility in the context means that for all contained steps:
    --
    --  1. We consider the domain to only contain one region
    --
    --  2. We consider all data with a representation depending on the
    --     domain to only have buffers concerning the singular region.
    --     This means that for distributed computation we also only
    --     ever need to transfer this data
    --
    --  3. On the flipside, all "KernelStep" that produce data
    --     associated with the domain needs only produce buffers for
    --     the region we are interested in.
    --
    --  So for example:
    --
    --    do let x = flow "x"; y = flow "y" x; z = flow "z" y; a = flow "a" z
    --       bla <- makeDomain reg        -- { bla -> [reg] }
    --       blub <- makeSplitDomain bla  -- { blub -> [reg0, reg1, reg2...] }
    --       bind x (kernel bla)          -- { out: x.0 -> [reg=x] }
    --       bind y (splitter bla blub x) -- { in: x.0 -> [reg=x]
    --                                         out: y.0 -> [reg0=y0, reg1=y1, reg2=y2] }
    --       distribute blub SeqSchedule $
    --         -- (y.0 is split here because it depends on domain "blub")
    --         bind z (process blub y)
    --            -- iteration/process 1: { in: y.0 -> [reg0=y0], out: z.0 -> [reg0=z0] }
    --            -- iteration/process 2: { in: y.0 -> [reg1=y1], out: z.0 -> [reg1=z1] }
    --            -- iteration/process 3: { in: y.0 -> [reg2=y2], out: z.0 -> [reg2=z2] }
    --         -- (y.0 is out of scope now, but z.0 gets merged back)
    --
    --       bind a (merger blub bla y)   -- { in: z.0 -> [reg0=z0, reg1=z1, reg2=z2]
    --                                         out: a.0 -> [reg=a] }
    --
    -- Note that data is *only* split if it *explicitly* mentions the
    -- domain. If the kernels are implemented correctly (!) this
    -- should not change semantics. So for example, moving the
    -- splitter into "distribute" should work equally well:
    --
    --       distribute blub SeqSchedule $
    --         -- (x.0 is *not* split here)
    --         bind y (splitter bla blub x)
    --            -- iteration/process 1: { in: x.0 -> [reg=x], out: y.0 -> [reg0=y0] }
    --            -- iteration/process 2: { in: x.0 -> [reg=x], out: z.0 -> [reg1=y1] }
    --            -- iteration/process 3: { in: x.0 -> [reg=x], out: z.0 -> [reg2=y2] }
    --         bind z (process blub y)
    --            -- iteration/process 1: { in: y.0 -> [reg0=y0], out: z.0 -> [reg0=z0] }
    --            -- iteration/process 2: { in: y.0 -> [reg1=y1], out: z.0 -> [reg1=z1] }
    --            -- iteration/process 3: { in: y.0 -> [reg2=y2], out: z.0 -> [reg2=z2] }
    --
    -- And continue as usual. Note that here "x" is actually presented
    -- in full to every distributed iteration/process - which might or
    -- might not be a bad idea depending on whether the amount of data
    -- transfer outweighs the benefits of having the work of
    -- "splitter" done in parallel. In either case, we permit this.
    --
    -- Note that so far we have only explained these ideas for single
    -- regions, and not region boxes. Suffice to say that all this
    -- generalises properly to n boxes - we split and merge entire
    -- rows or columns of data whenever *any* of the domain
    -- combinations is present.

-- | List all kernels used in schedule
stepsToKernels :: [Step] -> [KernelBind]
stepsToKernels = concatMap go
  where go (KernelStep kb)            = [kb]
        go (DistributeStep _ _ steps) = concatMap go steps
        go _other                     = []

-- | Context we need for unmarshalling kernel and domain IDs. This is
-- simply a collection of "KernelBind"s and "DomainI"s respectively,
-- indexed by their IDs.
type GetContext =  (IM.IntMap KernelBind, IM.IntMap DomainI)

putRegionData :: RegionData -> Put
putRegionData rdata = do
  B.put (Map.size rdata)
  forM_ (Map.assocs rdata) $ \(rbox, vec) -> do
    putRegionBox rbox
    putVector vec

getRegionData :: GetContext -> Get RegionData
getRegionData ctx = do
  rdsize <- B.get :: Get Int
  Map.fromList <$> replicateM rdsize ((,) <$> getRegionBox ctx <*> getVector)

putRegionBox :: RegionBox -> Put
putRegionBox rbox = do
  B.put (length rbox)
  forM_ rbox $ \reg -> case regionDomain reg of
    DomainI dom -> B.put (dhId dom) >> dhPutRegion dom reg

getRegionBox :: GetContext -> Get RegionBox
getRegionBox ctx = do
  rsize <- B.get :: Get Int
  replicateM rsize (do
    did <- B.get :: Get Int
    case IM.lookup did (snd ctx) of
      Just (DomainI dom) -> dhGetRegion dom ctx
      Nothing            -> fail $ "getRegionBox: Unknown domain ID " ++ show did ++ "!")
