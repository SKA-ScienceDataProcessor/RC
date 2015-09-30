{-# LANGUAGE FlexibleInstances, TypeFamilies, ScopedTypeVariables, GADTs, StandaloneDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Ord
import Data.Maybe

import Debug.Trace

import System.IO

newtype Flow ps a = Flow FlowI
  deriving Show

-- | Tag used for streams where we do not know the input data. Think
-- of it as referring to the output value, but not the data flow node.
data Anon
type AFlow a = Flow Anon a

aflow :: Flow ps a -> AFlow a
aflow (Flow fi) = Flow fi

data FlowI = FlowI
  { flHash    :: {-# UNPACK #-} !Int
  , flName    :: String
  , flDepends :: [FlowI]
  , flWildcard :: Maybe Int
  }
  deriving (Eq)

wildFlow :: Int -> Flow ps a
wildFlow i = Flow $ FlowI i ("wild" ++ show i) [] (Just i)

instance Hashable FlowI where
  i `hashWithSalt` (FlowI h _ _ _) = i `hashWithSalt` h
instance Show FlowI where
  showsPrec _ (FlowI _ n ds (Just w)) = showString "*" . showString n
  showsPrec _ (FlowI _ n ds Nothing)  = showString n . shows ds

class IsFlows fs where
  type Pars fs
  toList :: fs -> [FlowI]
  wilds :: fs
  fromList :: [FlowI] -> fs -- unsafe!
instance IsFlows () where
  type Pars () = ()
  toList () = []
  wilds = ()
  fromList [] = ()
instance IsFlows (Flow pa a) where
  type Pars (Flow pa a) = a
  toList (Flow f) = [f]
  wilds = (wildFlow 0)
  fromList [f] = (Flow f)
instance IsFlows (Flow pa a, Flow pb b) where
  type Pars (Flow pa a, Flow pb b) = (a, b)
  toList (Flow f0, Flow f1) = [f0, f1]
  wilds = (wildFlow 0, wildFlow 1)
  fromList [f0, f1] = (Flow f0, Flow f1)
instance IsFlows (Flow pa a, Flow pb b, Flow pc c) where
  type Pars (Flow pa a, Flow pb b, Flow pc c) = (a, b, c)
  toList (Flow f0, Flow f1, Flow f2) = [f0, f1, f2]
  wilds = (wildFlow 0, wildFlow 1, wildFlow 2)
  fromList [f0, f1, f2] = (Flow f0, Flow f1, Flow f2)
instance IsFlows (Flow pa a, Flow pb b, Flow pc c, Flow pd d) where
  type Pars (Flow pa a, Flow pb b, Flow pc c, Flow pd d) = (a, b, c, d)
  toList (Flow f0, Flow f1, Flow f2, Flow f3) = [f0, f1, f2, f3]
  wilds = (wildFlow 0, wildFlow 1, wildFlow 2, wildFlow 3)
  fromList [f0, f1, f2, f3] = (Flow f0, Flow f1, Flow f2, Flow f3)

type KernelFlow fs r = fs -> Flow (Pars fs) r
kernel :: IsFlows fs => String -> KernelFlow fs r
kernel name fs = Flow $ FlowI (hash (name, fis, noWild)) name fis noWild
  where fis = toList fs
        noWild = Nothing :: Maybe Int

data Repr
  = NoRepr
  | BufRepr

type KernelId = Int

data Kernel ps a
  = Kernel String (FlowI -> Strategy KernelI)

data KernelI = KernelI
  { kernId :: KernelId
  , kernFlow :: FlowI
  , kernName :: String
  }
  deriving Show

-- | Resource scheduling policy
data Schedule
  = SeqSchedule
  | ParSchedule
  deriving Show

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

-- | A strategy rule, explaining how to implement certain data flow patterns
newtype StratRule = StratRule (FlowI -> Maybe (Strategy ()))

-- | Kernel bindings
data KernelBind = KernelBind
  { smeKernel :: KernelI       -- ^ The kernel itself
  , smeDeps   :: [KernelId]    -- ^ Data dependencies
  }

instance Show KernelBind where
  showsPrec _ (KernelBind ki deps)
    = showString "Kernel " . shows (kernId ki) . showString ":" . showString (kernName ki)
      . showString " implementing " . showString (flName (kernFlow ki))
      . showString " using " . shows deps

-- | Schedule step
data Step where
  DomainStep :: Show a => DomainHandle a -> Step
    -- ^ Create a new domain
  SplitStep :: Show a => DomainHandle a -> DomainHandle a -> [Step] -> Step
    -- ^ Split a domain into regions
  KernelStep :: [DomainId] -> KernelBind -> Step
    -- ^ Execute a kernel for the given domain(s)
  DistributeStep :: Show a => DomainHandle a -> Schedule -> [Step] -> Step
    -- ^ Distribute steps across the given domain using the given
    -- scheduling policy.

deriving instance Show Step

-- | List all kernels used in schedule
stepsToKernels :: [Step] -> [KernelBind]
stepsToKernels = concatMap go
  where go (KernelStep _ kb)          = [kb]
        go (DistributeStep _ _ steps) = concatMap go steps
        go _other                     = []

type StratMap = HM.HashMap FlowI KernelBind
type Strategy a = State StratState a

-- | Floating flow - a kernel binding with a non-memoised value that
-- we might explicitly re-introduce at another part of the stregy
-- using "use".
data FFlow a = FFlow FlowI KernelBind

freshKernelId :: Strategy KernelId
freshKernelId = state $ \ss -> (ssKernelId ss, ss {ssKernelId = 1 + ssKernelId ss})

freshDomainId :: Strategy DomainId
freshDomainId = state $ \ss -> (ssDomainId ss, ss {ssDomainId = 1 + ssDomainId ss})

addStep :: Step -> Strategy ()
addStep step =  modify $ \ss -> ss { ssSteps = step : ssSteps ss }

uniqKernel :: IsFlows fl => String -> fl -> Strategy (Flow (Pars fl) a)
uniqKernel name fls = state $ \ss ->
  (kernel (name ++ "." ++ show (ssKernelId ss)) fls,
   ss {ssKernelId = 1 + ssKernelId ss})

prepareKernel :: Flow ps a -> [DomainId] -> Kernel ps a -> Strategy KernelBind
prepareKernel (Flow fi) ds (Kernel _ ks) = do

  -- Make kernel
  ki <- ks fi

  -- Look up dependencies (TODO: Only dependencies the concrete kernel
  -- implementation actually needs?)
  kis <- forM (flDepends fi) $ \p -> do

    -- Parameter flows must all be input flows
    when (p /= fi && (p `notElem` flDepends fi)) $
      fail $ "Parameter " ++ show p ++ " not a dependency of " ++ show fi ++ "!"

    -- Look up latest kernel ID
    ss <- get
    case HM.lookup p (ssMap ss) of
      Just sme | kern <- smeKernel sme
              -> return $ kernId kern
      Nothing -> do

        -- Not defined yet? Attempt to match a rule to produce it
        m_strat <- findRule (Flow p)
        case m_strat of
          Nothing -> fail $ "When binding kernel " ++ kernName ki ++ " to implement " ++
                            flName fi ++ ": Could not find a kernel calculating flow " ++
                            show p ++ "!"
          Just strat -> do

            -- Execute rule
            strat

            -- Lookup again. The rule should normaly guarantee that
            -- this doesn't fail any more.
            ss <- get
            case HM.lookup p (ssMap ss) of
              Just sme | kern <- smeKernel sme
                       -> return $ kernId kern
              Nothing  -> fail $ "When binding kernel " ++ kernName ki ++ " to implement " ++
                            flName fi ++ ": Failed to apply rule to calculate " ++ show p ++ "! " ++
                            "This should be impossible!"

  -- Add to kernel list
  let kern = KernelBind ki kis
  addStep $ KernelStep ds kern
  return kern

-- | Bind the given flow to a kernel. This is equivalent to first
-- setting a "rule" for the flow, then calling "calculate". More
-- efficient though.
bind :: Flow fs a -> Kernel fs a -> Strategy ()
bind (Flow fi) k = do
  entry <- prepareKernel (Flow fi) [] k
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss)}

bind1D :: DomainHandle d -> Flow fs a -> Kernel fs a -> Strategy ()
bind1D d (Flow fi) k = do
  entry <- prepareKernel (Flow fi) [dhId d] k
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss)}

rebind :: Flow fs a -> Kernel fs a -> Strategy ()
rebind = bind

rebind1D :: DomainHandle d -> Flow fs a -> Kernel fs a -> Strategy ()
rebind1D = bind1D

rule :: IsFlows fs => (fs -> Flow ps a) -> (fs -> Strategy ()) -> Strategy ()
rule flf strat = do

  -- Pass wildcard flows to function to get pattern
  let (Flow pat) = flf wilds

      -- Rule is now to match the given pattern, and if successful
      -- execute strategy and check that it actually implements the
      -- node.
      --
      -- TODO: We probably want to make a closure of the binds
      rule = StratRule $ \fi ->
        matchPattern fi pat >>=
        return . void . implementing (Flow fi) . strat

  modify $ \ss -> ss{ ssRules = rule : ssRules ss }

-- | Check whether a flow matches a pattern.
matchPattern :: forall fs. IsFlows fs => FlowI -> FlowI -> Maybe fs
matchPattern fi pat
  | fi == pat = Just wilds
  | Just i <- flWildcard pat
  = Just $ fromList $ set i fi $ toList (wilds :: fs)
  | flName fi == flName pat,
    length (flDepends fi) == length (flDepends pat),
    Just matches <- zipWithM matchPattern (flDepends fi) (flDepends pat)
  = mergeMatches matches
  | otherwise
  = Nothing
 where -- Sets n-th element in list. Edward Kmett is probably hating me
       -- now.
       set :: Int -> b -> [b] -> [b]
       set 0 x (_:ys) = x:ys
       set i x (y:ys) = y:set (i-1) x ys

-- | Merges data flow pattern match results
mergeMatches :: IsFlows fs => [fs] -> Maybe fs
mergeMatches []   = Just wilds
mergeMatches [fs] = Just fs
mergeMatches (fs0:fs1:rest) = do
  let merge :: FlowI -> FlowI -> Maybe FlowI
      merge f0 f1
        | Just{} <- flWildcard f0  = Just f1
        | Just{} <- flWildcard f1  = Just f0
        | f0 == f1                 = Just f0
        | otherwise                = Nothing
  fs' <- fromList <$> zipWithM merge (toList fs0) (toList fs1)
  mergeMatches (fs':rest)

-- | Calculate a flow. This can only succeed if there is a rule in scope that
-- explains how to do this.
calculate :: Flow fs a -> Strategy ()
calculate fl = do
  m_strat <- findRule fl
  case m_strat of
    Nothing -> fail $ "calculate: Could not find a rule matching " ++ show fl ++ "!"
    Just strat -> strat

-- | Calculate a flow. This can only succeed if there is a rule in scope that
-- explains how to do this.
findRule :: Flow fs a -> Strategy (Maybe (Strategy ()))
findRule (Flow fi) = do

  -- Find a matching rule
  rules <- ssRules <$> get
  let apply (StratRule r) = r fi
  return $ listToMaybe $ mapMaybe apply rules
    -- TODO: Warn about rule overlap?

float :: Flow fs a -> Kernel fs a -> Strategy (FFlow a)
float (Flow fi) k = do
  entry <- prepareKernel (Flow fi) [] k
  return $ FFlow fi entry

use :: FFlow a -> Strategy b -> Strategy b
use (FFlow fi entry) strat = state $ \ss ->
  let ss' = ss { ssMap = HM.insert fi entry (ssMap ss) }
      (r, ss'') = flip runState ss' strat
      ssMap' = case HM.lookup fi (ssMap ss) of
        Nothing  -> HM.delete fi (ssMap ss'')
        Just en0 -> HM.insert fi en0 (ssMap ss'')
  in (r, ss''{ ssMap = ssMap' } )

implementing :: Flow ps a -> Strategy () -> Strategy ()
implementing (Flow fi) strat = do
  -- Execute strategy
  strat
  -- Now verify that given flow was actually implemented
  ss <- get
  case HM.lookup fi (ssMap ss) of
    Just{}  -> return ()
    Nothing -> fail $ "Flow " ++ show fi ++ " was not implemented!"

-- ----------------------------------------------------------------------------
-- ---                             Domains                                  ---
-- ----------------------------------------------------------------------------

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

data Range = Range Int Int
  deriving Show

instance Show a => Show (DomainHandle a) where
  showsPrec _ dh = showString "Domain " . shows (dhId dh) . showString " [" .
                   foldr (.) id (intersperse (showString ", ") $
                                 map (shows . dhRegion dh) [0..dhSize dh-1]) .
                   showString "]"

rangeSize :: Range -> Int
rangeSize (Range low high) = high-low

-- | Create a new range domain
makeRangeDomain :: Range -> Strategy (DomainHandle Range)
makeRangeDomain range = do
  d <- mkDom []
  addStep $ DomainStep d
  return d
 where region []     _ = range
       region (n:ns) i
         | Range low high <- region ns (i `div` n)
         = Range (low + (high-low) * (i `mod` n) `div` n)
                 (low + (high-low) * ((i `mod` n)+1) `div` n)
       mkDom ns = do
         did' <- freshDomainId
         return $ DomainHandle did' (product ns) (region ns) (mkDom . (:ns))

-- | Split a domain into sub-regions. This creates a new partitioned region, which
-- can be used to distribute both computation as well as data.
split :: Show a => DomainHandle a -> Int -> (DomainHandle a -> Strategy ()) -> Strategy ()
split dh parts sub = modify $ \ss0 ->
  let (dh', ss1) = flip runState ss0 (dhSplit dh parts)
      (r, ss2) = flip runState ss1{ ssSteps = [] } (sub dh')
      splitStep = SplitStep dh' dh $ reverse $ ssSteps ss2
  in ss2{ ssSteps = splitStep : ssSteps ss1 }

-- | Perform computation in a distributed fashion.
distribute :: Show a => DomainHandle a -> Schedule -> Strategy () -> Strategy ()
distribute dh sched sub = modify $ \ ss0 ->
  let (r, ss1) = flip runState ss0{ ssSteps = [] } sub
      splitStep = DistributeStep dh sched $ reverse $ ssSteps ss1
  in ss1{ ssSteps = splitStep : ssSteps ss0 }

-- ----------------------------------------------------------------------------
-- ---                             Functional                               ---
-- ----------------------------------------------------------------------------

-- Data tags
data Tag -- ^ Initialisation (e.g. FFT plans)
data Vis -- ^ Visibilities (File name to OSKAR / raw visibilities / binned ...)
data UVGrid -- ^ UV grid
data Image -- ^ Image
data GCFs -- ^ A set of GCFs
data CleanResult -- ^ Result of cleaning (e.g. model + residual)

-- Abstract kernel signatures.
--
-- TODO: The string we use here is somewhat important for keeping them
-- apart - it would be more elegant if we could enforce them to be
-- unique in some other way.
--
-- TODO 2: All this would look *way* nicer with currying.
createGrid :: KernelFlow () UVGrid
createGrid = kernel "create grid"
grid :: KernelFlow (Flow a Vis, Flow b GCFs, Flow c UVGrid) UVGrid
grid = kernel "grid"
degrid :: KernelFlow (Flow a UVGrid, Flow b GCFs, Flow c Vis) Vis
degrid = kernel "degrid"
idft :: KernelFlow (Flow a Tag, Flow b UVGrid) Image
idft = kernel "idft"
dft :: KernelFlow (Flow a Tag, Flow b Image) UVGrid
dft = kernel "dft"
gcf :: KernelFlow (Flow a Tag, Flow b Vis) GCFs
gcf = kernel "gcf"
initRes :: KernelFlow () Image
initRes = kernel "residual init"
psfVis :: KernelFlow (Flow a Vis) Vis
psfVis = kernel "prepare vis for PSF"
clean :: KernelFlow (Flow a Image, Flow b Image) CleanResult
clean = kernel "clean"
cleanModel :: KernelFlow (Flow a CleanResult) Image
cleanModel = kernel "clean/model"
cleanResidual :: KernelFlow (Flow a CleanResult) Image
cleanResidual = kernel "clean/residual"

-- | Compound gridder actor
gridder :: Flow a Tag -> Flow b Vis -> Flow c GCFs -> Flow (Tag, UVGrid) Image
gridder tag vis gcfs = idft (tag, uvg)
 where uvg = grid (vis, gcfs, createGrid ())

-- | Compound degridder actor
degridder :: Flow a Tag -> Flow b Image -> Flow c Vis -> Flow d GCFs
          -> Flow (UVGrid, GCFs, Vis) Vis
degridder tag img vis gcfs = degrid (uvg, gcfs, vis)
 where uvg = dft (tag, img)

-- | Compound PSF gridder actor
psfGrid :: Flow a Tag -> Flow b Vis -> Flow c GCFs -> Flow (Tag, UVGrid) Image
psfGrid tag vis gcfs = gridder tag (psfVis vis) gcfs

-- | Compound cleaning actor
cleaner :: Flow a Image -> Flow b Image
        -> ( Flow CleanResult Image
           , Flow CleanResult Image)
cleaner dirty psf = (cleanResidual result, cleanModel result)
 where result = clean (dirty, psf)

-- | Compound major loop actor
majorLoop :: Int -> Flow a Tag -> Flow b Vis
         -> ( AFlow Image  -- ^ residual
            , AFlow Vis -- ^ visibilities
            )
majorLoop n tag vis = foldl go (aflow $ initRes (), aflow $ vis) [1..n]
 where gcfs = gcf (tag, vis)
       psf = psfGrid tag vis gcfs
       go (_res, vis) _i = (aflow res', aflow vis')
         where img = gridder tag vis gcfs
               (res', mod) = cleaner img psf
               vis' = degridder tag mod vis gcfs

-- ----------------------------------------------------------------------------
-- ---                               Kernels                                ---
-- ----------------------------------------------------------------------------

data Config = Config
  { cfgInput :: [(FilePath, Int)]
  , cfgOutput :: FilePath
  , cfgMajorLoops :: Int
  , cfgGrid :: GridPar
  }
data GridPar = GridPar

dummy :: String -> Kernel ps a
dummy n = Kernel n $ \fl -> do
  i <- freshKernelId
  return $ KernelI i fl n

halideWrapper :: String -> Kernel ps a
halideWrapper _ = dummy "halide"
cWrapper :: a -> Kernel ps b
cWrapper _ = dummy "c"

oskarReader :: [(FilePath, Int)] -> Kernel () Vis
oskarReader _ = dummy "oskar"
sorter :: Kernel a Vis
sorter = dummy "sorter"
setOnes :: Kernel Vis Vis
setOnes = dummy "ones"

gcfKernel :: GridPar -> Kernel (Tag, Vis) GCFs
gcfKernel _ = halideWrapper "gcfs"

fftCreatePlans :: GridPar -> Kernel () Tag
fftCreatePlans _ = dummy "fftPlans"
fftKern :: GridPar -> Kernel (Tag, Image) UVGrid
fftKern _ = dummy "fftKern"
ifftKern :: GridPar -> Kernel (Tag, UVGrid) Image
ifftKern _ = dummy "ifftKern"

gridInit :: GridPar -> Kernel () UVGrid
gridInit _ = dummy "gridInit"
gridKernel :: GridPar -> Kernel (Vis, GCFs, UVGrid) UVGrid
gridKernel _ = dummy "gridKernel"
degridKernel :: GridPar -> Kernel (UVGrid, GCFs, Vis) Vis
degridKernel _ = dummy "degridKernel"

cleanKernel :: Kernel (Image, Image) CleanResult
cleanKernel = halideWrapper "clean"
splitModel :: Kernel CleanResult Image
splitModel = dummy "splitModel"
splitResidual :: Kernel CleanResult Image
splitResidual = dummy "splitResidual"

imageSum :: Kernel a Image
imageSum = dummy "image summation"
imageWriter :: FilePath -> Kernel a Image
imageWriter _ = dummy "image writer"

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

scatterImaging :: Config -> DomainHandle d -> Flow a Tag -> Flow b Vis -> Strategy ()
scatterImaging cfg dh tag vis =
 implementing (fst $ majorLoop (cfgMajorLoops cfg) tag vis) $ do

  -- Sort visibility data
  rebind1D dh vis sorter

  -- Initialise FFTs
  let gpar = cfgGrid cfg
  bind1D dh tag $ cWrapper $ fftCreatePlans gpar

  -- Generate GCF
  let gcfs = gcf (tag, vis)
  bind1D dh gcfs (gcfKernel gpar)

  -- Make rules
  rule idft       $ \inp -> bind1D dh (idft inp) (ifftKern gpar)
  rule dft        $ \inp -> bind1D dh (dft inp) (fftKern gpar)
  rule createGrid $ \inp -> bind1D dh (createGrid inp) (gridInit gpar)
  rule grid       $ \inp -> bind1D dh (grid inp) (gridKernel gpar)
  rule degrid     $ \inp -> bind1D dh (degrid inp) (degridKernel gpar)
  rule clean      $ \inp -> bind1D dh (clean inp) cleanKernel
  rule cleanResidual $ \inp -> bind1D dh (cleanResidual inp) splitResidual
  rule cleanModel $ \inp -> bind1D dh (cleanModel inp) splitModel

  -- PSF
  bind1D dh (psfVis vis) (cWrapper setOnes)
  calculate $ psfGrid tag vis gcfs

  -- Loop
  forM_ [1..cfgMajorLoops cfg-1] $ \i -> do

    -- Force grid calculation - we do not want to share this between
    -- loop iterations!
    calculate $ createGrid ()

    -- Generate new visibilities
    calculate $ snd $ majorLoop i tag vis

  -- Calculate residual of last loop iteration
  calculate $ createGrid ()
  calculate $ fst $ majorLoop (cfgMajorLoops cfg) tag vis

-- Strategy implements imaging loop for a number of data sets
scatterImagingMain :: Config -> Strategy ()
scatterImagingMain cfg = do

  -- Make data set domain
  let dataSets = length (cfgInput cfg)
      dataSetRepeats = sum $ map snd $ cfgInput cfg
  dom <- makeRangeDomain (Range 0 dataSetRepeats)

  -- Create data flow for visibilities, build abstract data flow to do
  -- configured number of major loops over this input data
  vis <- uniqKernel "vis" ()
  tag <- uniqKernel "tag" ()
  let result = fst $ majorLoop (cfgMajorLoops cfg) tag vis

  -- Split by datasets
  split dom dataSets $ \ds -> distribute ds ParSchedule $ void $ do

    -- Split by number of runs.
    -- TODO: Number of runs should depend on data set!
    split ds 300 $ \repeat -> distribute ds SeqSchedule $ void $ do

      -- Read in visibilities. The domain handle passed in tells the
      -- kernel which of the datasets to load.
      bind1D ds vis $ oskarReader $ cfgInput cfg

      -- Implement this data flow
      implementing result $ void $ scatterImaging cfg repeat tag vis

    -- Sum up local images (TODO: accumulate?)
    rebind1D ds result imageSum

  -- Sum and write out the result
  rebind result imageSum
  rebind result (imageWriter $ cfgOutput cfg)

-- ----------------------------------------------------------------------------
-- ---                               Driver                                 ---
-- ----------------------------------------------------------------------------

dumpStrategy :: Strategy a -> IO ()
dumpStrategy strat = do

  -- Construct strategy map
  let kerns = stepsToKernels $ ssSteps $ execState strat initStratState

  -- Generate sorted kernel
  let kerns' = sortBy (comparing (kernId . smeKernel)) kerns

  let dumpKern (KernelBind (KernelI kid kfl kname) deps) = do
        putStrLn $ concat
          [ "Kernel ", show kid, ":", kname, " implementing ", flName kfl
          , " using ", show deps ]
  forM_ kerns' dumpKern

dumpStrategyDOT :: FilePath -> Strategy a -> IO ()
dumpStrategyDOT file strat = do

  -- Construct strategy map
  let kerns = stepsToKernels $ ssSteps $ execState strat initStratState

  -- Generate sorted kernel
  let kerns' = sortBy (comparing (kernId . smeKernel)) kerns

  -- Open output file
  h <- openFile file WriteMode
  hPutStrLn h "digraph strategy {"
  let kernName kid = "kernel" ++ show kid
  let dumpKern (KernelBind (KernelI kid kfl kname) deps) = do
        hPutStrLn h $ concat
          [ kernName kid, " [label=\"" ++ kname, " implementing ", flName kfl, "\"]"]
        forM_ (deps) $ \kid' ->
          hPutStrLn h $ concat
            [ kernName kid', " -> ", kernName kid]
  forM_ kerns' dumpKern
  hPutStrLn h "}"
  hClose h

dumpSteps :: Strategy a -> IO ()
dumpSteps strat = do

  -- Construct strategy map
  let steps = reverse $ ssSteps $ execState strat initStratState
      dump ind (DomainStep dh)
        = putStrLn $ ind ++ "Domain " ++ show dh
      dump ind (SplitStep dh' dh steps)
        = do putStrLn $ ind ++ "Split Domain " ++ show (dhId dh) ++ " into " ++ show dh'
             forM_ steps (dump ("  " ++ ind))
      dump ind (KernelStep dids kb)
        = putStrLn $ ind ++ "Kernel " ++ show dids ++ " " ++ show kb
      dump ind (DistributeStep did sched steps)
        = do putStrLn $ ind ++ "Distribute " ++ show did ++ " using " ++ show sched
             forM_ steps (dump ("  " ++ ind))

  forM_ steps (dump "")

testStrat :: Strategy ()
testStrat = scatterImagingMain $ Config
    [("input.vis", 300),
     ("input2.vis", 300),
     ("input3.vis", 300)]
    "output.img"
    10
    GridPar

main :: IO ()
main = dumpSteps testStrat
