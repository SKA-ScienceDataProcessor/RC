{-# LANGUAGE FlexibleInstances, TypeFamilies, ScopedTypeVariables #-}

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

kernel :: IsFlows fs => String -> fs -> Flow (Pars fs) a
kernel name fs = Flow $ FlowI (hash (name, fis, noWild)) name fis noWild
  where fis = toList fs
        noWild = Nothing :: Maybe Int

type KernelId = Int

newtype Kernel ps a = Kernel (FlowI -> Strategy KernelI)
data KernelI = KernelI
  { kernId :: KernelId
  , kernFlow :: FlowI
  , kernName :: String
  }
  deriving Show

-- | State while constructing a strategy
data StratState = StratState
  { ssKernelId :: {-# UNPACK #-}!KernelId -- ^ Next fresh kernel ID
  , ssMap      :: StratMap          -- ^ Kernels currently in scope
  , ssKernels  :: [StratMapEntry]   -- ^ Complete list of kernels, including out-of-scope ones
  , ssRules    :: [StratRule]       -- ^ Currently active rules
  }
initStratState :: StratState
initStratState = StratState 0 HM.empty [] []

-- | A strategy rule, explaining how to implement certain data flow patterns
newtype StratRule = StratRule (FlowI -> Maybe (Strategy ()))

data StratMapEntry = StratMapEntry
  { smeKernel :: KernelI       -- ^ The kernel itself
  , smeDeps   :: [KernelId]    -- ^ Data dependencies
  }
type StratMap = HM.HashMap FlowI StratMapEntry
type Strategy a = State StratState a
data FFlow a = FFlow FlowI StratMapEntry

stratFresh :: Strategy KernelId
stratFresh = state $ \ss -> (ssKernelId ss, ss {ssKernelId = 1 + ssKernelId ss})

uniqKernel :: IsFlows fl => String -> fl -> Strategy (Flow (Pars fl) a)
uniqKernel name fls = state $ \ss ->
  (kernel (name ++ "." ++ show (ssKernelId ss)) fls,
   ss {ssKernelId = 1 + ssKernelId ss})

prepareKernel :: Flow ps a -> Kernel ps a -> Strategy StratMapEntry
prepareKernel (Flow fi) (Kernel ks) = do

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
  let kern = StratMapEntry ki kis
  modify $ \ss -> ss { ssKernels = kern : ssKernels ss }
  return kern

-- | Bind the given flow to a kernel. This is equivalent to first
-- setting a "rule" for the flow, then calling "calculate". More
-- efficient though.
bind :: Flow fs a -> Kernel fs a -> Strategy ()
bind (Flow fi) k = do
  entry <- prepareKernel (Flow fi) k
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss) }

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
  entry <- prepareKernel (Flow fi) k
  return $ FFlow fi entry

use :: FFlow a -> Strategy b -> Strategy b
use (FFlow fi entry) strat = state $ \ss ->
  let ss' = ss { ssMap = HM.insert fi entry (ssMap ss) }
      (r, ss'') = flip runState ss' strat
      ssMap' = case HM.lookup fi (ssMap ss) of
        Nothing  -> HM.delete fi (ssMap ss'')
        Just en0 -> HM.insert fi en0 (ssMap ss'')
  in (r, ss''{ ssMap = ssMap' } )

implementing :: Flow ps a -> Strategy () -> Strategy (AFlow a)
implementing (Flow fi) strat = do
  -- Execute strategy
  strat
  -- Now verify that given flow was actually implemented
  ss <- get
  case HM.lookup fi (ssMap ss) of
    Just{}  -> return (Flow fi)
    Nothing -> fail $ "Flow " ++ show fi ++ " was not implemented!"

-- ----------------------------------------------------------------------------
-- ---                             Functional                               ---
-- ----------------------------------------------------------------------------

data Tag
data Vis
data UVGrid
data Image
data GCFs

type GridFlow = ( Flow Tag UVGrid
                , Flow (Tag, Vis, GCFs, UVGrid) UVGrid
                , Flow (Tag, UVGrid) Image)
gridder :: Flow a Tag -> Flow b Vis -> Flow c GCFs -> GridFlow
gridder tag vis gcfs = (create, grid, img)
 where create = kernel "create grid" tag
       grid = kernel "grid" (tag, vis, gcfs, create)
       img = kernel "idft" (tag, grid)

type DegridFlow = ( Flow (Tag, Image) UVGrid
                  , Flow (Tag, UVGrid, GCFs, Vis) Vis)
degridder :: Flow a Tag -> Flow b Image -> Flow c Vis -> Flow d GCFs
          -> DegridFlow
degridder tag img vis gcfs = (dft, degrid)
 where dft = kernel "dft" (tag, img)
       degrid = kernel "degrid" (tag, dft, gcfs, vis)

gcf :: Flow a Tag -> Flow b Vis -> Flow (Tag, Vis) GCFs
gcf tag vis = kernel "gcf" (tag,vis)

initRes :: Flow a Tag -> Flow Tag Image
initRes tag = kernel "residual init" (tag)

psfVis :: Flow b Vis -> Flow Vis Vis
psfVis vis = kernel "prepare vis for PSF" (vis)

psfGrid :: Flow a Tag -> Flow b Vis -> Flow c GCFs -> GridFlow
psfGrid tag vis gcfs = gridder tag (psfVis vis) gcfs

data CleanResult
clean :: Flow a Image -> Flow b Image
     -> ( Flow (Image, Image) CleanResult
        , Flow CleanResult Image
        , Flow CleanResult Image)
clean dirty psf = (result, res, mod)
 where result = kernel "clean" (dirty, psf)
       res = kernel "clean/residual" (result)
       mod = kernel "clean/model" (result)

majorLoop :: Int -> Flow a Tag -> Flow b Vis
         -> ( AFlow Image  -- ^ residual
            , AFlow Vis -- ^ visibilities
            )
majorLoop n tag vis = foldl go (aflow $ initRes tag, aflow $ vis) [1..n]
 where gcfs = gcf tag vis
       (_, _, psfImg) = psfGrid tag vis gcfs
       go (_res, vis) _i = (aflow res', aflow vis')
         where (_, _, img) = gridder tag vis gcfs
               (_, res', mod) = clean img psfImg
               (_, vis') = degridder tag mod vis gcfs

-- ----------------------------------------------------------------------------
-- ---                               Kernels                                ---
-- ----------------------------------------------------------------------------

data Config = Config
  { cfgInput :: FilePath
  , cfgMajorLoops :: Int
  , cfgGrid :: GridPar
  }
data GridPar = GridPar

dummy :: String -> Kernel ps a
dummy n = Kernel $ \fl -> do
  i <- stratFresh
  return $ KernelI i fl n

halideWrapper :: String -> Kernel ps a
halideWrapper _ = dummy "halide"
cWrapper :: a -> Kernel ps b
cWrapper _ = dummy "c"

oskarReader :: String -> Kernel () Vis
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

gridInit :: GridPar -> Kernel Tag UVGrid
gridInit _ = dummy "gridInit"
gridKernel :: GridPar -> Kernel (Tag, Vis, GCFs, UVGrid) UVGrid
gridKernel _ = dummy "gridKernel"
degridKernel :: GridPar -> Kernel (Tag, UVGrid, GCFs, Vis) Vis
degridKernel _ = dummy "degridKernel"

cleanKernel :: Kernel (Image, Image) CleanResult
cleanKernel = halideWrapper "clean"
splitModel :: Kernel CleanResult Image
splitModel = dummy "splitModel"
splitResidual :: Kernel CleanResult Image
splitResidual = dummy "splitResidual"

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

scatterImaging :: Config -> Flow a Tag -> Flow b Vis -> Strategy (AFlow Image)
scatterImaging cfg tag vis =
 implementing (fst $ majorLoop (cfgMajorLoops cfg) tag vis) $ do

  -- Sort visibility data
  bind vis sorter

  -- Initialise FFTs
  let gpar = cfgGrid cfg
  fftTag <- float tag $ cWrapper $ fftCreatePlans gpar

  -- Make rules
  rule (kernel "idft") $ \(inp  :: (AFlow Tag, AFlow UVGrid)) ->
    use fftTag $ bind (kernel "idft" inp) (ifftKern gpar)
  rule (kernel "dft") $ \(inp  :: (AFlow Tag, AFlow Image)) ->
    use fftTag $ bind (kernel "dft" inp) (fftKern gpar)
  rule (kernel "create grid") $ \(inp :: AFlow Tag) ->
    bind (kernel "create grid" inp) (gridInit gpar)
  rule (kernel "grid") $ \(inp :: (AFlow Tag, AFlow Vis, AFlow GCFs, AFlow UVGrid)) ->
    bind (kernel "grid" inp) (gridKernel gpar)
  rule (kernel "degrid") $ \(inp :: (AFlow Tag, AFlow UVGrid, AFlow GCFs, AFlow Vis)) ->
    bind (kernel "degrid" inp) (degridKernel gpar)
  rule (kernel "clean") $ \(inp :: (AFlow Image, AFlow Image)) ->
    bind (kernel "clean" inp) cleanKernel
  rule (kernel "clean/residual") $ \(inp :: (AFlow CleanResult)) ->
    bind (kernel "clean/residual" inp) splitResidual
  rule (kernel "clean/model") $ \(inp :: (AFlow CleanResult)) ->
    bind (kernel "clean/model" inp) splitModel

  -- Generate GCF
  let gcfs = gcf tag vis
  bind gcfs (gcfKernel gpar)

  -- PSF
  let (psfCreate, psfGridK, psf) = psfGrid tag vis gcfs
  bind (psfVis vis) (cWrapper setOnes)
  calculate psf

  -- Loop
  forM_ [1..cfgMajorLoops cfg-1] $ \i -> do

    -- Force grid calculation - we do not want to share this between
    -- loop iterations!
    calculate $ kernel "create grid" tag

    -- Generate new visibilities
    calculate $ snd $ majorLoop i tag vis

  -- Calculate residual of last loop iteration
  calculate $ fst $ majorLoop (cfgMajorLoops cfg) tag vis

-- Strategy implements major loop for these visibilities
scatterImagingMain :: Config -> Strategy (AFlow Image)
scatterImagingMain cfg = do

  tag <- uniqKernel "tag" ()
  bind tag (dummy "tag")

  -- Read in visibilities
  vis <- uniqKernel "vis" ()
  bind vis (oskarReader $ cfgInput cfg)

  scatterImaging cfg tag vis

-- ----------------------------------------------------------------------------
-- ---                               Driver                                 ---
-- ----------------------------------------------------------------------------

dumpStrategy :: Strategy a -> IO ()
dumpStrategy strat = do

  -- Construct strategy map
  let kerns = ssKernels $ execState strat initStratState

  -- Generate sorted kernel
  let kerns' = sortBy (comparing (kernId . smeKernel)) kerns

  let dumpKern (StratMapEntry (KernelI kid kfl kname) deps) = do
        putStrLn $ concat
          [ "Kernel ", show kid, ":", kname, " implementing ", flName kfl
          , " using ", show deps ]
  forM_ kerns' dumpKern

dumpStrategyDOT :: FilePath -> Strategy a -> IO ()
dumpStrategyDOT file strat = do

  -- Construct strategy map
  let kerns = ssKernels $ execState strat initStratState

  -- Generate sorted kernel
  let kerns' = sortBy (comparing (kernId . smeKernel)) kerns

  -- Open output file
  h <- openFile file WriteMode
  hPutStrLn h "digraph strategy {"
  let kernName kid = "kernel" ++ show kid
  let dumpKern (StratMapEntry (KernelI kid kfl kname) deps) = do
        hPutStrLn h $ concat
          [ kernName kid, " [label=\"" ++ kname, " implementing ", flName kfl, "\"]"]
        forM_ (deps) $ \kid' ->
          hPutStrLn h $ concat
            [ kernName kid', " -> ", kernName kid]
  forM_ kerns' dumpKern
  hPutStrLn h "}"
  hClose h

main :: IO ()
main = dumpStrategy $ scatterImagingMain (Config "input.vis" 10 GridPar)
