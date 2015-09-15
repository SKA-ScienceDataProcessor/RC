{-# LANGUAGE FlexibleInstances, TypeFamilies, RankNTypes, ImpredicativeTypes, LiberalTypeSynonyms #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Ord

import System.IO

newtype Flow ps a = Flow FlowI
  deriving Show
data Anon
type AFlow a = Flow Anon a

aflow :: Flow ps a -> AFlow a
aflow (Flow fi) = Flow fi

data FlowI = FlowI
  { flHash    :: {-# UNPACK #-} !Int
  , flName    :: String
  , flDepends :: [FlowI]
  }
  deriving (Eq)

instance Hashable FlowI where
  i `hashWithSalt` (FlowI h _ _) = i `hashWithSalt` h
instance Show FlowI where
  showsPrec _ (FlowI _ n ds) = showString n . shows ds

class IsFlow fs where
  type Pars fs
  toFlowI :: fs -> [FlowI]
instance IsFlow () where
  type Pars () = ()
  toFlowI () = []
instance IsFlow (Flow pa a) where
  type Pars (Flow pa a) = a
  toFlowI (Flow f) = [f]
instance IsFlow (Flow pa a, Flow pb b) where
  type Pars (Flow pa a, Flow pb b) = (a, b)
  toFlowI (Flow f0, Flow f1) = [f0, f1]
instance IsFlow (Flow pa a, Flow pb b, Flow pc c) where
  type Pars (Flow pa a, Flow pb b, Flow pc c) = (a, b, c)
  toFlowI (Flow f0, Flow f1, Flow f2) = [f0, f1, f2]
instance IsFlow (Flow pa a, Flow pb b, Flow pc c, Flow pd d) where
  type Pars (Flow pa a, Flow pb b, Flow pc c, Flow pd d) = (a, b, c, d)
  toFlowI (Flow f0, Flow f1, Flow f2, Flow f3) = [f0, f1, f2, f3]

kernel :: IsFlow fs => String -> fs -> SFlow a
kernel name fs = Flow $ FlowI (hash (name, fis)) name fis
  where fis = toFlowI fs

type KernelId = Int

newtype Kernel ps a = Kernel (FlowI -> Strategy KernelI)
data KernelI = KernelI
  { kernId :: KernelId
  , kernFlow :: FlowI
  , kernName :: String
  }
  deriving Show

data StratState = StratState
  { ssKernelId :: {-# UNPACK #-}!KernelId -- ^ Next fresh kernel ID
  , ssMap      :: StratMap          -- ^ Kernels currently in scope
  , ssKernels  :: [StratMapEntry]   -- ^ Complete list of kernels, including out-of-scope ones
  }
initStratState :: StratState
initStratState = StratState 0 HM.empty []

data StratMapEntry = StratMapEntry
  { smeKernel :: KernelI       -- ^ The kernel itself
  , smeDeps   :: [KernelId]    -- ^ Data dependencies
  }
type StratMap = HM.HashMap FlowI StratMapEntry
type Strategy a = State StratState a
data FFlow a = FFlow FlowI StratMapEntry

stratFresh :: Strategy KernelId
stratFresh = state $ \ss -> (ssKernelId ss, ss {ssKernelId = 1 + ssKernelId ss})

uniqKernel :: IsFlow fl => String -> fl -> Strategy (Flow Tag a)
uniqKernel name fls = state $ \ss ->
  (kernel (name ++ "." ++ show (ssKernelId ss)) fls,
   ss {ssKernelId = 1 + ssKernelId ss})

prepareKernel :: Flow ps a -> Kernel ps a -> Strategy StratMapEntry
prepareKernel (Flow fi) (Kernel ks) = do

  -- Make kernel
  ki <- ks fi

  -- Depending on own kernel?
  ss <- get
  kis <- forM (flDepends fi) $ \p -> do

    -- Parameter flows must all be input flows
    when (p /= fi && (p `notElem` flDepends fi)) $
      fail $ "Parameter " ++ show p ++ " not a dependency of " ++ show fi ++ "!"

    -- Look up latest kernel ID
    case HM.lookup p (ssMap ss) of
      Just sme | kern <- smeKernel sme
              -> return $ kernId kern
      Nothing -> fail $ "When binding kernel " ++ kernName ki ++ " to implement " ++ flName fi ++ ": Could not find a kernel calculating flow " ++ show p ++ "!"

  -- Add to kernel list
  let kern = StratMapEntry ki kis
  put $ ss { ssKernels = kern : ssKernels ss }
  return kern

bind :: Flow fs a -> Kernel fs a -> Strategy ()
bind (Flow fi) k = do
  entry <- prepareKernel (Flow fi) k
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss) }

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

type SFlow a = forall pa. Flow pa a

type GridFlow = ( Flow Tag UVGrid
                , Flow (Tag, Vis, GCFs, UVGrid) UVGrid
                , Flow (Tag, UVGrid) Image)
gridder :: Flow a Tag -> Flow b Vis -> Flow c GCFs -> GridFlow
gridder tag vis gcfs = (create, grid, img)
 where create = kernel "create grid" tag
       grid = kernel "grid" (tag, vis, gcfs, create)
       img = kernel "idft" (tag, grid)

type DegridFlow = ( Flow (Tag, Image) UVGrid
                  , Flow (Tag, Image, GCFs, Vis) Vis)
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

majorLoop :: forall a b. Int -> Flow a Tag -> Flow b Vis
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

oskarReader :: String -> Kernel Tag Vis
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
degridKernel :: GridPar -> Kernel (Tag, Image, GCFs, Vis) Vis
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

-- Strategy implementing gridding
gridS :: Config -> Flow a Tag -> Flow b GCFs -> Flow c Vis -> FFlow Tag
      -> Strategy (AFlow Image)
gridS cfg tag gcfs vis fftTag =
 let (create, grid, img) = gridder tag vis gcfs
     gpar = cfgGrid cfg
 in implementing img $ do
  bind create (gridInit gpar)
  bind grid (gridKernel gpar)
  use fftTag $ bind img (ifftKern gpar)

-- Strategy implementing a number of iterations of the major loop
scatterImagingLoop :: Config -> Flow a Tag -> Flow b Vis -> Flow c Image -> FFlow Tag -> Int
                   -> Strategy (AFlow Vis)
scatterImagingLoop cfg tag orig_vis psf fftTag i =
 implementing (snd $ majorLoop i tag orig_vis) $ do

  -- Get references to intermediate data flow nodes
  let (_, vis) = majorLoop (i-1) tag orig_vis
      gcfs = gcf tag orig_vis
      gpar = cfgGrid cfg

  -- Grid
  img <- gridS cfg tag gcfs vis fftTag
  let (cresult, _res, mod) = clean img psf
      (residual, new_vis) = degridder tag mod vis gcfs

  -- Clean, split out model
  bind cresult cleanKernel
  bind mod splitModel

  -- FFT and degrid
  use fftTag $ bind residual (fftKern gpar)
  bind new_vis (degridKernel gpar)

scatterImaging :: Config -> Flow a Tag -> Flow b Vis -> Strategy (AFlow Image)
scatterImaging cfg tag start_vis =
 implementing (fst $ majorLoop (cfgMajorLoops cfg) tag start_vis) $ do

  -- Sort visibility data
  bind start_vis sorter

  -- Initialise FFTs
  fftTag <- float tag (cWrapper (fftCreatePlans $ cfgGrid cfg))

  -- Generate GCF
  let gcfs = gcf tag start_vis
      gpar = cfgGrid cfg
  bind gcfs (gcfKernel gpar)

  -- PSF
  let (psfCreate, psfGridK, psf) = psfGrid tag start_vis gcfs
  implementing psf $ do
    bind (psfVis start_vis) (cWrapper setOnes)
    bind psfCreate (gridInit gpar)
    bind psfGridK (gridKernel gpar)
    use fftTag $ bind psf (ifftKern gpar)

  -- Loop
  forM_ [1..cfgMajorLoops cfg-1] $ scatterImagingLoop cfg tag start_vis psf fftTag

  -- Last loop iteration
  let (_, vis) = majorLoop (cfgMajorLoops cfg-1) tag start_vis
  img <- gridS cfg tag gcfs vis fftTag

  let (cresult, res, _mod) = clean img psf
  bind cresult cleanKernel
  bind res splitResidual

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
