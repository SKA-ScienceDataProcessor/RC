{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Ord

import System.IO

-- Stubs
newtype Flow a = Flow FlowI
  deriving Show

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
instance IsFlow (Flow a) where
  type Pars (Flow a) = a
  toFlowI (Flow f) = [f]
instance IsFlow (Flow a, Flow b) where
  type Pars (Flow a, Flow b) = (a, b)
  toFlowI (Flow f0, Flow f1) = [f0, f1]
instance IsFlow (Flow a, Flow b, Flow c) where
  type Pars (Flow a, Flow b, Flow c) = (a, b, c)
  toFlowI (Flow f0, Flow f1, Flow f2) = [f0, f1, f2]
instance IsFlow (Flow a, Flow b, Flow c, Flow d) where
  type Pars (Flow a, Flow b, Flow c, Flow d) = (a, b, c, d)
  toFlowI (Flow f0, Flow f1, Flow f2, Flow f3) = [f0, f1, f2, f3]

kernel :: IsFlow fs => String -> fs -> Flow a
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

uniqKernel :: IsFlow fl => String -> fl -> Strategy (Flow a)
uniqKernel name fls = state $ \ss ->
  (kernel (name ++ "." ++ show (ssKernelId ss)) fls,
   ss {ssKernelId = 1 + ssKernelId ss})

prepareKernel :: Flow a -> Kernel ps a -> [FlowI] -> Strategy StratMapEntry
prepareKernel (Flow fi) (Kernel ks) pars = do

  -- Make kernel
  ki <- ks fi

  -- Depending on own kernel?
  ss <- get
  kis <- forM pars $ \p -> do

    -- Parameter flows must all be input flows
    when (p /= fi && (p `notElem` flDepends fi)) $
      fail $ "Parameter " ++ show p ++ " not a dependency of " ++ show fi ++ "!"

    -- Look up latest kernel ID
    case HM.lookup p (ssMap ss) of
      Just sme | kern <- smeKernel sme
              -> return $ kernId kern
      Nothing -> fail $ "Could not find a kernel calculating flow " ++ show p ++ "!"

  -- Add to kernel list
  let kern = StratMapEntry ki kis
  put $ ss { ssKernels = kern : ssKernels ss }
  return kern

bind :: IsFlow fs => Flow a -> Kernel (Pars fs) a -> fs -> Strategy ()
bind (Flow fi) k ps = do
  entry <- prepareKernel (Flow fi) k (toFlowI ps)
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss) }

float :: IsFlow fs => Flow a -> Kernel (Pars fs) a -> fs -> Strategy (FFlow a)
float (Flow fi) k ps = do
  entry <- prepareKernel (Flow fi) k (toFlowI ps)
  return $ FFlow fi entry

use :: FFlow a -> Strategy b -> Strategy b
use (FFlow fi entry) strat = state $ \ss ->
  let ss' = ss { ssMap = HM.insert fi entry (ssMap ss) }
      (r, ss'') = flip runState ss' strat
      ssMap' = case HM.lookup fi (ssMap ss) of
        Nothing  -> HM.delete fi (ssMap ss'')
        Just en0 -> HM.insert fi en0 (ssMap ss'')
  in (r, ss''{ ssMap = ssMap' } )

assertEqual :: Flow a -> Flow a -> Strategy ()
assertEqual (Flow f0) (Flow f1)
  | f0 == f1  = return ()
  | otherwise = fail $ "assertEqual: " ++ show f0 ++ " /= " ++ show f1

implementing :: Flow a -> Strategy () -> Strategy (Flow a)
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

type GridFlow = (Flow UVGrid, Flow UVGrid, Flow Image)
gridder :: Flow () -> Flow Vis -> Flow GCFs -> GridFlow
gridder tag vis gcfs = (create, grid, img)
 where create = kernel "create grid" tag
       grid = kernel "grid" (tag, vis, gcfs, create)
       img = kernel "idft" (tag, grid)

type DegridFlow = (Flow UVGrid, Flow Vis)
degridder :: Flow () -> Flow Image -> Flow Vis -> Flow GCFs
         -> DegridFlow
degridder tag img vis gcfs = (dft, degrid)
 where dft = kernel "dft" (tag, img)
       degrid = kernel "degrid" (tag, dft, gcfs, vis)

gcf :: Flow () -> Flow Vis -> Flow GCFs
gcf tag vis = kernel "gcf" (tag,vis)

initRes :: Flow () -> Flow Image
initRes tag = kernel "residual init" (tag)

psfVis :: Flow Vis -> Flow Vis
psfVis vis = kernel "prepare vis for PSF" (vis)

psfGrid :: Flow () -> Flow Vis -> Flow GCFs -> GridFlow
psfGrid tag vis gcfs = gridder tag (psfVis vis) gcfs

data CleanResult
clean :: Flow Image -> Flow Image
     -> (Flow CleanResult, Flow Image, Flow Image)
clean dirty psf = (result, res, mod)
 where result = kernel "clean" (dirty, psf)
       res = kernel "clean/residual" (result)
       mod = kernel "clean/model" (result)

majorLoop :: Int -> Flow () -> Flow Vis
         -> ( Flow Image  -- ^ residual
            , Flow Vis    -- ^ visibilities
            )
majorLoop n tag vis = foldl go (initRes tag, vis) [1..n]
 where gcfs = gcf tag vis
       (_, _, psfImg) = psfGrid tag vis gcfs
       go (_res, vis) _i = (res', vis')
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

data Vis
data UVGrid
data Image
data GCFs

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
sorter :: Kernel Vis Vis
sorter = dummy "sorter"
setOnes :: Kernel Vis Vis
setOnes = dummy "ones"

gcfKernel :: GridPar -> Kernel Vis GCFs
gcfKernel _ = halideWrapper "gcfs"

fftCreatePlans :: GridPar -> Kernel () ()
fftCreatePlans _ = dummy "fftPlans"
fftKern :: GridPar -> Kernel ((), Image) UVGrid
fftKern _ = dummy "fftKern"
ifftKern :: GridPar -> Kernel ((), UVGrid) Image
ifftKern _ = dummy "ifftKern"

gridInit :: GridPar -> Kernel () UVGrid
gridInit _ = dummy "gridInit"
gridKernel :: GridPar -> Kernel (Vis, GCFs, UVGrid) UVGrid
gridKernel _ = dummy "gridKernel"
degridKernel :: GridPar -> Kernel (UVGrid, Vis, GCFs) Vis
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
gridS :: Config -> Flow () -> Flow GCFs -> Flow Vis -> FFlow ()
      -> Strategy (Flow Image)
gridS cfg tag gcfs vis fftTag =
 let (create, grid, img) = gridder tag vis gcfs
     gpar = cfgGrid cfg
 in implementing img $ do
  bind create (gridInit gpar) ()
  bind grid (gridKernel gpar) (vis, gcfs, create)
  use fftTag $ bind img (ifftKern gpar) (tag, grid)

-- Strategy implementing a number of iterations of the major loop
scatterImagingLoop :: Config -> Flow () -> Flow Vis -> Flow Image -> FFlow () -> Int
                   -> Strategy (Flow Vis)
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
  bind cresult cleanKernel (img, psf)
  bind mod splitModel cresult

  -- FFT and degrid
  use fftTag $ bind residual (fftKern gpar) (tag, mod)
  bind new_vis (degridKernel gpar) (residual, vis, gcfs)

scatterImaging :: Config -> Flow () -> Flow Vis -> Strategy (Flow Image)
scatterImaging cfg tag start_vis =
 implementing (fst $ majorLoop (cfgMajorLoops cfg) tag start_vis) $ do

  -- Sort visibility data
  bind start_vis sorter start_vis

  -- Initialise FFTs
  fftTag <- float tag (cWrapper (fftCreatePlans $ cfgGrid cfg)) ()

  -- Generate GCF
  let gcfs = gcf tag start_vis
      gpar = cfgGrid cfg
  bind gcfs (gcfKernel gpar) (start_vis)

  -- PSF
  let (psfCreate, psfGridK, psf) = psfGrid tag start_vis gcfs
  implementing psf $ do
    bind (psfVis start_vis) (cWrapper setOnes) start_vis
    bind psfCreate (gridInit gpar) ()
    bind psfGridK (gridKernel gpar) (psfVis start_vis, gcfs, psfCreate)
    use fftTag $ bind psf (ifftKern gpar) (tag, psfGridK)

  -- Loop
  forM_ [1..cfgMajorLoops cfg-1] $ scatterImagingLoop cfg tag start_vis psf fftTag

  -- Last loop iteration
  let (_, vis) = majorLoop (cfgMajorLoops cfg-1) tag start_vis
  img <- gridS cfg tag gcfs vis fftTag

  let (cresult, res, _mod) = clean img psf
  bind cresult cleanKernel (img, psf)
  bind res splitResidual cresult

-- Strategy implements major loop for these visibilities
scatterImagingMain :: Config -> Strategy (Flow Image)
scatterImagingMain cfg = do

  tag <- uniqKernel "tag" ()
  bind tag (dummy "tag") ()

  -- Read in visibilities
  vis <- uniqKernel "vis" ()
  bind vis (oskarReader $ cfgInput cfg)  ()

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
