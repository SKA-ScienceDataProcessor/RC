{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Ord

import Debug.Trace

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
  toFlowI :: fs -> [FlowI]
instance IsFlow () where
  toFlowI () = []
instance IsFlow (Flow a) where
  toFlowI (Flow f) = [f]
instance IsFlow (Flow a, Flow b) where
  toFlowI (Flow f0, Flow f1) = [f0, f1]
instance IsFlow (Flow a, Flow b, Flow c) where
  toFlowI (Flow f0, Flow f1, Flow f2) = [f0, f1, f2]
instance IsFlow (Flow a, Flow b, Flow c, Flow d) where
  toFlowI (Flow f0, Flow f1, Flow f2, Flow f3) = [f0, f1, f2, f3]

kernel :: IsFlow fs => String -> fs -> Flow a
kernel name fs = Flow $ FlowI (hash (name, fis)) name fis
  where fis = toFlowI fs

type KernelId = Int

newtype Kernel a = Kernel (Strategy KernelI)
data KernelI = KernelI KernelId String
  deriving Show

kernId :: KernelI -> KernelId
kernId (KernelI kid _) = kid

data StratState = StratState
  { ssKernelId :: {-# UNPACK #-}!KernelId
  , ssMap :: StratMap
  }
initStratState :: StratState
initStratState = StratState 0 HM.empty

data StratMapEntry = StratMapEntry
  { smeKernel :: KernelI       -- ^ The kernel itself
  , smePrev   :: Maybe StratMapEntry -- ^ Previous kernel binding
  , smeDeps   :: [KernelId]    -- ^ Data dependencies
  }
type StratMap = HM.HashMap FlowI StratMapEntry
type Strategy a = State StratState a
data FFlow a = FFlow FlowI StratMapEntry

stratFresh :: Strategy KernelId
stratFresh = state $ \ss -> (ssKernelId ss, ss {ssKernelId = 1 + ssKernelId ss})

prepareKernel :: Flow a -> Kernel a -> Strategy StratMapEntry
prepareKernel (Flow fi) (Kernel ks) = do

  -- Make kernel
  ki <- ks

  -- Lookup the latest kernel IDs for all incoming flows
  ss <- get
  kis <- forM (flDepends fi) $ \fi' ->
    case HM.lookup fi' (ssMap ss) of
      Just sme | (KernelI kid _) <- smeKernel sme
              -> return kid
      Nothing -> fail $ "Could not find a kernel calculating flow " ++ show fi' ++ "!"
  let pki = HM.lookup fi (ssMap ss)

  return $ StratMapEntry ki pki kis

set :: Flow a -> Kernel a -> Strategy ()
set (Flow fi) k = do
  entry <- prepareKernel (Flow fi) k
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss) }

float :: Flow a -> Kernel a -> Strategy (FFlow a)
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

assertEqual :: Flow a -> Flow a -> Strategy ()
assertEqual (Flow f0) (Flow f1)
  | f0 == f1  = return ()
  | otherwise = fail $ "assertEqual: " ++ show f0 ++ " /= " ++ show f1

-- Program starts here...

data Config = Config
  { cfgMajorLoops :: Int
  , cfgGrid :: GridPar
  }
data GridPar = GridPar

data Vis
data UVGrid
data Image
data GCFs

dummy :: String -> Kernel a
dummy n = Kernel $ do
  i <- stratFresh
  return $ KernelI i n

halideWrapper :: String -> Kernel a
halideWrapper _ = dummy "halide"
cWrapper :: a -> Kernel b
cWrapper _ = dummy "c"

oskarReader :: Kernel Vis
oskarReader = dummy "oskar"
sorter :: Kernel Vis
sorter = dummy "sorter"
setOnes :: Kernel Vis
setOnes = dummy "ones"

fftCreatePlans :: GridPar -> Kernel ()
fftCreatePlans _ = dummy "fftPlans"
fftKern :: GridPar -> Kernel UVGrid
fftKern _ = dummy "fftKern"
ifftKern :: GridPar -> Kernel Image
ifftKern _ = dummy "ifftKern"

gridInit :: GridPar -> Kernel UVGrid
gridInit _ = dummy "gridInit"
gridKernel :: GridPar -> Kernel UVGrid
gridKernel _ = dummy "gridKernel"
degridKernel :: GridPar -> Kernel Vis
degridKernel _ = dummy "degridKernel"

splitModel :: Kernel Image
splitModel = dummy "splitModel"
splitResidual :: Kernel Image
splitResidual = dummy "splitResidual"

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

majorLoop :: Int -> Flow () -> Flow Vis -> Flow GCFs
         -> ( Flow Image  -- ^ residual
            , Flow Vis    -- ^ visibilities
            )
majorLoop n tag vis gcfs = foldl go (initRes tag, vis) [1..n]
 where (_, _, psfImg) = psfGrid tag vis gcfs
       go (_res, vis) _i = (res', vis')
         where (_, _, img) = gridder tag vis gcfs
               (_, res', mod) = clean img psfImg
               (_, vis') = degridder tag mod vis gcfs

scatter_imaging :: Config -> Strategy ()
scatter_imaging cfg = do
 let tag = kernel "tag" ()
 set tag $ dummy "tag"

 -- Read in visibilities
 let orig_vis = kernel "vis" ()
 set orig_vis oskarReader

 -- Generate GCF
 let gcfs = gcf tag orig_vis
 set gcfs $ halideWrapper "gcfGen"

 -- Sort visibility data
 set orig_vis $ cWrapper sorter

 -- Initialise FFTs
 fftTag <- float tag $ cWrapper (fftCreatePlans $ cfgGrid cfg)

 -- PSF
 let (psfCreate, psfGridK, psfImg) = psfGrid tag orig_vis gcfs
     gpar = cfgGrid cfg
 set (psfVis orig_vis) $ cWrapper setOnes
 set psfCreate $ gridInit gpar
 set psfGridK $ gridKernel gpar
 use fftTag $ set psfImg (ifftKern gpar)

 -- Loop
 forM_ [1..cfgMajorLoops cfg-1] $ \i -> do

   let (_, vis) = majorLoop (i-1) tag orig_vis gcfs
       (create, grid, ifft) = gridder tag vis gcfs
   set create $ gridInit gpar
   set grid $ gridKernel gpar
   use fftTag $ set ifft $ ifftKern gpar

   let (cresult, _res, mod) = clean ifft psfImg
   set cresult $ halideWrapper "clean"
   set mod splitModel

   let (fft, new_vis) = degridder tag mod vis gcfs
   use fftTag $ set fft $ fftKern gpar
   set new_vis $ degridKernel gpar

 -- Last loop iteration
 let (_, vis) = majorLoop (cfgMajorLoops cfg-1) tag orig_vis gcfs
     (create, grid, ifft) = gridder tag vis gcfs
 set create $ gridInit (cfgGrid cfg)
 set grid $ gridKernel (cfgGrid cfg)
 use fftTag $ set ifft $ ifftKern gpar

 let (cresult, res, _mod) = clean ifft psfImg
 set cresult $ halideWrapper "clean"
 set res splitResidual

 -- Now "res" should be provably equivalent to the above. The check
 -- below is *expensive*!
 assertEqual res $ fst $ majorLoop (cfgMajorLoops cfg) tag orig_vis gcfs

dumpStrategy :: Strategy () -> IO ()
dumpStrategy strat = do

  -- Construct strategy map
  let sMap = ssMap $ execState strat initStratState

  -- Generate sorted kernel
  let kerns = sortBy (comparing (kernId . smeKernel . snd)) $ HM.toList sMap

  let dumpKern fl (StratMapEntry (KernelI kid kname) m_prev deps) = do
        case m_prev of
         Just prev -> dumpKern fl prev
         Nothing   -> return ()
        putStrLn $ concat
          [ "Kernel ", show kid, ":", kname, " implementing ", flName fl
          , " using ", show deps ]
  forM_ kerns (uncurry dumpKern)

main :: IO ()
main = dumpStrategy $ scatter_imaging (Config 10 GridPar)
