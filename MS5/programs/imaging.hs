{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators, StandaloneDeriving,
             FlexibleContexts #-}

-- Workaround
{-# LANGUAGE CPP, UndecidableInstances #-}

module Main where

import Flow
import Flow.Vector
import Flow.Kernel
import Flow.Builder

import Control.Monad

import Data.Typeable

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

deriving instance Typeable Tag
deriving instance Typeable Vis
deriving instance Typeable UVGrid
deriving instance Typeable Image
deriving instance Typeable GCFs
deriving instance Typeable CleanResult

-- Abstract kernel signatures.
--
-- TODO: The string we use here is somewhat important for keeping them
-- apart - it would be more elegant if we could enforce them to be
-- unique in some other way.
createGrid :: Flow UVGrid
createGrid = flow "create grid"
grid :: Flow Vis -> Flow GCFs -> Flow UVGrid -> Flow UVGrid
grid = flow "grid"
degrid :: Flow UVGrid -> Flow GCFs -> Flow Vis -> Flow Vis
degrid = flow "degrid"
idft :: Flow UVGrid -> Flow Image
idft = flow "idft"
dft :: Flow Image -> Flow UVGrid
dft = flow "dft"
gcf :: Flow Vis -> Flow GCFs
gcf = flow "gcf"
initRes :: Flow Image
initRes = flow "residual init"
psfVis :: Flow Vis -> Flow  Vis
psfVis = flow "prepare vis for PSF"
clean :: Flow Image -> Flow Image -> Flow CleanResult
clean = flow "clean"
cleanModel :: Flow CleanResult -> Flow Image
cleanModel = flow "clean/model"
cleanResidual :: Flow CleanResult -> Flow Image
cleanResidual = flow "clean/residual"
imageSum :: Flow Image -> Flow Image
imageSum = flow "image sum"

-- | Compound gridder actor
gridder :: Flow Vis -> Flow GCFs -> Flow Image
gridder vis gcfs = idft (grid vis gcfs createGrid)

-- | Compound degridder actor
degridder :: Flow Image -> Flow Vis -> Flow GCFs -> Flow Vis
degridder img vis gcfs = degrid (dft img) gcfs vis

-- | Compound PSF gridder actor
psfGrid :: Flow Vis -> Flow GCFs -> Flow Image
psfGrid vis gcfs = gridder (psfVis vis) gcfs

-- | Compound cleaning actor
cleaner :: Flow Image -> Flow Image -> (Flow Image, Flow Image)
cleaner dirty psf = (cleanResidual result, cleanModel result)
 where result = clean dirty psf

-- | Compound major loop iteration actor
majorIter :: Flow GCFs -> Flow Image
          -> (Flow Image, Flow Vis) -> Int
          -> (Flow Image, Flow Vis)
majorIter gcfs psf (_res, vis) _i = (res', vis')
  where img = gridder vis gcfs
        (res', mod') = cleaner img psf
        vis' = degridder mod' vis gcfs

-- | Compound major loop actor
majorLoop :: Int -> Flow Vis -> (Flow Image, Flow Vis)
majorLoop n vis
  = foldl (majorIter gcfs psf) (initRes, vis) [1..n]
 where gcfs = gcf vis
       psf = psfGrid vis gcfs

majorLoopSum :: Int -> Flow Vis -> Flow Image
majorLoopSum n vis = imageSum $ fst $ majorLoop n vis

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

-- Make data representations. Sadly, this can not be *quite* done with
-- deriving yet (#8165). In the meantime, we use preprocessor
#define DATAREPR_INSTANCE(NewRepr, Repr) \
  instance DataRepr NewRepr where \
    type ReprType NewRepr = ReprType (Repr); \
    reprNop (NewRepr r) = reprNop r; \
    reprAccess (NewRepr r) = reprAccess r; \
    reprCompatible (NewRepr r1) (NewRepr r2) = reprCompatible r1 r2
newtype ImageRepr = ImageRepr (VectorRepr () Image)
  deriving (Typeable, Show)
DATAREPR_INSTANCE(ImageRepr, VectorRepr () Image)
newtype UVGridRepr = UVGridRepr (VectorRepr () UVGrid)
  deriving (Typeable, Show)
DATAREPR_INSTANCE(UVGridRepr, VectorRepr () UVGrid)

-- By default images and grids are always consumed by the caller, as
-- they are large objects with lots of write operations, and we don't
-- want to duplicate them.
imgRepr :: ImageRepr
imgRepr = ImageRepr $ VectorRepr WriteAccess
uvgRepr :: UVGridRepr
uvgRepr = UVGridRepr $ VectorRepr WriteAccess

-- Plan representation is used by many kernels
planRepr :: VectorRepr () Tag
planRepr = VectorRepr ReadAccess

newtype RawVisRepr = RawVisRepr (VectorRepr () Vis)
  deriving (Typeable, Show)
DATAREPR_INSTANCE(RawVisRepr, VectorRepr () Vis)
newtype SortedVisRepr = SortedVisRepr (VectorRepr () Vis)
  deriving (Typeable, Show)
DATAREPR_INSTANCE(SortedVisRepr, VectorRepr () Vis)

-- Visibilities generally remain constant
rawVisRepr :: RawVisRepr
rawVisRepr = RawVisRepr $ VectorRepr ReadAccess
visRepr :: SortedVisRepr
visRepr = SortedVisRepr $ VectorRepr ReadAccess

-- GCFs too
gcfsRepr :: VectorRepr () GCFs
gcfsRepr = VectorRepr ReadAccess

dummy :: (DataRepr r, IsReprs rs, IsReprKern (ReprType r) rs)
      => String -> rs -> r -> ReprKernFun (ReprType r) rs
dummy name rs r = mappingKernel name rs r code
  where code _ _ = putStrLn name >> return nullVector

halideWrapper :: (DataRepr r, IsReprs rs, IsReprKern (ReprType r) rs)
              => String -> rs -> r -> ReprKernFun (ReprType r) rs
halideWrapper _ = dummy "halide"
cWrapper :: (DataRepr r, IsReprs rs, IsReprKern (ReprType r) rs)
         => String -> rs -> r -> ReprKernFun (ReprType r) rs
cWrapper _ = dummy "c"

oskarReader :: Typeable d => Domain d -> [(FilePath, Int)] -> Kernel Vis
oskarReader d _ = dummy "oskar" Z (RegionRepr d rawVisRepr)
sorter :: Flow Vis -> Kernel Vis
sorter = dummy "sorter" (rawVisRepr :. Z) visRepr

setOnes :: Flow Vis -> Kernel Vis
setOnes = dummy "ones" (visRepr :. Z) visRepr

gcfKernel :: GridPar -> Flow Tag -> Flow Vis -> Kernel GCFs
gcfKernel _ = halideWrapper "gcfs" (planRepr :. visRepr :. Z) gcfsRepr

fftCreatePlans :: GridPar -> Kernel Tag
fftCreatePlans _ = dummy "fftPlans" Z planRepr
fftKern :: GridPar -> Flow Tag -> Flow Image -> Kernel UVGrid
fftKern _ = dummy "fftKern" (planRepr :. imgRepr :. Z) uvgRepr
ifftKern :: GridPar -> Flow Tag -> Flow UVGrid -> Kernel Image
ifftKern _ = dummy "ifftKern" (planRepr :. uvgRepr :. Z) imgRepr

gridInit :: GridPar -> Kernel UVGrid
gridInit _ = dummy "gridInit" Z uvgRepr
gridKernel :: GridPar -> Flow Vis -> Flow GCFs -> Flow UVGrid -> Kernel UVGrid
gridKernel _ = dummy "gridKernel" (visRepr :. gcfsRepr :. uvgRepr  :. Z) uvgRepr
psfGridKernel :: GridPar -> Flow Vis -> Flow GCFs -> Flow UVGrid -> Kernel UVGrid
psfGridKernel _ = dummy "psfGridKernel" (visRepr :. gcfsRepr :. uvgRepr  :. Z) uvgRepr
degridKernel :: GridPar -> Flow UVGrid -> Flow GCFs -> Flow Vis -> Kernel Vis
degridKernel _ = dummy "degridKernel" (uvgRepr :. gcfsRepr :. visRepr :. Z) visRepr

cleanResRepr :: VectorRepr () CleanResult
cleanResRepr = VectorRepr WriteAccess
cleanKernel :: Flow Image -> Flow Image -> Kernel CleanResult
cleanKernel = halideWrapper "clean" (imgRepr :. imgRepr :. Z) cleanResRepr
splitModel :: Flow CleanResult -> Kernel Image
splitModel = dummy "splitModel" (cleanResRepr :. Z) imgRepr
splitResidual :: Flow CleanResult -> Kernel Image
splitResidual = dummy "splitResidual" (cleanResRepr :. Z) imgRepr

imageSumKernel :: Typeable d => Domain d -> Flow Image -> Kernel Image
imageSumKernel dom = dummy "image summation" (RegionRepr dom imgRepr :. Z) imgRepr
imageWriter :: FilePath -> Flow Image -> Kernel Image
imageWriter _ = dummy "image writer" (imgRepr :. Z) NoRepr

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

scatterImaging :: Typeable d => Config -> Domain d -> Flow Tag -> Flow Vis
               -> Strategy ()
scatterImaging cfg dh tag vis =
 implementing (fst $ majorLoop (cfgMajorLoops cfg) vis) $ do

  -- Sort visibility data
  let addDom :: IsKernelDef kf => kf -> kf
      addDom = regionKernel dh
  rebind vis $ addDom sorter

  -- Initialise FFTs
  let gpar = cfgGrid cfg
  bind tag $ addDom $ fftCreatePlans gpar

  -- Generate GCF
  let gcfs = gcf vis
  bind gcfs $ addDom $ gcfKernel gpar tag vis

  -- Make rules
  bindRule idft (addDom $ ifftKern gpar tag)
  bindRule dft (addDom $ fftKern gpar tag)
  bindRule createGrid (addDom $ gridInit gpar)
  bindRule grid (addDom $ gridKernel gpar)
  bindRule degrid (addDom $ degridKernel gpar)
  bindRule clean (addDom cleanKernel)
  bindRule cleanResidual (addDom splitResidual)
  bindRule cleanModel (addDom splitModel)

  -- PSF. Note that we bind a kernel here that implements *two*
  -- abstract kernel nodes!
  --let psfg = grid (psfVis vis) gcfs createGrid
  --bind psfg (psfGridKernel gpar vis gcfs createGrid)
  bindRule (grid . psfVis) (addDom $ psfGridKernel gpar)
  calculate $ psfGrid vis gcfs

  -- Loop
  forM_ [1..cfgMajorLoops cfg-1] $ \i -> do

    -- Force grid calculation - we do not want to share this between
    -- loop iterations!
    calculate createGrid

    -- Generate new visibilities
    calculate $ snd $ majorLoop i vis

  -- Calculate residual of last loop iteration
  calculate createGrid
  calculate $ fst $ majorLoop (cfgMajorLoops cfg) vis

-- Strategy implements imaging loop for a number of data sets
scatterImagingMain :: Config -> Strategy ()
scatterImagingMain cfg = do

  -- Make data set domain
  let dataSets = length (cfgInput cfg)
      dataSetRepeats = sum $ map snd $ cfgInput cfg
  dom <- makeRangeDomain 0 dataSetRepeats

  -- Create data flow for visibilities, build abstract data flow to do
  -- configured number of major loops over this input data
  tag <- uniq (flow "tag")
  let vis = flow "vis" tag

  -- Split by datasets
  let result = majorLoopSum (cfgMajorLoops cfg) vis
  ds <- split dom dataSets
  distribute ds ParSchedule $ void $ do

    -- Split by number of runs.
    -- TODO: Number of runs should depend on data set!
    rep <- split ds 3
    distribute rep SeqSchedule $ void $ do

      -- Read in visibilities. The domain handle passed in tells the
      -- kernel which of the datasets to load.
      bind vis $ oskarReader rep $ cfgInput cfg

      -- Implement this data flow
      scatterImaging cfg rep tag vis
      -- calculate $ fst $ majorLoop (cfgMajorLoops cfg) vis

    -- Sum up local images (TODO: accumulate?)
    bindRule imageSum (imageSumKernel rep)
    calculate result

  -- Sum and write out the result
  rebind result $ imageWriter (cfgOutput cfg)

-- Strategy implements imaging loop for a number of data sets
scatterSimple :: Config -> Strategy ()
scatterSimple  cfg = do

  -- Create data flow for visibilities, build abstract data flow to do
  -- configured number of major loops over this input data
  vis <- uniq (flow "vis")
  tag <- uniq (flow "tag")

  -- Read in visibilities. The domain handle passed in tells the
  -- kernel which of the datasets to load.
  dom <- makeRangeDomain 0 1
  bind vis $ oskarReader dom $ cfgInput cfg

  -- Implement this data flow
  scatterImaging cfg dom tag vis

  -- Sum and write out the result
  bindRule imageSum (imageSumKernel dom)
  let result = majorLoopSum (cfgMajorLoops cfg) vis
  rebind result (imageWriter (cfgOutput cfg))

testStrat :: Strategy ()
testStrat = scatterImagingMain $ Config
    [("input.vis", 3),
     ("input2.vis", 3),
     ("input3.vis", 3)]
    "output.img"
    1
    GridPar

main :: IO ()
main = dumpSteps testStrat >> execStrategy testStrat
