
module Kernel.GPU.ScatterGrid
  ( prepare
  , createGrid
  , grid
  , degrid
  ) where

import Control.Monad
import Data.Function ( on )
import Data.List
import Data.Word

import Data
import Kernel.GPU.Common as CUDA
import Vector

pregriddedSize :: Int
pregriddedSize = shortSize * 5 + ptrSize
  where shortSize = 16
        ptrSize = 64

foreign import ccall unsafe "&" scatter_grid_phaseRotate_kern :: CUDA.Fun
foreign import ccall unsafe "&" scatter_grid_pregrid_kern :: CUDA.Fun
foreign import ccall unsafe "&" scatter_grid_kern :: CUDA.Fun

type GCFMap = (Vector (DevicePtr CxDouble), Vector Int)

-- | Transfer all GCFs to GPU and generates the GCF pointer and size
-- lookup maps
prepareGCFs :: GCFSet -> IO (GCFSet, GCFMap)
prepareGCFs gcfSet = do
  let gcfs0 = filter ((>=0) . gcfMaxW) (gcfs gcfSet)
  gcfv <- allocHostVector (length gcfs0)
  gcf_suppv <- allocHostVector (length gcfs0)
  gcfs' <- forM (zip [0..] gcfs0) $ \(i,gcf) -> do
    data'@(DeviceVector _ p) <- toDeviceVector (gcfData gcf)
    pokeVector gcfv i p
    pokeVector gcf_suppv i (gcfSize gcf)
    return gcf{gcfData = data'}
  gcfv' <- toDeviceVector gcfv
  gcf_suppv' <- toDeviceVector gcf_suppv
  return (gcfSet{gcfs = gcfs'}, (gcfv', gcf_suppv'))


-- | Do the phase rotation. This transfers visibilities and positions
-- into GPU memory as a side-effect.
--
-- TODO: This should probably be a separate step in the top-level program?
phaseRotate :: Vis -> IO Vis
phaseRotate vis = do

  let visibilities = vectorSize (visData vis)
  visData' <- toDeviceVector (visData vis)
  visPos' <- dupDeviceVector (visPositions vis)
  CUDA.launchKernel scatter_grid_phaseRotate_kern
    (1,1,1) (min 1024 visibilities,1,1) 0 Nothing $
    mapArgs visData' visPos' visibilities
  CUDA.sync

  return vis{visData = visData', visPositions=visPos'}

-- | Do pregridding. This involves all transformations that we can do
-- prior to getting to actual gridding. We especially pre-select the
-- GCF here, taking oversampling into account.
pregrid :: GridPar -> Vis -> GCFSet -> GCFMap -> IO Vis
pregrid gridp vis gcfSet (gcfv, gcf_suppv) = do

  -- Sort & group baselines by smallest (!) used w-plane
  let wstep = gcfpStepW $ gcfsPar gcfSet
      sortedBls = sortBy (compare `on` baselineMinWPlane wstep) (visBaselines vis)
      numberedBls :: [(Int, VisBaseline)]
      numberedBls = zip [0..] sortedBls
      groupedBls = groupBy ((==) `on` baselineMinWPlane wstep . snd) numberedBls

  -- Pregrid parameters
  let scale :: Double
      scale = fromIntegral (gridWidth gridp) / gridLambda gridp
      grid_size = gridWidth gridp

  -- Pregrid baselines
  let visibilities = vectorSize (visData vis)
      totalPregridSize = pregriddedSize * visibilities
  pregridded <- allocDeviceVector totalPregridSize :: IO (Vector Word8)
  forM_ groupedBls $ \bls -> do

    -- Parameters of this baseline group
    let wplane = baselineMinWPlane wstep $ snd $ head bls
        Just gcf = findGCF gcfSet (fromIntegral wplane * wstep)
        max_supp = gcfSize gcf

    -- Marshal vector of pointers into positions & pregridded
    uvov' <- toDeviceVector =<< makeVector allocHostVector
       [ uvop | (i,_) <- bls
              , let DeviceVector _ uvop = pregridded `offsetVector` (pregriddedSize * i) ]
    posv' <- toDeviceVector =<< makeVector allocHostVector
       [ posp | (_,bl) <- bls
              , let DeviceVector _ posp = visPositions vis `offsetVector` vblOffset bl ]

    -- Lauch pregridding kernel. Blocks are baselines, and threads
    -- will process individual baselines points.
    let points = vblPoints $ snd $ head bls
        baselines = length bls
    CUDA.launchKernel scatter_grid_pregrid_kern
      (baselines,1,1) (min 1024 points,1,1) 0 Nothing $
      mapArgs scale wstep max_supp posv' gcfv gcf_suppv uvov' points grid_size
    CUDA.sync

    freeVector uvov'
    freeVector posv'

  return vis{ visBaselines = sortedBls
            , visPregridded = castVector pregridded}

-- | Does the (rather involved) preparation step for the scatter
-- gridder. This arranges things so that we:
--
--   1. Have all GCFs in device memory
--   2. Rotate and transform visibilities to minimise the work
--      the actual gridder has to do. This means that we determine
--      grid coordinates and pre-select the GCF to use.
prepare :: GridPar -> Vis -> GCFSet -> IO (Vis, GCFSet)
prepare gridp vis gcfSet = do

  -- Free old data, if any
  freeVector (visPregridded vis)
  freeVector (gcfTable gcfSet)

  -- Prepare GCFS
  (gcfSet', gcfMap) <- prepareGCFs gcfSet

  -- Phase rotation / visibilities transfer
  vis' <- phaseRotate vis

  -- Pregridding
  vis'' <- pregrid gridp vis' gcfSet' gcfMap

  -- Free map
  freeVector (fst gcfMap)
  freeVector (snd gcfMap)
  return (vis'', gcfSet')

createGrid :: GridPar -> GCFPar -> IO UVGrid
createGrid gp _ = do
   dat <- allocDeviceVector (gridFullSize gp)
   return $ UVGrid gp 0 dat

grid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
grid vis gcfSet grid = do

  -- Group baselines
  let wstep = gcfpStepW $ gcfsPar gcfSet
      numberedBls :: [(Int, VisBaseline)]
      numberedBls = zip [0..] $ visBaselines vis
      groupedBls = groupBy ((==) `on` baselineMinWPlane wstep . snd) numberedBls
  forM_ groupedBls $ \bls -> do

    -- Baseline group properties
    let wplane = baselineMinWPlane wstep $ snd $ head bls
        Just gcf = findGCF gcfSet (fromIntegral wplane * wstep)
        max_supp = gcfSize gcf
        points = vblPoints $ snd $ head bls

    -- Make vector of pointers into positions & pregridded
    let pregridded = castVector (visPregridded vis) :: Vector Word8
    uvov' <- toDeviceVector =<< makeVector allocHostVector
        [ uvop | (i,_) <- bls
               , let DeviceVector _ uvop = pregridded `offsetVector` (pregriddedSize * i) ]
    datv' <- toDeviceVector =<< makeVector allocHostVector
        [ datp | (_,bl) <- bls
               , let DeviceVector _ datp = visData vis `offsetVector` vblOffset bl ]

    -- Actually launch kernel. Blocks are baselines, and threads will
    -- sum up visibilities per convultion kernel po
    let baselines = length bls
        grid_size = gridWidth $ uvgPar grid
        grid_pitch = gridPitch $ uvgPar grid
    CUDA.launchKernel scatter_grid_kern
      (baselines,1,1) (min 1024 (max_supp*max_supp),1,1) 0 Nothing $
      mapArgs max_supp (uvgData grid) uvov' datv' points grid_size grid_pitch
    CUDA.sync

    freeVector uvov'
    freeVector datv'

  return grid

degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid = undefined

