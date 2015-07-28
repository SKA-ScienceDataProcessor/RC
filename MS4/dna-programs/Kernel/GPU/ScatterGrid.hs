
module Kernel.GPU.ScatterGrid
  ( prepare
  , createGrid
  , phaseRotate
  , grid
  , degrid
  ) where

import Control.Monad
import Data.Function ( on )
import Data.List
import Data.Word
import Foreign.Storable ( sizeOf )

import Data
import Kernel.GPU.Common as CUDA
import qualified Kernel.GPU.NvidiaDegrid as Nvidia
import Vector

pregriddedSize :: Int
pregriddedSize = shortSize * 3 + charSize * 2 + ptrSize
  where charSize = 1
        shortSize = 2
        ptrSize = 8

foreign import ccall unsafe "&" scatter_grid_phaseRotate_kern :: CUDA.Fun
foreign import ccall unsafe "&" scatter_grid_pregrid_kern :: CUDA.Fun
foreign import ccall unsafe "&" scatter_grid_kern :: CUDA.Fun

type GCFMap = (Vector (DevicePtr CxDouble), Vector Word32)

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
  freeVector (castVector $ visPregridded vis :: Vector Word8)

  -- Prepare GCFS
  (gcfSet', gcfMap) <- prepareGCFs gcfSet

  -- Pregridding
  vis' <- pregrid gridp vis gcfSet' gcfMap

  -- Free map
  freeVector (fst gcfMap)
  freeVector (snd gcfMap)
  return (vis', gcfSet')

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
    pokeVector gcf_suppv i $ fromIntegral $ gcfSize gcf
    return gcf{gcfData = data'}
  gcfv' <- toDeviceVector gcfv
  gcf_suppv' <- toDeviceVector gcf_suppv
  return (gcfSet{gcfs = gcfs'}, (gcfv', gcf_suppv'))


-- | Do the phase rotation. This transfers visibilities and positions
-- into GPU memory as a side-effect.
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
      scale = gridTheta gridp
      grid_size = gridWidth gridp

  -- Pregrid baselines
  let visibilities = vectorSize (visData vis)
      totalPregridSize = pregriddedSize * visibilities
  pregridded <- allocDeviceVector totalPregridSize :: IO (Vector Word8)
  visPos' <- toDeviceVector (visPositions vis)
  visData' <- toDeviceVector (visData vis)
  forM_ groupedBls $ \bls -> do

    -- Parameters of this baseline group
    let wplane = baselineMinWPlane wstep $ snd $ head bls
        Just gcf = findGCF gcfSet (fromIntegral wplane * wstep)
        max_supp = gcfSize gcf
        points = vblPoints $ snd $ head bls

    -- Marshal vector of pointers into positions & pregridded
    uvov' <- toDeviceVector =<< makeVector allocHostVector
       [ uvop | (i,_) <- bls
              , let DeviceVector _ uvop = pregridded `offsetVector` (pregriddedSize * points * i) ]
    posv' <- toDeviceVector =<< makeVector allocHostVector
       [ posp | (_,bl) <- bls
              , let DeviceVector _ posp = visPos' `offsetVector` vblOffset bl ]

    -- Launch pregridding kernel. Blocks are baselines, and threads
    -- will process individual baselines points.
    let baselines = length bls
    CUDA.launchKernel scatter_grid_pregrid_kern
      (baselines,1,1) (min 1024 points,1,1) 0 Nothing $
      mapArgs scale wstep posv' gcfv gcf_suppv uvov' max_supp points grid_size
    CUDA.sync

    freeVector uvov'
    freeVector posv'

  return vis{ visData = visData'
            , visPositions  = visPos'
            , visBaselines  = sortedBls
            , visPregridded = castVector pregridded}

createGrid :: GridPar -> GCFPar -> IO UVGrid
createGrid gp _ = do
   dat@(DeviceVector _ p) <- allocDeviceVector (gridFullSize gp)
   memset p (fromIntegral $ vectorByteSize dat) 0
   return $ UVGrid gp 0 dat

grid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
grid vis gcfSet uvgrid = do

  -- Group baselines
  let wstep = gcfpStepW $ gcfsPar gcfSet
      numberedBls :: [(Int, VisBaseline)]
      numberedBls = zip [0..] $ visBaselines vis
      groupedBls = groupBy ((==) `on` baselineMinWPlane wstep . snd) numberedBls

  -- Make vectors of pointers into positions & pregridded
  let pregridded = castVector (visPregridded vis) :: Vector Word8
  vs <- forM groupedBls $ \bls -> do
    let points = vblPoints $ snd $ head bls
    uvov <- toDeviceVector =<< makeVector allocHostVector
        [ uvop | (i,_) <- bls
               , let DeviceVector _ uvop = pregridded `offsetVector` (pregriddedSize * points * i) ]
    datv <- toDeviceVector =<< makeVector allocHostVector
        [ datp | (_,bl) <- bls
               , let DeviceVector _ datp = visData vis `offsetVector` vblOffset bl ]
    return (uvov, datv)
  CUDA.sync

  forM_ (zip vs groupedBls) $ \((uvov, datv), bls) -> do

    -- Baseline group properties
    let wplane = baselineMinWPlane wstep $ snd $ head bls
        Just gcf = findGCF gcfSet (fromIntegral wplane * wstep)
        max_supp = gcfSize gcf
        points = vblPoints $ snd $ head bls

    -- Actually launch kernel. Blocks are baselines, and threads will
    -- sum up visibilities per convolution kernel point. Every block
    -- will copy its visibilities & pregridded positions into shared
    -- memory, so we need to instruct CUDA to allocate enough memory
    -- for it.
    let baselines = length bls
        grid_size = gridWidth $ uvgPar uvgrid
        grid_pitch = gridPitch $ uvgPar uvgrid
        shared_mem = fromIntegral $ points * (sizeOf (undefined :: CxDouble) + pregriddedSize)
    CUDA.launchKernel scatter_grid_kern
      (baselines,1,1) (min 1024 (max_supp*max_supp),1,1) shared_mem Nothing $
      mapArgs (uvgData uvgrid) uvov datv max_supp points grid_size grid_pitch
  CUDA.sync

  -- Free temporary arrays
  forM_ vs $ \(uvov, datv) -> do
    freeVector uvov
    freeVector datv
  return uvgrid

degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid = Nvidia.degrid
