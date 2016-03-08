{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators,
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow
import Flow.Builder ( IsKernelDef )

import Kernel.Scheduling
import Kernel.Binning
import Kernel.Data
import Kernel.IO

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

gridderStrat :: Config -> Strategy ()
gridderStrat cfg = do

  -- Make index and point domains for visibilities
  (ddom, ixs) <- makeOskarDomain cfg 1
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom
  tdom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Create data flow for tag, bind it to FFT plans
  let gpar = cfgGrid cfg

  -- Data flow we want to calculate (just one)
  vis <- uniq $ flow "vis" ixs

  -- Create ranged domains for grid coordinates
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)

  -- Split and distribute over coordinate domain
  vdom <- split vdoms (gridTiles gpar)
  udom <- split udoms (gridTiles gpar)
  let uvdom = (udom, vdom)
  bind vis $ oskarReader ddom tdom (cfgInput cfg) 0 0 ixs

  -- Create w-binned domain, split
  wdoms <- makeBinDomain $ dkern $ binSizer gpar tdom uvdom vis
  wdom <- split wdoms (gridBins gpar)

  -- Bin visibilities and output
  rebind vis (dkern $ binner gpar tdom uvdom wdom)
  void $ bindNew $ dkern $ halideDump (visRepr uvdom wdom) "binned-vis" vis

main :: IO ()
main = do

  let gpar = (cfgGrid defaultConfig)
        { gridWidth = 2048
        , gridHeight = 2048
        , gridPitch = 2048
        , gridTheta = 0.10
        , gridTiles = 2048 `div` 64
        , gridBins = 1
        }
      config = defaultConfig
        { cfgInput  = [OskarInput "test_p00_s00_f00.vis" 1 1]
        , cfgPoints = 32131 * 200
        , cfgGrid   = gpar
        , cfgNodes  = 1
        }

  dumpSteps $ gridderStrat config
  execStrategyDNA False $ gridderStrat config
