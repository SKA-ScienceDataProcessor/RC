-- |
-- Common moulde for DNA
module DNA (
      -- * DNA monad
      DNA
    , rank
    , groupSize
    , logMessage
    , duration
       -- * Actors
    , Actor
    , actor
    , CollectActor
    , collectActor
    , Mapper
    , mapper
      -- ** Shell actors
    , Shell
    , Val
    , Grp
    , Scatter
    , eval
    , evalClosure
    , startActor
    , startCollector
    , startGroup
    , startGroupN
    , startCollectorGroup
    , startCollectorGroupMR
    , startMappers
      -- * CAD & Co
    -- , CAD
    , Location(..)
    , Res(..)
    , ResGroup(..)
    , useLocal
    , availableNodes
      -- * Connecting actors
    , sendParam
    -- , broadcastParamSlice
    , broadcast
    , connect
      -- ** Promises
    , Promise
    , Group
    , await
    , gather
    , delay
    , delayGroup
      -- * Start DNA program
    , dnaRun
      -- * Reexports
    , MonadIO(..)
    , remotable
    , mkStaticClosure
    ) where

import Control.Monad.IO.Class
import Control.Distributed.Process.Closure (mkStaticClosure,remotable)

import DNA.DSL
import DNA.Types
import DNA.Run
