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
      -- ** Shell actors
    , Shell
    , Val
    , Grp
    , Scatter
    , eval
    , startActor
    , startCollector
    , startGroup
    -- , startCollectorGroup
      -- * CAD & Co
    , CAD
    , Location(..)
    , Res(..)
    , ResGroup(..)
    , GrpFlag(..)
    , GroupType(..)
    , availableNodes
    , select
    , selectMany
      -- * Connecting actors
    , sendParam
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
    , MonadProcess(..)
    , remotable
    , mkStaticClosure
    ) where

import Control.Monad.IO.Class
import Control.Distributed.Process.Closure (mkStaticClosure,remotable)

import DNA.Types
import DNA.Run
import DNA.DNA
import DNA.Controller
