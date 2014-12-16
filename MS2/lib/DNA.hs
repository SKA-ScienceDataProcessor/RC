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
    , CollectorShell
    , ShellGroup
    , GroupCollect
    , eval
    , startActor
    , startCollector
    , startGroup
    , startCollectorGroup
      -- * CAD & Co
    , CAD
    , Location(..)
    , select
    , selectMany
      -- * Connecting actors
    , sendParam
    , broadcastParam
    , connect
    , broadcast
    , collect
    , connectCollectorGroup
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
