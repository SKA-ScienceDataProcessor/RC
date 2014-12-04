-- |
-- Common moulde for DNA
module DNA (
      -- * DNA monad
      DNA
    , liftP
    , rank
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
      -- * CAD & Co
    , CAD
    , makeCAD
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
    ) where

import Control.Monad.IO.Class
import Control.Distributed.Process.Closure (mkStaticClosure,remotable)

import DNA.Logging
import DNA.Run
import DNA.DNA
import DNA.Monitor
