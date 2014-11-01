-- |
-- Common moulde for DNA
module DNA (
      -- * DNA monad
      DNA
    , liftP
    , getNodes
      -- * Promises
    , Promise
    , await
    , Group
    , gather
      -- * Scattering data
    , Scatter
    , same
    , scatter
      -- * Spawning of actors
    , Actor
    , actor
    , eval
    , forkLocal
    , forkRemote
    , forkGroup
    , forkGroupFailout
      -- * Running program
    , dnaRun
      -- * Reexports
    , MonadIO(..)
    , mkStaticClosure
    , remotable
    ) where

import Control.Monad.IO.Class
import Control.Distributed.Process.Closure (mkStaticClosure,remotable)

import DNA.Logging
import DNA.Run
import DNA.DNA
