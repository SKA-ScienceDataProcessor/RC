-- |
-- Common moulde for DNA
module DNA (
      -- * DNA monad
      DNA
    , MonadIO(..)
    , liftP
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
    , startProcess
    , forkLocal
    , forkRemote
    , forkGroup
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Distributed.Static (closureApply)
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Closure
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary   (Binary)
import Data.Int
import Data.Typeable (Typeable)
import Data.Monoid   (Monoid(..))
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import GHC.Generics  (Generic)

import DNA.Logging
import DNA.Run
import DNA.DNA
