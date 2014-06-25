-- | Generic actors
module RC.Actors (
    worker
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)

import RC.Types
import RC.Combinators

-- | Worker process. It performs pure computation and request work
--   from master process by sending 'Idle' messages
worker
  :: (Serializable a, Serializable b)
  => ProcessId                  -- ^ Master process
  -> Closure (a -> b)           -- ^ Pure calculation to perform
  -> Process ()
worker pid closureF = do
  me  <- getSelfPid
  fun <- unClosure closureF
  send pid (Idle me)
  maybeLoop $ \(Batch wid a) -> do
    send pid (WorkResult wid $ fun a)
    send pid (Idle me)

