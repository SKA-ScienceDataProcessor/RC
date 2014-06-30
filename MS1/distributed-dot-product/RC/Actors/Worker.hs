{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Generic actors
module RC.Actors.Worker (
    worker
  , workerClosure
  , __remoteTable
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Static
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary  (encode)
import Data.Typeable (Typeable)
import Data.ByteString.Lazy (ByteString)

import RC.Types
import RC.Combinators

-- | Description of worker which evaluate pure function
data PureWorker a b = PureWorker
  (Static (SerializableDict a))
  (Static (SerializableDict b))
  (Closure (a -> b))

makeOneShotWorker :: Serializable a => PureWorker a b -> a -> Closure ()
makeOneShotWorker (PureWorker sdictA sdictB closF) a = do
  undefined

doOneShot :: SerializableDict a
          -> a
          -> Process ()
doOneShot SerializableDict a = do
  return ()
              


-- | Worker process. It performs pure computation and request work
--   from master process by sending 'Idle' messages
worker
  :: SerializableDict a
  -> SerializableDict b
  -> ProcessId                  -- ^ Master process
  -> (a -> b)                   -- ^ Pure calculation to perform
  -> Process ()
worker SerializableDict SerializableDict pid fun = do
  me  <- getSelfPid
  send pid (Idle me)
  maybeLoop $ \(Batch wid a) -> do
    send pid (WorkResult wid $ fun a)
    send pid (Idle me)

remotable [ 'worker
          ]

workerClosure
  :: forall a b. (Typeable a, Typeable b)
  => Static (SerializableDict a)
  -> Static (SerializableDict b)
  -> ProcessId
  -> Closure (a -> b)
  -> Closure (Process ())
workerClosure sdictA sdictB pid closF
  = closure decoder (encode pid) `closureApply` closF
  where
    decoder :: Static (ByteString -> (a -> b) -> Process ())
    decoder
      = ($(mkStatic 'worker) `staticApply` sdictA `staticApply` sdictB)
      `staticCompose`
        (staticDecode sdictProcessId)
-- FIXME: horrible boilerplate
