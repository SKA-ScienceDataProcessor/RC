{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
-- |
-- Common moulde for DNA
module DNA where

import Control.Applicative
import Control.Monad
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Closure
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary   (Binary)
import Data.Int
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as S
import GHC.Generics  (Generic)

import DNA.Logging
import DNA.Run


----------------------------------------------------------------
-- Data types and helpers
----------------------------------------------------------------

-- | Parameters for a subprocess. If process require more than one
--   parameter it's sent as tuple if it doesn't require parameters ()
--   is sent.
newtype Param a = Param a
                  deriving (Show,Eq,Typeable,Binary)


-- | Normally subprocess will only return value once. Fork* function
--   return promise which is hadle for obtaining result of
--   computation.
--
--   We use cloud haskell channels for commutincations instead of
--   send\/expect because this way we cannot mix up messages from
--   different sources.
--
--   FIXME: If we await more than once we await more than once we wll
--          deadlock and we have no way to detect such behaviour. Nor
--          we can detect if we do not await for promise. In this case
--          child process will be termintaed forcefully when parent
--          dies.
data Promise a = Promise ProcessId (ReceivePort a)


-- | Await result from promise. Function will block.
--
--   This function will as well receive messages about termination of
--   monitored processes.
--
--   FIXME: Currently we terminate when child dies abnormally. But for
--          some processes we may want to use different strategy.
--          We need to keep some data about known childs etc.
await :: Serializable a => Promise a -> Process a
await p@(Promise _ ch) = do
    -- Here we wait for either value from channel or for message that
    -- child died.
    receiveWait
        [ matchChan ch return
        , match $ \(ProcessMonitorNotification _ _ reason) ->
            case reason of
              DiedNormal -> await p
              _          -> terminate
        ]

-- | Cluster architecture description. Currently it's simply list of
--   nodes process can use.
newtype CAD = CAD [NodeId]
              deriving (Show,Eq,Typeable,Binary)



-- | Set of values which is produces by group of processes which
--   execute same code.
data Group a = Group !Int (ReceivePort a)

gather :: Group a -> (a -> a -> a) -> a -> Process a
gather (Group n0 ch) op = loop n0
  where
    loop 0 a = return a
    loop n !a0 = do
        receiveWait
            [ matchChan ch $ \a -> loop (n - 1) (op a0 a)
            , match $ \(ProcessMonitorNotification _ _ reason) ->
                case reason of
                  DiedNormal -> loop n a0
                  _          -> terminate
            ]

-- | Very important question is how to pass parameters to multiple
--   childs. We have two primary way to divide data.
--
--    1. Divide data on parent processa nd send values to childs.
--
--    2. Let childs to select suitable part of data by themselves
--       using their rank.
--
--   We provide applicative interface for data splitting. It's a bit
--   awkward to use. Ideally we'd want to use idiom bracket but
--   upcoming applicative-do in GHC 7.10 will be useful as well.
data Scatter a
    = Same a
    | Scatter (Int -> [a])
    deriving (Functor)

instance Applicative Scatter where
    pure = same
    Same    f <*> Same a    = Same (f a)
    Same    f <*> Scatter a = Scatter $ (fmap . fmap) f a
    Scatter f <*> Same a    = Scatter $ (fmap . fmap) ($ a) f
    Scatter f <*> Scatter a = Scatter $ \n -> zipWith ($) (f n) (a n)

runScatter :: Int -> Scatter a -> [a]
runScatter n (Same a) = replicate n a
runScatter n (Scatter f)
    | length xs == n = xs
    | otherwise      = error "runScatter: list length doesn't match!"
  where xs = f n


-- | Send same value to all nodes.
same :: a -> Scatter a
same = Same

-- | Scatter value
scatter :: (Int -> a -> [b]) -> a -> Scatter b
scatter f a = Scatter (\n -> f n a)




----------------------------------------------------------------
-- Starting of child processes
----------------------------------------------------------------


-- | Start process.
startProcess
    :: (Serializable a, Serializable b)
    => ([NodeId] -> a -> Process b)
    -> Process ()
startProcess action = do
    sendCh  <- expect
    nodes   <- expect
    Param a <- expect
    b       <- action nodes a
    sendChan sendCh b

-- | Fork process on local node
forkLocal :: (Serializable a, Serializable b)
          => [NodeId]           -- ^ List of nodes process allowed to use
          -> Process ()         -- ^ Process command
          -> a                  -- ^ Parameters to process
          -> Process (Promise b)
forkLocal nodes child a = do
    me  <- getSelfPid
    pid <- spawnLocal $ link me >> child
    (chSend,chRecv) <- newChan
    send pid chSend
    send pid (CAD nodes)
    send pid (Param a)
    return $ Promise me chRecv



-- | Fork process on remote node
forkRemote :: (Serializable a, Serializable b)
           => [NodeId]             -- ^ List of nodes process allowed to use
           -> NodeId               -- ^ Node to spawn on
           -> Closure (Process ()) -- ^ Sub process command
           -> a                    -- ^ Parameters sent to process
           -> Process (Promise b)
forkRemote nodes nid child a = do
    me <- getSelfPid
    (pid,_) <- spawnSupervised nid child
    (chSend,chRecv) <- newChan
    send pid chSend
    send pid (CAD nodes)
    send pid (Param a)
    return $ Promise me chRecv

-- | Create group of nodes
forkGroup :: (Serializable a, Serializable b)
          => [NodeId]
          -> Closure (Process ())
          -> Scatter a
          -> Process (Group b)
forkGroup nodes child scat = do
    when (null nodes) $
        error "Empty list of nodes"
    let n  = length nodes
        xs = runScatter n scat
    (chSend,chRecv) <- newChan
    pids <- forM_ (nodes `zip` xs) $ \(nid,a) -> do
        (pid,_) <- spawnSupervised nid child
        send pid chSend
        send pid (CAD [])       -- FIXME: How to allow children
        send pid (Param a)
    return $ Group n chRecv
