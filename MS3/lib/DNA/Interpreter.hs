{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Module for interpreting
module DNA.Interpreter (
      interpretDNA
    , theInterpreter  
      -- * CH
    , __remoteTable
    , theInterpreter__static
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Except
import Control.Monad.Operational
import Control.Concurrent.Async
import Control.Concurrent.STM (STM)
import Control.Distributed.Static  (closureApply)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
-- import Data.Binary   (Binary)
-- import Data.Typeable (Typeable)
-- import qualified Data.Map as Map
-- import           Data.Map   (Map)
import qualified Data.Set as Set
-- import           Data.Set   (Set)
import Text.Printf
-- import GHC.Generics  (Generic)

import DNA.Types
import DNA.Lens
import DNA.DSL
import DNA.Logging
import DNA.Interpreter.Message
import DNA.Interpreter.Run     hiding (__remoteTable)
import DNA.Interpreter.Spawn
import DNA.Interpreter.Types


----------------------------------------------------------------
-- DNA language interpreter
----------------------------------------------------------------

-- | Function which does actual interpretation of DNA program
interpretDNA :: DNA a -> DnaMonad a
interpretDNA (DNA m) =
  case view m of
    Return  a -> return a
    op :>>= cont -> do a <- runOp op
                       interpretDNA $ DNA $ cont a
  where
    runOp :: DnaF a -> DnaMonad a
    runOp op = case op of
      Kernel     io   -> execKernel io
      DnaRank         -> use stRank
      DnaGroupSize    -> use stGroupSize
      AvailNodes      -> Set.size <$> use stNodePool
      -- Spawning of actors
      EvalClosure     a c -> do Actor f <- lift $ unClosure c
                                interpretDNA $ f a
      SpawnActor              r a -> execSpawnActor r a
      SpawnCollector          r a -> execSpawnCollector r a
      SpawnGroup            r g a -> execSpawnGroup r g a
      SpawnGroupN         r g n a -> execSpawnGroupN r g n a
      SpawnCollectorGroup   r g a -> execSpawnCollectorGroup r g a
      SpawnCollectorGroupMR r g a -> execSpawnCollectorGroupMR r g a
      SpawnMappers          r g a -> execSpawnMappers r g a
      -- Data flow building
      Connect    a b  -> execConnect a b
      SendParam  a sh -> execSendParam a sh
      Delay sh        -> execDelay sh
      Await p         -> execAwait p
      DelayGroup sh   -> execDelayGroup sh
      GatherM p f b0  -> execGatherM p f b0

theInterpreter :: DnaInterpreter
theInterpreter = DnaInterpreter interpretDNA


----------------------------------------------------------------
-- Implementation of primitive commands for DNA DSL
----------------------------------------------------------------

-- Execute foreign kernel. Process will wait until kernel execution is
-- complete but will be able to handle events in meantime.
execKernel :: () => IO a -> DnaMonad a
-- BLOCKING
execKernel io = do
    -- FIXME: we can leave thread running! Clean up properly!
    a <- liftIO $ async io
    handleRecieve messageHandlers $ matchSTM' (waitSTM a)



-- Obtain promise from shell
execDelay :: Serializable b => Shell a (Val b) -> DnaMonad (Promise b)
-- IMMEDIATE
execDelay (Shell aid _ src) = do
    -- Notify shell's monitor about process
    me <- lift getSelfPid
    recordConnection aid (SingleActor me) []
    -- Send destination to the shell process!
    (chSend,chRecv) <- lift newChan
    let param :: Serializable b => SendEnd (Val b) -> SendPort b -> Process ()
        -- FIXME: local!
        param (SendVal ch) p = sendChan ch $ destFromLoc Local p
    lift $ param src chSend
    return $ Promise chRecv


execDelayGroup :: Serializable b => Shell a (Grp b) -> DnaMonad (Group b)
execDelayGroup (Shell child _ src) = do
    me            <- liftP getSelfPid
    (sendB,recvB) <- liftP newChan
    (sendN,recvN) <- liftP newChan
    let param :: Serializable b => SendEnd (Grp b) -> SendPort b -> Process ()
        param (SendGrp chans) chB = forM_ chans $ \ch -> sendChan ch (SendRemote [chB])
    liftP $ param src sendB
    recordConnection child (SingleActor me) [sendN]
    return $ Group recvB recvN

execGatherM :: Serializable a => Group a -> (b -> a -> IO b) -> b -> DnaMonad b
execGatherM grp step b0 = do
    -- FIXME: concurrency
    liftP $ doGatherM grp step b0



-- Wait until message from channel arrives
execAwait :: Serializable a => Promise a -> DnaMonad a
-- BLOCKING
execAwait (Promise ch) =
    handleRecieve messageHandlers $ matchChan' ch

-- Send parameter
execSendParam :: Serializable a => a -> Shell (Val a) b -> DnaMonad ()
-- IMMEDIATE
execSendParam a (Shell _ recv _) = case recv of
    RecvVal       ch  -> lift $ sendChan ch a
    RecvBroadcast grp -> case grp of
        RecvGrp p -> lift $ forM_ p $ \ch -> sendChan ch a

-- Connect two shells to each other
execConnect :: Serializable b => Shell a (tag b) -> Shell (tag b) c -> DnaMonad  ()
-- IMMEDIATE
execConnect (Shell childA _ sendEnd) (Shell childB recvEnd _) =
  case (sendEnd,recvEnd) of
    -- Val
    (SendVal chDst, RecvVal chB) -> do
        -- FIXME: Do we want to allow unsafe send here?
        lift $ sendChan chDst $ SendRemote [chB]
        recordConnection childA childB []
    (SendVal chDst, RecvBroadcast (RecvGrp chans)) -> do
        lift $ sendChan chDst $ SendRemote chans
        recordConnection childA childB []
    -- Grp
    (SendGrp chDst, RecvReduce chReduce) -> do
        let chB = map snd chReduce
        lift $ forM_ chDst $ \ch -> sendChan ch $ SendRemote chB
        recordConnection childA childB [chN | (chN,_) <- chReduce ]
    -- MR
    (SendMR chDst, RecvMR chans) -> do
        let chB = map snd chans
        lift $ forM_ chDst $ \ch -> sendChan ch chB
        recordConnection childA childB [chN | (chN,_) <- chans]
    -- IMPOSSIBLE
    --
    -- GHC cannot understand that pattern match is exhaustive
    _ -> error "Impossible: pattern match is not exhaustive"



----------------------------------------------------------------
-- Resource and connection management
----------------------------------------------------------------



-- Connect actor to another actor
recordConnection
    :: ActorID                  -- Source
    -> ActorID                  -- Destincation
    -> [SendPort Int]
    -> DnaMonad ()
recordConnection (SingleActor pid) dest _ = runController $
  case dest of
    SingleActor dst -> do
        Just st <- use $ stChildren . at pid
        case st of
          Right _          -> fatal "Impossible: group instead of process"
          Left ShellProc{} -> fatal "Impossible: shell could not be connected"
          Left Unconnected -> stChildren . at pid .= Just (Left (Connected [dst]))
          Left _           -> fatal "Double connect"
    ActorGroup gid -> do
        Just st <- use $ stChildren . at pid
        pids    <- getGroupPids gid
        case st of
          Right _          -> fatal "Impossible: group instead of process"
          Left ShellProc{} -> fatal "Impossible: shell could not be connected"
          Left Unconnected -> stChildren . at pid .= Just (Left (Connected pids))
          Left _           -> fatal "Double connect"
recordConnection (ActorGroup gid) dest port = runController $
  case dest of
    SingleActor pid -> do
        Just grp <- use $ stGroups . at gid
        case grp of
          GrUnconnected ty nProc ->
              stGroups . at gid .= Just (GrConnected ty nProc port [pid])
          GrConnected{}  -> fatal "Double connect"
          GrFailed       -> do lift $ lift $ send pid Terminate
                               dropGroup gid
    ActorGroup dstGid -> do
        pids <- getGroupPids dstGid
        Just grp <- use $ stGroups . at gid
        case grp of
          GrUnconnected ty nProc ->
              stGroups . at gid .= Just (GrConnected ty nProc port pids)
          GrConnected{} -> fatal "Double connect"
          GrFailed      -> do lift $ lift $ forM_ pids $ \p -> send p Terminate
                              dropGroup gid



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

destFromLoc :: Location -> SendPort a -> Dest a
destFromLoc Local  = SendLocally
destFromLoc Remote = SendRemote . (:[])


remotable [ 'theInterpreter ]
