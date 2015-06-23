{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
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
import Control.Monad.Reader
import Control.Monad.Operational
import Control.Concurrent.Async
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Data.Maybe
-- import Data.Binary   (Binary)
import Data.Typeable (Typeable {-,typeOf-})
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as T


import DNA.CH
import DNA.Types
import DNA.Lens
import DNA.Channel.File
import DNA.DSL                 hiding (logMessage,duration,createFileChan)
import DNA.Logging
import DNA.Interpreter.Message
import DNA.Interpreter.Run     hiding (__remoteTable)
import DNA.Interpreter.Spawn
import DNA.Interpreter.Testing
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
      Kernel msg mode hints io -> execKernel msg mode hints io
      DnaRank         -> envRank      <$> ask
      DnaGroupSize    -> envGroupSize <$> ask
      AvailNodes      -> Set.size <$> use stNodePool
      -- Logging
      LogMessage msg   -> message 0 msg
      Duration msg dna -> logDuration msg $ interpretDNA dna
      -- Spawning of actors
      EvalClosure     a c -> do Actor f <- liftP $ unClosure c
                                interpretDNA $ f a
      SpawnActor              r a -> execSpawnActor r a
      SpawnCollector          r a -> execSpawnCollector r a
      SpawnGroup            r g a -> execSpawnGroup r g a
      -- SpawnGroupN         r g n a -> execSpawnGroupN r g n a
      SpawnCollectorGroup   r g a -> execSpawnCollectorGroup r g a
      SpawnCollectorTree    r g a -> execSpawnCollectorTree r g a
      -- Data flow building
      Connect    a b  -> execConnect a b
      SendParam  a sh -> execSendParam a sh
      Broadcast  a sh -> execBroadcast a sh
      DistributeWork a f sh -> execDistributeWork a f sh
      Delay    loc sh -> execDelay loc sh
      Await p         -> execAwait p
      DelayGroup sh   -> execDelayGroup sh
      GatherM p f b0  -> execGatherM p f b0
      CrashMaybe p    -> crashMaybeWorker p
      CreateFileChan l n -> createFileChan l n
      WaitForResources aid -> execWaitForResources aid

theInterpreter :: DnaInterpreter
theInterpreter = DnaInterpreter interpretDNA


----------------------------------------------------------------
-- Implementation of primitive commands for DNA DSL
----------------------------------------------------------------

-- Execute foreign kernel. Process will wait until kernel execution is
-- complete but will be able to handle events in meantime.
execKernel :: () => String -> KernelMode -> [ProfileHint] -> Kern a -> DnaMonad a
-- BLOCKING
execKernel msg mode hints kern = do
    -- FIXME: we can leave thread running! Clean up properly!
    --
    -- FIXME: propagate exceptions
    --
    -- IO code to run. Process atributes are derived before the spawn.
    profAttrs <- processAttributes
    let code = logProfile msg hints profAttrs $ runKern kern
    -- Run according to requested mode
    a <- case mode of
           DefaultKernel -> liftIO $ async code
           BoundKernel   -> liftIO $ asyncBound code
    handleRecieve messageHandlers [matchSTM' (waitSTM a)]


-- Obtain promise from shell
execDelay :: Serializable b
          => Location -> Shell a (Val b) -> DnaMonad (Promise b)
-- IMMEDIATE
execDelay _loc (Shell aid) = do
    -- Create local variable
    (sendB,recvB) <- liftP newChan
    (var,dst) <- newVar $ RcvSimple (wrapMessage sendB)
    stActorDst . at aid .= Just (Left var)
    -- Send destination to an actor
    Just pids <- use $ stAid2Pid . at aid
    liftP $ T.forM_ pids $ \p ->
        send p (dst,[]::[SendPortId])
    --
    logConnect (Just aid) Nothing
    return $ Promise recvB

execDelayGroup :: Serializable b => Shell a (Grp b) -> DnaMonad (Group b)
execDelayGroup (Shell aid) = do
    -- Create local variable
    (sendB,recvB) <- liftP newChan
    (sendN,recvN) <- liftP newChan
    (var,dst) <- newVar $ RcvReduce [(wrapMessage sendB, sendN)]
    stActorDst . at aid .= Just (Left var)
    -- Send destination to an actor
    Just pids <- use $ stAid2Pid . at aid
    liftP $ T.forM_ pids $ \p ->
        send p (dst,[]::[SendPortId])
    --
    logConnect (Just aid) Nothing
    return $ Group recvB recvN

execGatherM :: Serializable a => Group a -> (b -> a -> IO b) -> b -> DnaMonad b
execGatherM = doGatherDna messageHandlers


-- Wait until message from channel arrives
execAwait :: Serializable a => Promise a -> DnaMonad a
-- BLOCKING
execAwait (Promise ch) =
    handleRecieve messageHandlers [matchChan' ch]


-- Send parameter
execSendParam :: Serializable a => a -> Shell (Val a) b -> DnaMonad ()
-- IMMEDIATE
execSendParam a (Shell aid) = do
    Just mdst <- use $ stActorRecvAddr . at aid
    case mdst of
      Nothing -> doFatal "Connecting to terminated actor"
      Just (dst,_) -> do
          stActorSrc . at aid .= Just (Left trySend)
          liftP $ trySend dst
          logConnect Nothing (Just aid)
  where
    trySend (RcvSimple dst) = do
        mch <- unwrapMessage dst
        case mch of
          Nothing -> doPanic "execSendParam: type error"
          Just ch -> sendChan ch a
    trySend RcvGrp{}    = doPanic "execSendParam: destination type mismatch, expect single, got group"
    trySend RcvReduce{} = doPanic "execSendParam: destination type mismatch, expect single, got reducer"

-- Send parameter
execBroadcast :: Serializable a => a -> Shell (Scatter a) b -> DnaMonad ()
-- IMMEDIATE
execBroadcast a (Shell aid) = do
    Just mdst <- use $ stActorRecvAddr . at aid
    case mdst of
      Nothing -> doFatal "Connecting to terminated actor"
      Just (dst,_) -> do
          stActorSrc . at aid .= Just (Left trySend)
          liftP $ trySend dst
          logConnect Nothing (Just aid)
  where
    trySend (RcvGrp dsts) = do
        mch <- sequence <$> mapM unwrapMessage dsts
        case mch of
          Nothing  -> doPanic "execBroadcast: type error"
          Just chs -> forM_ chs $ \ch -> sendChan ch a
    trySend _ = doPanic "execBroadcast: destination type mismatch. Expect group"

execDistributeWork
    :: Serializable b
    => a -> (Int -> a -> [b]) -> Shell (Scatter b) c -> DnaMonad ()
-- IMMEDIATE
execDistributeWork a f (Shell aid) = do
    Just mdst <- use $ stActorRecvAddr . at aid
    case mdst of
      Nothing -> doFatal "Connecting to terminated actor"
      Just (dst,_) -> do
          stActorSrc . at aid .= Just (Left trySend)
          liftP $ trySend dst
          logConnect Nothing (Just aid)
  where
    trySend (RcvGrp dsts) = do
        let n  = length dsts
            bs = f n a
        when (length bs /= n) $
            doFatal "Bad work distribution function"
        mch <- sequence <$> mapM unwrapMessage dsts
        case mch of
          Nothing  -> doPanic "execBroadcast: type error"
          Just chs -> forM_ (zip chs bs) $ uncurry sendChan
    trySend _ = doPanic "execBroadcast: destination type mismatch. Expect group"



-- Connect two shells to each other
execConnect :: (Typeable tag, Serializable b) => Shell a (tag b) -> Shell (tag b) c -> DnaMonad  ()
-- IMMEDIATE
execConnect (Shell aidSrc) (Shell aidDst) = do
    -- Check that actor is not connected already
    do m1 <- use $ stActorDst . at aidSrc
       m2 <- use $ stActorSrc . at aidDst
       case m1 of
         Just  _ -> doFatal "Double connect!"
         Nothing -> return ()
       case m2 of
         Just  _ -> doFatal "Double connect!"
         Nothing -> return ()
    -- Record connection
    stActorDst . at aidSrc .= Just (Right aidDst)
    stActorSrc . at aidDst .= Just (Right aidSrc)
    -- Check that neither actor failed and terminate other one if this is the case
    Just stSrc <- use $ stChildren . at aidSrc
    case stSrc of
      Failed -> terminateActor aidDst
      _      -> return ()
    Just stDst <- use $ stChildren . at aidDst
    case stDst of
      Failed -> terminateActor aidSrc
      _      -> return ()
    -- Send connection
    Just mdst <- use $ stActorRecvAddr . at aidDst
    case mdst of
      Nothing  -> doFatal "Connecting to terminated actor"
      Just dst -> do
          pids <- use $ stAid2Pid . at aidSrc
          T.forM_ pids $ T.mapM_ $ \p ->
              -- FIXME: very error prone just as other message sending
              --        routines
              liftP $ send p dst
          -- Handler special case
          case (dst,stSrc) of
            ((RcvReduce chs,_), Completed 0) -> liftP $ forM_ chs $ \(_,chN) -> sendChan chN 0
            ((RcvReduce _  ,_), Completed _) -> doPanic "Unconnected actor completed execution"
            _                                -> return ()
    logConnect (Just aidSrc) (Just aidDst)

createFileChan :: Location -> String -> DnaMonad (FileChan a)
createFileChan loc name = do
    workDir <- envWorkDir <$> ask
    m_chan <- liftIO $ createFileChanImp workDir loc name
    case m_chan of
      Nothing -> doPanic $ "Failed to create file channel " ++ name ++ "!"
      Just chan -> return chan


execWaitForResources :: AID -> DnaMonad ()
execWaitForResources aid = do
    Just pids <- use $ stAllAid2Pid    . at aid
    pool      <- forM (T.toList pids) $ \p -> use $ stUsedResources . at p
    case catMaybes pool of
      [] -> return ()
      _  -> do blockForMessage messageHandlers
               execWaitForResources aid

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

newVar :: RecvAddr -> DnaMonad (VID,RecvAddr)
newVar dst = do
    var <- VID <$> uniqID
    stVars . at var .= Just dst
    return (var,dst)

remotable [ 'theInterpreter ]
