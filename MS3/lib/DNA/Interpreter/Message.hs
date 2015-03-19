{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
-- | Handling of message for CH interpreter
module DNA.Interpreter.Message (
      -- * Message handlers
      messageHandlers
      -- * Helpers
    , MatchS(..)
    , toMatch
    , Match'(..)
    , matchSTM'
    , matchMsg'
    , matchChan'
    , handleRecieve
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Concurrent.STM (STM)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable

import DNA.Lens
import DNA.Types
import DNA.Interpreter.Types



----------------------------------------------------------------
-- Handlers for incoming messages
----------------------------------------------------------------

-- List of handlers for auxiliary messages
messageHandlers :: [MatchS]
messageHandlers =
    [ MatchS handleProcessTermination
    , MatchS handleTerminate
    , MatchS handleReady
    , MatchS handleDone
    , MatchS handleTimeout
    ]


-- Process need to terminate immediately
handleTerminate :: Terminate -> Controller ()
handleTerminate _ = fatal "Terminate arrived"


-- Monitored process terminated normally or abnormally
handleProcessTermination
    :: ProcessMonitorNotification
    -> Controller ()
handleProcessTermination (ProcessMonitorNotification _ pid reason) =
    case reason of
      DiedNormal -> handleProcessDone  pid
      _          -> handleProcessCrash pid

-- Monitored process terminated normally. We need to update registry
-- and maybe notify other processes.
handleProcessDone :: ProcessId -> Controller ()
handleProcessDone pid = do
    handlePidEvent pid
        -- In principle its possible that process terminates normally
        -- and we don't know about it. When one process in group crashes
        -- we kill all other processes and forget them. But it possible
        -- that one could terminate normally before receiving message
        (return ())
        (\p -> case p of
           Unconnected -> fatal "Impossible: Unconnected process terminated normally"
           Connected _ -> return Nothing
           Failed      -> fatal "Impossible: Normal termination after crash"
        )
        (\g _ -> case g of
           GrUnconnected{} -> fatal "Impossible: Unconnected process in group terminated normally"
           GrConnected _ (1, nD) ch _ -> do
               liftP $ forM_ ch $ \c -> sendChan c (nD + 1)
               return Nothing
           GrConnected ty (nR, nD) ch acps ->
               return $ Just $ GrConnected ty (nR-1, nD+1) ch acps
           GrFailed -> fatal "Impossible: Process terminated in complete group"
        )
    dropPID pid

-- Monitored process crashed or was disconnected
handleProcessCrash :: ProcessId -> Controller ()
handleProcessCrash pid = do
    handlePidEvent pid
        (return ())
        (\p -> case p of
           -- When process from which we didn't receive channels
           -- crashes we have no other recourse but to terminate.
           Unconnected  -> return $ Just Failed
           Connected acps -> do
               liftP $ forM_ acps $ \pp -> send pp Terminate
               return Nothing
           Failed       -> fatal "Impossible: Process crashed twice"
        )
        (\g gid -> case g of
           -- Normal groups
           GrUnconnected Normal _ -> do
               terminateGroup gid
               return $ Just GrFailed
           GrConnected Normal _ _ acps -> do
               terminateGroup gid
               liftP $ forM_ acps $ \p -> send p Terminate
               return Nothing
           -- Failout groups
           GrUnconnected Failout (n,k) ->
               return $ Just $ GrUnconnected Failout (n-1,k)
           GrConnected Failout (1, nD) ch _ -> do
               liftP $ forM_ ch $ \c -> sendChan c nD
               return Nothing
           GrConnected Failout (nR, nD) ch acps ->
               return $ Just $ GrConnected Failout (nR-1,nD) ch acps
           GrFailed -> return $ Just GrFailed
        )
    dropPID pid


-- Many-rank actor is ready to process next message.
handleReady :: (ProcessId,SendPort (Maybe Rank)) -> Controller ()
handleReady (pid,chRnk) = do
    -- FIXME: do better than pattern match failure
    Just (Right gid) <- use $ stChildren  . at pid
    Just (n,nMax)    <- use $ stCountRank . at gid
    -- Send new rank to actor
    case () of
      _| n >= nMax -> do
          Just chans <- use $ stPooledProcs . at gid
          liftP $ forM_ chans $ \c -> sendChan c Nothing
       | otherwise -> do
          liftP $ sendChan chRnk (Just $ Rank n)
          stCountRank . at gid .= Just (n+1,nMax)

-- Increment number of completed tasks for group of many-rank
-- processes.
--
-- FIXME: we will increase number of completed tasks when process exit
--        normally so we will have too many completed tasks
handleDone :: (ProcessId,DoneTask) -> Controller ()
handleDone (pid,_) =
    handlePidEvent pid
        (fatal "Shell: unknown process")
        (\_ -> fatal "Shell: must be group")
        (\g _ -> case g of
           GrConnected ty (nR,nD) ch acps ->
               return $ Just $ GrConnected ty (nR,nD+1) ch acps
           _ -> fatal "Invalid shell for group is received"
        )

-- Some process timed out
handleTimeout :: TimeOut -> Controller ()
handleTimeout (TimeOut aid) = case aid of
    SingleActor pid -> do
        m <- use $ stChildren . at pid
        case m of
          Just (Left  _) -> do liftP $ send pid Terminate
                               dropPID pid
          Just (Right _) -> fatal "Group ID encountered"
          Nothing        -> return ()
    ActorGroup gid -> do
        terminateGroup gid
        dropGroup gid


----------------------------------------------------------------
-- Extra functions for matching on messages
----------------------------------------------------------------

-- | Handlers for events which could be used with State monad with state s
data MatchS = forall a. Serializable a => MatchS (a -> Controller ())

-- | Wrapper for Match which allow to write Functor instance. For some
--   unfathomable reason Match doesn't have one!
data Match' a = forall x. Match'
                (x -> Process a)
                (forall b. (x -> Process b) -> Match b)

matchSTM' :: STM a -> Match' a
matchSTM' stm = Match' return (matchSTM stm)

matchChan' :: Serializable a => ReceivePort a -> Match' a
matchChan' ch = Match' return (matchChan ch)

matchMsg' :: Serializable a => Match' a
matchMsg' = Match' return match

instance Functor Match' where
    fmap f (Match' g mk) = Match' ((fmap . fmap) f g) mk

-- | Convert to normal match
toMatch :: Match' a -> Match a
toMatch (Match' a f) = f a


-- | Wait for message from Match' and handle auxiliary messages
handleRecieve
    :: [MatchS]
    -> Match' a
    -> DnaMonad a
handleRecieve auxMs mA
    = loop
  where
    matches s = toMatch (Right <$> mA)
              : [ match $ \a -> Left <$> runStateT (runController (f a)) s
                | MatchS f <- auxMs]
    loop = do
        s <- get
        r <- lift $ receiveWait $ matches s
        case r of
          Right a      -> return a
          Left ((),s') -> put s' >> loop
