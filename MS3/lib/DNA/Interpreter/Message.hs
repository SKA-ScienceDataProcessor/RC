{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
-- | Handling of message for CH interpreter
module DNA.Interpreter.Message where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Concurrent.STM (STM)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable

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
    ]


handleTerminate :: Terminate -> Controller ()
handleTerminate _ = fatal "Terminate arrived"

-- Handle termination of monitored process
handleProcessTermination
    :: ProcessMonitorNotification
    -> Controller ()
handleProcessTermination (ProcessMonitorNotification _ pid reason) =
    case reason of
      DiedNormal -> handleProcessDone  pid
      _          -> handleProcessCrash pid

handleProcessDone :: ProcessId -> Controller ()
handleProcessDone pid = do
    handlePidEvent pid
        -- In principle its possible that process terminates normally
        -- and we don't know about it. When one process in group crashes
        -- we kill all other processes and forget them. But it possible
        -- that one could terminate normally before receiving message
        (return ())
        (\p -> case p of
           ShellProc _ -> fatal "Impossible: shell process terminated normally"
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

handleProcessCrash :: ProcessId -> Controller ()
handleProcessCrash pid = do
    handlePidEvent pid
        (return ())
        (\p -> case p of
           -- When process from which we didn't receive channels
           -- crashes we have no other recourse but to terminate.
           ShellProc _  -> fatal "Shell crashed. No other thing to do"
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
