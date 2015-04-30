{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Data types for interpretation of DNA DSL using cloud haskell
module DNA.Interpreter.Types where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Class
import Control.Exception
import Control.Distributed.Process
import Data.Monoid
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import qualified Data.Foldable as T
import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)
import GHC.Generics  (Generic)
import Lens.Family.TH

import DNA.CH
import DNA.Types
import DNA.Lens
import DNA.DSL


----------------------------------------------------------------
-- Monads
----------------------------------------------------------------

-- | Type synonym for monad which is used for interpretation of
type DnaMonad = StateT StateDNA (ReaderT Env Process)

-- | Monad for event handlers. It adds explicit error handling
type Controller = ExceptT DnaError DnaMonad

-- | Error encountered during DNA computation
data DnaError
    = FatalErr String              -- ^ Fatal error
    | PanicErr String              -- ^ Internal error
    | Except SomeException         -- ^ Uncaught exception

data Panic = Panic String
             deriving (Show,Typeable)
instance Exception Panic

data Fatal = Fatal String
             deriving (Show,Typeable)
instance Exception Fatal
      
-- | Fatal error
fatal :: MonadError DnaError m => String -> m a
fatal = throwError . FatalErr

-- | Violation of internal invariant which should not ever happen
--   unless implementation is buggy.
panic :: MonadError DnaError m => String -> m a
panic = throwError . PanicErr

-- | Actually raise panic
doPanic :: MonadIO m => String -> m a
doPanic = liftIO . throw . Panic 

-- | Actually 
doFatal :: MonadIO m => String -> m a
doFatal = liftIO . throw . Fatal

    
-- | Run controller monad. On failure will terminate process forefully
runController :: Controller a -> DnaMonad a
runController m = do
    r <- runExceptT m
    case r of
      Left err -> case err of
        PanicErr e -> doPanic e
        FatalErr e -> doFatal e
        Except   e -> liftIO $ throw e
      Right a -> return a

-- | Run DNA monad
runDnaMonad
    :: AID
    -> Rank
    -> GroupSize
    -> Closure DnaInterpreter
    -> [NodeInfo]
    -> [DebugFlag]
    -> DnaMonad a
    -> Process a
runDnaMonad aid (Rank rnk) (GroupSize grp) interp nodes flags =
    flip runReaderT env . flip evalStateT s0
  where
    env = Env { envRank        = rnk
              , envGroupSize   = grp
              , envInterpreter = interp
              , envAID         = aid
              }
    s0 = StateDNA
           { _stCounter       = 0
           , _stDebugFlags    = flags
           , _stNodePool      = Set.fromList nodes
           , _stUsedResources = Map.empty
           , _stChildren      = Map.empty
           , _stActorSrc      = Map.empty
           , _stActorDst      = Map.empty
           , _stVars          = Map.empty
           , _stActorRecvAddr = Map.empty
           , _stAid2Pid       = Map.empty
           , _stPid2Aid       = Map.empty
           , _stActorClosure  = Map.empty
           }

-- | Run DNA monad using ActorParam as source of input parameters and
--   interpreter
runDnaParam :: ActorParam -> DNA a -> Process a
runDnaParam p action = do
  interpreter <- unClosure $ actorInterpreter p
  runDnaMonad (actorAID p)
              (actorRank p)
              (actorGroupSize p)
              (actorInterpreter p)
              (vcadNodePool $ actorNodes       p)
              (actorDebugFlags  p)
    $ dnaInterpreter interpreter action

-- | Evaluator for DNA monad. We have to pass closure with function
--   for interpreting DNA monad manually since we have to break
--   dependency loop. We also want to wrap function into
newtype DnaInterpreter = DnaInterpreter { dnaInterpreter :: forall a. DNA a -> DnaMonad a }
                       deriving Typeable


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Parameter send to actor on startup
data ActorParam = ActorParam
    { actorParent      :: Maybe ProcessId
      -- ^ Parent of an actor. Nothing for top level actor
    , actorInterpreter :: Closure DnaInterpreter
      -- ^ Interpreter for DNA DSL
    , actorRank        :: Rank
      -- ^ Rank of an actor
    , actorGroupSize   :: GroupSize
      -- ^ Size of group of actors
    , actorNodes       :: VirtualCAD
      -- ^ Nodes allocated to an actor
    , actorDebugFlags  :: [DebugFlag]
      -- ^ Extra flags for debugging
    , actorSendBack    :: SendPort (RecvAddr,[SendPortId])
      -- ^ Send receive address and list of port ID's back to the parent process
    , actorAID         :: AID
      -- ^ AID of an actor as viewed by parent
    }
    deriving (Typeable,Generic)
instance Binary ActorParam

-- | Parameters of an actor which are constant during its execution
data Env = Env
    { envRank        :: !Int
    , envGroupSize   :: !Int
    , envInterpreter :: !(Closure DnaInterpreter)
    , envAID         :: !AID
    }

-- | State of interpreter
--
--   Dataflow graph is append-only in the sense that we don't remove
--   actors which completed execution from graph but mark them as
--   completed instead.
--
--   Note that resources are allocated to individual CH processes not
--   to logical CH actors
--
--   Invariants:
--
--    * For every actor ID exist entry stChildren and stActorRecvAddr
--
--
data StateDNA = StateDNA
    { _stCounter     :: !Int
      -- Counter for generation of unique IDs
    , _stDebugFlags  :: [DebugFlag]

      -- Resources
    , _stNodePool      :: !(Set NodeInfo)
      -- ^ Unused nodes which could be reused
    , _stUsedResources :: !(Map ProcessId VirtualCAD)
      -- ^ Resources used by child actors. Note that resources are
      --   allocated per CH process not per actor.

      -- Append only dataflow graph
    , _stChildren      :: !(Map AID ActorState)
      -- ^ State of child actors
    , _stActorSrc :: !(Map AID (Either Message AID))
      -- ^ Source of an actor. It's either another actor or parent
      --   actor. In latter case we store encoded message.
      --   message
    , _stActorDst :: !(Map AID (Either VID AID))
      -- ^ Destination of an actor. It could be either other actor or
      --   variable local to parent actor.

      -- State of connections
    , _stVars          :: !(Map VID RecvAddr)
      -- ^ Mapping for local variables
    , _stActorRecvAddr :: !(Map AID (Maybe (RecvAddr,[SendPortId])))
      -- ^ Receive address of an actor. When actor terminated it's set
      --   to Nothing

      -- Mapping ProcessID <-> AID
    , _stPid2Aid :: !(Map ProcessId AID)
    , _stAid2Pid :: !(Map AID (Set ProcessId))

      -- Restarts
    , _stActorClosure   :: !(Map AID (Closure (Process ())))
      -- ^ Closure for the actor. All restartable actors have closure
      -- stored.

      
      --   -- Monitor resources
    -- , _stChildren :: !(Map ProcessId (Either ProcState GroupID))
    --   -- ^ State of monitored processes
    -- , _stGroups   :: !(Map GroupID GroupState)
    --   -- ^ State of groups of processes
    -- , _stConnUpstream   :: !(Map ActorID (ActorID,SomeSendEnd))
    --   -- ^ Upstream connection of an actor.
    -- , _stConnDownstream :: !(Map ActorID (Either (ActorID,SomeRecvEnd) SomeRecvEnd))
    --   -- ^ Downstream connection of an actor. Could be parent act
    -- , _stRestartable    :: !(Map ProcessId (Match' (SomeRecvEnd,SomeSendEnd,[SendPort Int]), Closure (Process ()), Message))
    --   -- ^ Set of processes which could be restarted

{-
      -- Many rank actors
    , _stCountRank :: !(Map GroupID (Int,Int))
      -- ^ Unused ranks 
    , _stPooledProcs   :: !(Map GroupID [SendPort (Maybe Rank)])
      -- ^ Groups for which we can send enough of ranks message
-}
    }

-- | State of actor.
data ActorState
    = Failed                    -- ^ Actor failed
    | Running RunInfo           -- ^ Actor is still running
    | Completed Int             -- ^ Actor finished execution successfully

-- | Information about running process. We store number of completed
--   processes and number of allowed failures. Number of still running
--   processes is accessible via AID<->{PID} mapping
data RunInfo = RunInfo
    { nCompleted     :: Int
      -- ^ Number of CH processes that complete execution 
    , allowedFailers :: Int
      -- ^ Number of allowed failures
    }

$(makeLenses ''StateDNA)

-- | Destination of actor.
actorDestinationAddr :: AID -> Controller (Maybe RecvAddr)
actorDestinationAddr aid = do
    maidDst <- use $ stActorDst . at aid
    case maidDst of
      Nothing             -> return Nothing
      Just (Left var)     -> use $ stVars . at var
      Just (Right aidDst) -> do Just m <- use $ stActorRecvAddr . at aidDst
                                -- FIXME: Pattern matching??
                                return $ fmap fst m



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Terminate actor forcefully
terminateActor :: (MonadProcess m, MonadState StateDNA m) => AID -> m ()
terminateActor aid = do
    mpids <- use $ stAid2Pid . at aid
    liftP $ T.forM_ mpids $ T.mapM_ $ \p -> do
        liftIO $ print p
        send p (Terminate "TERMINATE")

-- | Drop actor from the registry
dropActor :: (MonadState StateDNA m) => AID -> m ()
dropActor aid = do
    mpids <- use $ stAid2Pid . at aid
    stAid2Pid       . at aid .= Nothing
    stActorRecvAddr . at aid .= Just Nothing
    T.forM_ mpids $ T.mapM_ $ \p ->
        stPid2Aid . at p .= Nothing

-- | Put resources associated with PID to the pool
freeResouces :: (MonadState StateDNA m) => ProcessId -> m ()
freeResouces pid = do
    mr <- use $ stUsedResources . at pid
    case mr of
      Nothing -> return ()
      Just (VirtualCAD Local  _ ns) -> stNodePool %= (Set.fromList ns <>)
      Just (VirtualCAD Remote n ns) -> stNodePool %= (\xs -> Set.singleton n <> Set.fromList ns <> xs)

-- | Put resources associated with PID to the pool
freeActorResouces :: (MonadState StateDNA m) => AID -> m ()
freeActorResouces aid = do
    mpids <- use $ stAid2Pid . at aid
    T.forM_ mpids $ T.mapM_ freeResouces


-- Generate unique ID
uniqID :: Monad m => StateT StateDNA m Int
uniqID = do
    i <- use stCounter
    stCounter .= (i + 1)
    return i
