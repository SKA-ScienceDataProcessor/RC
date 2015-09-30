{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Data types for interpretation of DNA DSL using cloud haskell
module DNA.Interpreter.Types where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Monoid
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)
import GHC.Generics  (Generic)
import Lens.Family.TH

import DNA.Types
import DNA.Lens
import DNA.DSL
import DNA.CH
import DNA.Logging


----------------------------------------------------------------
-- Extra functions for matching on messages
--
-- FIXME: should be greatly simplified by Functor instance for
--        Match. We could get rid of MatchS altogether
----------------------------------------------------------------

-- | Handlers for events which could be used with State monad with state s
data MatchS = forall a. Serializable a => MatchS (a -> Controller ())

-- | Wait for message from Match' and handle auxiliary messages
handleRecieve
    :: [MatchS]
    -> [Match' a]
    -> DnaMonad a
handleRecieve auxMs mA
    = loop
  where
    matches e s
        =  map toMatch ((fmap . fmap) Right mA)
        ++ [ match $ \a -> Left <$> (flip runReaderT e . flip runStateT s . unDnaMonad . runController) (f a)
           | MatchS f <- auxMs]
    loop = do
        e <- ask
        s <- get
        r <- liftP $ receiveWait $ matches e s
        case r of
          Right a      -> return a
          Left ((),s') -> put s' >> loop

-- | Wait for message
blockForMessage :: [MatchS] -> DnaMonad ()
blockForMessage auxMs = do
    e  <- ask
    s  <- get
    s' <- liftP $ receiveWait $ matches e s
    put s'
  where
    matches e s
        = [ match $ \a -> (flip runReaderT e . flip execStateT s . unDnaMonad . runController) (f a)
          | MatchS f <- auxMs
          ]




----------------------------------------------------------------
-- Monads
----------------------------------------------------------------

-- | Type synonym for monad which is used for interpretation of DNA
--   programs.
--
--   It have custom definition of fail which is used to raise panic
--   level error.
newtype DnaMonad a = DnaMonad
    { unDnaMonad :: StateT StateDNA (ReaderT Env Process) a }
    deriving (Functor,Applicative,MonadIO,MonadProcess,MonadState StateDNA,MonadReader Env)

instance Monad DnaMonad where
    return           = DnaMonad . return
    DnaMonad m >>= f = DnaMonad $ m >>= fmap unDnaMonad f
    fail             = doPanic

instance MonadLog DnaMonad where
    logSource    = liftP logSource
    logLoggerOpt = _stLogOpt <$> get


-- | Monad for event handlers. It adds explicit error handling
type Controller = ExceptT DnaError DnaMonad

-- | Error encountered during DNA computation
data DnaError
    = FatalErr String              -- ^ Fatal error
    | PanicErr String              -- ^ Internal error
    | Except   SomeException       -- ^ Uncaught exception

-- | Unrecoverable error. Could only occur due to bug in DNA
--   implementation.
data PanicException = PanicException String
             deriving (Show,Typeable)
instance Exception PanicException

-- | Unrecoverable error. Actor must immediately stop execution
data FatalException = FatalException String
             deriving (Show,Typeable)
instance Exception FatalException

-- | Raise fatal error.
fatal :: MonadError DnaError m => String -> m a
fatal = throwError . FatalErr

-- | Violation of internal invariant which should not ever happen
--   unless implementation is buggy.
panic :: MonadError DnaError m => String -> m a
panic = throwError . PanicErr

-- | Actually raise panic
doPanic :: MonadLog m => String -> m a
doPanic msg = do
    panicMsg msg
    liftIO $ throw $ PanicException msg

-- | Write fatal error to log
doFatal :: MonadLog m => String -> m a
doFatal msg = do
    fatalMsg msg
    liftIO $ throw $ FatalException msg


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

-- | Run DNA monad using ActorParam as source of input parameters and
--   interpreter
runDnaParam :: ActorParam -> DNA a -> Process a
runDnaParam p action = do
  interpreter <- unClosure $ actorInterpreter p
  flip runReaderT env
    $ flip evalStateT s0
    $ unDnaMonad
    $ dnaInterpreter interpreter action
  where
    env = Env { envRank        = case actorRank      p of Rank      i -> i
              , envGroupSize   = case actorGroupSize p of GroupSize i -> i
              , envInterpreter = actorInterpreter p
              , envWorkDir     = actorWorkDir p
              }
    s0 = StateDNA
           { _stCounter       = 0
           , _stDebugFlags    = actorDebugFlags p
           , _stLogOpt        = actorLogOpt p
           , _stNodePool      = Set.fromList $ vcadNodePool $ actorNodes p
           , _stActors        = Map.empty
           , _stActorState    = Map.empty
           , _stActorSrc      = Map.empty
           , _stActorDst      = Map.empty
           , _stVars          = Map.empty
           , _stPid2Aid       = Map.empty
           }

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
    , actorSendBack    :: SendPort (RecvAddr Recv)
      -- ^ Send receive address and list of port ID's back to the parent process
    , actorLogOpt      :: LoggerOpt
      -- ^ Logging preferences
    , actorWorkDir     :: FilePath
      -- ^ Actor working directory
    }
    deriving (Show,Typeable,Generic)
instance Binary ActorParam

-- | Parameters of an actor which are constant during its execution
data Env = Env
    { envRank        :: !Int
    , envGroupSize   :: !Int
    , envInterpreter :: !(Closure DnaInterpreter)
    , envWorkDir     :: !FilePath
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
    { _stCounter     :: !Int        -- ^ Counter for generation of unique IDs
    , _stDebugFlags  :: [DebugFlag] -- ^ Extra debug flags
    , _stLogOpt      :: LoggerOpt   -- ^ Options for logger

      -- Resources
    , _stNodePool      :: !(Set NodeInfo)
      -- ^ Nodes which are currently not in use

      -- Append only dataflow graph
    , _stActors     :: !(Map AID ActorHier)
      -- ^ List of all spawned processes
    , _stActorState :: !(Map AID ActorState)
      -- ^ State of all groups of processes
    , _stActorSrc   :: !(Map AID ActorSrc)
      -- ^ Data source of an actor.
    , _stActorDst   :: !(Map AID ActorDst)
      -- ^ Data destination of an actor.
    , _stVars       :: !(Map VID (RecvAddr Recv))
      -- ^ All local variables
      
    , _stPid2Aid    :: !(Map ProcessId AID)
      -- ^ Mapping from PIDs to AIDs
    }

-- | Description of actor. It's persistent and doesn't change after
--   creation.
data ActorHier
    = SimpleActor               -- ^ Simple 1-process actor
    | GroupMember AID           -- ^ Actor which is member of a group
    | ActorGroup [AID]          -- ^ Group of actors
    | ActorTree  [AID]          -- ^ Tree actors
    deriving (Show,Eq,Typeable,Generic)

-- | State of actor.
data ActorState
    = Completed              -- ^ Actor completed execution successfully
    | Failed                 -- ^ Actor failed
    | Running    ProcInfo    -- ^ Actor is running
    | GrpRunning Int         -- ^ Group of actor is still running 
    deriving (Show,Typeable,Generic)

-- | Data source of an actor
data ActorSrc
    = SrcParent (RecvAddr Recv -> Process ())
      -- ^ Actor receive data from parent
    | SrcActor  AID
      -- ^ Actor receive data from another actor
    | SrcSubordinate
      -- ^ Actor is member of group of actors

-- | Destination of an actor
data ActorDst
    = DstParent VID -- ^ Actor sends data back to parent
    | DstActor  AID -- ^ Actor sends data to another actor
    deriving (Show,Eq,Typeable,Generic)


-- | Data about actor which corresponds to single CH process.
data ProcInfo = ProcInfo
  { _pinfoPID      :: ProcessId      -- ^ PID of actor
  , _pinfoRecvAddr :: RecvAddr Recv  -- ^ Address for receiving data
  , _pinfoNodes    :: VirtualCAD     -- ^ Allocated nodes
  , _pinfoClosure  :: Maybe SpawnCmd -- ^ Optional command for restart of process
  }
  deriving (Show,Typeable,Generic)

-- | Command for spawning actor
data SpawnCmd
    = SpawnSingle
        (Closure (Process ()))
        Rank
        GroupSize
        ActorHier
        RecvAddrType
        [SpawnFlag]
    deriving (Show,Eq)

$(makeLenses ''StateDNA)
$(makeLenses ''ProcInfo)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Get ID of top-level actor in group, maybe this is same ID
topLevelActor :: (MonadState StateDNA m, MonadError DnaError m) => AID -> m AID
topLevelActor aid = do
    mst <- use $ stActors . at aid
    case mst of
      Just (GroupMember a) -> return a
      Just  _              -> return aid
      Nothing              -> panic "Unknown AID"


-- | Obtain receive address for an actor
getRecvAddress
    :: (MonadState StateDNA m, MonadError DnaError m)
    => AID -> m (RecvAddr Recv)
getRecvAddress aid = do
    Just act <- use $ stActors     . at aid
    Just st  <- use $ stActorState . at aid
    case (act,st) of
      -- 1-process actor
      (_            , Completed) -> return RcvCompleted
      (_            , Failed   ) -> return RcvFailed
      (SimpleActor  , Running p) -> return $ p^.pinfoRecvAddr
      (GroupMember{}, Running p) -> return $ p^.pinfoRecvAddr
      -- Tree collector
      (ActorTree  as, GrpRunning{}) -> do
          RcvTree `liftM` forM as getRecvAddress
      -- Group of worker processes
      (ActorGroup as, GrpRunning{}) -> do
          RcvGrp `liftM` forM as getRecvAddress
      -- Impossible
      _ -> panic $ "getRecvAddress: " ++ show (act,st)

-- | Execute action for each sub actor in compound actor
traverseActor
    :: (MonadState StateDNA m, Monoid r)
    => (AID -> m r) -> AID -> m r
traverseActor action aid = do
    r0  <- action aid
    mst <- use $ stActors . at aid
    case mst of
      Just (ActorGroup as) -> do r <- forM as $ traverseActor action
                                 return $ r0 <> mconcat r
      Just (ActorTree  as) -> do r <- forM as $ traverseActor action
                                 return $ r0 <> mconcat r
      _                    -> return r0

-- | Send same message to all live processes in actor
sendToActor
    :: (MonadProcess m, MonadState StateDNA m, Serializable a)
    => AID -> a -> m ()
sendToActor aid x = flip traverseActor aid $ \a -> do
    use (stActorState . at a) >>= \case
        Just (Running p) -> liftP $ send (p^.pinfoPID) x
        _                -> return ()

-- | Terminate actor forcefully. This only kill actors' processes and
--   doesn't affect registry
terminateActor :: (MonadProcess m, MonadState StateDNA m) => AID -> m ()
terminateActor aid = sendToActor aid (Terminate "TERMINATE")


-- | Free actor's resources
freeActor :: (MonadProcess m, MonadState StateDNA m) => AID -> m ()
freeActor aid  = do
    me <- liftP getSelfNode
    st <- use $ stActorState . at aid
    case st of
      Just (Running p) -> case p^.pinfoNodes of
        VirtualCAD n ns -> stNodePool %=
            (\xs -> Set.delete (NodeInfo me) $ Set.singleton n <> Set.fromList ns <> xs)
      _                -> return ()

-- | Generate unique ID
uniqID :: MonadState StateDNA m => m Int
uniqID = do
    i <- use stCounter
    stCounter .= (i + 1)
    return i
