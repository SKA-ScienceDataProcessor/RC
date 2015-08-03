{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
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
import qualified Data.Foldable as T
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
    | Except SomeException         -- ^ Uncaught exception

data PanicException = PanicException String
             deriving (Show,Typeable)
instance Exception PanicException

data FatalException = FatalException String
             deriving (Show,Typeable)
instance Exception FatalException

-- | Fatal error
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
              , envAID         = actorAID p
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
    , actorAID         :: AID
      -- ^ AID of an actor as viewed by parent
    , actorLogOpt      :: LoggerOpt
      -- ^ Logging preferences
    , actorWorkDir     :: FilePath
      -- ^ Actor working directory
    }
    deriving (Typeable,Generic)
instance Binary ActorParam

-- | Parameters of an actor which are constant during its execution
data Env = Env
    { envRank        :: !Int
    , envGroupSize   :: !Int
    , envInterpreter :: !(Closure DnaInterpreter)
    , envAID         :: !AID
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
      
      -- Mapping ProcessID <-> AID
    , _stPid2Aid    :: !(Map ProcessId AID)
    }

-- | Hierarchy of actors
data ActorHier
    = SimpleActor               -- ^ Simple 1-process actor
    | GroupMember AID           -- ^ Actor which is member of a group
    | ActorGroup [AID]          -- 
    | ActorTree  [AID]          -- 
    deriving (Show,Eq)

-- | State of leaf actor.
data ActorState
    = Completed              -- ^ Actor completed execution successfully
    | Failed                 -- ^ Actor failed
    | Running    ProcInfo    -- ^ Actor is running
    | GrpRunning Int         -- ^ Group of actor is still running 
    deriving (Show)

-- | Data source of an actor
data ActorSrc
    = SrcParent (RecvAddr Recv -> Process ()) -- ^ Actor receive data from parent
    | SrcActor  AID                      -- ^ Actor receive data from another actor
    | SrcSubordinate                     -- ^ Actor is member of group of actors

-- | Destination of an actor
data ActorDst
    = DstParent VID -- ^ Actor sends data back to parent
    | DstActor  AID -- ^ Actor sends data to another actor
    deriving (Show,Eq)


-- | Data about actor which corresponds to single
data ProcInfo = ProcInfo
  { _pinfoPID      :: ProcessId
  , _pinfoRecvAddr :: RecvAddr Recv
  , _pinfoNodes    :: VirtualCAD
  , _pinfoClosure  :: Maybe SpawnCmd
  }
  deriving (Show)

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

-- | Get ID of top-level actor in group
topLevelActor :: MonadState StateDNA m => AID -> m AID
topLevelActor aid = do
    mst <- use $ stActors . at aid
    case mst of
      Just (GroupMember a) -> return a
      Just  _              -> return aid

-- | Obtain receive address for an actor
getRecvAddress
    :: (MonadState StateDNA m, MonadError DnaError m)
    => AID -> m (RecvAddr Recv)
getRecvAddress aid = do
    Just act <- use $ stActors     . at aid
    Just st  <- use $ stActorState . at aid
    case (act,st) of
      -- 1-process actor
      (SimpleActor  , Running p) -> return $ p^.pinfoRecvAddr
      (GroupMember{}, Running p) -> return $ p^.pinfoRecvAddr
      -- Tree collector
      (ActorTree  as, GrpRunning{}) -> do
          ms <- forM as $ \a -> do
              getRecvAddress a >>= \case
                RcvReduce m -> return m
                _           -> panic "Bad subordinate actor for tree"
          return $ RcvTree $ concat ms
      -- Group of worker processes
      (ActorGroup as, GrpRunning{}) -> do
          ms <- forM as $ \a -> do
              getRecvAddress a >>= \case
                RcvSimple m -> return m
                _           -> panic "Bad subordinate actor for group"
          return $ RcvGrp ms
      _ -> fail $ "getRecvAddress: " ++ show (act,st)

-- | Execute action for each sub actor in compound actor
traverseActor
    :: (MonadState StateDNA m, Monoid r)
    => (AID -> m r) -> AID -> m r
traverseActor action aid = do
    action aid
    mst <- use $ stActors . at aid
    case mst of
      Just (ActorGroup as) -> do r <- forM as $ traverseActor action
                                 return $ mconcat r
      Just (ActorTree  as) -> do r <- forM as $ traverseActor action
                                 return $ mconcat r
      _                    -> return mempty

-- | Send same message to all processes in actor
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


-- -- | Destination of actor.
-- actorDestinationAddr :: AID -> Controller (Maybe RecvAddr)
-- actorDestinationAddr aid = do
--     maidDst <- use $ stActorDst . at aid
--     case maidDst of
--       Nothing             -> return Nothing
--       Just (Left var)     -> use $ stVars . at var
--       Just (Right aidDst) -> do Just m <- use $ stActorRecvAddr . at aidDst
--                                 -- FIXME: Pattern matching??
--                                 return $ fmap fst m



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- -- | Stop actor and return resources to the pool
-- stopActorWith
--     :: (MonadState StateDNA m, MonadProcess m)
--     => ActorState
--     -> AID
--     -> m ()
-- stopActorWith newSt aid = do
--     mst <- use $ stChildren . at aid
--     case mst of
--       Just (Running st) -> case st of
--         SimpleActor   p   -> do free p
--                                 stChildren . at aid .= Just newSt
--         GroupMember _ p   -> do free p
--                                 stChildren . at aid .= Just newSt
--         ActorGroup as _   -> do T.forM_ as $ stopActorWith newSt
--                                 stChildren . at aid .= Just newSt
--         ActorTreeGroup as -> do T.forM_ as $ stopActorWith newSt
--                                 stChildren . at aid .= Just newSt
--       _ -> return ()
--   where
--     free p = do
--         me <- liftP getSelfNode
--         case _pinfoNodes p of
--           VirtualCAD n ns -> stNodePool %=
--             (\xs -> Set.delete (NodeInfo me) $ Set.singleton n <> Set.fromList ns <> xs)

-- topLevelActor :: (MonadState StateDNA m) => AID -> m AID
-- topLevelActor aid = do
--     Just st <- use $ stChildren . at aid
--     case st of
--       ActorGroup     a _ -> return a
--       ActorTreeGroup a   -> return a
--       _                  -> return aid

{-
-- | Drop actor from the registry. This only removes process from
--   registry and assumes that it dead already
dropActor :: (MonadState StateDNA m) => AID -> m ()
dropActor aid = do
    -- mst <- use $ stChildren . at aid
    -- case mst of
    --   Just (Running st) -> case st of
          
    undefined
    -- mpids <- use $ stAid2Pid . at aid
    -- stAid2Pid       . at aid .= Nothing
    -- stActorRecvAddr . at aid .= Just Nothing
    -- T.forM_ mpids $ T.mapM_ $ \p ->
    --     stPid2Aid . at p .= Nothing

-- | Put resources associated with PID to the pool
freeResouces :: (MonadProcess m, MonadState StateDNA m) => ProcessId -> m ()
freeResouces pid = do
    undefined
    -- mr <- use $ stUsedResources . at pid
    -- me <- liftP getSelfNode
    -- stUsedResources . at pid .= Nothing
    -- case mr of
    --   Nothing -> return ()
    --   Just (VirtualCAD n ns) -> stNodePool %= (\xs -> Set.delete (NodeInfo me) $ Set.singleton n <> Set.fromList ns <> xs)

-- | Put resources associated with PID to the pool
freeActorResouces :: (MonadProcess m, MonadState StateDNA m) => AID -> m ()
freeActorResouces aid = do
    undefined
    -- mpids <- use $ stAid2Pid . at aid
    -- T.forM_ mpids $ T.mapM_ freeResouces


sendToActor :: (MonadState StateDNA m, MonadProcess m, Serializable a) => AID -> a -> m ()
sendToActor aid a = do
    undefined
    -- pids <- use $ stAid2Pid . at aid
    -- liftP $ T.forM_ pids $ T.mapM_ (\p -> send p a)
-}

-- Generate unique ID
uniqID :: MonadState StateDNA m => m Int
uniqID = do
    i <- use stCounter
    stCounter .= (i + 1)
    return i
