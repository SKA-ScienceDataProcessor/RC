{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Code for starting remote actors
module DNA.Interpreter.Spawn (
      execSpawnActor
    , execSpawnCollector
    , execSpawnGroup
    , execSpawnGroupN
    , execSpawnCollectorGroup
    , execSpawnCollectorGroupMR
    , execSpawnMappers
    ) where

import Control.Applicative
import Control.Concurrent  (threadDelay)
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
import Data.Monoid
import Data.List
import qualified Data.Foldable as T
import qualified Data.Set as Set
-- import           Data.Set   (Set)
import Text.Printf
-- import GHC.Generics  (Generic)

import DNA.Types
import DNA.Lens
import DNA.DSL
import DNA.Logging
import DNA.Interpreter.Types
import DNA.Interpreter.Run
import DNA.Interpreter.Message


----------------------------------------------------------------
-- Functions for spawning actors
----------------------------------------------------------------

-- | Spawn simple actor on remote node
execSpawnActor
    :: (Serializable a, Serializable b)
    => Res
    -> Spawn (Closure (Actor a b))
    -> DnaMonad (Shell (Val a) (Val b))
-- BLOCKING
execSpawnActor res act = do
    -- Spawn actor
    spawnSingleActor res $ closureApply $(mkStaticClosure 'runActor) <$> act
    -- Get back shell for the actor
    --
    -- FIXME: We need to abort if process which should send us shell
    --        dies.
    handleRecieve messageHandlers matchMsg'


-- | Spawn collector actor on remote node
execSpawnCollector
    :: (Serializable a, Serializable b)
    => Res
    -> Spawn (Closure (CollectActor a b))
    -> DnaMonad (Shell (Grp a) (Val b))
-- BLOCKING
execSpawnCollector res act = do
    -- Spawn actor
    spawnSingleActor res $ closureApply $(mkStaticClosure 'runCollectActor) <$> act
    -- Get back shell for the actor
    --
    -- FIXME: See above
    handleRecieve messageHandlers matchMsg'


-- | Spawn group of normal processes
execSpawnGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (Actor a b))
    -> DnaMonad (Shell (Scatter a) (Grp b))
-- BLOCKING
execSpawnGroup res resG act = do
    -- Spawn actors
    (k,gid) <- spawnActorGroup res resG
             $ closureApply $(mkStaticClosure 'runActor) <$> act
    -- Assemble group
    --
    -- FIXME: Fault tolerance. We need to account for the fact some
    --        processes can crash before we have chance to get shell
    --        from them
    sh <- replicateM k $ handleRecieve messageHandlers matchMsg'
    return $ assembleShellGroup gid sh

execSpawnGroupN
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Int
    -> Spawn (Closure (Actor a b))
    -> DnaMonad (Shell (Val a) (Grp b))
execSpawnGroupN res resG n act = do
    -- Spawn actors
    (k,gid) <- spawnActorGroup res resG 
             $ closureApply $(mkStaticClosure 'runActorManyRanks) <$> act
    -- Assemble group
    -- FIXME: Fault tolerance
    msgs <- replicateM k $ handleRecieve messageHandlers matchMsg'
    let (chN,shells) = unzip msgs
    stPooledProcs . at gid .= Just chN
    return $ broadcast $ assembleShellGroup gid shells

-- | 
execSpawnCollectorGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (CollectActor a b))
    -> DnaMonad (Shell (Grp a) (Grp b))
execSpawnCollectorGroup res resG act = do
    -- Spawn actors
    (k,gid) <- spawnActorGroup res resG
             $ closureApply $(mkStaticClosure 'runCollectActor) <$> act
    -- Assemble group
    -- FIXME: Fault tolerance
    sh <- replicateM k $ handleRecieve messageHandlers matchMsg'
    return $ assembleShellGroupCollect gid sh

-- | Start group of collector processes
execSpawnCollectorGroupMR
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (CollectActor a b))
    -> DnaMonad (Shell (MR a) (Grp b))
execSpawnCollectorGroupMR res resG act = do
    -- Spawn actors
    (k,gid) <- spawnActorGroup res resG
             $ closureApply $(mkStaticClosure 'runCollectActorMR) <$> act
    -- Assemble group
    -- FIXME: Fault tolerance
    sh <- replicateM k $ handleRecieve messageHandlers matchMsg'
    return $ assembleShellGroupCollectMR gid sh

execSpawnMappers
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (Mapper a b))
    -> DnaMonad (Shell (Scatter a) (MR b))
execSpawnMappers res resG act = do
    -- Spawn actors
    (k,gid) <- spawnActorGroup res resG
             $ closureApply $(mkStaticClosure 'runMapperActor) <$> act
    -- Assemble group
    -- FIXME: Fault tolerance
    sh <- replicateM k $ handleRecieve messageHandlers matchMsg'
    return $ assembleShellMapper gid sh


----------------------------------------------------------------
-- Spawn helpers
----------------------------------------------------------------

-- Spawn actor which only uses single CH process.
spawnSingleActor
    :: Res
    -> Spawn (Closure (Process ()))
    -> DnaMonad ()
spawnSingleActor res spwn = do
    let (act,flags) = runSpawn spwn
    -- Acquire resources
    cad <- runController
         $ makeResource (if UseLocal `elem` flags then Local else Remote)
       =<< addLocal flags
       =<< requestResources res
    -- Start actor
    (pid,_) <- liftP $ spawnSupervised (vcadNode cad) act
    -- Add timeout for actor
    liftP $ setTimeout flags (SingleActor pid)
    -- Record data about actor
    stUsedResources . at pid .= Just cad
    stChildren      . at pid .= Just (Left Unconnected)
    -- Send auxiliary parameter
    sendActorParam pid (Rank 0) (GroupSize 1) cad

-- Spawn group of actors
spawnActorGroup
    :: Res                          -- Resourses allocated to group
    -> ResGroup                     -- How to split resources between actors
    -> Spawn (Closure (Process ())) -- Closure to process'
    -> DnaMonad (Int,GroupID)       -- Returns size of group and group ID
spawnActorGroup res resG spwn = do
    let (act,flags) = runSpawn spwn
    -- Acquire resources
    rs <- runController
         $ splitResources resG
       =<< addLocal flags
       =<< requestResources res
    let k = length rs
    -- Record group existence
    gid <- GroupID <$> uniqID
    let groupTy = if UseFailout `elem` flags
                  then Failout
                  else Normal
    stGroups . at gid .= Just (GrUnconnected groupTy (k,0))
    -- Spawn actors
    forM_ ([0..] `zip` rs) $ \(rnk,cad) -> do
        (pid,_) <- liftP
                 $ spawnSupervised (vcadNode cad) act
        sendActorParam pid (Rank rnk) (GroupSize k) cad
        stChildren . at pid .= Just (Right gid)
    -- Add timeout for actor
    liftP $ setTimeout flags (ActorGroup gid)
    --
    return (k,gid)


-- Create process for forcing timeout when process
setTimeout :: [SpawnFlag] -> ActorID -> Process ()
setTimeout flags aid = do
    me <- getSelfPid
    T.forM_ (getTimeout flags) $ \t -> spawnLocal $ do
        liftIO $ threadDelay $ round $ t * 1e6
        send me aid

getTimeout :: [SpawnFlag] -> Maybe Double
getTimeout = getLast . T.foldMap (Last . go)
  where go (UseTimeout t) = Just t
        go _              = Nothing

-- Send auxiliary parameters to an actor
sendActorParam
    :: ProcessId -> Rank -> GroupSize -> VirtualCAD -> DnaMonad ()
sendActorParam pid rnk g cad = do
    me     <- liftP getSelfPid
    interp <- use stInterpreter
    lift $ send pid ActorParam
                      { actorParent      = me
                      , actorInterpreter = interp
                      , actorRank        = rnk
                      , actorGroupSize   = g
                      , actorNodes       = vcadNodePool cad
                      }


assembleShellGroup :: GroupID -> [Shell (Val a) (Val b)] -> Shell (Scatter a) (Grp b)
assembleShellGroup gid shells =
    Shell (ActorGroup gid)
          (RecvGrp $ map getRecv shells)
          (SendGrp $ map getSend shells)
  where
    getRecv :: Shell (Val a) b -> SendPort a
    getRecv (Shell _ (RecvVal ch) _) = ch
    getRecv _ = error "assembleShellGroup: unexpected type of shell process"
    getSend :: Shell a (Val b) -> SendPort (Dest b)
    getSend (Shell _ _ (SendVal ch)) = ch

assembleShellGroupCollect :: GroupID -> [Shell (Grp a) (Val b)] -> Shell (Grp a) (Grp b)
assembleShellGroupCollect gid shells =
    Shell (ActorGroup gid)
          (RecvReduce $ getRecv =<< shells)
          (SendGrp    $ map getSend shells)
  where
    getRecv :: Shell (Grp a) b -> [(SendPort Int, SendPort a)]
    getRecv (Shell _ (RecvReduce ch) _) = ch
    getSend :: Shell a (Val b) -> SendPort (Dest b)
    getSend (Shell _ _ (SendVal ch)) = ch

assembleShellGroupCollectMR :: GroupID -> [Shell (MR a) (Val b)] -> Shell (MR a) (Grp b)
assembleShellGroupCollectMR gid shells =
    Shell (ActorGroup gid)
          (RecvMR  $ getRecv =<< shells)
          (SendGrp $ map getSend shells)
  where
    getRecv :: Shell (MR a) b -> [(SendPort Int, SendPort (Maybe a))]
    getRecv (Shell _ (RecvMR ch) _) = ch
    getSend :: Shell a (Val b) -> SendPort (Dest b)
    getSend (Shell _ _ (SendVal ch)) = ch

assembleShellMapper :: GroupID -> [Shell (Val a) (MR b)] -> Shell (Scatter a) (MR b)
assembleShellMapper gid shells =
    Shell (ActorGroup gid)
          (RecvGrp $ map getRecv shells)
          (SendMR  $ getSend =<< shells)
  where
    getRecv :: Shell (Val a) b -> SendPort a
    getRecv (Shell _ (RecvVal ch) _) = ch
    getRecv _ = error "assembleShellGroup: unexpected type of shell process"
    getSend :: Shell a (MR b) -> [SendPort [SendPort (Maybe b)]]
    getSend (Shell _ _ (SendMR ch)) = ch



----------------------------------------------------------------
-- Resource allocation
----------------------------------------------------------------

-- Allocate list of resources for actor/actors
requestResources :: Res -> Controller [NodeId]
requestResources r = do
    free <- Set.toList <$> use stNodePool
    taggedMessage "DNA" $ "Req: " ++ show r ++ " pool: " ++ show free
    case r of
     N n -> do  
        when (length free < n) $
            fatal $ printf "Cannot allocate %i nodes" n
        let (used,rest) = splitAt n free
        stNodePool .= Set.fromList rest
        return used
     Frac frac -> do
        let n = length free
            k = round $ fromIntegral n * frac
        let (used,rest) = splitAt k free
        stNodePool .= Set.fromList rest
        return used

-- Create virtual CAD for single actor
makeResource :: Location -> [NodeId] -> Controller VirtualCAD
makeResource Remote []     = fatal "Need positive number of nodes"
makeResource Remote (n:ns) = return (VirtualCAD Remote n ns)
makeResource Local  ns     = do
    n <- lift $ lift getSelfNode
    return $ VirtualCAD Local n ns

-- Add local node to the list of nodes if needed
addLocal :: [SpawnFlag] -> [NodeId] -> Controller [NodeId]
addLocal flags nodes
  | UseLocal `elem` flags = do
        n <- liftP getSelfNode
        return $ n:nodes
  | otherwise             = return nodes

-- Split resources for multiple actors
splitResources :: ResGroup -> [NodeId] -> Controller [VirtualCAD]
splitResources resG nodes = case resG of
    NWorkers k -> do
        chunks <- toNChunks k nodes
        forM chunks $ \ns -> case ns of
            []     -> fatal "Impossible: empty nodelist"
            -- FIXME: Local/Remote!
            n:rest -> return $ VirtualCAD Remote n rest
    NNodes k -> do
        when (length nodes < k) $
          fatal "Not enough nodes to schedule"
        chunks <- toSizedChunks k nodes
        forM chunks $ \ns -> case ns of
            []     -> fatal "Impossible: empty nodelist"
            -- FIXME: Local/Remote!
            n:rest -> return $ VirtualCAD Remote n rest



-- Split list to N chunks
toNChunks :: MonadError String m => Int -> [a] -> m [[a]]
toNChunks n items
    | n <= 0    = fatal "Non-positive number of chunks"
    | n >  len  = fatal "Cannot allocate enough items"
    | otherwise = return $ go size rest items
  where
    len = length items
    (size,rest) = len `divMod` n
    go _  _ [] = []
    go sz 0 xs = case splitAt  sz    xs of (as,rm) -> as : go sz 0     rm
    go sz r xs = case splitAt (sz+1) xs of (as,rm) -> as : go sz (r-1) rm

-- Split list to chunks of size N
toSizedChunks :: MonadError String m => Int -> [a] -> m [[a]]
toSizedChunks n items
    | n <= 0    = fatal "Non-positive size of chunk"
    | n >  len  = fatal "Chunk size is too large"
    | otherwise = toNChunks (len `div` n) items
  where
    len = length items
