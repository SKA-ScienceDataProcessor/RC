{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Code for starting remote actors
module DNA.Interpreter.Spawn (
      -- * Spawning actors
      execSpawnActor
    , execSpawnCollector
    , execSpawnGroup
    -- , execSpawnGroupN
    -- , execSpawnCollectorGroup
    -- , execSpawnCollectorGroupMR
    -- , execSpawnMappers
    ) where

import Control.Applicative
import Control.Concurrent  (threadDelay)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Distributed.Static  (closureApply)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Data.Monoid
import qualified Data.Foldable as T
import qualified Data.Set as Set
import Text.Printf

import DNA.CH
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
    (aid,ch) <- spawnSingleActor res
         $ closureApply $(mkStaticClosure 'runActor) <$> act
    -- Get back shell for the actor
    --
    -- FIXME: We need to abort if process which should send us shell
    --        dies. Or restart??? Or???
    dst <- handleRecieve messageHandlers [matchChan' ch]
    case dst of
      RcvSimple{} -> return ()
      _           -> error "OUCH! Val"
    -- liftIO $ print dst
    stActorRecvAddr . at aid .= Just dst
    -- liftIO . print =<< use stActorRecvAddr
    -- liftIO $ print dst
    return $ Shell aid


-- | Spawn collector actor on remote node
execSpawnCollector
    :: forall a b. (Serializable a, Serializable b)
    => Res
    -> Spawn (Closure (CollectActor a b))
    -> DnaMonad (Shell (Grp a) (Val b))
-- BLOCKING
execSpawnCollector res act = do
    -- Spawn actor
    (aid,ch) <- spawnSingleActor res
         $ closureApply $(mkStaticClosure 'runCollectActor) <$> act
    dst <- handleRecieve messageHandlers [matchChan' ch]
    case dst of
      RcvReduce{} -> return ()
      _           -> error "OUCH! Grp"
    stActorRecvAddr . at aid .= Just dst
    -- FIXME: See above
    -- liftIO . print =<< use stActorRecvAddr
    return $ Shell aid


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
    (k,aid,ch) <- spawnActorGroup res resG
             $ closureApply $(mkStaticClosure 'runActor) <$> act
    -- Assemble group
    --
    -- FIXME: Fault tolerance. We need to account for the fact some
    --        processes can crash before we have chance to get shell
    --        from them
    dsts    <- replicateM k $ handleRecieve messageHandlers [matchChan' ch]
    dstList <- forM dsts $ \d -> case d of
                 RcvSimple m -> return m
                 _           -> error "PANIC"
    stActorRecvAddr . at aid .= Just (RcvGrp dstList)
    -- liftIO . print =<< use stActorRecvAddr
    return $ Shell aid

{-
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
    msgs <- replicateM k $ handleRecieve messageHandlers [matchMsg']
    let (chN,shells) = unzip msgs
    stPooledProcs . at gid .= Just chN
    return $ broadcast $ assembleShellGroup gid shells
-}

{-
-- |
execSpawnCollectorGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (CollectActor a b))
    -> DnaMonad (Shell (Grp a) (Grp b))
execSpawnCollectorGroup res resG act = do
    undefined
    -- -- Spawn actors
    -- (k,gid) <- spawnActorGroup res resG
    --          $ closureApply $(mkStaticClosure 'runCollectActor) <$> act
    -- -- Assemble group
    -- -- FIXME: Fault tolerance
    -- sh <- replicateM k $ handleRecieve messageHandlers [matchMsg']
    -- return $ assembleShellGroupCollect gid sh
-}

{-
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
    sh <- replicateM k $ handleRecieve messageHandlers [matchMsg']
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
    sh <- replicateM k $ handleRecieve messageHandlers [matchMsg']
    return $ assembleShellMapper gid sh
-}

----------------------------------------------------------------
-- Spawn helpers
----------------------------------------------------------------

-- Spawn actor which only uses single CH process.
spawnSingleActor
    :: Res
    -> Spawn (Closure (Process ()))
    -> DnaMonad (AID,ReceivePort RecvAddr)
spawnSingleActor res spwn = do
    let (act,flags) = runSpawn spwn
    -- Acquire resources
    cad <- runController
         $ makeResource (if UseLocal `elem` flags then Local else Remote)
       =<< requestResources res
    -- Start actor
    aid     <- AID <$> uniqID
    (pid,_) <- liftP $ spawnSupervised (nodeId $ vcadNode cad) act
    -- Set registry
    stAid2Pid       . at aid .= Just (Set.singleton pid)
    stPid2Aid       . at pid .= Just aid
    stChildren      . at aid .= Just (Running (RunInfo 0 0))
    stUsedResources . at pid .= Just cad
    -- Add timeout for actor
    liftP $ setTimeout flags aid
    -- Send auxiliary parameter
    (chSend,chRecv) <- liftP newChan
    _ <- sendActorParam pid (Rank 0) (GroupSize 1) cad chSend
           (concat [fs | UseDebug fs <- flags])
    -- -- Record restart if needed
    -- -- -- -- -- -- --
    -- T.forM_ mmatch $ \m ->
    --     when (UseRespawn `elem` flags) $
    --         stRestartable . at pid .= Just (m,act,wrapMessage p)
    return (aid,chRecv)


-- Spawn group of actors
spawnActorGroup
    :: Res                          -- Resourses allocated to group
    -> ResGroup                     -- How to split resources between actors
    -> Spawn (Closure (Process ())) -- Closure to process'
    -> DnaMonad (Int,AID,ReceivePort RecvAddr) -- Returns size of group and group ID
spawnActorGroup res resG spwn = do
    let (act,flags) = runSpawn spwn
    -- Acquire resources
    rs <- runController
         $ splitResources resG
       =<< addLocal flags
       =<< requestResources res
    let k     = length rs
        nFail = if UseFailout `elem` flags then k else 0
    -- Record group existence
    aid <- AID <$> uniqID
    stChildren . at aid .= Just (Running (RunInfo 0 nFail))
    (chSend,chRecv) <- liftP newChan
    -- Spawn actors
    forM_ ([0..] `zip` rs) $ \(rnk,cad) -> do
        (pid,_) <- liftP
                 $ spawnSupervised (nodeId $ vcadNode cad) act
        sendActorParam pid (Rank rnk) (GroupSize k) cad chSend
            (concat [fs | UseDebug fs <- flags])
        stAid2Pid       . at aid %= Just . maybe (Set.singleton pid) (Set.insert pid)
        stPid2Aid       . at pid .= Just aid
        stUsedResources . at pid .= Just cad
    -- Add timeout for actor
    liftP $ setTimeout flags aid
    --
    return (k,aid,chRecv)


-- Create process for forcing timeout when process
setTimeout :: [SpawnFlag] -> AID -> Process ()
setTimeout flags aid = do
    me <- getSelfPid
    T.forM_ (getTimeoutInterval flags) $ \t -> spawnLocal $ do
        link me
        liftIO $ threadDelay $ round $ t * 1e6
        -- FIXME: Wrong message type!
        send me aid

getTimeoutInterval :: [SpawnFlag] -> Maybe Double
getTimeoutInterval = getLast . T.foldMap (Last . go)
  where go (UseTimeout t) = Just t
        go _              = Nothing


-- Send auxiliary parameters to an actor
sendActorParam
    :: ProcessId
    -> Rank
    -> GroupSize
    -> VirtualCAD
    -> SendPort RecvAddr
    -> [DebugFlag]
    -> DnaMonad ActorParam
sendActorParam pid rnk g cad ch flags = do
    me     <- liftP getSelfPid
    interp <- envInterpreter <$> ask
    let p = ActorParam
            { actorParent      = me
            , actorInterpreter = interp
            , actorRank        = rnk
            , actorGroupSize   = g
            , actorNodes       = cad
            , actorDebugFlags  = flags
            , actorSendBack    = ch
            }
    liftP $ send pid p
    return p



----------------------------------------------------------------
-- Resource allocation
----------------------------------------------------------------

-- Allocate list of resources for actor/actors
requestResources :: Res -> Controller [NodeInfo]
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
makeResource :: Location -> [NodeInfo] -> Controller VirtualCAD
makeResource Remote []     = fatal "Need positive number of nodes"
makeResource Remote (n:ns) = return (VirtualCAD Remote n ns)
makeResource Local  ns     = do
    n <- liftP getSelfNode
    return $ VirtualCAD Local (NodeInfo n) ns

-- Add local node to the list of nodes if needed
addLocal :: [SpawnFlag] -> [NodeInfo] -> Controller [NodeInfo]
addLocal flags nodes
  | UseLocal `elem` flags = do
        n <- liftP getSelfNode
        -- FIXME: We need to store information about local node
        --        somewhere
        return $ NodeInfo n : nodes
  | otherwise             = return nodes

-- Split resources for multiple actors
splitResources :: ResGroup -> [NodeInfo] -> Controller [VirtualCAD]
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
            n:rest -> return $ VirtualCAD Remote n rest
            -- FIXME: Local/Remote!



-- Split list to N chunks
toNChunks :: MonadError DnaError m => Int -> [a] -> m [[a]]
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
toSizedChunks :: MonadError DnaError m => Int -> [a] -> m [[a]]
toSizedChunks n items
    | n <= 0    = fatal "Non-positive size of chunk"
    | n >  len  = fatal "Chunk size is too large"
    | otherwise = toNChunks (len `div` n) items
  where
    len = length items
