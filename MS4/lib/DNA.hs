-- |
-- DNA is a data flow DSL aimed at expressing data movement and initiation of
-- computational kernels for numerical calculations. We use the "actor/channel"
-- paradigm, which allows for a descriptive approach that separates definition
-- from execution strategy. Our target is data intensive high performance computing
-- applications that employ hierarchical cluster scheduling techniques. Furthermore,
-- we provide infrastructure for detailed profiling and high availability, allowing
-- recovery for certain types of failures.
--
-- DNA is presently implemented as an embedded monadic DSL on top of the 
-- well-established distributed programming framework "Cloud Haskell".
-- This document describes the structure of the language at a high level,
-- followed by detailed specifications and use cases for the introduced
-- primitives. We will give examples at several points.
--
--
-- === High level structure
--
-- DNA programs are composed of actors and channels. DNA provides means for
-- defining an abstract data flow graph using programming language primitives.
--
-- Actors are executed concurrently, don't share state and can only communicate
-- using a restricted message passing scheme.
--
-- Every actor can receive either one or many inputs of the same type and produce
-- either one or multiple outputs. This depends on the type of the actor: For example,
-- a single 'Actor' will only ever accept one input parameter and produce one
-- result. On the other hand, a group of 'Actor's will produce an unordered set of
-- values of same type. Finally, a 'CollectActor' receives a group of values
-- while producing a single result. In general, actors have no knowledge where
-- their input parameters come from or where result will be sent, these connections
-- will be made from the outside.
--
-- Actors are spawned hierarchically, so every actor but the first will be created
-- by a parent actor. Communication is forced to flow along these hierarchies:
-- Both inputs and results can only be sent to and received from either the parent
-- actor or sibling actors on the same level.
--
-- Furthermore, DNA offers the possibility to spawn /groups/ of actors. Every actor
-- in a group will run the same code, but using different input parameters. To
-- distinguish actors in a group, they get assigned ranks from  /0/ to /N-1/.
-- Conceptually,  a group of actors is treated as single actor which runs on
-- several execution elements simultaneously.
--
-- To illustrate this, here is example of distributed dot product. We assume that
-- 'ddpComputeVector', 'ddpReadVector' and 'splitSlice' are already defined:
--
-- > -- Calculate dot product of slice of full vector
-- > ddpProductSlice = actor $ \(fullSlice) -> duration "vector slice" $ do
-- >    -- Calculate offsets
-- >    slices <- scatterSlice <$> groupSize
-- >    slice  <- (slices !!)  <$> rank
-- >    -- First we need to generate files on tmpfs
-- >    fname <- duration "generate" $ eval ddpGenerateVector n
-- >    -- Start local processes
-- >    shellVA <- startActor (N 0) $ useLocal >> return $(mkStaticClosure 'ddpComputeVector)
-- >    shellVB <- startActor (N 0) $ useLocal >> return $(mkStaticClosure 'ddpReadVector)
-- >    -- Connect actors
-- >    sendParam slice              shellVA
-- >    sendParam (fname, Slice 0 n) shellVB
-- >    futVA <- delay Local shellVA
-- >    futVB <- delay Local shellVB
-- >    -- Await results
-- >    va <- duration "receive compute" $ await futVA
-- >    vb <- duration "receive read"    $ await futVB
-- >    -- Clean up, compute sum
-- >    kernel "compute sum" [FloatHint 0 (2 * fromIntegral n)] $
-- >      return (S.sum $ S.zipWith (*) va vb :: Double)
-- >
-- > -- Calculate dot product of full vector
-- > ddpDotProduct :: Actor Int64 Double
-- > ddpDotProduct = actor $ \size -> do
-- >     -- Chunk & send out
-- >     shell <- startGroup (Frac 1) (NNodes 1) $ do
-- >         useLocal
-- >         return $(mkStaticClosure 'ddpProductSlice)
-- >     broadcast (Slice 0 size) shell
-- >     -- Collect results
-- >     partials <- delayGroup shell
-- >     duration "collecting vectors" $ gather partials (+) 0
-- >
-- > main :: IO ()
-- > main = dnaRun (...) $
-- >   liftIO . print =<<  eval ddpDotProduct (400*1000*1000)
--
-- This generates an actor tree of the following shape:
--
-- @
--         ddpDotProduct
--               |
--        ddpProductSlice
--        /             \\
-- ddpComputeVector  ddpReadVector
-- @
--
-- Here 'ddpDotProduct' is a single actor, which takes exactly one
-- parameter 'size' and produces exactly the sum as its output. On the
-- other hand, 'ddpProductSlice' is an actor group, which sums up a
-- portion of the full dot-product. Each actor in group spawns two
-- child actors: 'ddpComputeVector' and 'ddpReadVector' are two child
-- actors, which for our example are supposed to generate or read the
-- requested vector slice from the hard desk, respectively.
--
--
-- === Scheduling data flow programs for execution.
--
-- Scheduling, spawning and generation of the runtime data flow graph are handled
-- separately. The starting point for scheduling is the cluster architecture
-- descriptor, which describes the resources available to the program.
--
-- For DNA, we are using the following simple algorithm: First a
-- control actor starts the program. It's actor which passed to
-- 'runDna' as parameter. This actor will be assigned exclusively all
-- resources available to the program, which it can then in turn
-- allocate to it spawn child actors. When a child actor finishes
-- execution (either normally or abnormally), its resources are
-- returned to parent actor's resource pool and can be reused.
--
--
-- === High Availability
--
-- We must account for the fact that every actor could fail at any point. This could
-- not only happen because of hardware failures, but also due to programming errors.
-- In order to maintain the liveness of the data flow network, we must detect such
-- failures, no matter the concrete reason. In the worst case, our only choice is to
-- simply terminate all child processes and propagate the error to actors which depend
-- on the failed actor. This approach is obviously problematic for achieving
-- fault tolerance since we always have a single point of failure.
--
-- To improve stability, we need to make use of special cases. For
-- example, let us assume that a single actor instance in large group
-- fails. Then in some case it makes sense to simply ignore the
-- failure and discard the partial result. This is the \"failout\"
-- model. To use these semantics in the DNA program, all we need to do
-- is to specify 'failout' when spawning the actor with
-- 'startGroup'. To make use of failout example above should be changed to:
--
-- >     ...
-- >     shell <- startGroup (Frac 1) (NNodes 1) $ do
-- >         useLocal
-- >         failout
-- >         return $(mkStaticClosure 'ddpProductSlice)
-- >     ...
--
-- Another important recovery technique is restarting failed
-- processes. This obviously loses the current state of the restarted
-- process, so any accumulated data is lost. In the current design, we
-- only support this approach for 'CollectActor's. Similarly only
-- change to program is addition of 'respawnOnFail' to parameters of
-- actors.
--
--
--
-- === Profiling
--
-- For maintaing a robust system performance, we track
-- the performance of all actors and channels. This should allow us
-- to assess exactly how performance is shaped by not only scheduling
-- and resource allocation, but also performance of individual software
-- and hardware components. For example, we might decide
-- to change the scheduling with the goal of eliminating idle times,
-- optimise kernels better or decide to run a kernel on more suitable
-- computation hardware were available.
--
-- However, in order to facilitate making informed decisions about such changes,
-- it is not only important to collect raw performance numbers such as
-- time spent or memory consumed. For understanding the performance of
-- the whole system we need to put our measurements into context. This
-- means that we should associate them from the ground up with the
-- data flow structure of the program.
--
-- Our approach is therefore to implement profiling as an integral
-- service of the DNA runtime. The generated profile will automatically
-- track the overall performance of the system, capturing timings of all
-- involved actors and channels. Furthermore, wherever possible the data
-- flow program should contribute extra information about its activity,
-- such as number of floating point operations expected or
-- amount of raw data transferred. In the end, we will use the key
-- performance metrics derived from these values in order to visualise the
-- whole system performance in a way that will hopefully allow for
-- painless optimisation of the whole system.
module DNA (
      -- * DNA monad
      DNA
    , dnaRun
      -- ** Groups of actors
      -- |
      -- Actor could run in groups. These groups are treated as single
      -- logical actor. Each actor in group is assigned rank from /0/
      -- to /N-1/ where /N/ is group size. For uniformity single
      -- actors are treated as members of group of size 1. Both group
      -- size and rank could be accessed using 'rank' and 'groupSize'
    , rank
    , groupSize
      -- ** Logging and profiling
      -- |
      -- DNA programs write logs in GHC's eventlog format for
      -- recording execution progress and performance monitoring. Logs
      -- are written in following locations:
      -- @~\/_dna\/logs\/PID-u\/{N}\/program-name.eventlog@ if program
      -- was started using UNIX startup or
      -- @~\/_dna\/logs\/SLURM_JOB_ID-s\/{N}\/program-name.eventlog@
      -- if it was started by SLURM (see 'runDna' for detail of
      -- starting DNA program). They're stored in GHC's eventlog
      -- format.
    , logMessage
    , duration
      -- * Kernels
    , Kern
    , kernel
    , unboundKernel
    , ProfileHint(..)
    , floatHint, memHint, ioHint, haskellHint, cudaHint
      -- * Actors
    , Actor
    , actor
    , CollectActor
    , collectActor
      -- * Spawning
      -- |
      -- Actors could be spawned using start* functions. They spawn
      -- new actors which are executed asynchronously and usually on
      -- remote nodes. Nodes for newly spawned actor(s) are taken from
      -- pool of free nodes. If there's not enough nodes it's runtime
      -- error. eval* functions allows to execute actor synchronously. 

      -- ** Eval
    , eval
    , evalClosure
      -- ** Spawn parameters
    , Spawn
    , useLocal
    , failout
    , respawnOnFail
    , debugFlags
    , DebugFlag(..)
      -- ** Resources
      -- |
      -- These data types are used for describing how much resources
      -- should be allocated to nodes and are passed as parameters to
      -- start* functions.
    , Res(..)
    , ResGroup(..)
    , Location(..)
    , availableNodes
    , waitForResources
      -- ** Function to spawn new actors
      -- |
      -- All functions for starting new actors following same
      -- pattern. They take parameter which describe how many nodes
      -- should be allocated to actor(s) and 'Closure' to actor to be
      -- spawned. They all return handle to running actor (see
      -- documentation of 'Shell' for details).
      --
      -- Here is example of spawning single actor on remote node. To
      -- be able to create 'Closure' to execute actor on remote node
      -- we need to make it \"remotable\". For details of 'remotable'
      -- semantics refer to distributed-process documentation,. (This
      -- could change in future version of @distributed-process@ when
      -- it start use StaticPointers language extension)
      --
      -- > someActor :: Actor Int Int
      -- > someActor = actor $ \i -> ...
      -- >
      -- > remotable [ 'someActor ]
      --
      -- Finally we start actor and allocate 3 nodes to it:
      --
      -- > do a <- startActor (N 3) (return $(mkStaticClosure 'someActor))
      -- >    ...
      --
      -- In next example we start group of actors, use half of
      -- available nodes and local node in addition to that. These
      -- nodes will be evenly divided between 4 actors:
      --
      -- > do a <- startGroup (Frac 0.5) (NWorkers 4) $ do
      -- >           useLocal
      -- >           return $(mkStaticClosure 'someActor)
      -- >    ...
      --
      -- All other start* functions share same pattern and could be
      -- used in similar manner.
    , startActor
    , startGroup
      -- , startGroupN
    , startCollector
    , startCollectorTree
    , startCollectorTreeGroup
      -- ** Shell
    , Shell
    , Val
    , Grp
    , Scatter
      
      -- * Connecting actors

      -- | Each actor must be connected to exactly one destination and
      -- consequently could only receive input from a single
      -- source. Trying to connect an actor twice will result in
      -- a runtime error.
    , sendParam
    , broadcast
    , distributeWork
    , connect
    , FileChan
    , createFileChan
      -- * Promises
    , Promise
    , delay
    , await
    , Group
    , delayGroup
    , gather
      -- * Reexports
    , MonadIO(..)
    , remotable
    , mkStaticClosure
    ) where

import Control.Monad.IO.Class
import Control.Distributed.Process.Closure (mkStaticClosure,remotable)

import DNA.DSL
import DNA.Types
import DNA.Run
import DNA.Logging
