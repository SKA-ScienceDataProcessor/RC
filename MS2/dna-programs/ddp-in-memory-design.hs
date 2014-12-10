-- { language features - hopefully few } --
--

import DNA

-- DNA.startCH
-- use the SLURM_NODELIST environment variable to determine the hosts
-- I put sample C-code how to deal with Slurms weird nodelist format (conditional compile)
-- use the --min_port as the minimum port
-- use the --procs_per_node argument to start CH nodes on node:port
-- if this is the first process, it run the argument given (master_actor), 
-- others run a slave shell CH program which waits for a "start" message
-- this may create remote processes or not (in the latter case, DNA.start may have to inform
-- all nodes of their input / output processes

DNA.startCH master_actor

master_actor:: [String] -> IO (Int) -- might be a Process(Int), the return is the error code

master_actor args = do
	-- return lists of processIds
	computeProcs = DNA.schedule(DNA.nodeCount - 1) -- returns the nodes on which to spawn
	collectorProc = DNA.schedule(1); 
	-- prepare the computeProcs to run the computeActor,
	-- [ ] = an empty input list
	-- collectorProc =  output process list for the computeActor
	-- fileName, itemCount = input variables 
	-- a mechanism for args could be ( [Integer], [Double], [Ptr], [String] ) 
	-- True = means ignore a crash
	DNA.fork computeProcs computeActor [] collectorProc fileName True
	DNA.fork collectorProc collectorActor computeProcs [DNA.myPid] "" False
	-- the DF graph is complete
	DNA.start
	-- DNA.wait can receive failure messages.  It knows whom to notify from previous fork calls
	result = DNA.wait(collectorProc)

computeActor = do 
	DNA.waitForStart
	(offset, count) = DNA.slice DNA.doublesInFile fileName DNA.myProcCount DNA.myRank  
	fChPid = DNA.forkLocal DNA.chFileReadDoubles fileName offset count
     	compChPid = DNA.forkLocal DNA.chCompDoubles func offset count
     	(A1, A2) = DNA.wait fChPid compChPid
     	res = dpLocal A1 A2
     	DNA.join DNA.output res  -- sends res message to outputis, notifies parent of clean exit

collectorActor = do
	DNA.waitForStart
     	[nodeDP ] = DNA.wait input
     	res = sum [nodeDP] 
     	DNA.join res parent  —- default case, joint a parent and exit

DNA.monitor_actor = do   -- internals, not user coded
     	forever
          wait(failurePID)
          send(failurePid, owner(failurePid))

DNA.chFileReadDoubles
     …. 
     join(arrayPtr, parent)

DNA.chCompDoubles

