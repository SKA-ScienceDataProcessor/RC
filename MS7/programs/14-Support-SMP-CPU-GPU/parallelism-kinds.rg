-- Different parallelization opportunities demo.
--
-- Basically, daxpy.rg, executed thrice - local vectorization, local parallelism with Legion, distributed parallelism with Legion.
--
-- Copyright (C) 2016 Braam Research, LLC.

import "regent"

local support
do
    terralib.linklibrary(os.getenv("SDP_SUPPORT_LIBRARY"))
    support = terralib.includec("parallelism-kinds.h",{"-I."})
end

__demand(__cuda(__unroll(10)))
task subtask(ra : region(ispace(int1d), float),
             rb : region(ispace(int1d), float),
             rc : region(ispace(int1d), float))
where writes(rc), reads(ra, rb) do
  __demand(__vectorize)
  for i in ra do
    rc[i] = ra[i]*rb[i]
  end
  return 0
end

terra wait_for(x : int) end -- Used to force futures to complete.

task top_level()
    var n = 100000000 -- 100M floats - should use about 1..10ms of time, enough to spot a difference
    var indices = ispace(int1d, n)
    var ra = region(indices, float)
    var rb = region(indices, float)
    var rc = region(indices, float)

    var np = 100 -- number of subregions for data parallelism
    var colors = ispace(int1d, np)
    var pa = partition(equal, ra, colors)
    var pb = partition(equal, rb, colors)
    var pc = partition(equal, rc, colors)

    fill(ra, 1.0)
    fill(rb, 2.0)
    -- baseline: regular loop.
    var starttime = support.current_time_microseconds()
    __forbid(__vectorize)
    for i in indices do
        rc[i] = ra[i]*rb[i]
    end
    var endtime = support.current_time_microseconds()
    support.node_log("baseline: %ld microseconds", endtime-starttime)

    -- utilize vectorization.
    starttime = support.current_time_microseconds()
    __demand(__vectorize)
    for i in indices do
      rc[i] = ra[i]*rb[i]
    end
    endtime = support.current_time_microseconds()
    support.node_log("SIMD: %ld microseconds", endtime-starttime)

    -- utilize SMP parallelization.
    starttime = support.current_time_microseconds()
    do
      var _ = 0
      __demand(__parallel)
      for i = 0, np do
        _ += subtask(pa[i], pb[i], pc[i])
      end
      wait_for(_)
    end
    endtime = support.current_time_microseconds()
    support.node_log("parallel: %ld microseconds", endtime-starttime)

    starttime = support.current_time_microseconds()
    do
      var _ = 0
      __demand(__spmd)
      do
        for i = 0, np do
          _ += subtask(pa[i], pb[i], pc[i])
        end
      end
      wait_for(_)
    end
    endtime = support.current_time_microseconds()
    support.node_log("SPMD: %ld microseconds", endtime-starttime)

    -- utilize GPU.
    starttime = support.current_time_microseconds()
    wait_for(subtask(ra, rb, rc))
    endtime = support.current_time_microseconds()
    support.node_log("GPU: %ld microseconds", endtime-starttime)
end

regentlib.start(top_level)
