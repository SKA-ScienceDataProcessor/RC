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

task top_level()
    var n = 100000000 -- 100M floats - should use about 1..10ms of time, enough to spot a difference
    var indices = ispace(int1d, n)
    var ra = region(indices, float)
    var rb = region(indices, float)
    var rc = region(indices, float)
    fill(ra, 1.0)
    fill(rb, 2.0)
    -- baseline: regular loop.
    var starttime = support.current_time_milliseconds()
    for i in indices do
        rc(i) = ra*rb
    end
    var endtime = support.current_time_milliseconds()
    support.node_log("baseline: %ld milliseconds", endtime-starttime)

    -- utilize vectorization.
    starttime = support.current_time_milliseconds()
    __demand(__vectorize)
    for i in indices do
      rc(i) = ra*rb
    end
    var endtime = support.current_time_milliseconds()
    support.node_log("SIMD: %ld milliseconds", endtime-starttime)

    -- utilize SMP parallelization.
    starttime = support.current_time_milliseconds()
    __demand(__SPMD)
    for i in indices do
      rc(i) = ra*rb
    end
    endtime = support.current_time_milliseconds()
    support.node_log("SPMD: %ld milliseconds", endtime-starttime)

    -- utilize GPU.
    starttime = support.current_time_milliseconds()
    __demand(__gpu)
    for i in indices do
      rc(i) = ra*rb
    end
    endtime = support.current_time_milliseconds()
    support.node_log("GPU: %ld milliseconds", endtime-starttime)

end

regentlib.start(top_level)
