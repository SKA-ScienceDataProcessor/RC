-- Static scheduling demo.
--
-- Copyright (C) 2016 Braam Research, LLC.

import "regent"

local support
do
    terralib.linklibrary(os.getenv("SDP_SUPPORT_LIBRARY"))
    support = terralib.includec("static-scheduling-support.h",{"-I."})
end

task level_2_task(cpu : int, level_1_index : int, level_2_index : int)
    support.node_log("Bottom level task %d/%d/%d, SLURM node %d, SLURM task %d", cpu, level_1_index, level_2_index, support.current_slurm_node(), support.current_slurm_task());
end

task level_1_task(cpu : int, level_1_index : int)
    support.node_log("Level 1 task %d/%d, SLURM node %d, SLURM task %d", cpu, level_1_index, support.current_slurm_node(), support.current_slurm_task());
    __demand(__parallel)
    for j=0, 5 do
        level_2_task(cpu, i, j)
    end
end

task level_0_task(cpu : int)
    support.node_log("Root task %d. SLURM node %d, SLURM task %d", cpu, support.current_slurm_node(), support.current_slurm_task())
    for level_1_index=0, 2 do
        level_1_task(cpu, level_1_index)
    end
end

task start_task()
    support.node_log("starting everything");
    -- ask runtime to not wait while we are working.
    __demand(__parallel)
    for cpu=0, 4 do
        level_0_task(cpu)
    end
end

-- Register mappers and setup support vars.
support.register_mappers()

-- start main work.
regentlib.start(start_task)
