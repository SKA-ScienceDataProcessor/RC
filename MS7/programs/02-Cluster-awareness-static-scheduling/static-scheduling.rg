-- Static scheduling demo.
--
-- Copyright (C) 2016 Braam Research, LLC.

import "regent"

local support = terralib.includec("static-scheduling-support.h",{})

task bottom_level(i : int, j : int)
    support.node_log("Bottom level task %d:%d, SLURM node %d", i, j, support.current_slurm_node());
end

task second_level(i : int)
    support.node_log("Second level task %d, SLURM node %d",i, support.current_slurm_node());
    __demand(__parallel)
    for j=0, support.branching_factor() do
        bottom_level(i, j)
    end
end

task top_level()
    support.node_log("Root task, SLURM node %d",support.current_slurm_node());
    support.node_log("           total nodes %d, branching factor %d", support.num_slurm_nodes(), support.branching_factor());
    __demand(__parallel)
    for i=0, support.branching_factor() do
        second_level(i)
    end
end

-- Register mappers and setup support vars.
support.register_mappers()

-- start main work.