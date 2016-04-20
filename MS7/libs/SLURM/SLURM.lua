-- slurm.lua
--
-- SLURM information extraction, abstracted away.
--
-- Copyright (C) Braam Research, 2016

local SLURM = {}

-- return number of nodes allocated by SLURM.
SLURM.num_nodes = terra ()
    return tonumber(os.getenv("SLURM_NNODES"))
end

-- return list of nodes allocated by SLURM as Lua table.
