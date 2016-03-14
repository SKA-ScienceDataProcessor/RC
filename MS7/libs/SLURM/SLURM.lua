-- slurm.lua
--
-- SLURM information extraction, abstracted away.
--
-- Copyright (C) Braam Research, 2016

local SLURM = {}

-- return number of nodes allocated by SLURM.
function SLURM.num_nodes()
    return tonumber(os.getenv("SLURM_NNODES"))
end

-- return list of nodes allocated by SLURM as Lua table.
SLURM.nodes_list = {}
local SLURM_nodes = os.getenv("SLURM_NODELIST")
print("nodes: "..SLURM_nodes)

for n in SLURM_nodes:gmatch("[^ ]+") do
    print("node "..n)
    table.insert(SLURM.nodes_list, n)
end

return SLURM