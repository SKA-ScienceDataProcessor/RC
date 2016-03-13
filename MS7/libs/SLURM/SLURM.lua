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
function SLURM.nodes_list()
    nodes = {}
    SLURM_nodes = os.getenv("SLURM_NODELIST")
    print("nodes: "..SLURN_nodes)
    for n in SLURM_nodes:gmatch("[^ ]+") do
	print("node "..n)
        nodes.insert(n)
    end
    return nodes
end

return SLURM