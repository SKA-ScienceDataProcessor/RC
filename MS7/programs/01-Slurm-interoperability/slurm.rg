-- slurm.rg
--
-- SLURM interoperability simple example.
--
-- Copyright (C) Braam Research, 2016

local SLURM = require("SLURM")

import "regent"

local c = regentlib.c

-- Main entry point.
task main()
    c.printf("nodes count %d.\n", SLURM.num_nodes())
    var nodes = SLURM.nodes_list()
    for n in nodes do
	c.printf("node: '%s'\n", n);
    end
end

regentlib.start(main)
