-- slurm.rg
--
-- SLURM interoperability simple example.
--
-- Copyright (C) Braam Research, 2016

import "regent"

local c = regentlib.c;

task main()
    c.printf("in main\n")

end
regentlib.start(main)
