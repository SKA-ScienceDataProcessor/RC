-- Different parallelization opportunities demo.
--
-- Basically, daxpy.rg, executed thrice - local vectorization, local parallelism with Legion, distributed parallelism with Legion.
--
-- Copyright (C) 2016 Braam Research, LLC.

import "regent"

local
  lib = terralib.

task top_level()
end

regentlib.start(top_level)
