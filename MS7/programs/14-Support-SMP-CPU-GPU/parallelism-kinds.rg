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
end

regentlib.start(top_level)
