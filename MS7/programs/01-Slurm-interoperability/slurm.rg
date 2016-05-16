-- These are my ubuntu 15.10 locations. They may be different on RHEL.
terralib.includepath = terralib.includepath..";/usr/include/slurm"
terralib.linklibrary("/usr/lib/x86_64-linux-gnu/libslurm.so")

local c = terralib.includecstring [[
#include <stdlib.h>
#include <slurm.h>
]]

terra main()
  -- This is a real code to use on cluster:
  -- var nlists = c.getenv("SLURM_NODELIST")
  -- c.printf("NODELIST is: %s\n", nlists)
  -- var hl = c.slurm_hostlist_create(nlists)

  -- This is desktop test code
  var hl = c.slurm_hostlist_create("test[1-9]")

  var hname = c.slurm_hostlist_shift(hl)
  while hname ~= [rawstring](0) do
    c.printf("%s\n", hname)
    c.free(hname)
    hname = c.slurm_hostlist_shift(hl)
  end
end

import "regent"

task toplevel()
  main()
end

regentlib.start(toplevel)
