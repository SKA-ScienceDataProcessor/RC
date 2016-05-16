-- These are my ubuntu 15.10 locations. They may be different on RHEL.
terralib.includepath = terralib.includepath..";/usr/include/slurm"
terralib.linklibrary("/usr/lib/x86_64-linux-gnu/libslurm.so")

local c = terralib.includecstring [[
#include <stdlib.h>
#include <slurm.h>
]]

terra get_env_int(ev: rawstring)
  var val = c.getenv(ev)
  if val ~= [rawstring](0) then
    return c.atoi(val)
  end
  return 0
end

terra main()

  c.printf("Allocated nodes: %s\n", c.getenv("SLURMD_NODENAME"))
  c.printf("I'm %d, and I have %d CPUs\n", get_env_int("SLURM_NODEID"), get_env_int("SLURM_CPUS_ON_NODE"))
  
  var nlists = c.getenv("SLURM_NODELIST")
  c.printf("NODELIST is: %s\n", nlists)
  var hl = c.slurm_hostlist_create(nlists)

  -- This is desktop test code
  -- var hl = c.slurm_hostlist_create("test[1-9]")

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
