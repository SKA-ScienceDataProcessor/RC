require "slurm_lib"

local c = terralib.includecstring [[
#include <stdio.h>
]]

terra main()

  c.printf("Allocated nodes: %s\n", slurm.nodename())
  c.printf("I'm %d, and I have %d CPUs\n", slurm.nodeid(), slurm.cpus_on_node())
  
  var hl = slurm_job_nodelist()
  var hname = hostlist_shift(hl)
  while hname ~= [rawstring](0) do
    c.printf("%s\n", hname)
    sfree(hname)
    hname = hostlist_shift(hl)
  end
  hostlist_destroy(hl)

end

import "regent"

task toplevel()
  main()
end

regentlib.start(toplevel)
