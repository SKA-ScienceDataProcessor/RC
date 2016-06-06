require "getenv"

terralib.includepath = terralib.includepath..";/usr/include/slurm"
terralib.linklibrary("/usr/lib/x86_64-linux-gnu/libslurm.so")

local c = terralib.includecstring [[
#include <stdlib.h>
#include <slurm.h>
]]

local slurm_env_str = {
    "SLURM_CHECKPOINT_IMAGE_DIR"
  , "SLURM_CLUSTER_NAME"
  , "SLURM_JOB_NAME"
  , "SLURM_JOB_PARTITION"
  , "SLURMD_NODENAME"
  , "SLURM_SUBMIT_DIR"
  , "SLURM_SUBMIT_HOST"
  -- these needs further parsing
  , "SLURM_DISTRIBUTION"
  , "SLURM_GTIDS"
  , "SLURM_JOB_DEPENDENCY"
  , "SLURM_JOB_NODELIST"
  , "SLURM_NODE_ALIASES"
  , "SLURM_PROFILE"
  , "SLURM_TOPOLOGY_ADDR"
  , "SLURM_TOPOLOGY_ADDR_PATTERN"
  }

local slurm_env_int = {
    "SLURM_ARRAY_TASK_ID"
  , "SLURM_ARRAY_TASK_MAX"
  , "SLURM_ARRAY_TASK_MIN"
  , "SLURM_ARRAY_TASK_STEP"
  , "SLURM_ARRAY_JOB_ID"
  , "SLURM_CPUS_ON_NODE"
  , "SLURM_CPUS_PER_TASK"
  , "SLURM_JOB_ID"
  , "SLURM_JOB_CPUS_PER_NODE"
  , "SLURM_JOB_NUM_NODES"
  , "SLURM_LOCALID"
  , "SLURM_NODEID"
  , "SLURM_NTASKS"
  , "SLURM_NTASKS_PER_CORE"
  , "SLURM_NTASKS_PER_NODE"
  , "SLURM_NTASKS_PER_SOCKET"
  , "SLURM_PRIO_PROCESS"
  , "SLURM_PROCID"
  , "SLURM_RESTART_COUNT"
  , "SLURM_TASKS_PER_NODE"
  , "SLURM_TASK_PID"
  }

slurm = {}

function make_funs(env_table, rettype, fun)
  for _, e in pairs(env_table) do
    local j
    _, j = string.find(e, "_")
    local fname = string.lower(string.sub(e, j+1))
    slurm[fname] = terra() : rettype return fun(e) end
  end
end

make_funs(slurm_env_str, rawstring, get_env)
make_funs(slurm_env_int, int, get_env_int)

-- host list handling
terra slurm_job_nodelist() : c.hostlist_t return c.slurm_hostlist_create(slurm.job_nodelist()) end
terra hostlist_shift(hl : c.hostlist_t) : rawstring return c.slurm_hostlist_shift(hl) end
terra sfree(s : rawstring) : {} c.free(s) end
terra hostlist_destroy(hl : c.hostlist_t) : {} c.slurm_hostlist_destroy(hl) end
