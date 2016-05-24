require "getenv"

terralib.includepath = terralib.includepath..";/usr/include/slurm"
terralib.linklibrary("/usr/lib/x86_64-linux-gnu/libslurm.so")

local c = terralib.includecstring [[
#include <stdlib.h>
#include <slurm.h>
]]

terra slurm_array_task_id() : int return get_env_int("SLURM_ARRAY_TASK_ID") end
terra slurm_array_task_max() : int return get_env_int("SLURM_ARRAY_TASK_MAX") end
terra slurm_array_task_min() : int return get_env_int("SLURM_ARRAY_TASK_MIN") end
terra slurm_array_task_step() : int return get_env_int("SLURM_ARRAY_TASK_STEP") end
terra slurm_array_job_id() : int return get_env_int("SLURM_ARRAY_JOB_ID") end
terra slurm_cpus_on_node() : int return get_env_int("SLURM_CPUS_ON_NODE") end
terra slurm_cpus_per_task() : int return get_env_int("SLURM_CPUS_PER_TASK") end
terra slurm_job_id() : int return get_env_int("SLURM_JOB_ID") end
terra slurm_job_cpus_per_node() : int return get_env_int("SLURM_JOB_CPUS_PER_NODE") end
terra slurm_job_num_nodes() : int return get_env_int("SLURM_JOB_NUM_NODES") end
terra slurm_localid() : int return get_env_int("SLURM_LOCALID") end
terra slurm_nodeid() : int return get_env_int("SLURM_NODEID") end
terra slurm_ntasks() : int return get_env_int("SLURM_NTASKS") end
terra slurm_ntasks_per_core() : int return get_env_int("SLURM_NTASKS_PER_CORE") end
terra slurm_ntasks_per_node() : int return get_env_int("SLURM_NTASKS_PER_NODE") end
terra slurm_ntasks_per_socket() : int return get_env_int("SLURM_NTASKS_PER_SOCKET") end
terra slurm_prio_process() : int return get_env_int("SLURM_PRIO_PROCESS") end
terra slurm_procid() : int return get_env_int("SLURM_PROCID") end
terra slurm_restart_count() : int return get_env_int("SLURM_RESTART_COUNT") end
terra slurm_tasks_per_node() : int return get_env_int("SLURM_TASKS_PER_NODE") end
terra slurm_task_pid() : int return get_env_int("SLURM_TASK_PID") end

terra slurm_checkpoint_image_dir() : rawstring return get_env("SLURM_CHECKPOINT_IMAGE_DIR") end
terra slurm_cluster_name() : rawstring return get_env("SLURM_CLUSTER_NAME") end
terra slurm_job_name() : rawstring return get_env("SLURM_JOB_NAME") end
terra slurm_job_partition() : rawstring return get_env("SLURM_JOB_PARTITION") end
terra slurmd_nodename() : rawstring return get_env("SLURMD_NODENAME") end
terra slurm_submit_dir() : rawstring return get_env("SLURM_SUBMIT_DIR") end
terra slurm_submit_host() : rawstring return get_env("SLURM_SUBMIT_HOST") end

-- these needs further parsing
terra slurm_distribution() : rawstring return get_env("SLURM_DISTRIBUTION") end
terra slurm_gtids() : rawstring return get_env("SLURM_GTIDS") end
terra slurm_job_dependency() : rawstring return get_env("SLURM_JOB_DEPENDENCY") end
terra slurm_job_nodelist_() : rawstring return get_env("SLURM_JOB_NODELIST") end
terra slurm_node_aliases() : rawstring return get_env("SLURM_NODE_ALIASES") end
terra slurm_profile() : rawstring return get_env("SLURM_PROFILE") end
terra slurm_topology_addr() : rawstring return get_env("SLURM_TOPOLOGY_ADDR") end
terra slurm_topology_addr_pattern() : rawstring return get_env("SLURM_TOPOLOGY_ADDR_PATTERN") end

-- host list handling
terra slurm_job_nodelist() : c.hostlist_t return c.slurm_hostlist_create(slurm_job_nodelist_()) end
terra hostlist_shift(hl : c.hostlist_t) : rawstring return c.slurm_hostlist_shift(hl) end
terra sfree(s : rawstring) : {} c.free(s) end
terra hostlist_destroy(hl : c.hostlist_t) : {} c.slurm_hostlist_destroy(hl) end
