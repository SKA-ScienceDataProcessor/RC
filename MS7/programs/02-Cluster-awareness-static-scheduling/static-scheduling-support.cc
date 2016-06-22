// Static scheduling demo - mapper part.
//
// Copyright (C) Braam Research, 2016.

#include <stdarg.h>

#include "legion.h"
#include "realm.h"
#include "legion_types.h"
#include "default_mapper.h"
#include "id.h"

#include "static-scheduling-support.h"

using namespace Realm;
using namespace Legion;
using namespace Legion::Mapping;
using namespace LegionRuntime::Accessor;


LegionRuntime::Logger::Category log_logging("logging");

void node_log(char*fmt, ...) {
    va_list vl;
    char buf[1024];
    va_start(vl, fmt);
    vsprintf(buf, fmt, vl);
    //log_logging.print("%s", buf);
    printf("%s\n", buf); fflush(stdout);
} /* node_log */


class StaticSchedulingMapper : public DefaultMapper {
public:
  StaticSchedulingMapper(MapperRuntime *rt, Machine machine, Processor local, 
                         const char *mapper_name = NULL)
    : DefaultMapper(rt, machine, local, mapper_name)
  {
  }

public:
  virtual Processor default_policy_select_initial_processor(
                                    MapperContext ctx, const Task &task);
//  virtual void slice_domain(const Task *task, const Domain &domain,
//                            std::vector<DomainSplit> &slices);
//  virtual bool map_task(Task *task);
private:
  bool equal_to(std::string& a, char*b) {
    return a == std::string(b);
  }
  // compute CPU index for a task from task's ID.
  Processor task_cpu(const Task *task, Processor old_proc) {
    std::set<Processor> all_procs;
    machine.get_all_processors(all_procs);
    std::string taskname(task->get_task_name());
    int cpu = -1;
    int i = -1;
    int j = -1;

    int* args = (int*)task->args.base();
    unsigned arglen = task->args.size();

    unsigned node_to_search = -1;

    node_log("task name %s, arglen %d", taskname.c_str(), task->arglen);
    if (arglen >= sizeof(int)) {
        cpu = args[0];
    }
    if (arglen >= sizeof(int)*2) {
        i = args[1];
    }
    if (arglen >= sizeof(int)*3) {
        j = args[2];
    }
    if (equal_to(taskname, "level_0_task")) {
        assert(cpu >= 0 && cpu < 4);
        node_to_search = cpu;
    }
    else if (equal_to(taskname, "level_1_task")) {
        assert(i >= 0 && i < 2);
        assert(cpu >= 0 && cpu < 4);
        node_to_search = 4+cpu+i*4;
    }
    else if (equal_to(taskname, "level_2_task")) {
        assert(i >= 0 && i < 2);
        assert(cpu >= 0 && cpu < 4);
        assert(j >= 0 && j < 5);
        node_to_search = 12+cpu+j*4;
    }
    for (std::set<Processor>::const_iterator it = all_procs.begin();
            it != all_procs.end(); it++) {
        Realm::ID cpuid(*it);
        if (it->kind() == Processor::LOC_PROC && cpuid.node() == node_to_search) { // selecting node we need.
            return (*it);
        }
    }

    return old_proc;
  }
};

Processor StaticSchedulingMapper::default_policy_select_initial_processor(
                                    MapperContext ctx, const Task &task)
{
  return task_cpu(&task, local_proc);
}


static void create_mappers(Machine machine, HighLevelRuntime *runtime, const std::set<Processor> &local_procs)
{
  for (std::set<Processor>::const_iterator it = local_procs.begin();
        it != local_procs.end(); it++)
  {
    runtime->replace_default_mapper(new StaticSchedulingMapper(runtime->get_mapper_runtime(), machine, *it, "Bandwidth Latency Mapper"), *it);
  }
}



void register_mappers()
{
  HighLevelRuntime::set_registration_callback(create_mappers);
}

/* return current SLURM task index */
int current_slurm_task() {
    return atoi(getenv("SLURM_TASK_ID"));
} /* current_slurm_task */

/* return index of current SLURM node */
int current_slurm_node() {
    return atoi(getenv("SLURM_NODEID"));
} /* current_slurm_task */

char* current_slurm_node_name() {
    return getenv("SLURM_NODENAME");
} /* current_slurm_node_name */
