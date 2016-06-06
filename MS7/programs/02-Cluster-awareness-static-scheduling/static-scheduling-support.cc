// Static scheduling demo - mapper part.
//
// Copyright (C) Braam Research, 2016.

#include "legion.h"
#include "realm.h"
#include "legion_types.h"
#include "default_mapper.h"
#include "id.h"

#include <slurm/slurm.h>

using namespace LegionRuntime::HighLevel;
using namespace LegionRuntime::Accessor;

LegionRuntime::Logger::Category log_logging("logging");



class StaticSchedulingMapper : public DefaultMapper {
public:
  SenderReceiverMapper(Machine machine, 
      HighLevelRuntime *rt, Processor local);
public:
  virtual void select_task_options(Task *task);
//  virtual void slice_domain(const Task *task, const Domain &domain,
//                            std::vector<DomainSplit> &slices);
//  virtual bool map_task(Task *task);
  virtual void notify_mapping_result(const Mappable *mappable);
private:
  bool equal_to(std::string& a, char*b) {
    return a == std::string(b);
  }
  // compute CPU index for a task from task's ID.
  Processor task_cpu(Task*task, Processor old_proc) {
    std::set<Processor> all_procs;
    machine.get_all_processors(all_procs);
    std::string taskname(task->variants->name);
    int cpu = -1;
    int i = -1;
    int j = -1;
    int* args = (int*)task->args;
    if (task->arglen >= sizeof(int)) {
        cpu = args[0];
    }
    if (task -> arglen >= sizeof(int)*2) {
        i = args[1];
    }
    if (task -> arglen >= sizeof(int)*3) {
        i = args[2];
    }
    if (equal_to(taskname, "level_0_task")) {
    }
    return old_proc;
  }
};

void StaticSchedulingMapper::select_task_options(Task *task)
{
  task->inline_task = false;
  task->spawn_task = false;
  task->map_locally = false;
  task->profile_task = false;
  task->task_priority = 0;
  task->target_proc = task_cpu(task, task->target_proc);
}


static void create_mappers(Machine machine, HighLevelRuntime *runtime, const std::set<Processor> &local_procs)
{
  for (std::set<Processor>::const_iterator it = local_procs.begin();
        it != local_procs.end(); it++)
  {
    runtime->replace_default_mapper(new StaticSchedulingMapper(machine, runtime, *it), *it);
  }
}


void register_mappers()
{
  HighLevelRuntime::set_registration_callback(create_mappers);
}
