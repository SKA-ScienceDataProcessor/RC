// Static scheduling demo - mapper part.
//
// Copyright (C) Braam Research, 2016.

#include <stdarg.h>

#include "legion.h"
#include "realm.h"
#include "legion_types.h"
#include "default_mapper.h"
#include "id.h"

#include "bandwidth-latency-support.h"

using namespace LegionRuntime::HighLevel;
using namespace LegionRuntime::Accessor;

LegionRuntime::Logger::Category log_logging("logging");

void node_log(char*fmt, ...) {
    va_list vl;
    char buf[1024];
    va_start(vl, fmt);
    vsprintf(buf, fmt, vl);
    log_logging.print(buf);
} /* node_log */


class BandwidthLatencyMapper : public DefaultMapper {
public:
  BandwidthLatencyMapper(Machine machine, 
      HighLevelRuntime *rt, Processor local) : DefaultMapper(machine, rt, local)
  {
fprintf(stderr, "logging level %d\n", log_logging.get_level());
  }
public:
  virtual void select_task_options(Task *task);
//  virtual void slice_domain(const Task *task, const Domain &domain,
//                            std::vector<DomainSplit> &slices);
//  virtual bool map_task(Task *task);
//  virtual void notify_mapping_result(const Mappable *mappable);
private:
  bool equal_to(std::string& a, char*b) {
    return a == std::string(b);
  }
  // compute CPU index for a task from task's ID.
  Processor task_cpu(Task*task, Processor old_proc) {
    std::set<Processor> all_procs;
    machine.get_all_processors(all_procs);
    std::string taskname(task->variants->name);
    if (equal_to(taskname, "receive_change")) {
      for (std::set<Processor>::const_iterator it = all_procs.begin();
            it != all_procs.end(); it++) {
        log_logging.print("considering cpu %llx.\n",it->id);
        Realm::ID cpuid(*it);
        if (it->kind() == Processor::LOC_PROC && cpuid.node() == 1) { // selecting second node.
            return (*it);
        }
      }
    }
    return old_proc;
  }
};

void BandwidthLatencyMapper::select_task_options(Task *task)
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
    runtime->replace_default_mapper(new BandwidthLatencyMapper(machine, runtime, *it), *it);
  }
}


void register_mappers()
{
  HighLevelRuntime::set_registration_callback(create_mappers);
}
