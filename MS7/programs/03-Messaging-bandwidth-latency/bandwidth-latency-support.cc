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
    log_logging.print("%s", buf);
} /* node_log */


class BandwidthLatencyMapper : public DefaultMapper {
public:
  BandwidthLatencyMapper(MapperRuntime *rt, Machine machine, Processor local, 
                         const char *mapper_name = NULL)
    : DefaultMapper(rt, machine, local, mapper_name)
  {
    fprintf(stderr, "logging level %d\n", log_logging.get_level());
  }
public:
  virtual Processor default_policy_select_initial_processor(
                                    MapperContext ctx, const Task &task);
private:
  bool equal_to(std::string& a, char*b) {
    return a == std::string(b);
  }
  // compute CPU index for a task from task's ID.
  Processor task_cpu(const Task &task, Processor old_proc) {
    std::set<Processor> all_procs;
    machine.get_all_processors(all_procs);
    std::string taskname(task.get_task_name());
    if (taskname == "receive_change") {
      for (std::set<Processor>::const_iterator it = all_procs.begin();
            it != all_procs.end(); it++) {
        log_logging.print("considering cpu %llx.\n",it->id);
        Realm::ID cpuid(*it);
        if (cpuid.is_processor() && cpuid.proc.owner_node == 1) { // selecting second node.
            return (*it);
        }
      }
    }
    return old_proc;
  }
};

Processor BandwidthLatencyMapper::default_policy_select_initial_processor(
                                    MapperContext ctx, const Task &task)
{
  return task_cpu(task, local_proc);
}

static void create_mappers(Machine machine, HighLevelRuntime *runtime, const std::set<Processor> &local_procs)
{
  for (std::set<Processor>::const_iterator it = local_procs.begin();
        it != local_procs.end(); it++)
  {
    runtime->replace_default_mapper(new BandwidthLatencyMapper(runtime->get_mapper_runtime(), machine, *it, "Bandwidth Latency Mapper"), *it);
  }
}


void register_mappers()
{
  HighLevelRuntime::set_registration_callback(create_mappers);
}

/* return (runtime instance initialization) relative time in microseconds (good enough for our benchmark) */
long current_time_microseconds() {
  return Clock::current_time_in_microseconds();
} /* current_time_microseconds */

