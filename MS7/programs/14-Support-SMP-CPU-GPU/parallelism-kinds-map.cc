// Resource management demo program.
//
// Copyright (C) 2016 Braam Research, LLC.

#include <stdarg.h>
#include <cstdio>
#include <cassert>
#include <cstdlib>
#include <stdint.h>

#include "legion.h"
#include "realm.h"
#include "legion_types.h"
#include "default_mapper.h"
#include "id.h"

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


/*
 * One of the primary goals of Legion is
 * to make it easy to remap applications
 * onto different hardware.  Up to this point
 * all of our applications have been mapped
 * by the DefaultMapper that we provide.  The
 * DefaultMapper class provides heuristics for
 * performing mappings that are good but often
 * not optimal for a specific application or
 * architecture.   By creating a custom mapper
 * programmers can make application- or
 * architecture-specific mapping decisions.
 * Furthermore, many custom mappers can be
 * used to map the same application onto many
 * different architectures without having to
 * modify the primary application code.
 *
 * A common concern when mapping applications
 * onto a target machine is whether or not
 * mapping impacts correctness.  In Legion
 * all mapping decisions are orthogonal to
 * correctness.  In cases when correctness may
 * be impacted by a mapping decision (e.g. a
 * mapper maps a physical region for a task
 * into a memory not visible from the target
 * processor), then the Legion runtime will
 * notify the mapper that it tried to perform
 * an illegal mapping and allow it to retry.
 *
 * To introduce how to write a custom mapper
 * we'll implement an adversarial mapper 
 * that makes random mapping decisions
 * designed to stress the Legion runtime. 
 * We'll report the chosen mapping decisions
 * to show that Legion computes the correct
 * answer regardless of the mapping.
 */

// Mappers are classes that implement the
// mapping interface declared in legion.h.
// Legion provides a default impelmentation
// of this interface defined by the
// DefaultMapper class.  Programmers can
// implement custom mappers either by 
// extending the DefaultMapper class
// or by declaring their own class which
// implements the mapping interface from
// scratch.  Here we will extend the
// DefaultMapper which means that we only
// need to override the methods that we
// want to in order to change our mapping.
// In this example, we'll override four
// of the mapping calls to illustrate
// how they work.
class ParallelismKindsMapper : public DefaultMapper {
public:
  ParallelismKindsMapper(MapperRuntime *rt, Machine machine, Processor local, 
                         const char *mapper_name = NULL);
public:
  virtual Processor default_policy_select_initial_processor(
                                    MapperContext ctx, const Task &task);
private:
  // compute CPU index for a task from task's ID.
  Processor task_cpu(const Task& task, Processor old_proc) {
    assert(false);
    return old_proc;
  }
};

void mapper_registration(Machine machine, HighLevelRuntime *runtime, const std::set<Processor> &local_procs)
{
  for (std::set<Processor>::const_iterator it = local_procs.begin();
        it != local_procs.end(); it++)
  {
    runtime->replace_default_mapper(
        new ParallelismKindsMapper(runtime->get_mapper_runtime(), machine, *it, "Bandwidth Latency Mapper"), *it);
  }
}

// Here is the constructor for our adversial mapper.
// We'll use the constructor to illustrate how mappers can
// get access to information regarding the current machine.
ParallelismKindsMapper::ParallelismKindsMapper(MapperRuntime *rt, Machine machine, Processor local, 
                         const char *mapper_name)
  : DefaultMapper(rt, machine, local, mapper_name) // pass arguments through to DefaultMapper
{
}

Processor ParallelismKindsMapper::default_policy_select_initial_processor(
                                    MapperContext ctx, const Task &task)
{
  return task_cpu(task, local_proc);
}
