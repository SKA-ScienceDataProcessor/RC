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
  // compute CPU index for a task from task's ID.
  Processor task_cpu(Task*task, Processor old_proc) {
    std::set<Processor> all_procs;
    machine.get_all_processors(all_procs);
    unsigned node = 0, cpu = 0;
    unsigned max_node = 0;
    for (std::set<Processor>::const_iterator it = all_procs.begin();
            it != all_procs.end(); it++) {
      Realm::ID cpuid(*it);
      if (cpuid.node() > max_node)
        max_node = cpuid.node();
    }
    switch (task->task_id) {
      case SENDER1_TASK_ID: node = 0; break;
      case SENDER2_TASK_ID: node = 1; break;
      // cpu is the same for FILL tasks as for SENDER tasks.
      case FILL1_TASK_ID: node = 0; break;
      case FILL2_TASK_ID: node = 1; break;
      case RECEIVER1_TASK_ID: node = 2; break;
      case RECEIVER2_TASK_ID: node = 2; cpu = 1; break;
      default:
        log_logging.print("Unknown task id %x.\n",task->task_id);
        return old_proc;
    }
    if (node > max_node) {
      log_logging.print("Too high node index %d for task %x.\n",node,task->task_id);
      node = max_node;
    }
    log_logging.print("task %x has node %d, cpu index %d.\n",task->task_id, node, cpu);
    for (std::set<Processor>::const_iterator it = all_procs.begin();
            it != all_procs.end(); it++) {
      log_logging.print("considering cpu %llx.\n",it->id);
      Realm::ID cpuid(*it);
      if (it->kind() == Processor::LOC_PROC && cpuid.node() == node) { // our node - look for CPU.
        if (cpu == 0) {
          log_logging.print("assigned cpu %llx.\n",it->id);
          return (*it);
        }
        cpu--;
      }
    }
    printf("no processor for node %d, cpu %d.\n", node, cpu);
    return old_proc;
  }
};
