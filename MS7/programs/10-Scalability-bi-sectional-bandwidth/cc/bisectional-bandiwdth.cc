// Resource management demo program.
//
// Copyright (C) 2016 Braam Research, LLC.

#include <cstdio>
#include <cassert>
#include <cstdlib>
#include <stdint.h>
#include "legion.h"
#include "realm.h"
#include "legion_types.h"
#include "default_mapper.h"
#include "id.h"

using namespace Legion;
using namespace Legion::Mapping;
using namespace LegionRuntime::Accessor;

LegionRuntime::Logger::Category log_logging("logging");

enum TaskIDs {
  TOP_LEVEL_TASK_ID,
  SENDER1_TASK_ID,
  SENDER2_TASK_ID,
  FILL1_TASK_ID,
  FILL2_TASK_ID,
  RECEIVER1_TASK_ID,
  RECEIVER2_TASK_ID
};

enum FieldIDs {
  FIELD
};

inline
Processor task_cpu(const Machine & machine, const Task * task, Processor old_proc) {
  Machine::ProcessorQuery all_procs(machine);
  unsigned node, cpu = 0;
  unsigned max_node = 0;
  for (Machine::ProcessorQuery::iterator it = all_procs.begin();
      it != all_procs.end(); it++){
    Realm::ID cpuid(*it);
    log_logging.print("Cpu %llx sits on node %u", it->id, cpuid.node());
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
      log_logging.print("Unknown task id %x.",task->task_id);
      return old_proc;
  }
  if (node > max_node) {
    log_logging.print("Too high node index %d for task %x.", node, task->task_id);
    node = max_node;
  }
  log_logging.print("task %x has cpu index %d.",task->task_id, cpu);
  for (Machine::ProcessorQuery::iterator it = all_procs.begin();
      it != all_procs.end(); it++){
      log_logging.print("considering cpu %llx.",it->id);
      Realm::ID cpuid(*it);
      if (it->kind() == Processor::LOC_PROC && cpuid.node() == node) { // our node - look for CPU.
        if (cpu == 0) {
          log_logging.print("assigned cpu %llx.",it->id);
          return (*it);
        }
        cpu--;
      }
  }
  log_logging.print("no processor for node %d, cpu %d.", node, cpu);
  return old_proc;
}

class SenderReceiverMapper : public DefaultMapper
{
public:
  SenderReceiverMapper(Machine machine,
      HighLevelRuntime *rt, Processor local);
public:
  virtual void select_task_options(const MapperContext    ctx,
                                   const Task&  task,
                                   TaskOptions&     output);
};

void mapper_registration(Machine machine, HighLevelRuntime *rt,
                          const std::set<Processor> &local_procs)
{
  for (std::set<Processor>::const_iterator it = local_procs.begin();
        it != local_procs.end(); it++)
  {
    rt->replace_default_mapper(
        new SenderReceiverMapper(machine, rt, *it), *it);
  }
}

SenderReceiverMapper::SenderReceiverMapper(Machine m,
                                     HighLevelRuntime *rt, Processor p)
  : DefaultMapper(rt->get_mapper_runtime(), m, p, "SenderReceiverMapper")
{
  Machine::ProcessorQuery all_procs(machine);
  // unsigned this_node = Realm::ID(local_proc).node();
  // if (this_node == 0)
  if (all_procs.begin()->id + 1 == local_proc.id)
  {
    // Print out how many processors there are and each
    // of their kinds.
    log_logging.print("There are %ld processors:", all_procs.count());
    for (Machine::ProcessorQuery::iterator it = all_procs.begin();
         it != all_procs.end(); it++)
    {
      // For every processor there is an associated kind
      Processor::Kind kind = it->kind();
      switch (kind)
      {
        // Latency-optimized cores (LOCs) are CPUs
        case Processor::LOC_PROC:
          {
            log_logging.print("  Processor ID %llx is CPU", it->id);
            break;
          }
        // Throughput-optimized cores (TOCs) are GPUs
        case Processor::TOC_PROC:
          {
            log_logging.print("  Processor ID %llx is GPU", it->id);
            break;
          }
        // Utility processors are helper processors for
        // running Legion runtime meta-level tasks and
        // should not be used for running application tasks
        case Processor::UTIL_PROC:
          {
            log_logging.print("  Processor ID %llx is utility", it->id);
            break;
          }
        default:
          assert(false);
      }
    }
    // We can also get the list of all the memories available
    // on the target architecture and print out their info.
    Machine::MemoryQuery all_mems(machine);
    log_logging.print("There are %ld memories:", all_mems.count());

    for (Machine::MemoryQuery::iterator it = all_mems.begin();
         it != all_mems.end(); it++)
    {
      Memory::Kind kind = it->kind();
      size_t memory_size_in_kb = it->capacity() >> 10;
      switch (kind)
      {
        // RDMA addressable memory when running with GASNet
        case Memory::GLOBAL_MEM:
          {
            log_logging.print("  GASNet Global Memory ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // DRAM on a single node
        case Memory::SYSTEM_MEM:
          {
            log_logging.print("  System Memory ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // Pinned memory on a single node
        case Memory::REGDMA_MEM:
          {
            log_logging.print("  Pinned Memory ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // A memory associated with a single socket
        case Memory::SOCKET_MEM:
          {
            log_logging.print("  Socket Memory ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // Zero-copy memory betweeen CPU DRAM and
        // all GPUs on a single node
        case Memory::Z_COPY_MEM:
          {
            log_logging.print("  Zero-Copy Memory ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // GPU framebuffer memory for a single GPU
        case Memory::GPU_FB_MEM:
          {
            log_logging.print("  GPU Frame Buffer Memory ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // Disk memory on a single node
        case Memory::DISK_MEM:
          {
            log_logging.print("  Disk Memory ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // Block of memory sized for L3 cache
        case Memory::LEVEL3_CACHE:
          {
            log_logging.print("  Level 3 Cache ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // Block of memory sized for L2 cache
        case Memory::LEVEL2_CACHE:
          {
            log_logging.print("  Level 2 Cache ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        // Block of memory sized for L1 cache
        case Memory::LEVEL1_CACHE:
          {
            log_logging.print("  Level 1 Cache ID %llx has %ld KB",
                    it->id, memory_size_in_kb);
            break;
          }
        default:
            log_logging.print("  unknown type %d id %llx has %ld KB", kind,
                    it->id, memory_size_in_kb);
      }
    }

    // Look at only visible memories
    all_mems.has_affinity_to(local_proc);

    log_logging.print("There are %ld memories visible from processor %llx",
            all_mems.count(), local_proc.id);
    for (Machine::MemoryQuery::iterator it = all_mems.begin();
         it != all_mems.end(); it++)
    {
      // Edges between nodes are called affinities in the
      // machine model.  Affinities also come with approximate
      // indications of the latency and bandwidth between the
      // two nodes.  Right now these are unit-less measurements,
      // but our plan is to teach the Legion runtime to profile
      // these values on start-up to give them real values
      // and further increase the portability of Legion applications.
      std::vector<ProcessorMemoryAffinity> affinities;
      int results =
        machine.get_proc_mem_affinity(affinities, local_proc, *it);
      // We should only have found 1 results since we
      // explicitly specified both values.
      assert(results == 1);
      log_logging.print("  Memory %llx has bandwidth %d and latency %d",
              it->id, affinities[0].bandwidth, affinities[0].latency);
    }
  }
}

void SenderReceiverMapper::select_task_options(const MapperContext ctx,
                                   const Task&  task,
                                   TaskOptions&  output)
{
  output.inline_task = false;
  output.stealable = false;
  output.map_locally = false;
  output.initial_proc = task_cpu(machine, &task, task.target_proc);
}

void top_level_task(const Task *task,
                    const std::vector<PhysicalRegion> &regions,
                    Context ctx, HighLevelRuntime *runtime)
{
    int region_size = 16*1024*1024;

    log_logging.print("Top level entered.");
    const InputArgs &command_args = HighLevelRuntime::get_input_args();
    for (int i = 0; i < command_args.argc; i++)
    {
        if (strcmp("-n",command_args.argv[i]) == 0)
        {
            region_size = atoi(command_args.argv[i+1]);
            break;
        }
    }
    assert(region_size > 0);

    log_logging.print("Top level region size %d.", region_size);
    runtime->execute_task(ctx, TaskLauncher(SENDER1_TASK_ID, TaskArgument(&region_size, sizeof(region_size))));
    runtime->execute_task(ctx, TaskLauncher(SENDER2_TASK_ID, TaskArgument(&region_size, sizeof(region_size))));
}

void fill_task(const Task *task,
    const std::vector<PhysicalRegion> &regions,
                   Context ctx, HighLevelRuntime *runtime)
{
  log_logging.print("fill task, id %d", task->task_id);

  assert(task->arglen == sizeof(int));

  const int start = *((const int*)task->args);
  log_logging.print("start %d.",start);

  assert(regions.size() == 1);
  log_logging.print("assert(regions.size() == 1);");
  assert(task->regions.size() == 1);
  log_logging.print("assert(task->regions.size() == 1);");

  RegionAccessor<AccessorType::Generic, int8_t> acc_f =
    regions[0].get_field_accessor(FIELD).typeify<int8_t>();

  Domain dom = runtime->get_index_space_domain(ctx,
      task->regions[0].region.get_index_space());

  Rect<1> rect = dom.get_rect<1>();

  int sum = 0;
  int i = start;

  for (GenericPointInRectIterator<1> pir(rect); pir; pir++)
  {
    sum += i;
    acc_f.write(DomainPoint::from_point<1>(pir.p), i);
  }

  log_logging.print("sum %d.", sum);
} /* fill_task */

void sender_task(const Task *task,
                   const std::vector<PhysicalRegion> &regions,
                   Context ctx, HighLevelRuntime *runtime)
{
  // The 'TaskArgument' value passed to a task and its size
  // in bytes is available in the 'args' and 'arglen' fields
  // on the 'Task' object.
  //
  // Since there is no type checking when writing to
  // the runtime API (a benefit provided by our Legion compiler)
  // we encourage programmers to check that they are getting
  // what they expect in their values.
  assert(task->arglen == sizeof(int));

  const int region_size = *((const int*)task->args);

  log_logging.print("sender entered, id %d, region size %d.", task->task_id, region_size);

  int start = 1;
  int receiver_id = RECEIVER1_TASK_ID;
  int fill_id = FILL1_TASK_ID;
  if (task->task_id == SENDER2_TASK_ID) {
    receiver_id = RECEIVER2_TASK_ID;
    fill_id = FILL2_TASK_ID;
    start = 2;
  }
  log_logging.print("sender, receiver_id %d, fill_id %d.", receiver_id, fill_id);

  Rect<1> elem_rect(Point<1>(0),Point<1>(region_size-1));
  IndexSpace is = runtime->create_index_space(ctx,
                          Domain::from_rect<1>(elem_rect));
  FieldSpace main_fs = runtime->create_field_space(ctx);
  {
    FieldAllocator allocator =
      runtime->create_field_allocator(ctx, main_fs);
    allocator.allocate_field(sizeof(int8_t),FIELD);
  }
  LogicalRegion main_lr = runtime->create_logical_region(ctx, is, main_fs);

  TaskArgument fill_start = TaskArgument(&start, sizeof(start));
  TaskLauncher fill_launcher(fill_id, fill_start);
  fill_launcher.add_region_requirement(RegionRequirement(main_lr, WRITE_ONLY, ATOMIC, main_lr));
  fill_launcher.add_field(0, FIELD);
  runtime->execute_task(ctx, fill_launcher);

  TaskArgument ta = TaskArgument();
  TaskLauncher launcher(receiver_id, ta);
  launcher.add_region_requirement(RegionRequirement(main_lr, READ_ONLY, ATOMIC, main_lr));
  launcher.add_field(0, FIELD);
  runtime->execute_task(ctx, launcher);

  runtime->destroy_logical_region(ctx, main_lr);
  runtime->destroy_field_space(ctx, main_fs);
  runtime->destroy_index_space(ctx, is);

}

void receiver_task(const Task *task,
             const std::vector<PhysicalRegion> &regions,
             Context ctx, HighLevelRuntime *runtime)
{
  log_logging.print("receiver task, task id %d.", task->task_id);

  assert(regions.size() == 1);
  log_logging.print("assert(regions.size() == 1);");
  assert(task->regions.size() == 1);
  log_logging.print("assert(task->regions.size() == 1);");


  RegionAccessor<AccessorType::Generic, int8_t> acc_f =
    regions[0].get_field_accessor(FIELD).typeify<int8_t>();

  Domain dom = runtime->get_index_space_domain(ctx,
      task->regions[0].region.get_index_space());

  Rect<1> rect = dom.get_rect<1>();

  int sum = 0;

  for (GenericPointInRectIterator<1> pir(rect); pir; pir++)
  {
    sum += acc_f.read(DomainPoint::from_point<1>(pir.p));
  }

  log_logging.print("sum %d.", sum);

}

char buf[1024];
int main(int argc, char **argv)
{
  setvbuf(stdout, buf, _IONBF, sizeof(buf));
  HighLevelRuntime::set_top_level_task_id(TOP_LEVEL_TASK_ID);
  HighLevelRuntime::register_legion_task<top_level_task>(TOP_LEVEL_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  HighLevelRuntime::register_legion_task<fill_task>(FILL1_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  HighLevelRuntime::register_legion_task<fill_task>(FILL2_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  HighLevelRuntime::register_legion_task<sender_task>(SENDER1_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  HighLevelRuntime::register_legion_task<sender_task>(SENDER2_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  HighLevelRuntime::register_legion_task<receiver_task>(RECEIVER1_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/,
      AUTO_GENERATE_ID, TaskConfigOptions(true/*leaf*/), "receiver1_task");
  HighLevelRuntime::register_legion_task<receiver_task>(RECEIVER2_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/,
      AUTO_GENERATE_ID, TaskConfigOptions(true/*leaf*/), "receiver2_task");

  HighLevelRuntime::set_registration_callback(mapper_registration);

  return HighLevelRuntime::start(argc, argv);
}
