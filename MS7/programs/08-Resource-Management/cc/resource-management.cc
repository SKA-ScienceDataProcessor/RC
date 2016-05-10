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

using namespace LegionRuntime::HighLevel;
using namespace LegionRuntime::Accessor;

LegionRuntime::Logger::Category log_logging("logging");

/*
 * To illustrate task launches and futures in Legion
 * we implement a program to compute the first N
 * Fibonacci numbers.  While we note that this is not
 * the fastest way to compute Fibonacci numbers, it
 * is designed to showcase the functional nature of
 * Legion tasks and futures.
 */

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
class SenderReceiverMapper : public DefaultMapper {
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
    log_logging.print("task %x has cpu index %d.\n",task->task_id, cpu);
    for (std::set<Processor>::const_iterator it = all_procs.begin();
            it != all_procs.end(); it++) {
      log_logging.print("considering cpu %llx.\n",it->id);
      Realm::ID cpuid(*it);
      if (it->kind() == Processor::LOC_PROC && cpuid.node() == node && cpuid.index() == cpu)
        return (*it);
    }
    printf("no processor for node %d, cpu %d.\n", node, cpu);
    return old_proc;
  }
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

// Here is the constructor for our adversial mapper.
// We'll use the constructor to illustrate how mappers can
// get access to information regarding the current machine.
SenderReceiverMapper::SenderReceiverMapper(Machine m, 
                                     HighLevelRuntime *rt, Processor p)
  : DefaultMapper(m, rt, p) // pass arguments through to DefaultMapper
{
  // The machine object is a singleton object that can be
  // used to get information about the underlying hardware.
  // The machine pointer will always be provided to all
  // mappers, but can be accessed anywhere by the static
  // member method Machine::get_machine().  Here we get
  // a reference to the set of all processors in the machine
  // from the machine object.  Note that the Machine object
  // actually comes from the Legion low-level runtime, most
  // of which is not directly needed by application code.
  // Typedefs in legion_types.h ensure that all necessary
  // types for querying the machine object are in scope
  // in the Legion HighLevel namespace.
  std::set<Processor> all_procs;
  machine.get_all_processors(all_procs);
  // Recall that we create one mapper for every processor.  We
  // only want to print out this information one time, so only
  // do it if we are the mapper for the first processor in the
  // list of all processors in the machine.
  unsigned this_node = Realm::ID(local_proc).node();
  if (this_node == 0)
  {
    // Print out how many processors there are and each
    // of their kinds.
    printf("There are %ld processors:\n", all_procs.size());
    for (std::set<Processor>::const_iterator it = all_procs.begin();
          it != all_procs.end(); it++)
    {
      // For every processor there is an associated kind
      Processor::Kind kind = it->kind();
      switch (kind)
      {
        // Latency-optimized cores (LOCs) are CPUs
        case Processor::LOC_PROC:
          {
            printf("  Processor ID %llx is CPU\n", it->id); 
            break;
          }
        // Throughput-optimized cores (TOCs) are GPUs
        case Processor::TOC_PROC:
          {
            printf("  Processor ID %llx is GPU\n", it->id);
            break;
          }
        // Utility processors are helper processors for
        // running Legion runtime meta-level tasks and 
        // should not be used for running application tasks
        case Processor::UTIL_PROC:
          {
            printf("  Processor ID %llx is utility\n", it->id);
            break;
          }
        default:
          assert(false);
      }
    }
    // We can also get the list of all the memories available
    // on the target architecture and print out their info.
    std::set<Memory> all_mems;
    machine.get_all_memories(all_mems);
    printf("There are %ld memories:\n", all_mems.size());
    for (std::set<Memory>::const_iterator it = all_mems.begin();
          it != all_mems.end(); it++)
    {
      Memory::Kind kind = it->kind();
      size_t memory_size_in_kb = it->capacity() >> 10;
      switch (kind)
      {
        // RDMA addressable memory when running with GASNet
        case Memory::GLOBAL_MEM:
          {
            printf("  GASNet Global Memory ID %llx has %ld KB\n", 
                    it->id, memory_size_in_kb);
            break;
          }
        // DRAM on a single node
        case Memory::SYSTEM_MEM:
          {
            printf("  System Memory ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // Pinned memory on a single node
        case Memory::REGDMA_MEM:
          {
            printf("  Pinned Memory ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // A memory associated with a single socket
        case Memory::SOCKET_MEM:
          {
            printf("  Socket Memory ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // Zero-copy memory betweeen CPU DRAM and
        // all GPUs on a single node
        case Memory::Z_COPY_MEM:
          {
            printf("  Zero-Copy Memory ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // GPU framebuffer memory for a single GPU
        case Memory::GPU_FB_MEM:
          {
            printf("  GPU Frame Buffer Memory ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // Disk memory on a single node
        case Memory::DISK_MEM:
          {
            printf("  Disk Memory ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // Block of memory sized for L3 cache
        case Memory::LEVEL3_CACHE:
          {
            printf("  Level 3 Cache ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // Block of memory sized for L2 cache
        case Memory::LEVEL2_CACHE:
          {
            printf("  Level 2 Cache ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        // Block of memory sized for L1 cache
        case Memory::LEVEL1_CACHE:
          {
            printf("  Level 1 Cache ID %llx has %ld KB\n",
                    it->id, memory_size_in_kb);
            break;
          }
        default:
            printf("  unknown type %d id %llx has %ld KB\n", kind,
                    it->id, memory_size_in_kb);
      }
    }

    // The Legion machine model represented by the machine object
    // can be thought of as a graph with processors and memories
    // as the two kinds of nodes.  There are two kinds of edges
    // in this graph: processor-memory edges and memory-memory
    // edges.  An edge between a processor and a memory indicates
    // that the processor can directly perform load and store
    // operations to that memory.  Memory-memory edges indicate
    // that data movement can be directly performed between the
    // two memories.  To illustrate how this works we examine
    // all the memories visible to our local processor in 
    // this mapper.  We can get our set of visible memories
    // using the 'get_visible_memories' method on the machine.
    std::set<Memory> vis_mems;
    machine.get_visible_memories(local_proc, vis_mems);
    printf("There are %ld memories visible from processor %llx\n",
            vis_mems.size(), local_proc.id);
    for (std::set<Memory>::const_iterator it = vis_mems.begin();
          it != vis_mems.end(); it++)
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
      printf("  Memory %llx has bandwidth %d and latency %d\n",
              it->id, affinities[0].bandwidth, affinities[0].latency);
    }
  }
}

// The first mapper call that we override is the 
// select_task_options call.  This mapper call is
// performed on every task launch immediately after
// it is made.  The call asks the mapper to select 
// set properities of the task:
//
//  inline_task - whether the task should be directly
//    inlined into its parent task, using the parent
//    task's physical regions.  
//  spawn_task - whether the task is eligible for 
//    stealing (based on Cilk-style semantics)
//  map_locally - whether the task should be mapped
//    by the processor on which it was launched or
//    whether it should be mapped by the processor
//    where it will run.
//  profile_task - should the runtime collect profiling
//    information about the task while it is executing
//  target_proc - which processor should the task be
//    sent to once all of its mapping dependences have
//    been satisifed.
//
//  Note that these properties are all set on the Task
//  object declared in legion.h.  The Task object is
//  the representation of a task that the Legion runtime
//  provides to the mapper for specifying mapping
//  decisions.  Note that there are similar objects
//  for inline mappings as well as other operations.
//
//  For our adversarial mapper, we perform the default
//  choices for all options except the last one.  Here
//  we choose a random processor in our system to 
//  send the task to.
void SenderReceiverMapper::select_task_options(Task *task)
{
  task->inline_task = false;
  task->spawn_task = false;
  task->map_locally = false;
  task->profile_task = false;
  task->task_priority = 0;
  task->target_proc = task_cpu(task, task->target_proc);
}

#if 0
// The second call that we override is the slice_domain
// method. The slice_domain call is used by the runtime
// to query the mapper about the best way to distribute
// the points in an index space task launch throughout
// the machine. The maper is given the task and the domain
// to slice and then asked to generate sub-domains to be
// sent to different processors in the form of DomainSplit
// objects. DomainSplit objects describe the sub-domain,
// the target processor for the sub-domain, whether the
// generated slice can be stolen, and finally whether 
// slice_domain' should be recursively called on the
// slice when it arrives at its destination.
//
// In this example we use a utility method from the DefaultMapper
// called decompose_index_space to decompose our domain. We 
// recursively split the domain in half during each call of
// slice_domain and send each slice to a random processor.
// We continue doing this until the leaf domains have only
// a single point in them. This creates a tree of slices of
// depth log(N) in the number of points in the domain with
// each slice being sent to a random processor.
void SenderReceiverMapper::slice_domain(const Task *task, const Domain &domain,
                                     std::vector<DomainSplit> &slices)
{
  std::set<Processor> all_procs;
  machine.get_all_processors(all_procs);
  std::vector<Processor> split_set;
  for (unsigned idx = 0; idx < 2; idx++)
  {
    split_set.push_back(DefaultMapper::select_random_processor(
                        all_procs, Processor::LOC_PROC, machine));
  }

  DefaultMapper::decompose_index_space(domain, split_set, 
                                        1/*splitting factor*/, slices);
  for (std::vector<DomainSplit>::iterator it = slices.begin();
        it != slices.end(); it++)
  {
    Rect<1> rect = it->domain.get_rect<1>();
    if (rect.volume() == 1)
      it->recurse = false;
    else
      it->recurse = true;
  }
}
#endif

// The next mapping call that we override is the map_task
// mapper method. Once a task has been assigned to map on
// a specific processor (the target_proc) then this method
// is invoked by the runtime to select the memories in 
// which to create physical instances for each logical region.
// The mapper communicates this information to the runtime
// via the mapping fields on RegionRequirements. The memories
// containing currently valid physical instances for each
// logical region is provided by the runtime in the 
// 'current_instances' field. The mapper must specify an
// ordered list of memories for the runtime to try when
// creating a physical instance in the 'target_ranking'
// vector field of each RegionRequirement. The runtime
// attempts to find or make a physical instance in each 
// memory until it succeeds. If the runtime fails to find
// or make a physical instance in any of the memories, then
// the mapping fails and the mapper will be notified that
// the task failed to map using the 'notify_mapping_failed'
// mapper call. If the mapper does nothing, then the task
// is placed back on the list of tasks eligible to be mapped.
// There are other fields that the mapper can set in the
// process of the map_task call that we do not cover here.
//
// In this example, the mapper finds the set of all visible
// memories from the target processor and then puts them
// in a random order as the target set of memories, thereby
// challenging the Legion runtime to maintain correctness
// of data moved through random sets of memories.
#if 0
bool SenderReceiverMapper::map_task(Task *task)
{ 
printf("called map_task\n");
  std::set<Memory> vis_mems;
  machine.get_visible_memories(task->target_proc, vis_mems);  
  assert(!vis_mems.empty());
  for (unsigned idx = 0; idx < task->regions.size(); idx++)
  {
    std::set<Memory> mems_copy = vis_mems;  
    // Assign memories in a random order
    while (!mems_copy.empty())
    {
      unsigned mem_idx = (lrand48() % mems_copy.size());
      std::set<Memory>::iterator it = mems_copy.begin();
      for (unsigned i = 0; i < mem_idx; i++)
        it++;
      task->regions[idx].target_ranking.push_back(*it);
      mems_copy.erase(it);
    }
    task->regions[idx].virtual_map = false;
    task->regions[idx].enable_WAR_optimization = false;
    task->regions[idx].reduction_list = false;
    task->regions[idx].blocking_factor = 1;
  }
  // Report successful mapping results
  return true;
}
#endif

// The last mapper call we override is the notify_mapping_result
// call which is invoked by the runtime if the mapper indicated
// that it wanted to know the result of the mapping following
// the map_task call by returning true. The runtime invokes
// this call and the chosen memories for each RegionRequirement
// are set in the 'selected_memory' field. We use this call in
// this example to record the memories in which physical instances
// were mapped for each logical region of each task so we can
// see that the assignment truly is random.
void SenderReceiverMapper::notify_mapping_result(const Mappable *mappable)
{
  if (mappable->get_mappable_kind() == Mappable::TASK_MAPPABLE)
  {
    const Task *task = mappable->as_mappable_task();
    assert(task != NULL);
    for (unsigned idx = 0; idx < task->regions.size(); idx++)
    {
      printf("Mapped region %d of task %s (ID %lld) to memory %llx\n",
              idx, task->variants->name, 
              task->get_unique_task_id(),
              task->regions[idx].selected_memory.id);
    }
  }
}

void top_level_task(const Task *task,
                    const std::vector<PhysicalRegion> &regions,
                    Context ctx, HighLevelRuntime *runtime)
{
    int region_size = 16*1024*1024;

    printf("Top level entered.\n");
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

    printf("Top level region size %d.\n", region_size);
    runtime->execute_task(ctx, TaskLauncher(SENDER1_TASK_ID, TaskArgument(&region_size, sizeof(region_size))));
    runtime->execute_task(ctx, TaskLauncher(SENDER2_TASK_ID, TaskArgument(&region_size, sizeof(region_size))));
}

void fill_task(const Task *task,
    const std::vector<PhysicalRegion> &regions,
                   Context ctx, HighLevelRuntime *runtime)
{
  printf("fill task, id %d\n", task->task_id);

  assert(task->arglen == sizeof(int));

  const int start = *((const int*)task->args);
printf("start %d.\n",start);

  assert(regions.size() == 1);
printf("assert(regions.size() == 1);\n");
  assert(task->regions.size() == 1);
printf("assert(task->regions.size() == 1);\n");


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

  printf("sum %d.\n", sum);
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

  printf("sender entered, id %d, region size %d.\n", task->task_id, region_size);

  int start = 1;
  int receiver_id = RECEIVER1_TASK_ID;
  int fill_id = FILL1_TASK_ID;
  if (task->task_id == SENDER2_TASK_ID) {
    receiver_id = RECEIVER2_TASK_ID;
    fill_id = FILL2_TASK_ID;
    start = 2;
  }
  printf("sender, receiver_id %d, fill_id %d.\n", receiver_id, fill_id);

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
  printf("receiver task, task id %d.\n", task->task_id);

  assert(regions.size() == 1);
printf("assert(regions.size() == 1);\n");
  assert(task->regions.size() == 1);
printf("assert(task->regions.size() == 1);\n");


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

  printf("sum %d.\n", sum);

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
