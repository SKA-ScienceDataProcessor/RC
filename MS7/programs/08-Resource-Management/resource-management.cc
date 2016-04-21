// Resource management demo program.
//
// Copyright (C) 2016 Braam Research, LLC.

#include <cstdio>
#include <cassert>
#include <cstdlib>
#include <stdint.h>
#include "legion.h"

using namespace LegionRuntime::HighLevel;
using namespace LegionRuntime::Accessor;

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
  FILL_TASK_ID,
  RECEIVER1_TASK_ID,
  RECEIVER2_TASK_ID
};

enum FieldIDs {
  FIELD
};

void top_level_task(const Task *task,
                    const std::vector<PhysicalRegion> &regions,
                    Context ctx, HighLevelRuntime *runtime)
{
    int region_size = 16*1024*1024;

    printf("Top level entered.\n");
    const InputArgs &command_args = HighLevelRuntime::get_input_args();
    if (command_args.argc > 1)
    {
        region_size = atoi(command_args.argv[1]);
        assert(region_size > 0);
    }

    printf("Top level region size %d.\n", region_size);
    runtime->execute_task(ctx, TaskLauncher(SENDER1_TASK_ID, TaskArgument(&region_size, sizeof(region_size))));
    runtime->execute_task(ctx, TaskLauncher(SENDER2_TASK_ID, TaskArgument(&region_size, sizeof(region_size))));
}

void fill_task(const Task *task,
const std::vector<PhysicalRegion> &regions,
                   Context ctx, HighLevelRuntime *runtime)
{
  printf("fill task\n");

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

  int receiver_id = RECEIVER1_TASK_ID;
  if (task->task_id == SENDER2_TASK_ID)
    receiver_id = RECEIVER2_TASK_ID;

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

  int start = task->task_id == SENDER1_TASK_ID ? 1 : 2;
  TaskArgument fill_start = TaskArgument(&start, sizeof(start));
  TaskLauncher fill_launcher(FILL_TASK_ID, fill_start);
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
              
int main(int argc, char **argv)
{
  HighLevelRuntime::set_top_level_task_id(TOP_LEVEL_TASK_ID);
  HighLevelRuntime::register_legion_task<top_level_task>(TOP_LEVEL_TASK_ID,
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

  return HighLevelRuntime::start(argc, argv);
}
