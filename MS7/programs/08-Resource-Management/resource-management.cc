// Resource management demo program.
//
// Copyright (C) 2016 Braam Research, LLC.

#include <cstdio>
#include <cassert>
#include <cstdlib>
#include "legion.h"
using namespace LegionRuntime::HighLevel;

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
  RECEIVER1_TASK_ID,
  RECEIVER2_TASK_ID
};

void top_level_task(const Task *task,
                    const std::vector<PhysicalRegion> &regions,
                    Context ctx, HighLevelRuntime *runtime)
{
    int region_size = 116*1024*1024;
    runtime->execute_task(ctx, TaskLauncher(SENDER_TASK_ID, TaskArgument(region_size, sizeof(n))));
    runtime->execute_task(ctx, TaskLauncher(SENDER_TASK_ID, TaskArgument(region_size, sizeof(n))));
}

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

  int receiver_id = RECEIVER1_TASK_ID;
  if (task->task_id == SENDER2_TASK_ID)
    receiver_id = RECEIVER2_TASK_ID;

  TaskLauncher t1(receiver_id, TaskArgument(&fib1,sizeof(fib1)));
  runtime->execute_task(ctx, t1);

}

void receiver_task(const Task *task,
             const std::vector<PhysicalRegion> &regions,
             Context ctx, HighLevelRuntime *runtime)
{
  assert(false);
}
              
int main(int argc, char **argv)
{
  HighLevelRuntime::set_top_level_task_id(TOP_LEVEL_TASK_ID);
  HighLevelRuntime::register_legion_task<top_level_task>(TOP_LEVEL_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  // Note that tasks which return values must pass the type of
  // the return argument as the first template paramenter.
  HighLevelRuntime::register_legion_task<sender_task>(SENDER1_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  HighLevelRuntime::register_legion_task<sender_task>(SENDER2_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  // The sum-task has a very special property which is that it is
  // guaranteed never to make any runtime calls.  We call these
  // kinds of tasks "leaf" tasks and tell the runtime system
  // about them using the 'TaskConfigOptions' struct.  Being
  // a leaf task allows the runtime to perform significant
  // optimizations that minimize the overhead of leaf task
  // execution.  Note that we also tell the runtime to 
  // automatically generate the variant ID for this task
  // with the 'AUTO_GENERATE_ID' argument.
  HighLevelRuntime::register_legion_task<receiver_task>(RECEIVER1_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/, 
      AUTO_GENERATE_ID, TaskConfigOptions(true/*leaf*/), "receiver1_task");
  HighLevelRuntime::register_legion_task<receiver_task>(RECEIVER2_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/, 
      AUTO_GENERATE_ID, TaskConfigOptions(true/*leaf*/), "receiver2_task");

  return HighLevelRuntime::start(argc, argv);
}
