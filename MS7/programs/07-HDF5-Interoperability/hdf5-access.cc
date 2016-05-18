// HDF5 access demo (modified Resource management demo program.).
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
  SUM_PRINT_TASK_ID
};

enum FieldIDs {
  FIELD
};

void top_level_task(const Task *task,
                    const std::vector<PhysicalRegion> &regions,
                    Context ctx, HighLevelRuntime *runtime)
{
    printf("Top level entered.\n");
    const InputArgs &command_args = HighLevelRuntime::get_input_args();
    if (command_args.argc > 1)
    {
        region_size = atoi(command_args.argv[1]);
        assert(region_size > 0);
    }

    TaskArgument arg();
    TaskLauncher sum_print_lr(SUM_PRINT_TASK_ID, arg);

}

int main(int argc, char **argv)
{
  HighLevelRuntime::set_top_level_task_id(TOP_LEVEL_TASK_ID);
  HighLevelRuntime::register_legion_task<top_level_task>(TOP_LEVEL_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);
  HighLevelRuntime::register_legion_task<sum_print_task>(SUM_PRINT_TASK_ID,
      Processor::LOC_PROC, true/*single*/, false/*index*/);

  return HighLevelRuntime::start(argc, argv);
}
