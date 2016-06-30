-- Copyright 2016 Braam Research, LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

import "regent"

-- terralib.linklibrary("./report.so")
terralib.linklibrary("./report-ibv.so")

local c = regentlib.c

local std = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
void reg_mappers();
]]

local number_of_tests = 10

local struct rep {
    datap : &c.legion_processor_t
  , procs : uint64
  , cpup  : &c.legion_processor_t
  , cpus  : uint64
}

terra cmp_procs(pa : &opaque, pb : &opaque)
  var pat : int = c.legion_processor_kind([&c.legion_processor_t](pa)[0])
  var pbt : int = c.legion_processor_kind([&c.legion_processor_t](pb)[0])
  return pat - pbt
end

terra report()
  var machine = c.legion_machine_create();

  var n = c.legion_machine_get_all_processors_size(machine)
  var procp = [&c.legion_processor_t](std.malloc(sizeof(c.legion_processor_t) * n))
  c.legion_machine_get_all_processors(machine, procp, n)
  c.qsort(procp, n, sizeof(c.legion_processor_t), cmp_procs);

  var off : uint64 = n
  for i = 0, n do
    if c.legion_processor_kind(procp[i]) == c.LOC_PROC then
      off = i
      break
    end
  end

  var ncpus : uint64 = 0
  for i = off, n do
    if c.legion_processor_kind(procp[i]) == c.LOC_PROC then ncpus = ncpus + 1
    else break
    end
  end

  c.legion_machine_destroy

  return rep{procp, n, procp + off, ncpus}
end

local fill_val = 666

task write(output : region(ispace(int1d, 1), int))
where writes(output)
do
  for p in output.ispace do output[p] = fill_val end
end

task read(input : region(ispace(int1d, 1), int))
where reads(input)
do
  for p in input.ispace do
    if input[p] ~= fill_val then std.printf("Corrupted: %d!\n", input[p]) end
  end
end

task run_test( num_points : uint64
             , ps : ispace(int1d, num_points)
             , r : region(ps, int)
             , p_disjoint : partition(disjoint, r, ps)
             )
where
  writes reads(r)
do
  __demand(__parallel)
  for i = 0, num_points do
    write(p_disjoint[i])
  end

  __demand(__parallel)
  for i = 0, num_points do
    read(p_disjoint[i])
  end
end

task main()
  var rep = report()
  std.printf("We have %lld processors in total, %lld cpus:\n", rep.procs, rep.cpus)
  for i = 0, rep.cpus do
    std.printf("\t%llx : %x\n", rep.cpup[i].id, c.legion_processor_kind(rep.cpup[i]))
  end
  c.free(rep.datap)

  var num_points = rep.cpus / 2
  var ps = ispace(int1d, num_points)
  var r = region(ps, int)

  var p_disjoint = partition(equal, r, ps)

  for i=0, number_of_tests do
    run_test(num_points, ps, r, p_disjoint)
  end

  std.puts("SUCCESS!")
end

std.reg_mappers()
regentlib.start(main)
