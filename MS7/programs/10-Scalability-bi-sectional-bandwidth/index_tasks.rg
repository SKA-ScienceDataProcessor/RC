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

task write(output : region(ispace(ptr, 1), int))
where writes(output)
do
  for p in output do @p = fill_val end
end

task read(input : region(ispace(ptr, 1), int))
where reads(input)
do
  for p in input do
    if @p ~= fill_val then std.puts("Data are corrupted!") end
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
  var r = region(ispace(ptr, num_points), int)
  new(ptr(int, r), num_points)
  var rc = c.legion_coloring_create()
  for i = 0, num_points do
    c.legion_coloring_ensure_color(rc, i)
  end
  var p_disjoint = partition(disjoint, r, rc)
  c.legion_coloring_destroy(rc)

  -- FIXME: try it several times and record the times
  -- (we perform random cpus bisection)

  __demand(__parallel)
  for i = 0, num_points do
    write(p_disjoint[i])
  end

  __demand(__parallel)
  for i = 0, num_points do
    read(p_disjoint[i])
  end

  std.puts("SUCCESS!")
end

std.reg_mappers()
regentlib.start(main)
