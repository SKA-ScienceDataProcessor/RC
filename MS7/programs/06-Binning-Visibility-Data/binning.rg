-- Binning demo.
--
-- Copyright (C) 2016 Braam Research, LLC.

import "regent"

local c = terralib.includecstring [[
#include <stdio.h>
#include <legion_c.h>
]]

local dim = 1000
local tile_size = 50
local num_of_pieces = dim / tile_size

local num_of_points = 100

local opaque = terralib.global(int[dim])

struct sparse {
    x : int
  , v : int
}

terra setdata(s : sparse)
  opaque[s.x] = s.v
end

terra printdata()
  for i : int = 0, dim do
    c.printf("%5d", opaque[i])
    if (i+1) % 10 == 0 then c.puts("") end
  end
end

task printres(r : region(ispace(ptr), sparse))
where
  reads(r) -- artificial dependency to wait for data be ready
do
  printdata()
end

task go(r : region(ispace(ptr), sparse))
where
    reads(r)
  , writes(r) -- artificial dependency
do
  for i in r.ispace do
    setdata(r[i])
  end
end

terra pp(x : int, y : int)
  c.printf("%5d %5d\n", x, y)
end

task top_level()
  -- Sample sparse
  var is = ispace(ptr, num_of_points)
  var r = region(is, sparse)
  new(ptr(sparse, r), num_of_points)

  var n = 0
  for i in is do
    r[i].x = n * (dim/num_of_points) -- evenly distribute. don't pay any attn to it
    r[i].v = 100 + n
    n = n + 1
  end

  var bw = c.legion_coloring_create()
  for i in is do
    var color = (r[i].x / tile_size) % 2
    -- c.printf("%d-th elt valued as %d is colored to %d\n", r[i].x, r[i].v, color)
    c.legion_coloring_add_point(bw, color, __raw(i))
  end
  var chequered = partition(disjoint, r, bw)
  c.legion_coloring_destroy(bw)

  -- To make Regent happy
  var c0 = chequered[0]
  var c1 = chequered[1]

  --[[
  for c in c0 do
    pp(c.x, c.v)
  end
  for c in c1 do
    pp(c.x, c.v)
  end
  --]]

  var col0 = c.legion_coloring_create()
  for i in c0.ispace do
    var color = c0[i].x / tile_size
    c.legion_coloring_add_point(col0, color, __raw(i))
  end
  var p0 = partition(disjoint, c0, col0)
  c.legion_coloring_destroy(col0)

  var col1 = c.legion_coloring_create()
  for i in c1.ispace do
    var color = c1[i].x / tile_size
    c.legion_coloring_add_point(col1, color, __raw(i))
  end
  var p1 = partition(disjoint, c1, col1)
  c.legion_coloring_destroy(col1)

  ---[[
  for n = 0, num_of_pieces do
    go(p0[n])
  end
  ---]]
  ---[[
  for n = 0, num_of_pieces do
    go(p1[n])
  end
  ---]]

  printres(r)

  --[[
  for i in is do
    c.printf("%5d", r[i].x)
  end
  --]]

end

-- start main work.
regentlib.start(top_level)
