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

function make_coloring_task(fun)
  local task gago(r : region(ispace(ptr), sparse))
          where
            reads(r)
          do
            var coloring = c.legion_coloring_create()
            for i in r.ispace do
              var color = [fun (rexpr r[i].x end)]
              c.legion_coloring_add_point(coloring, color, __raw(i))
            end
            var p = partition(disjoint, r, coloring)
            c.legion_coloring_destroy(coloring)
            return p
         end
  return gago
end
  
local function chequer(body)
  return rexpr (([body] / tile_size) % 2) end
end

local function tile(body)
  return rexpr ([body] / tile_size) end
end

local chequer_task = make_coloring_task(chequer)
local tile_task = make_coloring_task(tile)

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

  var chequered = chequer_task(r)

  --[[
  var c0 = chequered[0]
  var c1 = chequered[1]
  for c in c0 do
    pp(c.x, c.v)
  end
  for c in c1 do
    pp(c.x, c.v)
  end
  --]]

  var p0 = tile_task(chequered[0])
  var p1 = tile_task(chequered[1])

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
