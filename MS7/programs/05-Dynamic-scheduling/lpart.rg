import "regent"

local c = regentlib.c

task print_partition(input : region(ispace(int1d), int))
where
  reads(input)
do
  for i in input.ispace do
    c.printf("%5d", input[i])
  end
  c.puts("")
end

task solve(size : int, k : int, input : region(ispace(int1d), int))
where
  reads(input)
do
  var box = ispace(int2d, { x = size, y = k })
  var lin = ispace(int1d, size)

  var possible_cost_table = region(box, int)
  var partition_table     = region(box, int)
  var p = region(lin, int)

  p[0] = input[0];

  for i = 1, size do
    p[i] = p[i - 1] + input[i];
  end
  for i = 0, size do
    possible_cost_table[{x = i, y = 0}] = p[i]
    partition_table[{x = i, y = 0}] = 0
  end
  for j = 1, k do
    possible_cost_table[{x = 0, y = j}] = input[0]
    partition_table[{x = 0, y = j}] = 0
  end
  for i = 1, size do
    for j = 1, k do
      possible_cost_table[{x = i, y = j}] = 1000000000
      for r = 0, i do
        var cost = max(possible_cost_table[{x = r, y = j - 1}], p[i] - p[r])
        if (possible_cost_table[{x = i, y = j}] > cost) then
            possible_cost_table[{x = i, y = j}] = cost
            partition_table[{x = i, y = j}] = r
        end
      end
    end
  end

  -- Now color each partition
  var coloring = c.legion_domain_coloring_create()
  var lo : int64[1], hi : int64[1]
  var n = size - 1
  var ki = k - 1
  while ki > 0 do
    lo[0] = partition_table[{x = n, y = ki}] + 1
    hi[0] = n
    c.legion_domain_coloring_color_domain(
      coloring,
      ki,
      c.legion_domain_from_rect_1d {
        lo = c.legion_point_1d_t { x = lo },
        hi = c.legion_point_1d_t { x = hi }
      }
    )
    n = partition_table[{x = n, y = ki}]
    ki = ki - 1
  end
  lo[0] = 0
  hi[0] = n
  c.legion_domain_coloring_color_domain(
    coloring,
    0, -- ki == 0
    c.legion_domain_from_rect_1d {
      lo = c.legion_point_1d_t { x = lo },
      hi = c.legion_point_1d_t { x = hi }
    }
  )
  var parts = partition(disjoint, input, coloring)
  c.legion_domain_coloring_destroy(coloring)

  return parts
end

task main()
  var size = 9
  var k = 3

  var isi = ispace(int1d, 9)
  var i = region(isi, int)
  i[0], i[1], i[2], i[3], i[4], i[5], i[6], i[7], i[8], i[9] = 4, 2, 1, 4, 5, 6, 7, 8, 9

  var parts = solve(9, 3, i)

  for part = 0, k do
    print_partition(parts[part])
  end

end

regentlib.start(main)
