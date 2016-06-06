local c = terralib.includecstring [[ 
  #include <stdio.h>
  #include <stdlib.h>
  #include <legion_c.h>
]]

import "regent"

terra print_val(x : int, y : int, v : double)
  c.printf("o[%d][%d]: %10.2f\n", x, y, v)
end

terra puts(s: rawstring)
  c.printf("%s\n", s)
end

task trans( i: region(ispace(int2d), double)
          , o: region(ispace(int2d), double)
          )
where
  reads(i),
  writes(o)
do
  for rc in i.ispace do
      var co = int2d{x = rc.y, y = rc.x}
      o[co] = i[rc]
  end
end

-- true - "vertical" partitioning, false - "horizontal" partitioning
task structured_partition(r : region(ispace(int2d), double), dir : bool, rx : int64, ry : int64, pieces : int64)
  var qr : c.lldiv_t = c.lldiv(rx, pieces) -- need rsize >= pieces
  var small_size = qr.quot
  var big_size = small_size + 1
  var big_pieces = qr.rem

  var d0 : int, d1 : int
  if dir
  then d0, d1 = 0, 1
  else d0, d1 = 1, 0
  end

  var coloring = c.legion_domain_coloring_create()

  var curr_lo : int64[2], curr_hi : int64[2]
  curr_lo[d1] = 0
  curr_hi[d1] = ry - 1

  curr_lo[d0] = 0
  for cb = 0, big_pieces do
    curr_hi[d0] = curr_lo[d0] + big_size - 1 -- can "optimize" using small_size
    c.legion_domain_coloring_color_domain(
      coloring,
      cb,
      c.legion_domain_from_rect_2d {
        lo = c.legion_point_2d_t { x = curr_lo },
        hi = c.legion_point_2d_t { x = curr_hi }
      }
    )
    curr_lo[d0] = curr_lo[d0] + big_size
  end
  for cs = big_pieces, pieces do
    curr_hi[d0] = curr_lo[d0] + small_size - 1
    c.legion_domain_coloring_color_domain(
      coloring,
      cs,
      c.legion_domain_from_rect_2d {
        lo = c.legion_point_2d_t { x = curr_lo },
        hi = c.legion_point_2d_t { x = curr_hi }
      }
    )
    curr_lo[d0] = curr_lo[d0] + small_size
  end

  var p = partition(disjoint, r, coloring)
  
  c.legion_domain_coloring_destroy(coloring)

  return p
end

task test(r: int, c : int, k : int)
  var isi = ispace(int2d, { x = r, y = c })
  var iso = ispace(int2d, { x = c, y = r })

  var i = region(isi, double)
  var o = region(iso, double)

---[[
  puts("Original:")
  for rc in isi do
    i[rc] = [double](rc.x) * 2.0 + [double](rc.y) * 3.0
    -- print_val(rc.x, rc.y, i[rc])
  end
---]]

  var pi = structured_partition(i, true,  r, c, k)
  var po = structured_partition(o, false, r, c, k)

  for n =0, k do
    trans(pi[n], po[n])
  end

---[[
  for n = 0, k do
    puts("Input partitioned")
    for cr in pi[n].ispace do
      print_val(cr.x, cr.y, i[cr])
    end
  end

  for n = 0, k do
    puts("Output partitioned")
    for cr in po[n].ispace do
      print_val(cr.x, cr.y, o[cr])
    end
  end
---]]

end

task main()
  test(7, 5, 2)
end
regentlib.start(main)
