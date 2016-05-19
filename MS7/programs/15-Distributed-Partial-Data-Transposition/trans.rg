local c = terralib.includecstring [[ 
  #include <stdio.h>
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

task test(r: int, c : int, k : int)
  var isi = ispace(int2d, { x = r, y = c })
  var iso = ispace(int2d, { x = c, y = r })

  var i = region(isi, double)
  var o = region(iso, double)

  puts("Original:")
  for rc in isi do
    i[rc] = [double](rc.x) * 2.0 + [double](rc.y) * 3.0
    print_val(rc.x, rc.y, i[rc])
  end

  var si = ispace(int2d, { x = r, y = k })
  var pi = partition(equal, i, si)

  -- parallel
  for s in si do
    var isub = pi[s]
    trans(isub, o)
  end

  puts("Transposed:")
  for cr in iso do
    print_val(cr.x, cr.y, o[cr])
  end
end

task main()
  test(7, 5, 2)
end
regentlib.start(main)
