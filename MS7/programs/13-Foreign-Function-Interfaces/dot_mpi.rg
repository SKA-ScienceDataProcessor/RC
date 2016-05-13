terralib.includepath = terralib.includepath..";/usr/lib/openmpi/include"
terralib.linklibrary("/usr/lib/openmpi/lib/libmpi.so")

local c = terralib.includecstring [[
#include <stdio.h>
#include <mpi.h>

/* Terra can't deal with nontrivial macros */
MPI_Comm __COMM_WORLD(){return MPI_COMM_WORLD;}
MPI_Datatype __DBL(){return MPI_DOUBLE;}
]]

terra main()

  var loc_dot:double, dot:double
  var a:double[31], b:double[31], loc_dots:double[31]

  var my_rank:int, p:int, n:int, tag:int, loc_n:int
  var en:int, bn:int

  n = 8
  tag = 50

  for i : int = 0, n+1 do
    a[i] = i
    b[i] = i+1
  end

  c.MPI_Init([&int](0), [&&&int8](0))
  c.MPI_Comm_rank(c.__COMM_WORLD(), &my_rank)
  c.MPI_Comm_size(c.__COMM_WORLD(), &p)

   loc_n = n/p
   bn = 1+(my_rank)*loc_n
   en = bn + loc_n-1
   c.printf("my_rank = %d loc_n = %d\n",my_rank,loc_n)
   c.printf("my_rank = %d bn = %d\n",my_rank,bn)
   c.printf("my_rank = %d en = %d\n",my_rank,en)
   loc_dot = 0.0

  for i : int = bn, en+1 do
    loc_dot = loc_dot + a[i]*b[i]
  end

  c.printf("my_rank = %d loc_dot = %f\n", my_rank, loc_dot)

  c.MPI_Gather( &loc_dot,  1, c.__DBL()
              , &loc_dots, 1, c.__DBL()
              , 0, c.__COMM_WORLD())

  if my_rank == 0 then
    dot = loc_dot;
   	for source:int = 1, p do
      dot = dot + loc_dots[source];
    end
    c.printf( "dot product = %f",dot)
  end
  c.MPI_Finalize()

end

import "regent"

task toplevel()
  main()
end

regentlib.start(toplevel)
