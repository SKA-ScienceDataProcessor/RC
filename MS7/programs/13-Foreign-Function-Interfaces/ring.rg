terralib.includepath = terralib.includepath..";/usr/lib/openmpi/include"
terralib.linklibrary("/usr/lib/openmpi/lib/libmpi.so")

local c = terralib.includecstring [[
#include <stdio.h>
#include <mpi.h>

/* Terra can't deal with nontrivial macros */
MPI_Comm __COMM_WORLD(){return MPI_COMM_WORLD;}
MPI_Datatype __INT(){return MPI_INT;}
MPI_Status * __STATUS_IGNORE(){return MPI_STATUS_IGNORE;}
]]

terra main()
  var rank:int, size:int, nxt:int, prev:int, message:int
  var tag:int = 201

  c.MPI_Init([&int](0), [&&&int8](0))
  c.MPI_Comm_rank(c.__COMM_WORLD(), &rank)
  c.MPI_Comm_size(c.__COMM_WORLD(), &size)

  nxt = (rank + 1) % size
  prev = (rank + size - 1) % size

  if rank == 0 then
        message = 10
        c.printf("Process 0 sending %d to %d, tag %d (%d processes in ring)\n",
               message, nxt, tag, size)
        c.MPI_Send(&message, 1, c.__INT(), nxt, tag, c.__COMM_WORLD())
        c.printf("Process 0 sent to %d\n", nxt)
  end

  while true do

    c.MPI_Recv(&message, 1, c.__INT(), prev, tag, c.__COMM_WORLD(), c.__STATUS_IGNORE())
    if rank == 0 then
      message = message - 1
      c.printf("Process 0 decremented value: %d\n", message)
    end

    c.MPI_Send(&message, 1, c.__INT(), nxt, tag, c.__COMM_WORLD())
    if message == 0 then
      c.printf("Process %d exiting\n", rank)
      break
    end

  end

  if rank == 0 then
    c.MPI_Recv(&message, 1, c.__INT(), prev, tag, c.__COMM_WORLD(), c.__STATUS_IGNORE())
  end

  c.MPI_Finalize()
end

import "regent"

task toplevel()
  main()
end

regentlib.start(toplevel)
