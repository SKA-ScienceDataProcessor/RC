-- slurm.rg
--
-- SLURM interoperability simple example.
--
-- Copyright (C) Braam Research, 2016

import "regent"

-- Regent's C library:
local c = regentlib.c;

-- Terra's interface for gethostname.
local unistd = terralib.includec("unistd.h")

-- and pid_t type
local systypes = terralib.includec("sys/types.h")

-- Use Terra's C interface to get and print host name.
terra print_hostname()
    var hostname : int8[1024]
    hostname[0] = 0
    unistd.gethostname(hostname, 1024)
    c.printf("hostname: %s\n", hostname)
end

-- Use Terra's C interface to get and print PID.
terra print_pid()
    var pid : systypes.pid_t
    pid = unistd.getpid()
    c.printf("pid: %d\n", pid)
end

-- combine printing.
task print_pid_nodename()
    print_hostname()
    print_pid()
end

-- Main entry point.
task main()
    c.printf("in main\n")
    print_pid_nodename()
end

regentlib.start(main)
