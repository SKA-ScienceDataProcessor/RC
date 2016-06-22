Profiling information
=====================

Requirements
------------

Documentation is [here](https://docs.google.com/document/d/1qK4YqM_avtN62ijsy_3F69nZDjgOM7FiInmVofIVvNQ/edit?ts=57093891#heading=h.gifm5sc1d7ck).

Implementation
--------------

Our build system supports use of Legion runtime profiling.

To compile program with profiling enabled, use PROFILING=1 as command argument for make:

    PROFILING=1 NET=ibv NODES=3 TASKS=12 make clean run

Please note that Legion uses compile-time rejection test for different message levels.
Profiling messages are at the level LEVEL_DEBUG. The default level is LEVEL_PRINT (much higher).
When you enable profiling you have to make all Legion objects from clean state,
otherwise information might be lost.

If you do not change the profiling flag but changed source code, you can remake your program:

    PROFILING=1 NET=ibv NODES=2 TASKS=12 make run

In this case information will not be lost.