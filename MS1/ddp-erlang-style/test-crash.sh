#!/bin/bash
ln dist/build/ddp-erlang-style-SIMD-eventlog/ddp-erlang-style-SIMD-eventlog ddp-erlang-style
./create-floats INPUT 30 2 1
./create-floats INPUT 30 2 2
./ddp-erlang-style slave --ip localhost --port 60001 &
./ddp-erlang-style slave --ip localhost --port 60002 &
./ddp-erlang-style slave --ip localhost --port 60003 &
sleep 1
./ddp-erlang-style master --crash --ip localhost --port 44440 --filename INPUT > out
