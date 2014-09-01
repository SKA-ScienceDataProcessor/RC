#!/bin/bash



#!/bin/sh
cat >cad.example.txt <<EOF
localhost:44440
localhost:60001
localhost:60002
localhost:60003
EOF


ln dist/build/ddp-erlang-style-SIMD-eventlog/ddp-erlang-style-SIMD-eventlog ddp-erlang-style
./create-floats INPUT 30 2 1
./create-floats INPUT 30 2 2
./ddp-erlang-style slave --cad cad.example.txt --ip localhost --port 60001 &
./ddp-erlang-style slave --cad cad.example.txt --ip localhost --port 60002 &
./ddp-erlang-style slave --cad cad.example.txt --ip localhost --port 60003 &
sleep 1
./ddp-erlang-style master --cad cad.example.txt --ip localhost --port 44440 --filename INPUT
