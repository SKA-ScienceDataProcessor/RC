#!/bin/bash

./cloud3 slave localhost 60001 &
./cloud3 slave localhost 60002 &
./cloud3 slave localhost 60003 &
sleep 1
./cloud3 master localhost 44440
