#!/bin/sh
#
# Test script for running program on single computer

EXE=./dist/build/dot-product/dot-product

for PORT in 8080 8081 8082 8083; do
    $EXE slave localhost $PORT &
done

# Sleep to allow slaves to initialize
sleep 1
$EXE master localhost 8000
