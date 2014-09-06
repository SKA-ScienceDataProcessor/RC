#!/usr/bin/python

__author__ = 'mcsquaredjr'

from dna_lib import NODE_FILE, CAD_FILE, PROCS_PER_NODE, MIN_PORT, get_ip

with open(NODE_FILE, "r") as nodes:
    with open(CAD_FILE, "w") as cad:
        for line in nodes:
            ip = get_ip()
            for proc in range(1, PROCS_PER_NODE + 1):
                cad.write(ip + ":" + str(MIN_PORT + proc) + "\n")

