#!/usr/bin/python

__author__ = 'mcsquaredjr'

from dna_lib import NODE_FILE, CAD_FILE, PROCS_PER_NODE, MIN_PORT, get_ip
import socket

with open(NODE_FILE, "r") as nodes:
    with open(CAD_FILE, "w") as cad:
        for line in nodes:
            if line.strip() == "localhost" or "127.0.0.1":
                ip = socket.gethostbyname(socket.gethostname())
            else:
                ip = socket.gethostbyname(line.strip())
            for proc in range(1, int(PROCS_PER_NODE) + 1):
                cad.write(ip + ":" + str(int(MIN_PORT) + proc) + "\n")

