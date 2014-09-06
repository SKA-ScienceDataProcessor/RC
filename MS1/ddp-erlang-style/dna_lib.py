__author__ = 'mcsquaredjr'

import os
import socket

node_file = os.environ["NODES"]
cad_file = os.environ["CAD"]
procs_per_nod = os.environ["PROCS_PER_NODE"]
itemcount = os.environ["ITEMCOUNT"]
ddp = os.environment["DDP"]



def my_lines():
    ip = socket.gethostbyname(socket.gethostname())
    with open(cad_file, "r") as cad:
        lines = []
        for line in cad:
            ip_str, port = line.split(":")
            if ip_str == str(ip):
                lines.append(line)


def chunk_number(i):
    if i == 0 or i == 1:
        return 0
    else:
        return i - 1


def chunk_count(i):
    with open(cad_file) as cad:
        for i, l in enumerate(cad):
            pass
    return i + 1 - 2