__author__ = 'mcsquaredjr'

import os

node_file = os.environ["NODES"]
cad_file = os.environ["CAD"]
procs_per_nod = os.environ["PROCS_PER_NODE"]


def my_lines(ip):
    with open(cad_file, "r") as cad:
        lines = []
        for line in cad:
            ip, port = line.split(":")
            if ip == str(ip):
                line.append(line)