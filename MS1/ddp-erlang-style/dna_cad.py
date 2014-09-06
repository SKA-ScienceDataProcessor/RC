__author__ = 'mcsquaredjr'

from dna_lib import NODE_FILE, CAD_FILE, PROC_PER_NODE, MIN_PORT, get_ip


with open(NODE_FILE, "r") as nodes:
    with open(CAD_FILE, "w") as cad:
        for line in nodes:
            ip = get_ip(line)
            for proc in range(1, PROC_PER_NODE + 1):
                cad.write(ip + ":" + str(MIN_PORT + proc) + "\n")

