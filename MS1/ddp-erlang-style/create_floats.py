#!/usr/bin/python
__author__ = 'mcsquaredjr'

from dna_lib import *
from string import Template
import subprocess

CMD = "../create-floats INPUT $ITEMCOUNT $CHUNKCOUNT $CHUNKNO"

lines = my_lines()
myip = get_ip()
chunks = chunk_numbers()

for i, line in enumerate(lines):
    port = int(MIN_PORT) + i + 1
    msg = "PYTHON DEBUG: IP: {0}\tPORT: {1}\tCHUNK NUMBER:{2}\t"
    print msg.format(myip, port, chunks[i])
    cmd_str = Template(CMD).substitute(ITEMCOUNT=ITEMCOUNT, CHUNKCOUNT=chunk_count(), CHUNKNO=chunks[i])
    print cmd_str
    subprocess.call(cmd_str, shell=True)
