#!/usr/bin/python
__author__ = 'mcsquaredjr'

from dna_lib import *
from string import Template
import subprocess

CMD = "../create-floats INPUT $ITEMCOUNT $CHUNKCOUNT $CHUNKNO $IP:$PORT"

lines = my_lines()
myip = get_ip()
chunks = chunk_numbers()

for i, line in enumerate(lines):
    port = int(MIN_PORT) + i + 1
    cmd_str = Template(CMD).substitute(ITEMCOUNT=ITEMCOUNT, CHUNKCOUNT=chunk_count(), CHUNKNO=chunks[i], IP=myip, PORT=port)
    print cmd_str
    subprocess.call(cmd_str, shell=True)
