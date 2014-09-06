#!/usr/bin/python
__author__ = 'mcsquaredjr'

import os

from dna_lib import *
import time
from string import Template
import subprocess


DEBUG = True
CMD = "$DDP $ROLE --filename INPUT --cad $CAD --ip $IP --port $PORT ../../INPUT $DDP_OPTS"


children = []
os.makedirs(get_ip())
old_dir = os.getcwd()
os.chdir(get_ip())
rls = roles()

children = []

for i, line in enumerate(my_lines()):
    child = os.fork()
    if child:
        children.append(child)
    else:
        port = int(MIN_PORT) + i + 1
        os.makedirs(str(port))
        os.chdir(str(port))
        if rls[i] == "master":
            time.sleep(5)
        print "I am in: ", str(port)

        cmd_str = Template(CMD).substitute(DDP=DDP, ROLE=rls[i], CAD=CAD_FILE, PORT=port, DDP_OPTS=DDP_OPTS, IP=get_ip())
        if DEBUG:
            print "Would execute: ", cmd_str
            print "Would rename:", "../../eventlog.{0}.{1}".format(get_ip(), port)
        else:
            subprocess.call(cmd_str, shell=True)
            os.rename(DDP + ".eventlog", "../../eventlog.{0}.{1}".format(get_ip(), port))
            os._exit(0)

os.chdir(old_dir)

for child in children:
    os.waitpid(child, 0)