#!/usr/bin/python
__author__ = 'mcsquaredjr'

import os

from dna_lib import *
import time
from string import Template
import subprocess


DEBUG = False

CMD_MASTER = "$DDP master --cad $CAD_FILE --ip $IP --port $PORT --filename ../../INPUT $DDP_OPTS"
CMD_SLAVE = "$DDP slave --cad $CAD_FILE --ip $IP --port $PORT $DDP_OPTS"
CMD_LOG = "$GHC_EVENTS show $IN > $OUT"

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
            cmd_str = Template(CMD_MASTER).substitute(DDP=DDP, CAD_FILE=CAD_FILE, PORT=port, DDP_OPTS=DDP_OPTS, IP=get_ip())
            print "Running master:", cmd_str
        else:
            cmd_str = Template(CMD_SLAVE).substitute(DDP=DDP, CAD_FILE=CAD_FILE, PORT=port, DDP_OPTS=DDP_OPTS, IP=get_ip())
            print "Running slave:", cmd_str

        #subprocess.call(cmd_str, shell=True)
        code = os.system(cmd_str)
        if code != 0:
            print "*** ERROR: calling {0}. Exiting with code {1}.".format(cmd_str, code)
            sys.exit(-1)

        src_name = os.path.split(DDP)[-1] + ".eventlog"
        dst_name = "../../eventlog.{0}.{1}".format(get_ip(), port)
        os.rename(src_name, dst_name)
        cmd_log = Template(CMD_LOG).substitute(GHC_EVENTS=GHC_EVENTS, IN=dst_name, OUT=dst_name + ".txt")
        #subprocess.call(cmd_log, shell=True)
        code = os.system(cmd_log)
        if code != 0:
            print "*** ERROR: calling {0}. Exiting with code {1}.".format(cmd_log, code)
            sys.exit(-1)


        os._exit(0)

os.chdir(old_dir)

for child in children:
    os.waitpid(child, 0)
