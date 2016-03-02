#!/usr/bin/python
"""
Program for creating HTML plots
"""

import os
import sys
import json
import time

from readevtlog import *

def imaging_iters(logs):

    start_time = 40.0
    start_msg = "kernel init"
    end_msg = "imaging cleanup"

    got_start = False
    for k in sorted(logs):
        tt = logs[k].time
        for e in tt :
            if e.msg == start_msg:
                start = e.t1
                got_start = True
            if got_start and e.msg == end_msg:
                print e.t2-start, ",",
        print ""

data_commands = {
    "imaging_iters"  : imaging_iters,
    }

# Get parameters
cmd  = sys.argv[1]
nm   = sys.argv[2]

# Open input files
logs = read_timelines(nm)

# Write table
data_commands[cmd](logs)
