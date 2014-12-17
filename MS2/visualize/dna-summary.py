#!/usr/bin/python
"""
Print log for given DNA program run
"""

import os
import sys
from readevtlog import *

home   = os.getenv('HOME')
logdir = os.path.join(home,'_dna','logs',sys.argv[1])
for d in sorted(os.listdir(logdir)) :
    print "====",d,"================"
    [nm] = os.listdir(os.path.join(logdir,d))
    nm = os.path.join(logdir,d,nm)
    for l in read_log_entries(stream_eventlog(nm)) :
        print l
