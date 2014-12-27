#!/usr/bin/python
"""
Print log for given DNA program run
"""

import os
import sys
from readevtlog import *

class EntryFilter :
    """
    Filter for log entries
    """
    def __init__(self, strs) :
        self.neg = [s[1:] for s in strs if s[0] == '-']
        self.pos = [s[1:] for s in strs if s[0] == '+']
    def isOK(self, e) :
        def and_(xs) : return reduce( lambda x, y: x and y, xs, True )
        def or_ (xs) : return reduce( lambda x, y: x or  y, xs, False)
        neg = [pat == e.tag for pat in self.neg]
        pos = [pat == e.tag for pat in self.pos]
        return not (or_(neg)) and and_(pos)

def filter_evt(f, items) :
    for i in items :
        if f.isOK(i) :
            yield i

home    = os.getenv('HOME')
logdir  = os.path.join(home,'_dna','logs',sys.argv[1])
filters = EntryFilter( sys.argv[2:] )

for d in sorted(os.listdir(logdir)) :
    print "====",d,"================"
    [nm] = os.listdir(os.path.join(logdir,d))
    nm = os.path.join(logdir,d,nm)
    for l in filter_evt(filters, read_log_entries(stream_eventlog(nm))) :
        print l
