#!/usr/bin/python
"""
Program for creating plots
"""

import os
import sys
import matplotlib
import matplotlib.pyplot as plt

from readevtlog import *

## ================================================================
## Plotting commands
## ================================================================

def start_time(logs) :
    "Plot start time of CH processes"
    def candle(i, t, col) :
        plt.plot( [t,t], [i-0.5,i+0.5], color=col, lw=2)

    offset_timelines(logs)
    for k in sorted(logs) :
        tt   = logs[k]
        unix = tt.get_sync_event_times("UNIX")
        ch   = tt.get_sync_event_times("CH")
        for i in unix :
            candle(k, i.wall_clock * 1000, 'black')
        for i in ch :
            candle(k, i.wall_clock * 1000, 'red')

def ddp_timings(logs) :
    "Plot timings for all CH processes"
    cols = {
        'vector slice'    : 'black',
        'receive compute' : 'red',
        'receive read'    : 'blue',
        'compute vector'  : 'green',
        'read vector'     : 'magenta',
        'compute sum'     : 'blue',
        'collecting vectors' : 'black',
    }
    n = 0
    for k in sorted(logs) :
        tt = logs[k]
        for e in tt.time :
            plt.plot( [e.t1, e.t2], [n,n], color=cols[e.msg] )
            n += 1
    plt.ylim( [-1, n] )



def simple_bar(logs, nm) :
    def gen_bar() :
        for k in sorted(logs) :
            tt = logs[k]
            es = tt.get_durations_for_name(nm)
            if es :
                yield es[0].dt()
            else :
                yield 0
    #
    plt.title(nm)
    times = [t for t in gen_bar()]
    xs    = range(len(times))
    print xs
    print times
    plt.bar( xs, times, color="red", edgecolor="none")
    plt.xlim([-1, len(xs)+1])


## ================================================================
## Main
## ================================================================

commands = {
    "start_time"  : start_time,
    "simple_bar"  : simple_bar,
    "ddp_timings" : ddp_timings
    }

nm   = sys.argv[1]
cmd  = sys.argv[2]
logs = read_timelines(nm)
commands[cmd](logs, *sys.argv[3:])
plt.show()
