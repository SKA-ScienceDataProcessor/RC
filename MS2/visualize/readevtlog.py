#!/usr/bin/python
"""
Reader for event log
"""

import re
import os
import subprocess

## ================================================================
## Basic eventlog reading
## ================================================================

class LogEntry :
    """Single entry in the event log which corresponds to user message.
       It contain timestamp, message tag and raw message
    """
    def __init__(self,t,tag,msg) :
        self.t   = t
        self.tag = tag
        self.msg = msg

    def __str__(self) :
        return "t={}: [{}] {}".format(self.t, self.tag, self.msg)
    def __repr__(self) :
        return "LogEntry{{t={}: [{}] {}}}".format(self.t, self.tag, self.msg)


def read_log_entries(stream) :
    "Read stream of LogEntry from textual stream of eventlog"
    regex = re.compile("([0-9]+): cap [0-9]+: ([A-Z]+) +(.*)")
    for line in stream :
        m = regex.match(line)
        if m is None :
            continue
        t   = int( m.group(1) )
        tag = m.group(2)
        msg = m.group(3)
        yield LogEntry(t,tag,msg)


def stream_eventlog(fname) :
    "Read eventlog using ghc-event unility line by line"
    p = subprocess.Popen(['ghc-events','show',fname], stdout=subprocess.PIPE)
    while True :
        l = p.stdout.readline()
        if l == '' :
            break
        yield l
    p.wait()



## ================================================================
## Classify eventlog events
## ================================================================


class Interval :
    "Time interval"
    def __init__(self,t1,t2,pid,msg) :
        self.t1  = t1
        self.t2  = t2
        self.pid = pid
        self.msg = msg
    def __str__(self) :
        return "{}-{} [{}] {}".format(self.t1,self.t2,self.pid,self.msg)
    def __repr__(self) :
        return "Interval{{{}-{} [{}] {}}}".format(self.t1,self.t2,self.pid,self.msg)
    def dt(self) :
        return self.t2 - self.t1


class Timeline :
    "Timeline of events for some particular node"
    def __init__(self, stream) :
        self.time     = []
        self.sync     = []
        self.messages = []

        stack = {}
        # Build list of events
        regex = re.compile(r'\[([^\]]+)\] (.*)')
        def parse_msg( s ) :
            m = regex.match( e.msg )
            if m is None :
                raise Exception("Bad message")
            return (m.group(1), m.group(2))
            
        for e in stream :
            if e.tag == "SYNCHRONIZATION" :
                self.sync.append(e)
            elif e.tag == "MESSAGE" :
                self.messages.append(e)
            elif e.tag == "START" :
                pid,msg = parse_msg( e.msg )
                i = Interval(e.t, None, pid, msg )
                self.time.append(i)
                if stack.has_key( (pid,msg) ) :
                    stack[(pid,msg)].append(i)
                else :
                    stack[(pid,msg)] = [i]
            elif e.tag == "END" :
                pid,msg = parse_msg( e.msg )
                l = stack.pop((pid,msg))
                l[-1].t2 = e.t
                l.pop()
                if l :
                    stack[(pid,msg)] = l

                
def read_timelines(dir) :
    "read dictionary of timelines"
    res = {}
    home = os.getenv("HOME")
    dir  = os.path.join(home,'_dna','logs',dir)
    for d in os.listdir(dir) :
        logd = os.path.join(dir,d)
        [nm] = os.listdir(logd)
        nm   = os.path.join(logd,nm)
        t = Timeline( read_log_entries(stream_eventlog(nm) ) )
        res[d] = t
    return res