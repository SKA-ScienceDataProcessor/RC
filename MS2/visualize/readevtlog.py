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
        # Convert timestamp to seconds
        self.t   = t / 1e9
        self.tag = tag
        self.msg = msg

    def __str__(self) :
        return "t={}: [{}] {}".format(self.t, self.tag, self.msg)
    def __repr__(self) :
        return "LogEntry{{t={}: [{}] {}}}".format(self.t, self.tag, self.msg)


def read_log_entries(stream) :
    "Read stream of LogEntry from textual stream of eventlog"
    timestamp_re = re.compile(" *([0-9]+): (.*)")
    dna_re = re.compile("cap [0-9]+: ([A-Z]+) +(.*)")
    for line in stream :
        # Get timestamp
        m = timestamp_re.match(line)
        if m is None :
            continue
        t = int( m.group(1) )
        line = m.group(2)
        # Is tagged message in DNA format?
        m = dna_re.match(line)
        if m is None :
            tag = "RAW"
            msg = line
        else :
            tag = m.group(1)
            msg = m.group(2)
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
        return "{}-{} [{}] '{}'".format(self.t1,self.t2,self.pid,self.msg)
    def __repr__(self) :
        return "Interval{{{}-{} [{}] '{}'}}".format(self.t1,self.t2,self.pid,self.msg)
    def dt(self) :
        return self.t2 - self.t1

class Sync :
    "Sync event"
    def __init__(self,t,msg) :
        self.t = t
        m = re.match(r'([0-9]+\.[0-9]*) (.*)', msg)
        self.wall_clock = float(m.group(1))
        self.msg = m.group(2)

    def __str__(self) :
        return "t={} at {} '{}'".format(self.t, self.wall_clock, self.msg)
    def __repr__(self) :
        return "Sync{{t={} at {} '{}'}}".format(self.t, self.wall_clock, self.msg)

class Timeline :
    "Timeline of events for some particular node"
    def __init__(self, stream) :
        self.time     = []
        self.sync     = []
        self.messages = []
        self.raw      = []

        stack = {}
        # Build list of events
        regex = re.compile(r'\[([^\]]+)\] (.*)')
        def parse_msg( s ) :
            m = regex.match( e.msg )
            if m is None :
                raise Exception("Bad message")
            return (m.group(1), m.group(2))

        for e in stream :
            if e.tag == "SYNC" :
                self.sync.append(Sync(e.t, e.msg))
            elif e.tag == "MESSAGE" :
                self.messages.append(e)
            elif e.tag == "RAW" :
                self.raw.append(e);
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

    def get_sync_event_times(self, nm) :
        "Return sync events with given message"
        return [e for e in self.sync if e.msg == nm]
    def get_durations_for_name(self,nm) :
        "List of durations for given name"
        return [e for e in self.time if e.msg == nm]

    def get_args(self) :
        "Arguments passed to the program that produced the event-log"
        args_re = re.compile('capset [0-9]+: args: \["(.*)"\]')
        for e in self.raw :
            m = args_re.match(e.msg)
            if m is None:
                continue
            # Not quite 100% right, but it should do for now.
            return m.group(1).split('","')
        return []
    def get_env(self) :
        "Arguments passed to the program that produced the event-log"
        args_re = re.compile('capset [0-9]+: env: \["(.*)"\]')
        for e in self.raw :
            m = args_re.match(e.msg)
            if m is None:
                continue
            # See above. We additionally make a dictionary out of it.
            env = {}
            env_re = re.compile('(\w+)=(.*)')
            for s in m.group(1).split('","'):
                m = env_re.match(s)
                if m is None: continue
                env[m.group(1)] = m.group(2)
            return env
        return []


def offset_timelines(logs) :
    "Set minimal wall clock time in logs to 0"
    def min_t(timeline) :
        return min([e.wall_clock for e in timeline.sync])
    t0 = min([min_t(e) for e in logs.itervalues()])
    for timeline in logs.itervalues() :
        for e in timeline.sync :
            e.wall_clock -= t0

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
        res[int(d)] = t
    return res
