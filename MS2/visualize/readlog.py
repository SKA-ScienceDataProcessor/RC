#!/usr/bin/python
"""
Functions and classes for reading logs
"""
import re
import os


## ================================================================
## Reading of logs

class LogEntry(object) :
    "Single entry of the log"
    def __init__(self, line) :
        m = re.match(r'(\d+\.\d+) ([^ ]*) \[([^ \]]*)\]: (.*)', line)
        self.t   = float( m.group(1) )
        self.pid = m.group(2)
        self.tag = m.group(3)
        self.msg = m.group(4)
    def __str__(self) :
        return '{} {} [{}] {}'.format( self.t, self.pid, self.tag, self.msg )
    def __repr__(self) :
        return self.__str__()

def read_log(fname) :
    "Generator which reads log entries from file"
    with open(fname) as f :
        while True :
            line = f.readline()
            if line == '' :
                break
            yield LogEntry(line)

def group_by_pid(log) :
    "Group log entries by PID of process"
    dct = {}
    for e in log :
        a = dct.get(e.pid, [])
        a.append( e )
        dct[e.pid] = a
    return dct

def strip_acp(dct) :
    "Remove ACP processes from log"
    for k in dct.keys() :
        if not [ e for e in dct[k] if e.tag <> "ACP" ] :
            dct.pop(k)


################################################################
## Intervals

class Interval(object) :
    def __init__(self,e) :
        self.children = []
        self.t1  = e.t
        self.pid = e.pid
        self.msg = e.msg
    def __str__(self) :
        return '{}-{} {} {}'.format( self.t1, self.t2, self.pid, self.msg )
    def __repr__(self) :
        return self.__str__()
    def dt(self) :
        return self.t2 - self.t1
        

def build_timeline( entries ) :
    "Arrange timing events into tree"
    res   = []
    stack = []
    for e in entries :
        if e.tag == 'START' :
            if stack :
                a = Interval(e)
                stack[-1].children.append(a)
                stack.append(a)
            else :
                a = Interval(e)
                res.append(a)
                stack.append(a)
        elif e.tag == 'END' :
            a = stack.pop()
            a.t2 = e.t
        else :
            # Something other
            res.append( e )
    return res


## ================================================================
## read logs for whole DNA program

def read_dna_logs(dir) :
    res = {}
    for d in os.listdir(dir) :
        log = group_by_pid( read_log( os.path.join(dir,d,"log") ) )
        strip_acp( log )
        for k in log.keys() :
            log[k] = build_timeline( log[k] )
        res[d] = log
    return res
