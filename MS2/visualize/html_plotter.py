#!/usr/bin/python
"""
Program for creating HTML plots
"""

import os
import sys

from readevtlog import *

def write_prefix():
    f.write('''
<!doctype html>
<html>
<head>
  <script src="http://code.jquery.com/jquery-latest.min.js"></script>
  <script src="http://d3js.org/d3.v3.min.js" charset="utf-8"></script>
  <script src="d3-timeline/src/d3-timeline.js"></script>
  <script type="text/javascript">
    window.onload = function() {
      var data = [
    ''')

def write_suffix():
    f.write('''
        ];
      var chart = d3.timeline()
        .beginning(-1)
        .tickFormat(
           { format: d3.time.format("%M:%S.%L"),
             tickInterval: 1,
             tickTime: d3.time.second,
             tickSize: 5 })
        .stack();

      var svg = d3.select("#timeline").append("svg").attr("width", "1000")
        .datum(data).call(chart);
    }
  </script>
  <style type="text/css">
    .axis path,
    .axis line {
      fill: none;
      stroke: black;
      shape-rendering: crispEdges;
    }

    .axis text {
      font-family: sans-serif;
      font-size: 10px;
    }

    .timeline-label {
      font-family: sans-serif;
      font-size: 12px;
    }
  </style>
</head>
<body>
  <div>
    <h3>Timeline</h3>
    <div id="timeline"></div>
  </div>
</body>
</html>
    ''')

def timeline(logs) :
    "Timeline of execution"

    n = 0
    for k in sorted(logs) :
        tt = logs[k]
        for e in tt.time :
            f.write('       {times: [{"starting_time": %g, "ending_time": %g, "label": "%s:%s"}]},\n'
                    % (1000*e.t1, 1000*e.t2, e.pid, e.msg) )

commands = {
    "timeline"  : timeline,
    }

cmd  = sys.argv[1]
nm   = sys.argv[2]
out  = sys.argv[3]

logs = read_timelines(nm)
f    = open(out, 'w')
write_prefix()

commands[cmd](logs, *sys.argv[4:])

write_suffix()
f.close()
