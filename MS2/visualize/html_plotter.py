#!/usr/bin/python
"""
Program for creating HTML plots
"""

import os
import sys
import json
import time

from readevtlog import *

def write_prefix():
    f.write('''
<!doctype html>
<html>
<head>
  <title>DNA performance report</title>
  <script src="http://code.jquery.com/jquery-latest.min.js"></script>
  <script src="http://d3js.org/d3.v3.min.js" charset="utf-8"></script>
  <script src="d3-timeline/src/d3-timeline.js"></script>
  <script type="text/javascript">
    window.onload = function() {
    ''')

def make_class_name(name) :
    "Makes a valid CCS class name out of the given string"
    # I have a feeling this won't work for long...
    return re.sub('[^-_A-Za-z0-9]+', '', name)

def write_timeline_data(logs, conf) :
    "Timeline of execution, data generation"
    n = 0

    # Collect all delay types
    delays = set()
    for k in sorted(logs) :
        for e in logs[k].time :
            if conf.get(e.msg,{}).get('ignore', False) :
                continue
            delays.add(e.msg)

    f.write('''
      var colorScale = d3.scale.category20().domain({});'''.format(list(delays)))

    f.write('''
      var color; var elems; var i;''')
    for a in delays:
        f.write('''
      color = colorScale("%s");
      elems = document.querySelectorAll(".%s");
      console.log(elems.length);
      for (i = 0; i < elems.length; i++)
        elems[i].style.backgroundColor = color;'''
            % (a, make_class_name(a)));

    f.write('''
      var data = [''')
    for k in sorted(logs) :
        def pid_func(e) : return e.pid
        tt = sorted(logs[k].time, key=pid_func)
        pid = None
        end = 0
        for e in tt :

            # Get configuration, check whether we're suppoed to ignore
            # this one.
            if conf.get(e.msg,{}).get('ignore', False) :
                continue

            # Create row
            f.write('\n            {"label": "%s", type:"%s", times:['
                    % ('' if e.pid == pid else e.pid,e.msg))
            pid = e.pid

            # Make sure we have a certain minimum width. This is a hack.
            end = e.t2
            if end == None or end < e.t1+0.005:
                end = e.t1 + 0.005

            # Write entry
            f.write('''
                {"starting_time": %g, "ending_time": %g, "label": "%s", "type": "%s"},'''
                    % (1000*e.t1, 1000*end, e.msg, e.msg))
            f.write('\n            ]},')

    f.write('''
        ];''')


def write_middle():
    f.write('''
      var chart = d3.timeline()
        .beginning(-1)
        .tickFormat(
           { format: d3.time.format("%M:%S"),
             tickInterval: 1,
             tickTime: d3.time.second,
             tickSize: 5 })
        .stack()
        .colorProperty('type')
        .colors(colorScale)
        .margin({left:150, right:150, top:0, bottom:0});

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

    table, th, td {
      padding: 3px;
    }
    table {
      border-collapse: collapse;
    }
    td.key {
      font-weight: bold;
      font-family: serif;
      text-align: right;
      min-width: 150px;
    }
    table.program-conf td.val {
      font-family: monospace;
    }
    table.statistics td {
      text-align: center;
      min-width: 80px;
    }

    h4 {
      box-shadow: 0 1px 0 rgba(0,0,0,0.1);
    }
  </style>
</head>
<body>
  <div>''')

def write_timeline_body(logs, conf) :
    "Timeline of execution, html body"

    # Get arguments (we assume they are the same for all nodes - which
    # right now they often are)
    args = logs[0].get_args()
    f.write('''
    <h4>Program Configuration</h4>
    <table class="program-conf">
      <tr><td class="key">Executable:</td><td class="val">{}</td></tr>
      <tr><td class="key">Arguments:</td><td class="val">{}</td></tr>
    </table>'''
            .format(args[0], ' '.join(args[1:])))

    # Get program environment (see above)
    env = logs[0].get_env()
    if env.has_key('SLURM_STEP_NUM_TASKS') :
        f.write('''
    <h4>SLURM Configuration</h4>
    <table class="slurm_conf">
      <tr><td class="key">Job:</td><td>{} {}, started by {}</td></tr>
      <tr><td class="key">Nodes:</td><td>{}: {}</td></tr>
      <tr><td class="key">Tasks:</td><td>{} = {}</td></tr>
      <tr><td class="key">Procs:</td><td>{}</td></tr>
      <tr><td class="key">CPUS:</td><td>{}</td></tr>
    </table>'''
                .format(env.get('SLURM_JOB_NAME', '-'),
                        env.get('SLURM_JOB_UID', '-'),
                        env.get('SLURM_JOB_USER', '-'),
                        env.get('SLURM_NNODES', '-'),
                        env.get('SLURM_JOB_NODELIST', '-'),
                        env.get('SLURM_NTASKS', '-'),
                        env.get('SLURM_TASKS_PER_NODE', '-'),
                        env.get('SLURM_NPROCS', '-'),
                        env.get('SLURM_JOB_CPUS_PER_NODE', '-')))

    # Show timeline
    f.write('''
    </table>
    <h4>Timeline</h4>
    <div id="timeline"></div>''')

    # Build stats
    instances = {}
    total_time = {}
    for k in logs :
        t = 0
        for e in logs[k].time :
            instances[e.msg] = 1 + instances.get(e.msg, 0)
            if e.t2 != None :
                total_time[e.msg] = (e.t2 - e.t1) + total_time.get(e.msg, 0)

    # Make table
    def format_rate(rate) :
        if rate < 10000 :
            return '%.2f ' % rate
        if rate < 10000000 :
            return '%.2f k' % (rate / 1000)
        if rate < 10000000000 :
            return '%.2f M' % (rate / 1000000)
        if rate < 10000000000000 :
            return '%.2f G' % (rate / 1000000000)
        return '%.2f T' % (rate / 1000000000000)
    f.write('''
    </table>
    <h4>Statistics</h4>
    <table class="statistics">
      <tr><td></td><th>Instances</th><th>Time</th><th colspan=2>IO</th><th colspan=2>FLOPS</th></tr>''')

    for a in instances.iterkeys() :
        # Get configuration for this key
        econf = conf.get(a, {})
        if econf.get('ignore', False) :
            continue

        # Calculate rates
        io_sum = '-'; io_rate = '-'
        if econf.has_key('io') :
            io_sum = format_rate(econf['io']) + "B"
            io_rate = format_rate(econf['io'] / total_time[a]) + "B/s"
        flop_sum = '-'; flop_rate = '-'
        if econf.has_key('flop') :
            flop_sum = format_rate(econf['flop']) + "FLOP"
            flop_rate = format_rate(econf['flop'] / total_time[a]) + "FLOP/s"

        # Print row
        f.write('''
      <tr><td class='%s key'>%s</td><td>%d</td><td>%02d:%02d.%03d</td>
          <td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>'''
                % (make_class_name(a), a, instances[a],
                   int(total_time[a] / 60),
                   int(total_time[a]) % 60,
                   int(total_time[a] * 1000) % 1000,
                   io_sum, io_rate, flop_sum, flop_rate))
    f.write('''
    </table>''')

def write_suffix():
    f.write('''
  </div>
</body>
</html>''')

data_commands = {
    "timeline"  : write_timeline_data,
    }
body_commands = {
    "timeline"  : write_timeline_body,
    }

# Get parameters
cmd  = sys.argv[1]
nm   = sys.argv[2]
out  = sys.argv[3]

# Open input and ouput files
logs = read_timelines(nm)
f    = open(out, 'w')

# Load configuration file, if any
if len(sys.argv) > 4:
    conf_file = open(sys.argv[4], "r")
    conf = json.load(conf_file)
    conf_file.close()
else :
    conf = {}

# Compose HTML file
write_prefix()
data_commands[cmd](logs, conf)
write_middle()
body_commands[cmd](logs, conf)
write_suffix()
f.close()
