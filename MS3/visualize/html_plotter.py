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
  <script src="d3-timeline.js"></script>
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
        def pid_func(e) : return e.tags.get('pid',e.tags2.get('pid',''))
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
                    % ('' if pid_func(e) == pid else pid_func(e),e.msg))
            pid = pid_func(e)

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
             tickInterval: 5,
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
    if env.has_key('SLURM_NODEID') :
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
    total_hints = {}
    total_tags = {}
    total_tags_time = {}
    for k in logs :
        t = 0
        for e in logs[k].time :
            instances[e.msg] = 1 + instances.get(e.msg, 0)
            if e.t2 != None :
                total_time[e.msg] = (e.t2 - e.t1) + total_time.get(e.msg, 0)

            cur_hints = total_hints.get(e.msg, {})
            cur_tags = total_tags.get(e.msg, {})
            cur_tags_time = total_tags_time.get(e.msg, {})
            hint_re = re.compile(r'hint:')
            for t in e.tags.iterkeys():
                if hint_re.match(t) != None:
                    cur_hints[t] = int(e.tags[t]) + cur_hints.get(t, 0)
            for t in e.tags2.iterkeys():
                if hint_re.match(t) == None:
                    d = e.diff(t)
                    if d != None: cur_tags[t] = d + cur_tags.get(t, 0)
                    d = e.diff_t(t)
                    if d != None: cur_tags_time[t] = d + cur_tags_time.get(t, 0)
            total_hints[e.msg] = cur_hints
            total_tags[e.msg] = cur_tags
            total_tags_time[e.msg] = cur_tags_time


    # Make table
    f.write('''
    </table>
    <h4>Statistics</h4>
    <table class="statistics">
      <tr><td></td><th>Instances</th><th>Time</th><th colspan=4>IO</th><th colspan=4>Instructions</th></tr>''')

    def format_num(rate) :
        if rate < 1000 :
            return '%d ' % rate
        if rate < 1000000 :
            return '%.2f k' % (float(rate) / 1000)
        if rate < 1000000000 :
            return '%.2f M' % (float(rate) / 1000000)
        if rate < 1000000000000 :
            return '%.2f G' % (float(rate) / 1000000000)
        return '%.2f T' % (float(rate) / 1000000000000)
    class Metric:
        "Performance metric"
        def __init__(self, name, val, hint, time, unit):
            self.name = name; self.val = val; self.hint=hint
            self.time = time; self.unit = unit
        def valid(self):
            return (self.val != None and self.val > 0) or self.hint != None
        def format_name(self):
            if self.name == None: return '';
            else: return self.name + ':'
        def format_val(self):
            if self.name == None: return ''
            if self.val == None: return '-'
            return format_num(self.val) + self.unit
        def format_hint(self):
            if self.hint == None: return '';
            if self.val == 0 or self.val == None:
                return '[' + format_num(self.hint) + self.unit + ']'
            return '[%.1f%%]' % (float(100) * self.val / self.hint);
        def format_rate(self):
            if self.time == None: return ''
            if self.val == 0 or self.val == None:
                if self.hint != None and self.hint != 0:
                    return '[' + format_num(self.hint / self.time) + self.unit + '/s]'
                return '0'
            return format_num(self.val / self.time) + self.unit + '/s'

    for a in instances.iterkeys() :
        print a, total_hints[a], total_tags[a]

        # Get configuration for this key
        econf = conf.get(a, {})
        if econf.get('ignore', False) :
            continue

        # Put reference values where we can determine them
        referenceTable = {
            'cuda:memset-time': ['cuda:memset-bytes'],
            'cuda:memcpy-time-host': ['cuda:memcpy-bytes-host'],
            'cuda:memcpy-time-device': ['cuda:memcpy-bytes-device'],
            'cuda:kernel-time': [ 'cuda:gpu-float-ops'
                                , 'cuda:gpu-float-ops-add'
                                , 'cuda:gpu-float-ops-mul'
                                , 'cuda:gpu-float-ops-fma'
                                , 'cuda:gpu-double-ops'
                                , 'cuda:gpu-double-ops-add'
                                , 'cuda:gpu-double-ops-mul'
                                , 'cuda:gpu-double-ops-fma'
                                , 'cuda:gpu-float-instrs'
                                , 'cuda:gpu-double-instrs'
                                ]
        }
        for (time_attr, val_attrs) in referenceTable.items():
            if total_tags[a].has_key(time_attr):
                for val_attr in val_attrs:
                    total_tags_time[a][val_attr] = total_tags[a][time_attr]

        # Calculate metrics
        metrics = {'io':[], 'instr':[] }
        def mk_metric(cat, name, tag, hint_tag, time_factor, unit):
            # Figure out reference time
            if total_tags_time[a].has_key(tag):
                time = float(total_tags_time[a][tag]) / time_factor;
            else:
                time = total_time.get(a,0)
            metric = Metric(name, total_tags[a].get(tag),
                                  total_hints[a].get(hint_tag),
                                  time, unit);
            if metric.valid():
                metrics.setdefault(cat, []).append(metric)
        ms = 1000000
        us = 1000000000
        mk_metric('io', 'disk read', 'proc:read-bytes', 'hint:read-bytes', us, 'B');
        mk_metric('io', 'disk write', 'proc:write-bytes', 'hint:write-bytes', us, 'B');
        mk_metric('io', 'CUDA memset', 'cuda:memset-bytes', None, us, 'B');
        mk_metric('io', 'CUDA read', 'cuda:memcpy-bytes-host', 'hint:memcpy-bytes-host', us, 'B');
        mk_metric('io', 'CUDA write', 'cuda:memcpy-bytes-device', 'hint:memcpy-bytes-device', us, 'B');
        mk_metric('instr', 'instructions', 'perf:cpu-instructions', None, us, 'OP');
        mk_metric('instr', 'x87', 'perf:x87-ops', None, us, 'OP');
        mk_metric('instr', 'float', 'perf:scalar-float-ops', 'hint:float-ops', us, 'OP');
        mk_metric('instr', 'double', 'perf:scalar-double-ops', 'hint:double-ops', us, 'OP');
        mk_metric('instr', 'float (sse)', 'perf:sse-float-ops', None, us, 'OP');
        mk_metric('instr', 'double (sse)', 'perf:sse-double-ops', None, us, 'OP');
        mk_metric('instr', 'float (avx)', 'perf:avx-float-ops', None, us, 'OP');
        mk_metric('instr', 'double (avx)', 'perf:avx-double-ops', None, us, 'OP');
        mk_metric('instr', 'float (gpu)', 'cuda:gpu-float-ops', 'hint:gpu-float-ops', us, 'OP');
        mk_metric('instr', 'float (gpu add)', 'cuda:gpu-float-ops-add', None, us, 'OP');
        mk_metric('instr', 'float (gpu mul)', 'cuda:gpu-float-ops-mul', None, us, 'OP');
        mk_metric('instr', 'float (gpu fma)', 'cuda:gpu-float-ops-fma', None, us, 'OP');
        mk_metric('instr', 'double (gpu)', 'cuda:gpu-double-ops', 'hint:gpu-double-ops', us, 'OP');
        mk_metric('instr', 'double (gpu add)', 'cuda:gpu-double-ops-add', None, us, 'OP');
        mk_metric('instr', 'double (gpu mul)', 'cuda:gpu-double-ops-mul', None, us, 'OP');
        mk_metric('instr', 'double (gpu fma)', 'cuda:gpu-double-ops-fma', None, us, 'OP');
        mk_metric('instr', 'float (gpu?)', 'cuda:gpu-float-instrs', None, us, 'OP');
        mk_metric('instr', 'double (gpu?)', 'cuda:gpu-double-instrs', None, us, 'OP');


        # Print row(s)
        defMetric = Metric(None, None, None, None, '')
        rows = max([1, len(metrics['io']), len(metrics['instr'])])
        for i in range(0, rows):
            time = total_time.get(a, 0)
            ioMetric = instrMetric = defMetric
            if i < len(metrics['io']): ioMetric = metrics['io'][i]
            if i < len(metrics['instr']): instrMetric = metrics['instr'][i]
            f.write('''
      <tr><td class='%s key'>%s</td><td>%s</td><td>%s</td>
          <td>%s</td><td>%s</td><td>%s</td><td>%s</td>
          <td>%s</td><td>%s</td><td>%s</td><td>%s</td>
      </tr>'''
                % (make_class_name(a),
                   a if i == 0 else '',
                   '%d' % instances[a] if i == 0 else '',
                   '%02d:%02d.%03d' % (int(time / 60),
                                       int(time) % 60,
                                       int(time * 1000) % 1000)
                     if i == 0 else '',
                   ioMetric.format_name(),
                   ioMetric.format_val(),
                   ioMetric.format_hint(),
                   ioMetric.format_rate(),
                   instrMetric.format_name(),
                   instrMetric.format_val(),
                   instrMetric.format_hint(),
                   instrMetric.format_rate(),
                   ))
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
