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

    # Collect all delay types, figure out profile length
    delays = set()
    total_time = 0
    for k in sorted(logs) :
        for e in logs[k].time :
            if conf.get(e.msg,{}).get('ignore', False) :
                continue
            delays.add(e.msg)
            if e.t2 != None: total_time = max(e.t2, total_time)
            if e.t1 != None: total_time = max(e.t1, total_time)

    f.write('''
      var colorScale = d3.scale.category20().domain({0});'''.format(list(delays)))

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
        done = set()
        for e in tt :

            # Get configuration, check whether we're supposed to ignore
            # this one.
            if conf.get(e.msg,{}).get('ignore', False) :
                continue
            # Already done?
            if e.msg in done:
                continue

            # Create row
            f.write('\n            {"label": "%s", type:"%s", times:['
                    % ('' if pid_func(e) == pid else pid_func(e),e.msg))
            pid = pid_func(e)

            # Write entries
            for e2 in tt :
                if e.msg != e2.msg:
                    continue

                # Make sure we have a certain minimum width. This is a hack.
                end = e2.t2
                if end == None or end < e2.t1+total_time/1000:
                    end = e2.t1 + total_time/1000

                f.write('''
                {"starting_time": %g, "ending_time": %g, "label": "%s", "type": "%s"},'''
                    % (1000*e2.t1, 1000*end, e2.msg if e2.t1 == e.t1 else '', e2.msg))
            f.write('\n            ]},')

            done.add(e.msg)

    f.write('''
        ];''')

    # Figure out a good tick interval
    tickInterval = 1
    for i in [2,5,10,20,30,60,120]:
        if i * 20 > total_time: break
        tickInterval = i

    f.write('''
      var chart = d3.timeline()
        .beginning(-1)
        .tickFormat(
           { format: d3.time.format("%%M:%%S"),
             tickInterval: %d,
             tickTime: d3.time.second,
             tickSize: 5 })
        .stack()
        .colorProperty('type')
        .colors(colorScale)
        .margin({left:150, right:150, top:0, bottom:0});

      var svg = d3.select("#timeline").append("svg").attr("width", "1500")
        .datum(data).call(chart);''' % (tickInterval))

def write_middle():
    f.write('''
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
    table.statistics tr.first {
      border-top: 1px solid black;
    }
    table.statistics col.group {
      border-left: 1px solid black;
    }
    table.statistics td {
      text-align: center;
      min-width: 80px;
    }
    h4 {
      box-shadow: 0 1px 0 rgba(0,0,0,0.1);
    }

    .hintbased {
      background-color: lightgray;
    }
    .hintokay {
      background-color: lightgreen;
    }
    .hintwarning {
      background-color: yellow;
    }
    .hinterror {
      background-color: pink;
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
      <tr><td class="key">Executable:</td><td class="val">{0}</td></tr>
      <tr><td class="key">Arguments:</td><td class="val">{1}</td></tr>
    </table>'''
            .format(args[0], ' '.join(args[1:])))

    # Get program environment (see above)
    env = logs[0].get_env()
    if env.has_key('SLURM_NODEID') :
        f.write('''
    <h4>SLURM Configuration</h4>
    <table class="slurm_conf">
        <tr><td class="key">Job:</td><td>{0} {1}, started by {2}</td></tr>
        <tr><td class="key">Nodes:</td><td>{3}: {4}</td></tr>
        <tr><td class="key">Tasks:</td><td>{5} = {6}</td></tr>
        <tr><td class="key">Procs:</td><td>{7}</td></tr>
        <tr><td class="key">CPUS:</td><td>{8}</td></tr>
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
      <colgroup>
        <col span="2"/>
        <col span="1" class="group"/>
        <col span="1" class="group"/>
        <col span="4"/>
        <col span="1" class="group"/>
        <col span="4"/>
      </colgroup>
      <tr><td></td><th>Instances</th><th>Time</th><th colspan=5>IO</th><th colspan=5>Instructions</th></tr>
      <tr><td /><td /><td />
          <td /><td>Value</td><td>Expected</td><td>Rate</td><td>Time</td>
          <td /><td>Value</td><td>Expected</td><td>Rate</td><td>Time</td></tr>''')

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

    err_threshold = 1.5
    warn_threshold = 1.1

    class Metric:
        "Performance metric"
        def __init__(self, name, val, hint, time, unit):
            self.name = name; self.val = val; self.hint=hint
            self.time = time; self.unit = unit
        def valid(self):
            return (self.val != None and self.val > 0) or self.hint != None
        def error(self):
            return max(float(self.val) / self.hint, float(self.hint) / self.val)
        def format_name(self):
            if self.name == None: return ('', '');
            else: return ('', self.name + ':')
        def format_val(self):
            if self.name == None: return ('', '')
            if self.val == None: return ('', '-')
            return ('', format_num(self.val) + self.unit)
        def format_hint(self):
            if self.hint == None: return ('', '');
            if self.val == 0 or self.val == None:
                return ('hintbased', '[' + format_num(self.hint) + self.unit + ']')
            # Check hint discrepancy
            if self.error() >= err_threshold:
                return ('hinterror', '[' + format_num(self.hint) + self.unit + ']')
            elif self.error() >= warn_threshold:
                style = 'hintwarning'
            else:
                style = 'hintokay'
            return (style, '[%.1f%%]' % (float(100) * self.val / self.hint));
        def format_rate(self):
            if self.time == None or self.time == 0: return ('', '')
            if self.hint != None and self.hint != 0:
                if self.val == 0 or self.val == None: # or self.error() > err_threshold:
                    return ('hintbased', '[' + format_num(self.hint / self.time) + self.unit + '/s]')
            if self.val == None:
                return ('', '-')
            if self.val == 0:
                return ('', '0')
            return ('', format_num(self.val / self.time) + self.unit + '/s')
        def format_time(self):
            if self.time == None: return ('', '')
            return ('', 'x %02d:%02d.%03d' % (int(self.time / 60),
                                              int(self.time) % 60,
                                              int(self.time * 1000) % 1000))

    for a in instances.iterkeys() :
        print a, total_hints[a], total_tags[a], total_tags_time[a]

        # Get configuration for this key
        econf = conf.get(a, {})
        if econf.get('ignore', False) :
            continue

        # Derive performance values
        sumTable = { 'perf:float-ops': { 'perf:scalar-float-ops': 1
                                       , 'perf:sse-float-ops': 4
                                       , 'perf:avx-float-ops': 8
                                       }
                   , 'perf:double-ops': { 'perf:scalar-double-ops': 1
                                        , 'perf:sse-double-ops': 2
                                        , 'perf:avx-double-ops': 4
                                        }
                   }
        for (sumAttr, weightedVals) in sumTable.items():
            sum = 0
            time = None
            for (valAttr, weight) in weightedVals.items():
                if total_tags[a].has_key(valAttr):
                    sum += weight * total_tags[a][valAttr]
                    time = total_tags_time[a][valAttr]
            if not time is None:
                total_tags[a][sumAttr] = sum
                total_tags_time[a][sumAttr] = time

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
        ms = 1000
        us = 1000000
        ns = 1000000000
        mk_metric('io', 'disk read', 'proc:read-bytes', 'hint:read-bytes', ns, 'B');
        mk_metric('io', 'disk write', 'proc:write-bytes', 'hint:write-bytes', ns, 'B');
        mk_metric('io', 'CUDA memset', 'cuda:memset-bytes', None, ns, 'B');
        mk_metric('io', 'CUDA read', 'cuda:memcpy-bytes-host', 'hint:memcpy-bytes-host', ns, 'B');
        mk_metric('io', 'CUDA write', 'cuda:memcpy-bytes-device', 'hint:memcpy-bytes-device', ns, 'B');
        mk_metric('io', 'RAM read', 'perf:mem-read-bytes', 'hint:mem-read-bytes', ns, 'B');
        mk_metric('instr', 'instructions', 'perf:cpu-instructions', None, ns, 'OP');
        mk_metric('instr', 'x87', 'perf:x87-ops', None, ns, 'OP');
        mk_metric('instr', 'float', 'perf:float-ops', 'hint:float-ops', ns, 'OP');
        mk_metric('instr', 'double', 'perf:double-ops', 'hint:double-ops', ns, 'OP');
        mk_metric('instr', 'float (scalar)', 'perf:scalar-float-ops', '', ns, 'OP');
        mk_metric('instr', 'double (scalar)', 'perf:scalar-double-ops', '', ns, 'OP');
        mk_metric('instr', 'float (sse)', 'perf:sse-float-ops', None, ns, 'OP');
        mk_metric('instr', 'double (sse)', 'perf:sse-double-ops', None, ns, 'OP');
        mk_metric('instr', 'float (avx)', 'perf:avx-float-ops', None, ns, 'OP');
        mk_metric('instr', 'double (avx)', 'perf:avx-double-ops', None, ns, 'OP');
        mk_metric('instr', 'float (gpu)', 'cuda:gpu-float-ops', 'hint:gpu-float-ops', ns, 'OP');
        mk_metric('instr', 'instructions (gpu)', 'cuda:gpu-instructions', None, ns, 'OP');
        mk_metric('instr', 'float (gpu add)', 'cuda:gpu-float-ops-add', None, ns, 'OP');
        mk_metric('instr', 'float (gpu mul)', 'cuda:gpu-float-ops-mul', None, ns, 'OP');
        mk_metric('instr', 'float (gpu fma)', 'cuda:gpu-float-ops-fma', None, ns, 'OP');
        mk_metric('instr', 'double (gpu)', 'cuda:gpu-double-ops', 'hint:gpu-double-ops', ns, 'OP');
        mk_metric('instr', 'double (gpu add)', 'cuda:gpu-double-ops-add', None, ns, 'OP');
        mk_metric('instr', 'double (gpu mul)', 'cuda:gpu-double-ops-mul', None, ns, 'OP');
        mk_metric('instr', 'double (gpu fma)', 'cuda:gpu-double-ops-fma', None, ns, 'OP');
        mk_metric('instr', 'float (gpu)?', 'cuda:gpu-float-instrs', None, ns, 'OP');
        mk_metric('instr', 'double (gpu)?', 'cuda:gpu-double-instrs', None, ns, 'OP');

        # Print row(s)
        defMetric = Metric(None, None, None, None, '')
        rows = max([1, len(metrics['io']), len(metrics['instr'])])
        for i in range(0, rows):
            time = total_time.get(a, 0)
            ioMetric = instrMetric = defMetric
            if i < len(metrics['io']): ioMetric = metrics['io'][i]
            if i < len(metrics['instr']): instrMetric = metrics['instr'][i]
            row_classes = ''
            if i == 0: row_classes += 'first'
            f.write('''
      <tr class='%s'>
          <td class='key %s'>%s</td><td>%s</td><td>%s</td>
          <td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td>
          <td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td>
      </tr>'''
                % ((row_classes,
                    make_class_name(a),
                    a if i == 0 else '',
                    '%d' % instances[a] if i == 0 else '',
                    '%02d:%02d.%03d' % (int(time / 60),
                                        int(time) % 60,
                                        int(time * 1000) % 1000)
                      if i == 0 else '') +
                   ioMetric.format_name() +
                   ioMetric.format_val() +
                   ioMetric.format_hint() +
                   ioMetric.format_rate() +
                   ioMetric.format_time() +
                   instrMetric.format_name() +
                   instrMetric.format_val() +
                   instrMetric.format_hint() +
                   instrMetric.format_rate() +
                   instrMetric.format_time()
                   ))
    f.write('''
    </table>
    <h4>Legend</h4>
    <table>
      <th>Colour<th></td><th>Explanation</th>
      <tr><td class='hintokay'>Okay</td><td>Performance as predicted (+-%d%%)</td></tr>
      <tr><td class='hintwarning'>Warning</td><td>Medium performance discrepancy (+-%d%%)</td></tr>
      <tr><td class='hinterror'>Error</td><td>Large performance discrepancy, assuming data corrupt</td></tr>
      <tr><td class='hintbased'>Hint-based</td><td>Hint is used for metric calculation, measurement discarded</td></tr>
    </table>
    ''' % (100*(warn_threshold-1), 100*(err_threshold-1)))

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
