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
    function plot() {
    ''')

def make_class_name(name) :
    "Makes a valid CCS class name out of the given string"
    # I have a feeling this won't work for long...
    return re.sub('[^-_A-Za-z0-9]+', '', name)

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

hint_re = re.compile(r'hint:')
def extractTags(e, cur_hints = {}, cur_tags = {}, cur_tags_time = {}):
    for t in e.tags.iterkeys():
        if hint_re.match(t) != None:
            cur_hints[t] = int(e.tags[t]) + cur_hints.get(t, 0)
    for t in e.tags2.iterkeys():
        if hint_re.match(t) == None:
            d = e.diff(t)
            if d != None: cur_tags[t] = d + cur_tags.get(t, 0)
            d = e.diff_t(t)
            if d != None: cur_tags_time[t] = d + cur_tags_time.get(t, 0)
    return (cur_hints, cur_tags, cur_tags_time)

err_threshold = 1.5
warn_threshold = 1.1

efficiency_refs = {
    'double (gpu)': 100 * 1000 * 1000 * 1000
}

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
            if self.val == 0 or self.val == None or self.val > self.hint : # or self.error() > err_threshold:
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

    def efficiency(self):
        if efficiency_refs.has_key(self.name) and self.time > 0:
            v = self.val
            if v == 0 or v == None:
                v = self.hint
            if v == 0 or v == None:
                return None
            return float(v) / efficiency_refs[self.name] / self.time
        else:
             return None

def makeMetrics(etime, hints, tags, tags_time):

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
        time_sum = 0
        weight_sum = 0
        for (valAttr, weight) in weightedVals.items():
            if tags.has_key(valAttr):
                sum += weight * tags[valAttr]
                time_sum += weight * tags_time[valAttr]
                weight_sum += weight
        if time_sum > 0:
            tags[sumAttr] = sum
            tags_time[sumAttr] = time_sum / weight_sum

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
        if tags.has_key(time_attr):
            for val_attr in val_attrs:
                tags_time[val_attr] = tags[time_attr]

    # Calculate metrics
    metrics = {'io':[], 'instr':[] }
    def mk_metric(cat, name, tag, hint_tag, time_factor, unit):
        # Figure out reference time
        if tags_time.has_key(tag):
            time = float(tags_time[tag]) / time_factor;
        else:
            time = etime
        metric = Metric(name, tags.get(tag),
                              hints.get(hint_tag),
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

    return metrics

def write_timeline_data(logs, conf) :
    "Timeline of execution, data generation"
    n = 0

    # Collect all delay types, figure out profile length
    delays = set()
    total_time = 0
    overall_time = {}
    for k in sorted(logs) :
        for e in logs[k].time :
            if conf.get(e.msg,{}).get('ignore', False) :
                continue
            delays.add(e.msg)
            if e.t2 != None: total_time = max(e.t2, total_time)
            if e.t1 != None: total_time = max(e.t1, total_time)
            if e.t1 != None and e.t2 != None:
                overall_time[e.msg] = (e.t2 - e.t1) + overall_time.get(e.msg, 0)

    delays = sorted(delays)
    f.write('''
      var colorScale = d3.scale.category20().domain({0});'''.format(list(delays)))

    f.write('''
      var color; var elems; var i;''')
    for a in delays:
        f.write('''
      color = colorScale("%s");
      elems = document.querySelectorAll(".%s");
      for (i = 0; i < elems.length; i++)
        elems[i].style.backgroundColor = color;'''
            % (a, make_class_name(a)));

    f.write('''
      var data = [''')

    for k in sorted(logs) :

        # Bin by message
        tt_hash = {}
        for e in logs[k].time:
            if tt_hash.has_key(e.msg):
                tt_hash[e.msg].append(e)
            else:
                tt_hash[e.msg] = [e]

        # Go through bins, sorted by earliest first message
        pid = None
        for tts in sorted(tt_hash.itervalues(), key=lambda v: v[0].t1):

            # Get configuration, check whether we're supposed to ignore
            # this one.
            e = tts[0]
            if conf.get(e.msg,{}).get('ignore', False) :
                continue

            # Create row, declare PID. Note that we are only looking
            # at the first time for a certain message here - it might
            # be that other messages were coming from other PIDs. This
            # - at best - hints at the process distribution.
            pid_new = e.tags.get('pid',e.tags2.get('pid',''))
            if pid_new == pid:
                f.write('\n            {times:[')
            else:
                f.write('\n            {"label": "%s", times:[' % pid_new)
                pid = pid_new
            first = True

            # Write entries
            for e2 in tts :

                # Get metrics for this kernel
                hints, tags, tags_time = extractTags(e2, {}, {}, {})
                metrics = makeMetrics(e2.t2 - e2.t1, hints, tags, tags_time)
                print e2.msg, "Hints:", hints, "Data:", tags, "Time:", tags_time
                eff = None
                for m in metrics['instr']:
                    eff = m.efficiency()
                if eff is None:
                    eff = 0.05

                # Make sure we have a certain minimum width. This is a hack.
                end = e2.t2
                if end == None or end < e2.t1+total_time/1000:
                    end = e2.t1 + total_time/1000

                f.write('''
                {"starting_time": %g, "ending_time": %g, "label": "%s", "type": "%s", "height": "%g"},'''
                    % (1000*e2.t1, 1000*end, e2.msg if first else '', e2.msg, eff))
                first = False
            f.write('\n            ]},')

    f.write('''
        ];

      // Stack up bars?
      var kernelSplit = document.getElementById('kernel_split').checked;
      if (!kernelSplit) {
        newData = [];
        lastLabel = '';
        for (var i = 0; i < data.length; i++) {
          label = lastLabel;
          if ('label' in data[i]) {
            label = data[i].label.substr(0, data[i].label.lastIndexOf(':'));
          }
          for (var j = 0; j < data[i].times.length; j++) {
            data[i].times[j].label = undefined;
          }
          if (label == lastLabel) {
            newData[newData.length-1].times =
              newData[newData.length-1].times.concat(data[i].times);
          } else {
            newData.push(data[i]);
            newData[newData.length-1].label = label;
            lastLabel = label;
          }
        }
        data = newData
        console.log(data);
      }

      // Strip idle times?
      var stripIdle = document.getElementById('strip_idle').checked;
      if (stripIdle) {
        for (var i = 0; i < data.length; i++) {
          var time = 0;
          data[i].times.sort(function (a,b) { return a.starting_time - b.starting_time; });
          for (var j = 0; j < data[i].times.length; j++) {
            var length = data[i].times[j].ending_time - data[i].times[j].starting_time;
            data[i].times[j].starting_time = time;
            time += length;
            data[i].times[j].ending_time = time;
          }
        }
      }
    ''')

    # Figure out a good tick interval
    tickInterval = 1
    for i in [2,5,10,20,30,60,120]:
        if i * 10 > total_time: break
        tickInterval = i
    print "tickInterval= ", tickInterval

    f.write('''
      var chart = d3.timeline()
        .beginning(-1)
        .tickFormat(
           { format: d3.time.format("%%M:%%S"),
             tickInterval: %d,
             tickTime: d3.time.second,
             tickSize: 5 })
        .stack().showTimeAxisTick()
        .colorProperty('type')
        .colors(colorScale)
        .margin({left:300, right:20, top:0, bottom:0});

      d3.selectAll("#timeline svg").remove();
      var svg = d3.select("#timeline").append("svg").attr("width", window.innerWidth-30)
        .datum(data).call(chart);''' % (tickInterval))

    # Generate overview layout. We use a "1-row" stacked bar layout.
    f.write('''
      var balanceData = [''')
    for name in delays:
        f.write('''
        [{ name: "%s", x:0, y: %f }],''' % (name, overall_time[name]))
    f.write('''
      ];
      var balanceDataStacked = d3.layout.stack()(balanceData);
''')

    # Generate visualisation
    f.write('''
      d3.selectAll("#balance svg").remove();
      var balance = document.getElementById('balance'),
          width = balance.offsetWidth, height = balance.offsetHeight;
      var scale = height / %f;
      var svg = d3.select("#balance").append("svg")
         .attr("width", width)
         .attr("height", height);
      svg.selectAll('rect')
         .data(balanceDataStacked)
         .enter()
         .append('rect')
         .attr('y', function (d) { return scale*d[0].y0; })
         .attr('x', function (d) { return 0; })
         .attr('width', function (d) { return width; })
         .attr('height', function (d) { return scale*d[0].y; })
         .attr('fill', function (d) { return colorScale(d[0].name); });
      svg.selectAll('text')
         .data(balanceDataStacked)
         .enter()
         .append('text')
         .text(function(d) {
            percent = 100 * scale*d[0].y/height;
            if (scale*d[0].y > 12)
              return percent.toFixed(1) + "%%";
            else
              return "";
          })
         .attr('y', function (d) { return 6+scale*(d[0].y0+d[0].y/2); })
         .attr('x', function (d) { return width/2; })
         .attr("text-anchor", "middle");

    ''' % (sum(overall_time.itervalues())))

def write_middle():
    f.write('''
    }
    window.onload = plot;
    window.onresize = plot;
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
    #timeline text {
      font-family: sans-serif;
      font-size: 12px;
      text-anchor: end;
    }
    #timeline .timeline-label {
      text-anchor: start;
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
    <h4>Timeline</h4>
    <p>Visualisation options:
       <input onclick="plot();" type="checkbox" id="kernel_split">Split by kernel</input> /
       <input onclick="plot();" type="checkbox" id="strip_idle">Strip idle times</input></p>
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
            total_hints[e.msg], total_tags[e.msg], total_tags_time[e.msg] = \
              extractTags(e, total_hints.get(e.msg, {}),
                          total_tags.get(e.msg, {}),
                          total_tags_time.get(e.msg, {}))

    # Generate overall metrics
    all_metrics = []
    for a in instances.iterkeys() :
        # print a, total_hints[a], total_tags[a], total_tags_time[a]

        # Get configuration for this key
        econf = conf.get(a, {})
        if econf.get('ignore', False) :
            continue

        # Make metrics
        metrics = makeMetrics(total_time.get(a,0),
                              total_hints[a], total_tags[a], total_tags_time[a])
        rows = max([1, len(metrics['io']), len(metrics['instr'])])
        all_metrics.append((a, rows, metrics))

    all_metrics = sorted(all_metrics, key=lambda m: m[0])
    total_rows = sum(map(lambda m: m[1], all_metrics))

    # Make table
    f.write('''
    <h4>Statistics</h4>
    <table class="statistics">
      <colgroup>
        <col span="1"/>
        <col span="2"/>
        <col span="1" class="group"/>
        <col span="1" class="group"/>
        <col span="4"/>
        <col span="1" class="group"/>
        <col span="4"/>
      </colgroup>
      <tr><th>Balance</th>
          <th>Kernel</th>
          <th>Instances</th><th>Time</th>
          <th colspan="5">IO</th>
          <th colspan="5">Instructions</th></tr>
      <tr><td></td><td></td><td></td><td></td>
          <td></td><td>Value</td><td>Expected</td><td>Rate</td><td>Time</td>
          <td></td><td>Value</td><td>Expected</td><td>Rate</td><td>Time</td></tr>''')

    first = True
    for a, rows, metrics in all_metrics :

        # Print row(s)
        defMetric = Metric(None, None, None, None, '')
        for i in range(0, rows):
            time = total_time.get(a, 0)
            ioMetric = instrMetric = defMetric
            if i < len(metrics['io']): ioMetric = metrics['io'][i]
            if i < len(metrics['instr']): instrMetric = metrics['instr'][i]
            row_classes = ''
            if i == 0: row_classes += 'first'
            f.write('''
            <tr class='%s'>''' % row_classes)
            if first:
                f.write('''
                <td rowspan="%d" id="balance">''' % total_rows)
            first = False
            f.write('''
          <td class='key %s'>%s</td><td>%s</td><td>%s</td>
          <td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td>
            <td class='%s'>%s</td><td class='%s'>%s</td>
          <td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td>
            <td class='%s'>%s</td><td class='%s'>%s</td>
      </tr>'''
                % ((make_class_name(a),
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
      <tr><th>Colour</th><th>Explanation</th></tr>
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
