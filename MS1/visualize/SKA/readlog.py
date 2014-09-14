__author__ = "mcsquaredjr"
__date__ = "12-Aug-2014"
__ver__ = "0.1"

"""Set of functions and classes to read profiling logs from a directory and to
manipulate the data.
"""


import os
import time
import re
import json

START_MARKER = "START "
END_MARKER = "END "
ARGS_MARKER = "args"
SYNC_MARKER = "SYNCHRONIZATION "

class LogEntry(object):
    """Class representing log entry with name, start, end times and
    duration.
    """
    def __init__(self, start, end, event, port_no=0, cap="", log_name="", cad_name=""):
        self.start = start
        self.end = end
        self.event = event
        self.cap = cap
        self.log_name = log_name
        if len(log_name) > 0:
            path_parts = os.path.split(log_name)
            self.node_name = path_parts[-1].strip(".event.log.txt")
        else:
            self.node_name = ""
        self.port_no = port_no
        self.cad_name = cad_name

    def description(self):
        out_str = "Event {:<30}\tetime: {:>10} ms.\tSource: {}".format("<" + self.event + ">",
                                                                         self.duration,
                                                                         self.log_name)
        print out_str

    @property
    def duration(self):
        if (self.end is not None) and (self.start is not None):
            duration = (int(self.end) - int(self.start)) / 1.0e6 #microseconds
        else:
            duration = None
        return duration


    def to_list(self):
        """Convert log entry to array. Return a list of attributes.
        """
        lst = []
        lst.append(self.event)
        lst.append(int(self.start))
        lst.append(int(self.end))

        lst.append(self.duration)
        lst.append(self.cap)
        lst.append(self.node_name)

        lst.append(self._num_node())
        lst.append(self.log_name)
        lst.append(self.port_no)
        return lst


    def _num_node(self):
        fname = os.path.split(self.log_name)[-1]
        host_port = fname.strip("eventlog.").strip(".txt")
        host_port = (host_port[::-1].replace(".", ":", 1))[::-1]
        if self.cad_name != "":
            linecount = 1
            with open(self.cad_name) as cad:
                for line in cad:
                    if host_port == line.strip():
                        return linecount
                    linecount += 1
        else:
            return -1


def _find_incomplete_entry(entries, event):
    # Find incomplete entry for given even in the list of entries

    for entry in entries:
        if entry.event == event:
            if entry.end is None:
                return entry
    return None




def parse_log(fname, cad_name, syncStarts, verbose=False):
    with open(fname) as f:
        entries = []
        for line in f:
            if line.find(ARGS_MARKER) >= 0:
                args_str = line.split(": ")[-1]
                if json.loads(args_str)[1] == "master":
                    port = int(json.loads(args_str)[-5])
                else:
                    port = int(json.loads(args_str)[-3])
            try:
                (time_stamp, cap, event) = line.split(": ")
                if event.find(SYNC_MARKER) >= 0:
                    matches = re.match("[^00-9]+([0-9]+)\.([0-9]+)",event)
                    sec_part = float(matches.group(1))
                    ps_part = float(matches.group(2))
                    syncStarts[fname] = float(time_stamp), sec_part, ps_part
                if event.find(START_MARKER) >= 0:
                    name_start = event[len(START_MARKER):-1]
                    entry = LogEntry(time_stamp, None, name_start, port_no=port, log_name=fname, cad_name=cad_name)
                    entries.append(entry)
                if event.find(END_MARKER) >= 0:
                    if name_start == event[len(END_MARKER):-1]:
                        entry.end = time_stamp
                        if verbose:
                            entry.description()
                            name_start = ""
                    else:
                        if verbose:
                            print "=== Looking for incomplete entry: ", event[len(END_MARKER):-1]

                        entry = _find_incomplete_entry(entries, event[len(END_MARKER):-1].strip())
                        if entry is not None:
                            entry.end = time_stamp
                            print "Time stamp: ", time_stamp
                        else:
                            print "*** Warning: could not find incomplete entry: {} in file {}".format(event[len(END_MARKER):-1], fname)
                        name_start = ""
            except ValueError:
                pass



    return entries



def unique_events(evt_log_lst):
    """Take list of event logs and return all unique events.
    """
    # Transpose list of lists
    print "DEBUG: * "*10
    print evt_log_lst
    transposed_logs = map(list, zip(*evt_log_lst))
    events = set(transposed_logs[0])
    return sorted(list(events))


def unique_nodes(evt_log_lst):
    transposed_logs = map(list, zip(*evt_log_lst))
    nodes = set(transposed_logs[5])
    return sorted(list(nodes))

def min_port_no(evt_log_lst):
    transposed_logs = map(list, zip(*evt_log_lst))
    ports = set(transposed_logs[8])
    return min(ports)


def list_logs(folder_name, pattern="eventlog"):
    """Return names of all log files in the current folder.
     """
    log_files = []
#    try:
#        os.chdir(folder_name)
#    except OSError, e:
#        print "\n*** Error: directory does not exist. Exiting. Stack trace was: \n{}".format(e)
#        sys.exit(100)

    for afile in os.listdir(folder_name):
        if afile.find('eventlog') >= 0:
            log_files.append(os.path.join(folder_name, afile))

    cad_file = os.path.join(folder_name, "CAD_dna.txt")
    return log_files, cad_file



def events_dict(logs_folder, offset=True):
    """Process all logs in logs_folder and return dictionary with keys:
        'list' list of all event log entries
        'np_array' abridged list converted to numpy array, all values are numerical
        'events' list of unique events
        'nodes' list of all nodes
    """
    starts = dict()
    log_files, cad_file = list_logs(logs_folder)
    entries = []
    entries_lst = []

    out_dict = dict()

    for afile in log_files:
        entries += parse_log(afile, cad_file, starts, True)

    for _f, v in starts.iteritems():
        smalles_ns, smallest_s, smallest_ps = v
        break

    for _f, v in starts.iteritems():
        ns, s, ps = v
        if (s<smallest_s) or (s==smallest_s and ps < smallest_ps):
            smallest_s = s
            smallest_ps = ps
            smallest_ns = ns

    print "smallests ", smallest_s, smallest_ps

    if offset:
        # Adjustements added by Serguey
        for entry in entries:
            print "entry before ", entry.to_list()
            start_ns, start_s, start_ps = starts[entry.log_name]
            offset = (start_s - smallest_s)*1000000000.0 + (start_ps - smallest_ps)/1000.0 - start_ns
            print "offset ", offset
            entry.start = float(entry.start) + offset
            entry.end = float(entry.end) + offset
            print "entry after ", entry.to_list()
            entries_lst.append(entry.to_list())
    else:
        for entry in entries:
            entries_lst.append(entry.to_list())

    events = unique_events(entries_lst)
    nodes = unique_nodes(entries_lst)
    min_port = min_port_no(entries_lst)

    out_dict['list'] = entries_lst
    out_dict['events'] = events
    out_dict['nodes'] = nodes
    out_dict['min_port'] = min_port

    return out_dict



def run_test():

    logs_folder = "/Users/serge/Desktop/16node-txt"
    log_files, cad_file = list_logs(logs_folder)
    #log_files = ["/Users/serge/Downloads/textualLogs_32nodes/slave.tesla106.eventlog.txt"]


    print "\n=== Starting processing\n"
    start_time = time.time()
    entries = []
    starts = dict()
    for afile in log_files:
        entries += parse_log(afile, cad_file, starts, True)


    end_time = time.time()



    entries_list = []
    for entry in entries:
        entries_list.append(entry.to_list())

    print len(entries_list)
    print unique_events(entries_list)
    print len(unique_nodes(entries_list))

    out_dict = events_dict(logs_folder)
    print out_dict



if __name__ == "__main__":
    # Run everything
    run_test()
