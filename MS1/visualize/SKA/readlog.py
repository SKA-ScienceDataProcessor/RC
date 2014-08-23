__author__ = "mcsquaredjr"
__date__ = "12-Aug-2014"
__ver__ = "0.1"

"""Set of functions and classes to read profiling logs from a directory and to
manipulate the data.
"""


import os
import sys
import time
import re
import numpy as np

START_MARKER = "START "
END_MARKER = "END "


class LogEntry(object):
    """Class representing log entry with name, start, end times and
    duration.
    """
    def __init__(self, start, end, event, cap="", log_name=""):
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

    def description(self):
        out_str = "Event {:<30}\tetime: {:>10} ms.\tSource: {}".format("<" + self.event + ">",
                                                                         self.duration,
                                                                         self.log_name)
        print out_str

    @property
    def duration(self):
        if (self.end is not None) and (self.start is not None):
            duration = (int(self.end) - int(self.start)) / 1.0e6
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
        if self.node_name.find("master") >= 0:
            lst.append(0)
        else:
            num_node = re.findall(r"\d+", self.node_name)
            lst.append(int(num_node[0]))

        lst.append(self.log_name)

        return lst



def _find_incomplete_entry(entries, event):
    # Find incomplete entry for given even in the list of entries

    for entry in entries:
        if entry.event == event:
            if entry.end is None:
                return entry
    return None




def parse_log(fname, verbose=False):
    with open(fname) as f:
        entries = []
        for line in f:
            try:
                (time_stamp, cap, event) = line.split(": ")
                if event.find(START_MARKER) >= 0:
                    name_start = event[len(START_MARKER):-1]
                    entry = LogEntry(time_stamp, None, name_start, log_name=fname)
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
    transposed_logs = map(list, zip(*evt_log_lst))
    events = set(transposed_logs[0])
    return sorted(list(events))


def unique_nodes(evt_log_lst):
    transposed_logs = map(list, zip(*evt_log_lst))
    nodes = set(transposed_logs[5])
    return sorted(list(nodes))


def list_logs(folder_name, pattern="eventlog"):
    """Return names of all log files in the current folder.
     """
    log_files = []
    try:
        os.chdir(folder_name)
    except OSError, e:
        print "\n*** Error: directory does not exist. Exiting. Stack trace was: \n{}".format(e)
        sys.exit(100)

    for afile in os.listdir("."):
        if afile.find('eventlog') >= 0:
            log_files.append(os.path.join(folder_name, afile))
    return log_files



def events_dict(logs_folder):
    """Process all logs in logs_folder and return dictionary with keys:
        'list' list of all event log entries
        'np_array' abridged list converted to numpy array, all values are numerical
        'events' list of unique events
        'nodes' list of all nodes
    """
    log_files = list_logs(logs_folder)
    entries = []
    entries_lst = []

    out_dict = dict()

    for afile in log_files:
        entries += parse_log(afile, True)

    for entry in entries:
        entries_lst.append(entry.to_list())

    events = unique_events(entries_lst)
    nodes = unique_nodes(entries_lst)

    out_dict['list'] = entries_lst
    out_dict['events'] = events
    out_dict['nodes'] = nodes

    return out_dict



def run_test():

    logs_folder = "/Users/serge/Downloads/textualLogs_32nodes"
    log_files = list_logs(logs_folder)
    log_files = ["/Users/serge/Downloads/textualLogs_32nodes/slave.tesla106.eventlog.txt"]


    print "\n=== Starting processing\n"
    start_time = time.time()
    entries = []
    for afile in log_files:
        entries += parse_log(afile, True)


    end_time = time.time()


    print "===\nProcessing complete, elapsed time: {:.4} s".format(end_time - start_time)

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
