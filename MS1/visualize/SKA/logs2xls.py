__author__ = 'mcsquaredjr'


import readlog
import writexls
import os

def logs2xls(folder_name, xls_name):
    """Read logs from folder_name and wrrite to xls_name
    """
    log_files = readlog.list_logs(folder_name)
    entries = []
    entries_lst = []

    for afile in log_files:
        entries += readlog.parse_log(afile, True)



    for entry in entries:
        #print "----->", entry.start, entry.end, entry.event, entry.log_name
        entries_lst.append(entry.to_list())

    unique_events = readlog.unique_events(entries_lst)
    unique_nodes = readlog.unique_nodes(entries_lst)

    xw = writexls.XLS_Writer(xls_name)
    xw.new_sheet("48 nodes")
    #xw.write_node(unique_nodes)
    xw.write_event_headers(unique_events)
    xw.write_events(entries_lst)
    xw.make_xls()

    print len(entries_lst)





if __name__ == '__main__':
    folder_name = "/Users/serge/Downloads/textualLogs_32nodes"
    xls_name = os.path.split(folder_name)[-1] + ".xls"
    logs2xls(folder_name, xls_name)
