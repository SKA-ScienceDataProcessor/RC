__author__ = 'mcsquaredjr'



import xlwt

TITLE = xlwt.easyxf('font: bold True, height 320;')
HEADER = xlwt.easyxf('font: bold True, height 240;')
HEADER_C = xlwt.easyxf('font: bold True, height 240; alignment: horizontal center;')

WRAPPED = xlwt.easyxf('alignment: wrap True;')


class XLS_Writer(object):

    def __init__(self, out_file_name, sheet_prefix=""):
        """Create an instance and initialize variables"""
        self.xls_name = out_file_name
        self.wb = xlwt.Workbook()
        self.count = 1
        self.prefix = sheet_prefix
        self.ws = None
        self.event_column_pos = 1
        self.unique_events = None
        self.cur_row = 2



    def new_sheet(self, name):
        """Add new worksheet to workbook"""
        # Format cells accordingly for different kind of sheets
        self.ws = self.wb.add_sheet(name)
        for i in range(30):
            self.ws.col(i).width = 256*18


    def write_node(self, nodes):
        if self.ws is not None:
            self.ws.row(1).write(0, "Node", HEADER)
            for i, node in enumerate(nodes):
                self.ws.row(i + 2).write(0, node, WRAPPED)

    def _write_event_header(self, event):
        """Write single event column to a sheet.
        """
        if self.ws is not None:
            # Make headers, merging cells r1, r2, c1, c2
            self.ws.write_merge(0, 0, self.event_column_pos, self.event_column_pos + 2, event, HEADER_C)
            self.ws.row(1).write(self.event_column_pos, "Start", HEADER_C)
            self.ws.row(1).write(self.event_column_pos + 1, "End", HEADER_C)
            self.ws.row(1).write(self.event_column_pos + 2, "Duration", HEADER_C)
            self.event_column_pos += 3


    def write_event_headers(self, events):
        self.unique_events = events
        for event in events:
            self._write_event_header(event)

    def _write_event(self, event_data):
        """Write single event column to a sheet.
        """
        if self.ws is not None:
            if self.unique_events is not None:
                event = event_data[0]
                ind = self.__find_pos(event)
                if ind >= 0:
                    pos = ind * 3 + 1
                    self.ws.row(self.cur_row).write(0, event_data[5])
                    self.ws.row(self.cur_row).write(pos, event_data[1])
                    self.ws.row(self.cur_row).write(pos + 1, event_data[2])
                    self.ws.row(self.cur_row).write(pos + 2, event_data[3])

        self.cur_row += 1

    def write_events(self, events_lst):
        for event_data in events_lst:
            self._write_event(event_data)

    def __find_pos(self, event):
        # Find and return position of an event in the event list, otherwise return -1
        for i in range(len(self.unique_events)):
            if event == self.unique_events[i]:
                return i
            i += 1

        return -1


    def make_xls(self):
        """Save xls file to disk"""
        if self.ws is not None:
            self.wb.save(self.xls_name)


if __name__ == "__main__":
    xw = XLS_Writer("test_xls.xls", "prefix_")
    xw.new_sheet("sheet name")
    xw.write_node([1, 3, 1, 0])
    xw.write_event([["one", "two", "three"], ["one", "two", "three"]])
    xw.write_event([["one", "two", "three"], ["one", "two", "three"]])
    xw.make_xls()

