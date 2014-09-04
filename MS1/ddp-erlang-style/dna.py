__author__ = 'mcsquaredjr'


import argparse

if __name__== "__main__":
    parser = argparse.ArgumentParser(description='Schedule DNA job.')
    parser.add_argument('temp_file', type=str, help='temporary file name')
    parser.add_argument('exec_name', type=str, help='executable file name')

    args = parser.parse_args()
    print "Temporariry file: ", args.temp_file
    print "Executable file name: ", args.exec_name