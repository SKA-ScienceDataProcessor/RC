#!/usr/bin/env python
__author__ = 'mcsquaredjr'

import os
import sys
import shutil
import socket
import subprocess

from optparse import OptionParser, Option, SUPPRESS_HELP
from string import Template


CMD_SRUN = "srun -p $partition --nodelist $node -N1 -n1 --exclusive "

CMD_LN_MINUS_S = Template(CMD_SRUN + "ln -s /ramdisks/floats $HOME/input_data.txt")
CMD_CREATE_FLOATS = Template(CMD_SRUN + "./create-floats /ramdisks/floats $item_count $num_of_processes $chunk_no")
CMD_START_EXECUTABLE = Template(CMD_SRUN + "$executable $mode --cad $CAD --ip $ipaddr --port $port +RTS -l-au &")

CMD_RM_MINUS_RF = Template(CMD_SRUN + "rm -rf $HOME/input_data.txt")
CMD_TOUCH = Template(CMD_SRUN + "touch /ramdisks/file.$ip_addr")

######################################################################################
#                              CLASS OPTIONS PARSER                                  #
######################################################################################
class PAOptionParser(OptionParser, object):
    def __init__(self, *args, **kw):
        self.posargs = []
        super(PAOptionParser, self).__init__(*args, **kw)

    def add_posarg(self, *args, **kw):
        pa_help = kw.get("help", "")
        kw["help"] = SUPPRESS_HELP
        o = self.add_option("--%s" % args[0], *args[1:], **kw)
        self.posargs.append((args[0], pa_help))

    def get_usage(self, *args, **kwargs):
        self.usage = "%%prog %s [options]\n\nPositional Arguments:\n %s" % \
        (' '.join(["<%s>" % arg[0] for arg in self.posargs]), '\n '.join(["%s: %s" % (arg) for arg in self.posargs]))
        return super(self.__class__, self).get_usage(*args, **kwargs)

    def parse_args(self, *args, **kwargs):
        args = sys.argv[1:]
        args0 = []
        for p, v in zip(self.posargs, args):
            args0.append("--%s" % p[0])
            args0.append(v)
        args = args0 + args
        options, args = super(self.__class__, self).parse_args(args, **kwargs)
        if len(args) < len(self.posargs):
            msg = 'Missing value(s) for "%s"\n' % ", ".join([arg[0] for arg in self.posargs][len(args):])
            self.error(msg)
        return options, args

######################################################################################
#                                CLASS DNA RUNNER                                    #
######################################################################################
class DNARunner(object):

    def __init__(self, jobid, partition, numcores, executable, vector_len=15000000, minport=79001):
        """Initialize DNA runner buy providing SLURM jobid, partition name, number of cores, and
        name of Haskell executable
        """
        self.jobid = jobid
        self.partition  = partition
        self.numcores = numcores
        self.executable = executable
        self.minport = minport
        self.vector_len = vector_len

    # ==== properties goes below
    @property
    def nodes(self):
        return ["bronco.local", "bronco.local", "bronco.local"]

    @property
    def cores(self):
        return [core for core in range(1, self.numcores + 1)]

    @property
    def ports(self):
        return [port for port in range(self.minport, self.minport + self.numcores)]


    @property
    def cad_name(self):
        return os.path.join(str(self.jobid), "CAD.file")

    # ==== methods
    def ipaddr(self, node):
        """Return IP address for a given node.
        """
        return socket.gethostbyname(node)


    def make_dirs(self):
        """Create directories where executables will be stored per JOBID/IP/CORE
        """
        old_dir = os.getcwd()
        for node in self.nodes:
            dir_name = str(self.jobid)
            if not os.path.exists(dir_name):
                try:
                    os.makedirs(dir_name)
                except Exception, e:
                    print "*** Error in make_dirs: {0}. Exiting.".format(e)
                    sys.exit(-1)

            os.chdir(dir_name)
            for core in self.cores:
                try:
                    os.makedirs(os.path.join(self.ipaddr(node).replace(".", "-"), str(core)))
                except Exception, e:
                    print "*** Error in make_dirs: {0}. Exiting.".format(e)
                    sys.exit(-1)
        os.chdir(old_dir)


    def write_CAD(self):
        with open(self.cad_name, "w+") as cad:
            for node in self.nodes:
                for port in self.ports:
                    line = self.ipaddr(node) + ":" + str(port) + "\n"
                    cad.write(line)



    def copy_executables(self):
        """Create input files and copy executables to each node.
        """
        print "Current directory is: ", os.getcwd()
        for i, node in enumerate(self.nodes):
            for core in self.cores:
                # Contstruct a path to copy exectuables
                dest_name = os.path.join(os.path.join(str(self.jobid),
                                                      self.ipaddr(node).replace(".", "-"),
                                                      str(core),
                                                      self.executable))
                try:
                    shutil.copyfile(self.executable, dest_name)
                except Exception, e:
                    print "*** Error in copy_files: {0}. Exiting.".format(e)
                    sys.exit(-1)


    def create_inputs(self, execute):
        """Generate input files and create a link to $HOME.
        """
        # Number of processes
        # TODO: if there is just one node results are incorrect
        num_of_processes = self.numcores * (len(self.nodes) - 1) # accurately would be minus 2 but we create it on the collector too
        print "\n==== Create inputs and links: \n"
        for i, node in enumerate(self.nodes):
            if node == 0:
                # master node
                chunk_no = 0
            else:
                chunk_no = 12 * i
            # Write file
            cmd_create = CMD_CREATE_FLOATS.substitute(partition=self.partition,
                                                      node=node,
                                                      item_count=self.vector_len * num_of_processes,
                                                      num_of_processes = num_of_processes,
                                                      chunk_no=chunk_no)
            self._execute_cmd(cmd_create, execute=execute)
            # Create link
            cmd_link = CMD_LN_MINUS_S.substitute(partition=self.partition,
                                                 node=node,
                                                 HOME=os.environ["HOME"])
            self._execute_cmd(cmd_link)


    def run_ddp(self, master, execute):
        """Start executable on every node.
        """
        if master:
            print "\n==== Staring master: \n"
            mode = "master"
            nodes = [self.nodes[0]]
        else:
            print "\n==== Staring slaves: \n"
            mode = "slave"
            nodes = self.nodes[1:]

        for node in nodes:
            for i, port in enumerate(self.ports):
                dest_name = os.path.join(os.path.join(str(self.jobid),
                                                      self.ipaddr(node).replace(".", "-"),
                                                      str(i + 1),
                                                      self.executable))

                cmd_exe = CMD_START_EXECUTABLE.substitute(partition=self.partition,
                                                          node=node,
                                                          executable=dest_name,
                                                          mode=mode,
                                                          CAD=self.cad_name,
                                                          ipaddr=self.ipaddr(node),
                                                          port=port)
                self._execute_cmd(cmd_exe)


    def _execute_cmd(self, cmd_str, execute=False):
        """Execute or output (execute==False) command string. Never call this method directly.
        """
        if execute:
            try:
                subprocess.call(cmd_str, shell=True)
            except Exception, e:
                print "*** Error in execute_cmd: {0}. Exiting.".format(e)
                sys.exit(-1)
        else:
            print cmd_str




    def collect_logs(self):
        """Remove input files, executables, and move logs to a proper location.
        """
        pass





def run_test():
    dna = DNARunner(77777, "tesla", 12, "test.sh")
    dna.make_dirs()
    dna.write_CAD()
    dna.copy_executables()
    dna.create_inputs(execute=False)
    dna.run_ddp(master=False, execute=False)
    dna.run_ddp(master=True, execute=False)


if __name__== "__main__":
    # parser = PAOptionParser()
    # parser.add_posarg("temp_file", help="temporary file name")
    # parser.add_posarg("exec_name", dest="executable file name")
    #
    # values, args = parser.parse_args()
    # print values, args
    run_test()
