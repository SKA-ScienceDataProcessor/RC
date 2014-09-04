__author__ = 'mcsquaredjr'
__date__ = "09/03/14"

"""Function startingCHproess implemented in Python.
"""

from string import Template

CMD_SRUN = "srun -p $partition --nodelist $machine -N1 -n1 --exclusive "

CMD_LN_MINUS_S = Template(CMD_SRUN + "ln -s /ramdisks/file.$machine $HOME/input_data.txt")
CMD_CREATE_FLOATS = Template(CMD_SRUN + "./create-floats /ramdisks/file.$machine $item_count $num_of_proc $chunk_no")
CMD_START_SLAVE = Template(CMD_SRUN + "./slave.$machine slave --cad $PWD/CAD.$JOBID.file --ip $ip_addr --port $port_num +RTS -l-au &")

CMD_RM_MINUS_RF = Template(CMD_SRUN + "rm -rf $HOME/input_data.txt")
CMD_TOUCH = Template(CMD_SRUN + "touch /ramdisks/file.$ip_addr")

def run_command(pwd, home, rc, cad_file, job_id, num_cores=12, test=True):
    """Create input files and start processes on master and slave machines,
    reading list of reserved ips from cad_fil. If test=True, just output commands,
    without running them.
    """
    with open(cad_file, "r") as cf:
        ip_address_p = ""
        for i, line in enumerate(cf):
            (ip_add, port) = line.split(":")
            if i <= num_cores - 1:
                # Run on master
                if test:
                    print CMD_RM_MINUS_RF.substitute(partion=partition, machine=machine)







