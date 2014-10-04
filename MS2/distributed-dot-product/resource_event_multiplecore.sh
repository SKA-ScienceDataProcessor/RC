#!/bin/bash
rc=$1

#################################################################
# Please note that you need to adapt this script to your job
# Submitting it as is will fail. 
##################################################################
# Define the job name
#SBATCH -J dot-product 
#
#SBATCH --partition=tesla
# Advised: your Email here, for job notification
#SBATCH --mail-user=jaya@braamresearch.com
#SBATCH --mail-type=ALL
#     (ALL = BEGIN, END, FAIL, REQUEUE)
#
# Set a pattern for the output file.
#SBATCH --output=slurm-%j.out
#SBATCH --profile=ALL
#SBATCH --acctg-freq=network=infiniband/ofed
#  By default both standard output and  standard  error are 
# directed to a file of the name "slurm-%j.out", where the "%j" 
# is replaced with the job allocation number.   The filename 
# pattern may contain one or more replacement symbols, which are 
# a percent sign "%" followed by a letter (e.g. %j).
#
# Supported replacement symbols are:
#     %j     Job allocation number.
#     %N     Main node name.  
#
##################################################################
# The requested run-time
#
#SBATCH --time=00:05:00
#SBATCH --exclusive
# Acceptable time formats include "minutes", "minutes:seconds", 
# "hours:minutes:seconds", "days-hours", "days-hours:minutes" 
# and "days-hours:minutes:seconds"
#
# Slurm will kill your job after the requested period.
# The default time limit is the partition’s time limit.
#
# Note that the lower the requested run-time, the higher the
# chances to get scheduled to 'fill in the gaps' between other
# jobs. 
#
##################################################################
# Requested number of cores. Choose either of, or both of
#
####SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#
# Set a to be the number of process you want to launch and b the
# number of threads per process. Typically, for an MPI job with 8 
# processes, set a=8, b=1. For an OpenMP job with 12 threads, set
# a=1, b=12. For a hybrid 8-process, 12-threads per process MPI 
# job with OpenMP 'inside', set a=8, b=12.
# 
# You can also set 
#SBATCH --nodes=6
#SBATCH --ntasks-per-node=12
#SBATCH --qos=gpu0
# 
# to force your jobs to run at most at c at a time on a single 
# node. The --exclusive option reservers the whole node for your
# job. Remove one '#' before them to activate. 
#
##################################################################
# Requested memory for each core
#
#SBATCH --mem-per-cpu=1024
#
# Set the memory requirements for the job in MB. Your job will be
# allocated exclusive access to that amount of RAM. In the case it
# overuses that amount, Slurm will kill it. The default value is 
# around 2GB per core.
#
# Note that the lower the requested memory, the higher the
# chances to get scheduled to 'fill in the gaps' between other
# jobs. 
#
##################################################################
 
# end of job

##################################################################
SLURM_LOCALID=1
PWD=`pwd`

echo "Current directory: `pwd`"
JOBID=$SLURM_JOB_ID

echo -e "JobID: $JOBID and ntasks :$SLURM_NTASKS_PER_NODE\n======"
echo "Time: `date`"s
echo "Running on master node: `hostname`"

if [ "$SLURM_JOB_NODELIST" ]; then
        #! Create a machine file:
        export NODEFILE=`generate_pbs_nodefile`
        cat $NODEFILE | uniq > $PWD/machine.file.$JOBID
        echo -e "\nNodes allocated:\n================"
        echo `cat $PWD/machine.file.$JOBID | sed -e 's/\..*$//g'`
fi

slurmJobNodeList=$PWD/machine.file.$JOBID

$PWD/dna_event_multiplecore.sh check $rc $JOBID $slurmJobNodeList
if [ $? -eq 0 ]; then
	echo -e "dna check performed!!"
else
	echo -e "Some issue occured in dna check !!"
	scancel $JOBID
	exit
fi

$PWD/dna_event_multiplecore.sh generatingCADFile $rc $JOBID $slurmJobNodeList 
if [ $? -eq 0 ]; then
	echo -e "generatingCADFile performed!!"
else
	echo -e "Some issue occured in generatingCADFile !!"
	scancel $JOBID
	exit
fi

$PWD/dna_event_multiplecore.sh md5sum $rc 0 0 
if [ $? -eq 0 ]; then
	echo -e "md5sum performed!!"
else
	echo -e "Some issue occured in md5sum !!"
	scancel $JOBID
	exit
fi

$PWD/dna_event_multiplecore.sh startingCHprocess $rc $JOBID machine.file.$JOBID 
if [ $? -eq 0 ]; then
	echo -e "startingCHprocess performed!!"
else
	echo -e "Some issue occured in startingCHprocess !!"
	scancel $JOBID
	exit
fi

$PWD/dna_event_multiplecore.sh collectLogs 0 $JOBID $slurmJobNodeList
if [ $? -eq 0 ]; then
	echo -e "collectLogs performed!!"
else
	echo -e "Some issue occured in collectLogs !!"
	exit
fi

wait
