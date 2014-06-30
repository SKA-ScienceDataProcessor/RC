#!/bin/bash
set -x


##################################################################
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
#SBATCH --ntasks=6
#SBATCH --cpus-per-task=2
#
# Set a to be the number of process you want to launch and b the
# number of threads per process. Typically, for an MPI job with 8 
# processes, set a=8, b=1. For an OpenMP job with 12 threads, set
# a=1, b=12. For a hybrid 8-process, 12-threads per process MPI 
# job with OpenMP 'inside', set a=8, b=12.
# 
# You can also set 
#SBATCH --nodes=6
#SBATCH --ntasks-per-node=6
# 
# to force your jobs to run at most at c at a time on a single 
# node. The --exclusive option reservers the whole node for your
# job. Remove one '#' before them to activate. 
#
##################################################################
# Requested memory for each core
#
#SBATCH --mem-per-cpu=128 
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
EXE=$PWD/dist/build/dot-product/dot-product
cd $PWD 
echo -e "Changed directory to `pwd`.\n"
echo -e "Initiating all CH process\n==================================\n"
$PWD/initiate_CHprocess.sh $SLURM_JOB_NODELIST $SLURM_JOB_ID $EXE
wait

