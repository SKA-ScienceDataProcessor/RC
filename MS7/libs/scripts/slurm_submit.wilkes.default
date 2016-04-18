#!/bin/bash
#!
#! Example SLURM job script for Wilkes (Ivy Bridge, Connect-IB, K20c)
#! Last updated: Sat Apr 18 13:05:27 BST 2015
#!

#!#############################################################
#!#### Modify the options in this section as appropriate ######
#!#############################################################

#! sbatch directives begin here ###############################
#! Name of the job:
#SBATCH -J gasnet-mxm
#! Which project should be charged (NB Wilkes projects end in '-GPU'):
#SBATCH -A CORTEX-GPU
#! How many whole nodes should be allocated?
#SBATCH --nodes=2
#! How many (MPI) tasks will there be in total? (<=nodes*12)
#SBATCH --ntasks=2
#! How much wallclock time will be required?
#SBATCH --time=00:02:00
#! What types of email messages do you wish to receive?
#SBATCH --mail-type=FAIL
#! Uncomment this to prevent the job from being requeued (e.g. if
#! interrupted by node failure or system downtime):
#SBATCH --no-requeue

#! Do not change:
#SBATCH --qos GPU1
#SBATCH -p tesla

#! sbatch directives end here (put any additional directives above this line)

#! Notes:
#! Charging is determined by node number*walltime. Allocation is in entire nodes.
#! The --ntasks value refers to the number of tasks to be launched by SLURM only. This
#! usually equates to the number of MPI tasks launched. Reduce this from nodes*12 if
#! you need 1 task per GPU (use nodes*2), or if demanded by memory requirements, or if
#! OMP_NUM_THREADS>1.

#! Number of nodes and tasks per node allocated by SLURM (do not change):
numnodes=$SLURM_JOB_NUM_NODES
numtasks=$SLURM_NTASKS
mpi_tasks_per_node=$(echo "$SLURM_TASKS_PER_NODE" | sed -e  's/^\([0-9][0-9]*\).*$/\1/')
echo $mpi_tasks_per_node
#! ############################################################
#! Modify the settings below to specify the application's environment, location 
#! and launch method:

#! Optionally modify the environment seen by the application
#! (note that SLURM reproduces the environment at submission irrespective of ~/.bashrc):
#. /etc/profile.d/modules.sh                # Leave this line (enables the module command)
#module purge                               # Removes all modules still loaded
#module load default-wilkes                 # REQUIRED - loads the basic environment
#module load default-impi                # REQUIRED - loads the basic environment

#! Insert additional module load commands after this line if needed:

#! Full path to application executable: 
application="/home/mf582/GASNet-1.26.0/mxm-conduit/testam"
#application="./testhello"

#! Run options for the application:
options="5 10240"
options=""

#! Work directory (i.e. where the job will run):
workdir="$SLURM_SUBMIT_DIR"  # The value of SLURM_SUBMIT_DIR sets workdir to the directory
                             # in which sbatch is run.

#! Are you using OpenMP (NB this is unrelated to OpenMPI)? If so increase this
#! safe value to no more than 12:
export OMP_NUM_THREADS=1

#! Number of MPI tasks to be started by the application per node and in total (do not change):
np=$[${numnodes}*${mpi_tasks_per_node}]

#! The following variables define a sensible pinning strategy for Intel MPI tasks -
#! this should be suitable for both pure MPI and hybrid MPI/OpenMP jobs:
export I_MPI_PIN_DOMAIN=omp:compact # Domains are $OMP_NUM_THREADS cores in size
export I_MPI_PIN_ORDER=scatter # Adjacent domains have minimal sharing of caches/sockets
#! Notes:
#! 1. These variables influence Intel MPI only.
#! 2. Domains are non-overlapping sets of cores which map 1-1 to MPI tasks.
#! 3. I_MPI_PIN_PROCESSOR_LIST is ignored if I_MPI_PIN_DOMAIN is set.
#! 4. If MPI tasks perform better when sharing caches/sockets, try I_MPI_PIN_ORDER=compact.


#! Uncomment one choice for CMD below (add mpirun/mpiexec options if necessary):

#! Choose this for a MPI code (possibly using OpenMP) using Intel MPI.
#! Delete gpu_run_task.sh if you do _not_ wish to bind even MPI ranks to GPU0 and IB card0,
#! and odd ranks to GPU1 and IB card1. This binding may improve performance and is
#! REQUIRED for Turbostream.
#CMD="mpirun -ppn $mpi_tasks_per_node -np $np gpu_run_task.sh $application $options"

export GASNET_IBV_SPAWNER=mpi
export GASNET_SPAWNER=mpi
export GASNET_CONDUIT=ibv

export I_MPI_PMI_LIBRARY=/usr/local/Cluster-Apps/slurm/lib/libpmi.so

#CMD="mpirun -ppn $mpi_tasks_per_node -np $np $application $options"
CMD="srun -n $np ./regent.py examples/circuit.rg"

#CMD="srun $application"

#! Choose this for a pure shared-memory OpenMP parallel program on a single node:
#! (OMP_NUM_THREADS threads will be created):
#CMD="$application $options"

#! Choose this for a MPI code (possibly using OpenMP) using OpenMPI:
#CMD="mpirun -npernode $mpi_tasks_per_node -np $np $application $options"


###############################################################
### You should not have to change anything below this line ####
###############################################################

cd $workdir
echo -e "Changed directory to `pwd`.\n"

JOBID=$SLURM_JOB_ID

echo -e "JobID: $JOBID\n======"
echo "Time: `date`"
echo "Running on master node: `hostname`"
echo "Current directory: `pwd`"


echo -e "\nnumtasks=$numtasks, numnodes=$numnodes, mpi_tasks_per_node=$mpi_tasks_per_node (OMP_NUM_THREADS=$OMP_NUM_THREADS)"

echo -e "\nExecuting command:\n==================\n$CMD\n"

eval $CMD 
