#!/bin/bash
#
#SBATCH --job-name=ddp-ska
#SBATCH --output=ddp.%j.txt
#SBATCH --partition=tesla
#SBATCH --time=5:00
#SBATCH --mem-per-cpu=MaxMemPerCPU
#SBATCH --nodes=100    # XXX start with 1
#SBATCH --tasks=100     # XXX same
#SBATCH --qos=gpu0

set -x
set -e

export CAD_FILE=CAD_dna.txt
export ITEMCOUNT=1250000
export PROCS_PER_NODE=12
export CAD=`pwd`/$SLURM_JOB_ID/CAD_dna.txt
export DDP=`pwd`/dist/build/ddp-erlang-style-SIMD-eventlog/ddp-erlang-style-SIMD-eventlog
export DDP_OPTS=" +RTS -l-au"
export MIN_PORT=44000

export NODE_FILE=`generate_pbs_nodefile`

mkdir $SLURM_JOB_ID
cd $SLURM_JOB_ID
cat $NODE_FILE | uniq > nodes.file
../dna_cad.py
ln -s /ramdisks/INPUT.$SLURM_JOB_ID  INPUT
srun --exclusive ../create_floats.py
srun --exclusive ../ddp.py
srun --exclusive rm /ramdisks/INPUT.$SLURM_JOB_ID
rm INPUT $NODE_FILE
