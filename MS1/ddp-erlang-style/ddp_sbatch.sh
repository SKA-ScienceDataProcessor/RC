#!/bin/bash
#
#SBATCH --job-name=ddp-ska
#SBATCH --output=ddp.%j.txt
#SBATCH --partition=tesla
#SBATCH --time=1:00
#SBATCH --mem-per-cpu=1000
#SBATCH --nodes=100    # XXX start with 1
#SBATCH --tasks=100     # XXX same
#SBATCH --qos=gpu0

export $ITEMCOUNT=15000000
export $PROCS_PER_NODE=12
export $NODES=nodes.txt
export $CAD=CAD_dna.txt
export $DDP=`pwd`/dist/build/ddp-erlang-style-SIMD-eventlog/ddp-erlang-style-SIMD-eventlog
export $DDP_OPTS="-RTS -+RTS -l-au"
export $MIN_PORT= 44000

mkdir $SLURM_JOB_ID
cd $SLURM_JOB_ID
export NODEFILE=`generate_pbs_nodefile`
cat $NODEFILE | uniq > nodes.file ; rm $NODEFILE
../dna_cad.py
ln -s /ramdisks/INPUT.$SLURM_JOB_ID  INPUT
srun --exclusive ../create_floats.py cadfile 
srun --exclusive ../ddp.py cadfile
srun --exclusive rm /ramdisks/INPUT.$SLURM_JOB_ID
rm INPUT

