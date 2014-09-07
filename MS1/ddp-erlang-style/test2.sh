#!/bin/bash

set -x
set -e

export SLURM_JOB_ID=testdir
export CAD_FILE=cad_file.txt
export ITEMCOUNT=15000000
export PROCS_PER_NODE=4
export CAD=`pwd`/$SLURM_JOB_ID/CAD_dna.txt
export DDP=`pwd`/dist/build/ddp-erlang-style-SIMD-eventlog/ddp-erlang-style-SIMD-eventlog
export DDP_OPTS=" +RTS -l-au"
export MIN_PORT=44000
export NODE_FILE=nodes.txt


mkdir $SLURM_JOB_ID

cd $SLURM_JOB_ID
cat > $NODE_FILE <<EOF
localhost
EOF


# cat $NODE_FILE | uniq > nodes.file
../dna_cad.py
ln -s /ramdisks/INPUT.$SLURM_JOB_ID  INPUT
../create_floats.py 
../ddp.py
rm /ramdisks/INPUT.$SLURM_JOB_ID
rm INPUT $NODE_FILE

