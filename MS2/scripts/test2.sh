#!/bin/bash

set -x
set -e

export GHC_EVENTS=`pwd`/.cabal-sandbox/bin/ghc-events
export SLURM_JOB_ID=testdir
export CAD_FILE=`pwd`/$SLURM_JOB_ID/CAD_dna.txt
export CAD=`pwd`/$SLURM_JOB_ID/CAD_dna.txt
#export DDP=`pwd`/dist/build/ddp-erlang-style-SIMD-eventlog/ddp-erlang-style-SIMD-eventlog
export DDP=`pwd`/dist/build/ddp-erlang-style/ddp-erlang-style
export DDP_OPTS=" +RTS -l-au"
export MIN_PORT=44000
export NODE_FILE=nodes.txt
export ITEMCOUNT=1500000
export PROCS_PER_NODE=4

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

