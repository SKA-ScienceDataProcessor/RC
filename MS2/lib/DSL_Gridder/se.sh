#!/bin/bash
## export CPATH=/usr/local/Cluster-Apps/cuda/6.5/include:$CPATH
export CPATH=$CPATH:/usr/local/Cluster-Apps/cuda/6.5/include
export FPATH=/usr/local/Cluster-Apps/cuda/6.5/include:$FPATH
export LIBRARY_PATH=/usr/local/Cluster-Apps/cuda/6.5/lib64:/usr/local/Cluster-Apps/cuda/6.5/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=/usr/local/Cluster-Apps/cuda/6.5/lib64:/usr/local/Cluster-Apps/cuda/6.5/lib:/usr/local/Cluster-Apps/gcc/4.8.1/lib64:/usr/local/Cluster-Apps/gcc/4.8.1/lib:$HOME/opt/lib:$LD_LIBRARY_PATH
export PATH=/usr/local/Cluster-Apps/cuda/6.5/bin:/usr/local/Cluster-Apps/cuda/6.5/samples/bin/x86_64/linux/release:$HOME/opt/bin:$PATH
export CUDA_INSTALL_PATH=/usr/local/Cluster-Apps/cuda/6.5
export CUDA_VERSION=6.5
$*
