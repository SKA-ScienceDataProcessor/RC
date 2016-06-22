# Building custom mapper example from Legion examples.
#
# We use it as a useful test harness for our build script.

EXEC = static-scheduling

SRCS = static-scheduling.rg static-scheduling-support.cc

LIBRARY = static-scheduling-support

CXX_FLAGS += -I/usr/local/Cluster-Apps/slurm/include
CC_FLAGS += -I/usr/local/Cluster-Apps/slurm/include

include ../../libs/scripts/build-rules.mk
