# Makefile include file to provide "compilation" for local runs of Regent files.
# Defines implicit rule to compile executable ("make" or "make exec") and cleanup rule ("make clean").
# Runs first executable (in EXEC list below) by "make run" and any executable using "make run-EXECUTABLE".
# Use in Makefile like the following:
# -------------------------------------
# # Makefile example:
#
# # Set executable name (myprog-upd and myprog-ibv will be built):
# EXEC=myprog
#
# # Sources to build from:
# SRCS= a.cc b.cc main.cc
#
# # All rules needed are provided by makefile.inc (which you read right now).
# include ../../scripts/makefile.inc
# -------------------------------------

# Set nodes counter if none given to 1.
NODES ?= 1

# Set task counter if none given to 1.
TASKS ?= 1

# Set threads count per task to 1 if none given.
THREADS ?= 1

# We do not provide default values for:
#  - memory bounds (TASKMB, set to be empty - no bounds needed)
#  - GPU accelerator type (GPU)
#  - network conduit (NET)

all: \$(EXEC)

help:
	echo "Available targets: help (this text), exec (will build $(EXEC)), run, clean"

run: \$(EXEC) \$(EXEC)-local \$(EXEC)-ibv
	./\${word 1, \$(EXEC)} -nodes=\$(NODES) -tasks=\$(TASKS) -threads=\$(THREADS) -mem=\$(TASKMB) -gpu=\$(GPU) -net=\$(NET) \$(EXEC_ARGS)

clean:
	rm -f \$(EXEC) \$(*(EXEC)-local \$(EXEC)-ibv

\$(EXEC)-local: \$(SRCS)
	CONDUIT=udp GASNET_CONDUIT=udp GASNET_ROOT=$GASNET_ROOT LG_RT_DIR=$BUILDDIR/Legion-udp/runtime OUTFILE=\$(EXEC)-local \
	make -f $BUILDDIR/Legion-udp/runtime/runtime.mk

\$(EXEC)-ibv: \$(SRCS)
	CONDUIT=ibv GASNET_CONDUIT=ibv GASNET_ROOT=$GASNET_ROOT LG_RT_DIR=$BUILDDIR/Legion-ibv/runtime OUTFILE=\$(EXEC)-ibv \
	make -f $BUILDDIR/Legion-ibv/runtime/runtime.mk

$\(EXEC): \$(SRCS)
	cp $SCRIPT_DIR

