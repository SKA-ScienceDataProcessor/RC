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

# Set time required, HH:MM:SS. By default - one minute.
TIME ?= 00:01:00


# We do not provide default values for:
#  - memory bounds (TASKMB, set to be empty - no bounds needed)
#  - GPU accelerator type (GPU)
#  - network conduit (NET)

ifeq ($(strip $(PROFILING)),1)
RUNTIME_ARGS += -hl:prof $(NODES)
DEBUG := 0
endif

all: $(EXEC)

help:
	echo "Available targets: all (will build $(EXEC)), help (this text), run, clean"

run: $(EXEC)
	SDP_SCRIPT_DIR="$(SDP_SCRIPT_DIR)" SDP_BUILDDIR=$(SDP_BUILDDIR) \
	    ./${word 1, $(EXEC)} -nodes=$(NODES) -tasks=$(TASKS) -threads=$(THREADS) -mem=$(TASKMB) -gpu=$(GPU) -net=$(NET) -time=$(TIME) -- $(RUNTIME_ARGS) $(EXEC_ARGS)

clean:
	rm -f $(EXEC) $(EXEC)-local $(EXEC)-ibv *.o *.a
	LG_RT_DIR=$(SDP_BUILDDIR)/Legion-udp/runtime make -f $(SDP_BUILDDIR)/Legion-udp/runtime/runtime.mk clean
ifeq ($(SDP_USE_IBV),1)
	LG_RT_DIR=$(SDP_BUILDDIR)/Legion-udp/runtime make -f $(SDP_BUILDDIR)/Legion-ibv/runtime/runtime.mk clean
endif

EXEC_WITH_CONDUITS = $(EXEC)-local

ifeq ($(SDP_USE_IBV),1)
EXEC_WITH_CONDUITS += $(EXEC)-ibv
endif

# Flags for directing the runtime makefile what to include
DEBUG           ?= 1		# Include debugging symbols
OUTPUT_LEVEL    ?= LEVEL_PRINT	# Compile time logging level
SHARED_LOWLEVEL ?= 0		# Use shared-memory runtime (not recommended)
USE_CUDA        ?= 0		# Include CUDA support (requires CUDA)
USE_GASNET      ?= 0		# Include GASNet support (requires GASNet)
USE_HDF         ?= 0		# Include HDF5 support (requires HDF5)
ALT_MAPPERS     ?= 0		# Include alternative mappers (not recommended)

# You can modify these variables, some will be appended to by the runtime makefile
INC_FLAGS	?=
CC_FLAGS	?= -DMAX_FIELDS=64
NVCC_FLAGS	?=
GASNET_FLAGS	?=

ifeq ($(DEV_ENV),1)
LD_FLAGS	?=
else
LD_FLAGS	?= -lirc
endif

ifeq ($(DEBUG),1)
CC_FLAGS += -DDEBUG=1
endif

ifeq ($(PROFILING),1)
CC_FLAGS += -DLEGION_PROF_MESSAGES=1 -DLEGION_PROFILE=1
endif

###########################################################################
#
#   Don't change anything below here
#   
###########################################################################

process-profile:
	$(SDP_BUILDDIR)/Legion-udp/tools/legion_prof.py $(PROF_OPTIONS) $(PROF_FILES)

$(EXEC): $(EXEC_WITH_CONDUITS)
	cp $(SDP_SCRIPT_DIR)/runner_script $(EXEC)
	echo "run $(EXEC)" >>$(EXEC)
	chmod a+x $(EXEC)

$(EXEC)-local: $(SRCS)
	rm -f *.o liblegion.a librealm.a
	DEBUG=$(DEBUG) OUTPUT_LEVEL=$(OUTPUT_LEVEL) USE_GASNET=1 CC_FLAGS="$(CC_FLAGS)" USE_CUDA=$(USE_CUDA) USE_HDF=$(USE_HDF) \
	SHARED_LOWLEVEL=$(SHARED_LOWLEVEL) GASNET=$(SDP_BUILDDIR)/gasnet-udp/release GEN_SRC=$(SRCS) LD_FLAGS="$(LD_FLAGS)" \
	CONDUIT=udp GASNET_CONDUIT=udp LG_RT_DIR=$(SDP_BUILDDIR)/Legion-udp/runtime OUTFILE=$(EXEC)-local \
	make -j4 -f $(SDP_BUILDDIR)/Legion-udp/runtime/runtime.mk

ifeq ($(SDP_USE_IBV),1)
$(EXEC)-ibv: $(SRCS)
	rm -f *.o liblegion.a librealm.a
	DEBUG=$(DEBUG) OUTPUT_LEVEL=$(OUTPUT_LEVEL) USE_GASNET=1 CC_FLAGS="$(CC_FLAGS)" USE_CUDA=$(USE_CUDA) USE_HDF=$(USE_HDF) \
	SHARED_LOWLEVEL=$(SHARED_LOWLEVEL) GASNET=$(SDP_BUILDDIR)/gasnet-ibv/release GEN_SRC=$(SRCS) LD_FLAGS="$(LD_FLAGS)" \
	CONDUIT=ibv GASNET_CONDUIT=ibv LG_RT_DIR=$(SDP_BUILDDIR)/Legion-ibv/runtime OUTFILE=$(EXEC)-ibv \
	make -j4 -f $(SDP_BUILDDIR)/Legion-ibv/runtime/runtime.mk
endif
