GASNET_ROOT=../../../../../build/gasnet/release
LG_RT_DIR=../../../../../build/Legion_new/runtime

# Flags for directing the runtime makefile what to include
DEBUG           = 1		# Include debugging symbols
OUTPUT_LEVEL    = LEVEL_DEBUG	# Compile time logging level
SHARED_LOWLEVEL = 0		# Use shared-memory runtime (not recommended)
USE_CUDA        = 0		# Include CUDA support (requires CUDA)
USE_GASNET      = 1		# Include GASNet support (requires GASNet)
USE_HDF         = 0		# Include HDF5 support (requires HDF5)
ALT_MAPPERS     = 0		# Include alternative mappers (not recommended)

OUTFILE		?= resource-management
GEN_SRC		?= resource-management.cc

include $(LG_RT_DIR)/runtime.mk
