#!/usr/bin/bash

# This script builds, runs and profiles Legion performance tests and some applications.
# The only meaningful way to run it is on cluster.

# Verify environment.
function bad_env {
    echo "Need SDP_BUILDDIR, SDP_SCRIPTS_DIR and SDP_USE_IBV variables set"
    exit 1
}
if [ -z "$SDP_BUILDDIR" ] ; then
    bad_env
fi
if [ -z "$SDP_SCRIPTS_DIR" ] ; then
    bad_env
fi
if [ -z "$SDP_USE_IBV" ] ; then
    bad_env
fi

if [ "$SDP_USE_IBV" != "1" ] ; then
    echo "SDP_USE_IBV should be 1 - we will run against IBV conduit on several nodes."
    exit 1
fi

# ------------------------------------------------------------------------------
# Building and running a performance test.

# Directory for performance tests.
performance_tests_dir="$SDP_BUILDDIR/Legion-ibv/test/performance/realm"
function build_run_perf_app {
    echo "building '$1'"
    cd "$performance_tests_dir/$1"
    export EXEC="$2"
    export SRCS="$3"
    make -f $SDP_SCRIPTS_DIR/cluster_build_rules.mk || exit 1

    # execute thrice.
    ./$EXEC
    ./$EXEC
    ./$EXEC
}

# ------------------------------------------------------------------------------
# Building performance apps, run them thrice. Use our building/running framework, it is useful and straightforward to use.

build_run_perf_app event_latency event_latency "event_latency.cc"
build_run_perf_app event_throughput event_throughput "event_throughput.cc"
