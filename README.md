
SDP - DSL Project: Radio Cortex
==

The goal of this project is to explore high-level data-flow and DSL
programming techniques for implementing SKA SDP pipelines (or programs
something sufficiently close to it).

Setup Instructions
--

This repository is structured into several milestones, each with
different priorities as we learn more about the desirable
characteristics of the pipeline implementation. They often share code,
but will vary significantly in functionality.

* [MS1:](https://github.com/SKA-ScienceDataProcessor/RC/tree/master/MS1) First experiments with Erlang-inspired actor languages

* [MS2:](https://github.com/SKA-ScienceDataProcessor/RC/tree/master/MS2) DNA library for distribution and resource allocation, first attempts at gridding using [Halide](http://halide-lang.org) and C++ / Cuda kernels.

* [MS3:](https://github.com/SKA-ScienceDataProcessor/RC/tree/master/MS3) Systematic performance comparison of gridding implementation,
  introduction of profiling

* [MS4:](https://github.com/SKA-ScienceDataProcessor/RC/tree/master/MS4) First complete imaging pipeline, abstraction over kernels
 
Background on DSL design
--

Our libraries for data flow and actor interfaces are documented on the [github wiki of this repository](https://github.com/SKA-ScienceDataProcessor/RC/wiki)
