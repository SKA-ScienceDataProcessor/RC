
Radio Cortex Prototype
==

The goal of this prototype is to be a testbed for high-level data-flow
programming techniques for implementing the SKA SDP pipeline (or
something sufficiently close to it).

Organisation
--

This repository is structured into several milestones, each with
different priorities as we learn more about the desirable
characteristics of the pipeline implementation. They often share code,
but will vary significantly in functionality.

* *MS1:* First experiments with Erlang-inspired actor languages

* *MS2:* DNA library for distribution and resource allocation, first
  attempts at gridding

* *MS3:* Systematic performance comparison of gridding implementation,
  introduction of profiling

* *MS4:* First complete imaging pipeline, abstraction over kernels
