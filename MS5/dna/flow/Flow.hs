
-- | The DNA flow library implements strategic network-level
-- programming. The top-level idea here is that we separate our
-- programming workflow into two distinct steps:
--
--   1. Define program function: Declare what output data should be
--      produced depending on input data.
--
--   2. Define strategy: Define *how* to produce the output data in
--      the most efficient way. This generally involves distributing
--      data and computation to make use of available resources, as
--      well as arranging storage and computation order for locality.
--
-- For the design of this library, we do this in parallel on two
-- levels:
--
--    * For the network level, program function is represented using
--      "Flow"s and strategy is given by a "Strategy".
--
--    * For the kernel level, Halide is going to use function
--      definitions and strategic annotations to achieve the same
--      effect.
--
-- At this point, we make no correctness guarantees or checks between
-- these two levels, which means that for this library, the functional
-- language will be only informal. It is the responsibility of the
-- "Strategy" to ensure that the bound kernels implement the function
-- envisioned by the "Flow" in question.

module Flow
  ( -- * Abstract data flow
    -- | The functional program description. As the network strategy
    -- language only deals with satisfying data dependencies, at this
    -- point "Flow"s are basically a pure data dependency graph with
    -- some type tags attached to it. Their structure forms a weak
    -- contract that implementing "Strategy"s must honour.
    Flow, flow,

    -- * Strategy

    -- | Instantiates "Flow" networks and binds "Kernel"s
    -- to implement them. The result of this will be a number of steps
    -- that can be executed as a distributed program, such as with
    -- 'execStrategyDNA'.
    Strategy,

    -- ** Binding

    -- | Bindings tell the strategy to use "Kernel"s to
    -- calculate the data for "Flow"s of a matching type. Note that
    -- binding has operational semantics: The binding location
    -- determines both execution order as well as distribution (see
    -- 'distribute'). Therefore if you write
    --
    --   > bind kernel1 flow1
    --   > distribute dom $
    --   >   bind kernel2 flow2
    --
    -- This means that the second kernel is going to get invoked
    -- strictly after the first one finished executing! (NOTE: Almost
    -- curious that we haven't found a need for a parallelism operator
    -- yet - all our parallelism is in domains...)
    --
    -- It is therefore important to note that there are two very
    -- different ways that kernels get bound:
    --
    --   1. Manually using 'bind', 'rebind' or 'bindNew': Immediately
    --      binds a 'Kernel' using concrete data dependencies.
    --
    --   2. Automatically using 'bindRule' and 'calculate': Registers
    --      a rule for how to implement 'Flow' networks of a certain
    --      shape. Rule application will be attempted automatically
    --      whenever a missing data dependency is encountered.
    --
    -- Generally, it is a good idea to stick with 'bindRule',
    -- especially because it means that the 'Strategy' does not have
    -- to explicitly label every single 'Flow' to calculate. However,
    -- there are still a number of scenarios where you need finer
    -- control over bindings. For example, it is best to pull in input
    -- data using 'bindNew' and re-binding is only possible manually
    -- using 'rebind'.
    bind, rebind, uniq, bindNew,
    bindRule, calculate,

    -- ** Failure recovery
    recover,

    -- ** Profiling

    -- | [Note that 'ProfileHint', 'floatHint', 'memHint', 'ioHint',
    -- 'haskellHint' and 'cudaHint' are imported from "DNA" -- disregard
    -- the examples.]
    hints, ProfileHint(..), floatHint, memHint, ioHint, haskellHint, cudaHint,

    -- * Domains

    -- | 'Domain's are index sets, characterising the ways the data of
    -- a 'Flow' or the computation of a 'Kernel' can be split up.
    --
    -- Furthermore, a 'Domain' is associated with a concept of
    -- /locality/. This is what allows us to talk about 'Region'
    -- domain sub-sets, which we understand to be a neighbourhoods of
    -- values that have close locality with each other. For example,
    -- absolute distance would be an obvious locality measure for a
    -- coordinate domain. Consequently, a 'Range' is the obvious
    -- choice for a 'Region' of this domain - the neighbourhood of a
    -- central value.
    --
    -- A 'Flow' or 'Kernel' using such a domain domain implies that it
    -- makes sense for the 'Flow' to group data with close coordinates
    -- together, and the 'Kernel' would be able to continue working
    -- efficiently if we restrict it to work on a smaller
    -- 'Region'. This is the maximally relaxed principle that allows
    -- our language to distribute both data and work.
    --
    -- Note that as domains themselves are assumed to be infinite, our
    -- 'Domain' values actually more precisely refer to 'Region' sets
    -- of the domain. For primitive 'Domain's as created by, say,
    -- 'makeRangeDomain' the region set will only contain a single
    -- 'Region'. 'split' can be used to obtain a 'Domain' with a
    -- region sets that has been split up further.
    Domain,
    split, distribute,
    Schedule(..),

    -- ** Range domains

    -- | For 'Range' domains, every 'Region' is a range between two
    -- integer values. Locality is given by numerical distance.
    Range, makeRangeDomain,

    -- ** Bin domains

    -- | For 'Bin' domains, every 'Bin' consists of indices where a certain
    -- property @f@ falls into a given 'Double' range. Locality is
    -- given by numerical distance of the values of @f@.
    --
    -- This is useful for dealing with "indirections" in the data.
    -- This means that two pieces of data are local in relation to
    -- each other not depending on the index, but depending on a
    -- property of the data itself. Our main driving example are
    -- visibilities, the locality of which depend on their @uvw@
    -- coordinates.
    Bins, makeBinDomain,

    -- * Data Representations

    -- | Data representations define how the data of a 'Flow' is
    -- represented as a - potentially distributed - data structure.
    --
    -- The idea is that while a 'Flow' describes some abstract data,
    -- the encoding is determined by the 'Kernel's that produce their
    -- data (and the 'Strategy' that employs them). We need to reason
    -- about this for two main reasons:
    --
    --  1. To direct distribution of global data structures when we
    --      'distribute' kernels. The idea is that every data
    --      representation has a set of attached domains, which
    --      enumerate the ways that the data can be split up. Note
    --      however that so far this split does not happen
    --      automatically, but needs an appropriate 'Kernel' to do the
    --      dirty work.
    --
    --  2. It characterises the distribution patterns of 'Kernel's:
    --     This might be more subtle, but saying that a kernel
    --     produces a data representation that can be split along a
    --     'Domain' is the same as saying that the kernel's
    --     computation can be split according to the same
    --     'Domain'. Therefore a kernel's distribution pattern is
    --     completely determined by the distribution properties of its
    --     output data.
    --
    --  3. We also want to make sure that 'Kernel's communicate using
    --     compatible data representations -- effectively a second
    --     data flow type check. The fact that data representations
    --     are open especially means that special 'Kernel' classes can
    --     attach extra data about the data format to data
    --     representations, which are going to get checked for
    --     consistency automatically.

    NoRepr(..), RegionRepr(..), RangeRepr(..), BinRepr(..),
    MarginRepr, marginRepr,

    -- * Kernel

    -- | Kernels are the actual work-horses in our language. Their
    -- definition tpyically has only two parts: Their input and output
    -- data representations as well as the code that is going to
    -- perform the actual work.
    --
    -- A completely configured 'Kernel' will still need 'Flow's to
    -- actually satisfy its data dependencies - however this should
    -- generally be left to the 'Strategy'. So a typical 'Kernel'
    -- definition will return a partially applied function:
    --
    --  > myKernel :: Config
    --  >          -> Flow A -> Flow B
    --  >          -> Kernel C
    --  > myKernel cfg = kernel "kernel name" (aRepr :. bRepr :. Z) retRepr $ \... -> do
    --  >   --- ... kernel code ...
    --
    -- Where @aRepr@ and @bRepr@ are the two parameter data
    -- representations and @retRepr@ is the output data
    -- representation. Note that the output data representation is
    -- also going to determine the allowed distribution patterns of
    -- the kernel. Virtually all kernel declarations look this way.
    --
    -- Note that it is quite common for the configuration to have an
    -- influence on input and output data representations - data
    -- layouts but especially 'Domain's. Where this is the case, it is
    -- often expected that the 'Kernel' should work no matter into how
    -- much regions the 'Domain' happens to be split.
    Kernel, kernel,
    mappingKernel, mergingKernel, foldingKernel,
    allocReturns,
    regionKernel,

    -- * Halide

    -- ** Data
    halideRepr, dynHalideRepr, binHalideRepr,

    -- ** Standard kernels
    halidePrint, halideDump, halideTextDump2D,

    -- ** Kernel binders
    halideKernel0, halideKernel1, halideKernel2, halideKernel3,
    halideKernel1Write, halideKernel2Write,

    -- * Execute
    dumpStrategy, dumpStrategyDOT, dumpSteps,
    execStrategy, execStrategyDNA
  ) where

import Flow.Builder
import Flow.Domain
import Flow.Kernel
import Flow.Halide
import Flow.Run

import DNA (ProfileHint(..), floatHint, memHint, ioHint, haskellHint, cudaHint )
