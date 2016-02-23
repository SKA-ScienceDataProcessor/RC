module Kernel.Utils where

import Flow(ProfileHint (..))

setDblOpts :: Int -> ProfileHint -> ProfileHint
setDblOpts opts h@FloatHint{} = h {hintDoubleOps = opts}
setDblOpts opts h@CUDAHint{} = h {hintCudaDoubleOps = opts}
setDblOpts _ _ = error "Wrong hint for setDblOpts"
