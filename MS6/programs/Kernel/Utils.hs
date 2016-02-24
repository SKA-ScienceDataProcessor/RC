module Kernel.Utils where

import Flow(ProfileHint (..))

setDblOpts :: Int -> ProfileHint -> ProfileHint
setDblOpts opts h@FloatHint{} = h {hintDoubleOps = opts}
setDblOpts opts h@CUDAHint{} = h {hintCudaDoubleOps = opts}
setDblOpts _ ph = ph

-- Perhaps these should be separated ... at least, CUDA stuff ...
setReadOps :: Int -> ProfileHint -> ProfileHint
setReadOps n h@MemHint{} = h {hintMemoryReadBytes = n}
setReadOps n h@IOHint{} = h {hintReadBytes = n}
setReadOps n h@CUDAHint{} = h {hintCopyBytesDevice = n}
setReadOps _ ph = ph

setWriteOps :: Int -> ProfileHint -> ProfileHint
setWriteOps n h@IOHint{} = h {hintWriteBytes = n}
setWriteOps n h@CUDAHint{} = h {hintCopyBytesHost = n}
setWriteOps _ ph = ph

setAllOps :: Int -> Int -> Int -> ProfileHint -> ProfileHint
setAllOps nops nread nwritten = setDblOpts nops . setReadOps nread . setWriteOps nwritten
