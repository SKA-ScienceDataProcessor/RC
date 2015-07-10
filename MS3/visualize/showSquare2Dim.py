# Visualize square double prec. complex arrays

import sys
import math
import numpy as np
import matplotlib.pyplot as pl

af0 = np.fromfile(sys.argv[1], dtype=np.complex128, count=-1, sep="")
lsize = math.sqrt(af0.size)
af = af0.view()
af.shape = (lsize,lsize)

pl.subplot(121)
pl.imshow(af.real)
pl.title(r"$G(u,v,w)$: Real")
pl.subplot(122)
pl.imshow(af.imag)
pl.title(r"$G(u,v,w)$: Imag")
pl.show()
