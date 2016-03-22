-- sqrt.rg
--
-- Regent prorgam to approximate square root.
--
-- Copyright (C) Braam Research, 2016

import "regent"

local c = regentlib.c

terra absf(a : float) : float
    if a < 0
        then return (-a)
        else return a
    end
end

terra maxf(a : float, b : float) : float
    if a > b
        then return a
        else return b
    end
end

-- Main entry point.
task main()
    -- define variable as a region - we'll call task over it.
    var X = 9.806	-- free fall acceleration.
    var X1 = X/2
    while (absf(X-X1*X1)>(maxf(X,X1)*0.00001)) do
        X1 = (X1+X/X1)/2
    end
    c.printf("g: %lf, sqrt(g) %lf.\n", X, X1)
end

regentlib.start(main)
