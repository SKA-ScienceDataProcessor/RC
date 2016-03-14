-- sqrt.rg
--
-- Regent prorgam to approximate square root.
--
-- Copyright (C) Braam Research, 2016

import "regent"

local c = regentlib.c

-- Main entry point.
task main()
    -- define variable as a region - we'll call task over it.
    var X = 9.806	-- free fall acceleration.
    var X1 = X/2
    while (math.abs(X-X1*X1)<(math.max(X,X1)*0.00001)) do
        X1 = (X1+X/X1)/2
    end
    c.printf("g: %ld, sqrt(g) %ld.\n", X, X1)
end

regentlib.start(main)
