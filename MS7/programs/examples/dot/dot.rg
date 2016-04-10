-- slurm.rg
--
-- SLURM interoperability simple example.
--
-- Copyright (C) Braam Research, 2016

import "regent"

task dotp(is : ispace(int1d), x : region(is, float), y : region(is,float)) : float
where reads (x,y)
do
    var sum = 0.0
    for i in is do
        sum = sum + x[i]*y[i]
    end
    return sum
end

local c = regentlib.c  -- for printf.

task main()
    var is = ispace(int1d, 10)
    var x = region(is, float)
    var y = region(is, float)
    fill(x, 1)
    fill(y, 2)
    c.printf("dotp result: %f\n", dotp(is, x, y))
end

regentlib.start(main)
