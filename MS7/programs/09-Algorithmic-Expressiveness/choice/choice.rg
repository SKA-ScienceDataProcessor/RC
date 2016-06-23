-- choice.rg
--
-- Performing data-dependent choice of operation.
--
-- Copyright (C) Braam Research, 2016

import "regent"

local c
do c = regentlib.c
end

-- Main entry point.
task main()
    var n = 10
    var is = ispace(ptr, n)
    var rg = region(is, int32)

    new(ptr(int32, rg), n)

    -- fill region with semiinteresting date.
    var k = 1
    for i in is do
        rg[i] = k
        k = k + 1
    end

    -- coloring provide a way to compute selection criterion.
    var cspace = c.legion_coloring_create()
    for i in rg.ispace do
        var x = rg[i]
        var color = 0
        -- applying predicate to assign different colors.
        if x % 2 > 0 then
            color = 1
        end
        c.legion_coloring_add_point(cspace, color, __raw(i))
    end
    var parts = partition(disjoint, rg, cspace)
    c.legion_coloring_destroy(cspace)
    var incr = 0
    for partIndex=0, 2 do
        var part = parts[partIndex]
        for i in part.ispace do
            part[i] = part[i] + incr
        end
        -- odd elements will increase by 10, even stay the same.
        incr = incr + 10
    end
    c.printf("resulting elements:")
    for i in rg.ispace do
        c.printf(" %4d", rg[i])
    end
    c.printf("\n")
end

regentlib.start(main)
