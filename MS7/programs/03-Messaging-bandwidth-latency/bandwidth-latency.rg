-- Messaging bandwidth and latency measurements demo.
--
-- Copyright (C) 2016 Braam Research, LLC.

import "regent"

local support
do
    terralib.linklibrary(os.getenv("SDP_SUPPORT_LIBRARY"))
    support = terralib.includec("bandwidth-latency-support.h",{"-I."})
end

task receive_change(is : ispace(int1d), r : region(is, int))
where writes(r)
do
    support.node_log("received")
    for i in is do
        r[i] = 1
    end
    support.node_log("modified, sent back")
end

task top_level()
    var size = 1024
    for i=10, 27 do
        var is = ispace(int1d, size)
        var r = region (is, int)
        fill (r,0)
        support.node_log("sending message size %d",size)
        receive_change(is, r)
        for j in is do
            if r[j] == 1 then
                support.node_log("received back")
            else
                support.node_log("invalid data received")
            end
            break
        end
        size = size * 2
    end
end

-- Register mappers and setup support vars.
support.register_mappers()

-- start main work.
regentlib.start(top_level)

