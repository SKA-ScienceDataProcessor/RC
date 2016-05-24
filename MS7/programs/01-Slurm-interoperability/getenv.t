local c = terralib.includecstring [[
#include <stdlib.h>
]]

terra get_env(ev: rawstring) return c.getenv(ev) end

terra get_env_int(ev: rawstring)
  var val = c.getenv(ev)
  if val ~= [rawstring](0) then
    return c.atoi(val)
  end
  return 0
end
