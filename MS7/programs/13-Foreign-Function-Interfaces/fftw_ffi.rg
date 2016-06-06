import "regent"

local c = terralib.includecstring [[
#include "legion_c.h"
#include <stdio.h>
#define _USE_MATH_DEFINES
#include <math.h>

#define REAL 0
#define IMAG 1

#include <fftw3.h>

/* Terra can't deal with nontrivial macros */
int __FWD(){return FFTW_FORWARD;}
int __EST(){return FFTW_ESTIMATE;}
]]

local ffi = require 'ffi'
if ffi.os ~= "Windows" then
  terralib.linklibrary("libfftw3.so")
else
  terralib.linklibrary("libfftw3-3.dll")
end

terra get1dptr(
    regs : &c.legion_physical_region_t
  , ctx : c.legion_context_t
  , runtime : c.legion_runtime_t
  )
  var r = c.legion_physical_region_get_logical_region(regs[0])
  var is = r.index_space
  var d = c.legion_index_space_get_domain(runtime, ctx, is)
  var rect = c.legion_domain_get_rect_1d(d)
  var acc = c.legion_physical_region_get_accessor_generic(regs[0])

  var subrect : c.legion_rect_1d_t
  var offsets : c.legion_byte_offset_t[1]
  return [&c.fftw_complex](c.legion_accessor_generic_raw_rect_ptr_1d(
                         acc, rect, &subrect, &(offsets[0])))
end

task gen_data(
   N : int
 , d: region(ispace(int1d), double)
 )
where
  writes(d)
do
  var data = get1dptr(__physical(d), __context(), __runtime())
  var Nd : double = N
  var theta : double
  for n = 0,N do
    var nd : double = n
    theta = nd / Nd * c.M_PI
    data[n][c.REAL] = c.cos(10.0 * theta) + 0.5 * c.cos(15.0 * theta)
    data[n][c.IMAG] = c.sin(10.0 * theta) + 0.5 * c.sin(15.0 * theta)
  end
end

task do_fftw(
    N : int
  , i : region(ispace(int1d), double)
  , o : region(ispace(int1d), double)
  )
where
  reads(i),
  writes(o)
do
  var data = get1dptr(__physical(i), __context(), __runtime())
  var result = get1dptr(__physical(o), __context(), __runtime())
  var plan = c.fftw_plan_dft_1d(N, data, result, c.__FWD(), c.__EST())
  c.fftw_execute(plan)
  c.fftw_destroy_plan(plan)
  return result
end

task show_max(
   N : int
 , d: region(ispace(int1d), double)
 )
where
  reads(d)
do
  var data = get1dptr(__physical(d), __context(), __runtime())
  var maxa : double = 0.0
  for n = 0,N do
    var mag : double = c.sqrt(data[n][c.REAL] * data[n][c.REAL] + data[n][c.IMAG] * data[n][c.IMAG])
    if mag > maxa then maxa = mag end
  end
  c.printf("%g\n", maxa)
end

task toplevel()
  var isi = ispace(int1d, 64*2)
  var iso = ispace(int1d, 64*2)

  var i = region(isi, double)
  var o = region(iso, double)

  gen_data(64, i)
  do_fftw(64, i, o)
  show_max(64, o)
end

regentlib.start(toplevel)
