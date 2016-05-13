import "regent"

local c = terralib.includecstring [[
#include <stdio.h>
#include <malloc.h>
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

terra gen_data(N : int)
  var data = [&c.fftw_complex](c.malloc(sizeof(c.fftw_complex)*N))
  var Nd : double = N
  var theta : double
  for n = 0,N do
    var nd : double = n
    theta = nd / Nd * c.M_PI
    data[n][c.REAL] = c.cos(10.0 * theta) + 0.5 * c.cos(15.0 * theta)
    data[n][c.IMAG] = c.sin(10.0 * theta) + 0.5 * c.sin(15.0 * theta)
  end
  return data
end

terra do_fftw(N : int, data : &c.fftw_complex)
  var result = [&c.fftw_complex](c.malloc(sizeof(c.fftw_complex)*N))
  var plan = c.fftw_plan_dft_1d(64, data, result, c.__FWD(), c.__EST())
  c.fftw_execute(plan)
  c.fftw_destroy_plan(plan)
  return result
end

terra show_max(N : int, data : &c.fftw_complex)
  var maxa : double = 0.0
  for n = 0,N do
    var mag : double = c.sqrt(data[n][c.REAL] * data[n][c.REAL] + data[n][c.IMAG] * data[n][c.IMAG])
    if mag > maxa then maxa = mag end
  end
  c.printf("%g\n", maxa)
end

task toplevel()
  var data = gen_data(64)
  var result = do_fftw(64, data)
  c.free(data)
  show_max(64, result)
  c.free(result)
end

regentlib.start(toplevel)
