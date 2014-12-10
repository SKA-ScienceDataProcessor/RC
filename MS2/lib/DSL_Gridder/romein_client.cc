// (C) 2012  John Romein/ASTRON

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#if defined __AVX__ //&& !defined __CUDA__ && !defined __OPENCL__
#include <immintrin.h>
#endif

#if defined __SSE3__ && !defined __CUDA__ && !defined __OPENCL__
#include <tmmintrin.h>
#endif

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>

#if defined __linux__
#include <sys/time.h>
#endif

//#if defined __OPENCL__
//#include <boost/multi_array.hpp>
//#endif

#include "Common.h"
#include "Defines.h"

#if defined __OPENCL__
#if MODE != MODE_SIMPLE && MODE != MODE_OVERSAMPLE
#error Unsupported MODE in OpenCL
#endif
#if defined USE_TEXTURE && ORDER == ORDER_W_V_OV_U_OU
#error Unsupported ORDER with USE_TEXTURE
#endif
#endif



#if MODE == MODE_SIMPLE
typedef double2 SupportType[SUPPORT_V][SUPPORT_U];
#elif MODE == MODE_OVERSAMPLE
#if ORDER == ORDER_W_OV_OU_V_U
typedef double2 SupportType[W_PLANES][OVERSAMPLE_V][OVERSAMPLE_U][SUPPORT_V][SUPPORT_U];
#elif ORDER == ORDER_W_V_OV_U_OU
typedef double2 SupportType[W_PLANES][SUPPORT_V][OVERSAMPLE_V][SUPPORT_U][OVERSAMPLE_U];
#endif
#elif MODE == MODE_INTERPOLATE
typedef double2 SupportType[W_PLANES][SUPPORT_V + 2][SUPPORT_U + 2];
#endif

typedef double2 GridType[GRID_V][GRID_U][POLARIZATIONS];
typedef double3 UVWtype[BASELINES][TIMESTEPS][CHANNELS];
typedef double2 VisibilitiesType[BASELINES][TIMESTEPS][CHANNELS][POLARIZATIONS];


unsigned nrThreads;


void addGrids(GridType a, const GridType b[])
{
#if defined __AVX__
#pragma omp parallel for
  for (int i = 0; i < GRID_V * GRID_U * POLARIZATIONS / 2; i ++) {
    __m256 sum = ((__m256 *) b)[i];

    for (unsigned g = 1; g < nrThreads; g ++)
      sum = _mm256_add_ps(sum, ((__m256 *) b[g])[i]);

    ((__m256 *) a)[i] = sum;
  }
#elif defined __SSE2__ && !defined __CUDA__ && !defined __OPENCL__
#pragma omp parallel for schedule(static, 65536)
  for (int i = 0; i < GRID_V * GRID_U * POLARIZATIONS; i ++) {
    __m128 sum = ((__m128 *) b)[i];

    for (unsigned g = 1; g < nrThreads; g ++)
      sum = _mm_add_ps(sum, ((__m128 *) b[g])[i]);

    ((__m128 *) a)[i] = sum;
  }
#else
#pragma omp parallel for
  for (int v = 0; v < GRID_V; v ++)
    for (unsigned u = 0; u < GRID_U; u ++)
      for (unsigned pol = 0; pol < POLARIZATIONS; pol ++) {
        double2 sum = b[0][v][u][pol];

        for (unsigned g = 1; g < nrThreads; g ++)
          sum += b[g][v][u][pol];

        a[v][u][pol] += sum;
      }
#endif
}


#if defined __OPENCL__

const char *errorMessage(cl::Error &error)
{
  switch (error.err()) {
    case CL_SUCCESS:                            return "Success!";
    case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
    case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
    case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
    case CL_OUT_OF_RESOURCES:                   return "Out of resources";
    case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
    case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
    case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
    case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
    case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
    case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
    case CL_MAP_FAILURE:                        return "Map failure";
    case CL_INVALID_VALUE:                      return "Invalid value";
    case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
    case CL_INVALID_PLATFORM:                   return "Invalid platform";
    case CL_INVALID_DEVICE:                     return "Invalid device";
    case CL_INVALID_CONTEXT:                    return "Invalid context";
    case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
    case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
    case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
    case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
    case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
    case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
    case CL_INVALID_SAMPLER:                    return "Invalid sampler";
    case CL_INVALID_BINARY:                     return "Invalid binary";
    case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
    case CL_INVALID_PROGRAM:                    return "Invalid program";
    case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
    case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
    case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
    case CL_INVALID_KERNEL:                     return "Invalid kernel";
    case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
    case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
    case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
    case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
    case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
    case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
    case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
    case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
    case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
    case CL_INVALID_EVENT:                      return "Invalid event";
    case CL_INVALID_OPERATION:                  return "Invalid operation";
    case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
    case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
    case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
    default:                                    return "Unknown";
  }
}

#endif


unsigned optimalNrThreads(unsigned jobs, unsigned maxNrThreads)
{
  return (unsigned) ceilf(jobs / ceilf((double) jobs / maxNrThreads) / 32) * 32;
}


#if defined __CUDA__

#if MODE == MODE_SIMPLE || MODE == MODE_OVERSAMPLE
#if defined USE_TEXTURE
// texture<double2, 1, cudaReadModeElementType> supportTexture;
texture<int4, 1, cudaReadModeElementType> supportTexture;
#endif
#elif MODE == MODE_INTERPOLATE
// texture<double2, 3, cudaReadModeElementType> supportTexture;
texture<int4, 3, cudaReadModeElementType> supportTexture;
#endif


#if 0
__global__ void computeSupportFunction(SupportType support, float w, float2 cellSize)
{
#if 1
  for (int w = 0; w < W_PLANES; w ++) {
    for (int v = 1; v < SUPPORT_V + 1; v ++) {
      for (int u = 1; u < SUPPORT_U + 1; u ++) {
        support[w][v][u] = make_float2(10000 * w + 100 * v + u, 0);
      }

      support[w][v][0] = make_float2(0, 0);
      support[w][v][SUPPORT_U + 1] = make_float2(0, 0);
    }

    for (int u = 0; u < SUPPORT_U + 2; u ++) {
      support[w][0][u] = make_float2(0, 0);
      support[w][SUPPORT_V + 1][u] = make_float2(0, 0);
    }
  }

#else
  w *= 2.0f * (float) M_PI;

  float ccfx[SUPPORT_U], ccfy[SUPPORT_V];
  float x2[SUPPORT_U], y2[SUPPORT_V];

  float ccellx = 1.0f / cellSize.x;
  float ccelly = 1.0f / cellSize.y;

  for (int x = threadIdx.x; x < SUPPORT_U; x += blockDim.x) {
    float tmp = (x - SUPPORT_U / 2) * ccellx;
    x2[x] = tmp * tmp;
  }

  for (int y = threadIdx.x; y < SUPPORT_V; y += blockDim.x) {
    float tmp = (y - SUPPORT_V / 2) * ccelly;
    y2[y] = tmp * tmp;
  }

  for (unsigned i = threadIdx.x; i < SUPPORT_U * SUPPORT_V; i += blockDim.x) {
    unsigned x = i % SUPPORT_U;
    unsigned y = i / SUPPORT_U;

    float r2 = x2[x] + y2[x];

    if (r2 < 1.0f) {
      float phase = w * (1.0f - sqrt(1.0f - r2));
      float wt = ccfx[x] * ccfy[y];
      float s, c;
      // s = sin(phase), c = cos(phase);
      __sincosf(phase, &s, &c);
      support[REAL][y][x] = wt * c;
      support[IMAG][y][x] = -wt * s;
    } else {
      support[REAL][y][x] = 0;
      support[IMAG][y][x] = 0;
    }
  }
#endif
}
#endif



#if 0
__device__ void atomicAdd1(float *ptr, float sum)
{
  float old_v, new_v;

  do {
    old_v = *ptr;
    new_v = old_v + sum;
  } while (atomicCAS((unsigned *) ptr, __float_as_int(old_v), __float_as_int(new_v)) != __float_as_int(old_v));
}


__device__ void atomicAdd(float2 *ptr, float2 sum)
{
  union {
    float2 f2;
    unsigned long long ull;
  } old_v, new_v;

  do {
    old_v.f2 = *ptr;
    new_v.f2.x = old_v.f2.x + sum.x;
    new_v.f2.y = old_v.f2.y + sum.y;
  } while (atomicCAS((unsigned long long *) ptr, old_v.ull, new_v.ull) != old_v.ull);
}
#endif

__device__ void atomicAdd(double2 *ptr, double2 sumXX, double2 sumXY, double2 sumYX, double2 sumYY)
{
#if 1
  atomicAdd(&ptr[0].x, sumXX.x);
  atomicAdd(&ptr[0].y, sumXX.y);
  atomicAdd(&ptr[1].x, sumXY.x);
  atomicAdd(&ptr[1].y, sumXY.y);
  atomicAdd(&ptr[2].x, sumYX.x);
  atomicAdd(&ptr[2].y, sumYX.y);
  atomicAdd(&ptr[3].x, sumYY.x);
  atomicAdd(&ptr[3].y, sumYY.y);
#elif 1
  ptr[0].x += sumXX.x;
  ptr[0].y += sumXX.y;
  ptr[1].x += sumXX.x;
  ptr[1].y += sumXX.y;
  ptr[2].x += sumXX.x;
  ptr[2].y += sumXX.y;
  ptr[3].x += sumXX.x;
  ptr[3].y += sumXX.y;
#else
  atomicAdd(ptr + 0, sumXX);
  atomicAdd(ptr + 1, sumXY);
  atomicAdd(ptr + 2, sumYX);
  atomicAdd(ptr + 3, sumYY);
#endif
}


__device__ void addSupportPixel(double2 &sum, double2 supportPixel, double2 vis)
{
  sum.x += supportPixel.x * vis.x;
  sum.y += supportPixel.x * vis.y;
  sum.x -= supportPixel.y * vis.y;
  sum.y += supportPixel.y * vis.x;
}


#if 0
__device__ void addSupportPixel(float2 &sumXX, float2 &sumXY, float2 &sumYX, float2 &sumYY, float2 supportPixel, float2 visXX, float2 visXY, float2 visYX, float2 visYY)
{
  sumXX.x += supportPixel.x * visXX.x;
  sumXY.x += supportPixel.x * visXY.x;
  sumYX.x += supportPixel.x * visYX.x;
  sumYY.x += supportPixel.x * visYY.x;

  sumXX.y += supportPixel.x * visXX.y;
  sumXY.y += supportPixel.x * visXY.y;
  sumYX.y += supportPixel.x * visYX.y;
  sumYY.y += supportPixel.x * visYY.y;

  sumXX.x -= supportPixel.y * visXX.y;
  sumXY.x -= supportPixel.y * visXY.y;
  sumYX.x -= supportPixel.y * visYX.y;
  sumYY.x -= supportPixel.y * visYY.y;

  sumXX.y += supportPixel.y * visXX.x;
  sumXY.y += supportPixel.y * visXY.x;
  sumYX.y += supportPixel.y * visYX.x;
  sumYY.y += supportPixel.y * visYY.x;
}
#endif



__shared__ int4   shared_info[TIMESTEPS][CHANNELS];
#if MODE == MODE_INTERPOLATE
__shared__ double2 shared_frac[TIMESTEPS][CHANNELS];
#endif
__shared__ double2 shared_vis[TIMESTEPS][CHANNELS][POLARIZATIONS];


__device__ void loadIntoSharedMem(const VisibilitiesType visibilities,
                                  const UVWtype uvw,
                                  const uint2 supportPixelsUsed[BASELINES])
{
//__prof_trigger(0);
  unsigned bl = blockIdx.x;
  uint2    supportSize = supportPixelsUsed[bl];

  for (unsigned ch = threadIdx.x; ch < CHANNELS * TIMESTEPS; ch += blockDim.x) {
//__prof_trigger(1);
    double3   coords = uvw[bl][0][ch];

#if MODE == MODE_SIMPLE
    unsigned u_int  = __double2int_rn(coords.x);
    unsigned v_int  = __double2int_rn(coords.y);
#else
    unsigned u_int  = __double2int_rz(coords.x);
    unsigned v_int  = __double2int_rz(coords.y);
    double    u_frac = coords.x - u_int;
    double    v_frac = coords.y - v_int;
#endif

#if MODE == MODE_SIMPLE
    shared_info[0][ch] = make_int4(-u_int % supportSize.x, -v_int % supportSize.y, 0, u_int + GRID_U * v_int);
#elif MODE == MODE_OVERSAMPLE
#if ORDER == ORDER_W_OV_OU_V_U
    unsigned uv_frac_w_offset = (unsigned) coords.z * SUPPORT_V * SUPPORT_U * OVERSAMPLE_V * OVERSAMPLE_U + SUPPORT_U * SUPPORT_V * (OVERSAMPLE_U * (unsigned) (OVERSAMPLE_V * v_frac) + (unsigned) (OVERSAMPLE_U * u_frac));
#elif ORDER == ORDER_W_V_OV_U_OU
    unsigned uv_frac_w_offset = (unsigned) coords.z * SUPPORT_V * OVERSAMPLE_V * SUPPORT_U * OVERSAMPLE_U + (unsigned) (OVERSAMPLE_V * v_frac) * SUPPORT_U * OVERSAMPLE_U + (unsigned) (OVERSAMPLE_U * u_frac);
#endif
    shared_info[0][ch] = make_int4(-u_int % supportSize.x, -v_int % supportSize.y, uv_frac_w_offset, u_int + GRID_U * v_int);
#elif MODE == MODE_INTERPOLATE
    supportSize.x += 1, supportSize.y += 1;
    shared_info[0][ch] = make_int4(-u_int % supportSize.x, -v_int % supportSize.y, __double_as_int(coords.z + .5f), u_int + GRID_U * v_int);
    shared_frac[0][ch] = make_double2(u_frac, v_frac);
#endif
  }

  for (unsigned i = threadIdx.x; i < CHANNELS * TIMESTEPS * POLARIZATIONS / 2; i += blockDim.x)
    ((double4 *) shared_vis)[i] = ((double4 *) visibilities[bl])[i];
}


__device__ void convolve(GridType grid,
#if !defined USE_TEXTURE
                         const SupportType support,
#endif
                         const uint2 supportPixelsUsed[BASELINES])
{
  unsigned bl        = blockIdx.x;

#if MODE == MODE_SIMPLE || MODE == MODE_OVERSAMPLE
  uint2  supportSize = supportPixelsUsed[bl];
#elif MODE == MODE_INTERPOLATE
  uint2  supportSize = make_uint2(supportPixelsUsed[bl].x + 1, supportPixelsUsed[bl].y + 1);
  double2 scale       = make_double2((double) SUPPORT_U / (supportSize.x - 1), (double) SUPPORT_V / (supportSize.y - 1));
  double2 offset      = make_double2(scale.x *.5f + 1.f, scale.y *.5f + 1.f);
#endif

  /*if (threadIdx.x < optimalNrThreads)*/ {
    //for (int i = threadIdx.x; i < supportSize.x * supportSize.y; i += optimalNrThreads) {
    for (int i = supportSize.x * supportSize.y - threadIdx.x - 1; i >= 0; i -= blockDim.x) {
//__prof_trigger(2);
#if MODE == MODE_SIMPLE || MODE == MODE_OVERSAMPLE
      int box_u = - (i % supportSize.x);
      int box_v = - (i / supportSize.x);
#elif MODE == MODE_INTERPOLATE
      int box_u = (i + 2) % supportSize.x - supportSize.x;
      int box_v = (i / supportSize.x + 2) % supportSize.y - supportSize.y;
#endif

      double2 sumXX = make_double2(0, 0);
      double2 sumXY = make_double2(0, 0);
      double2 sumYX = make_double2(0, 0);
      double2 sumYY = make_double2(0, 0);

      unsigned grid_point = threadIdx.x;

      /*for (unsigned time = 0; time < TIMESTEPS; time ++)*/ {
//#pragma unroll 2
        for (unsigned ch = 0; ch < CHANNELS * TIMESTEPS; ch ++) {
//__prof_trigger(3);
          int4 info = shared_info[0][ch];

          int my_support_u = box_u + info.x;
          int my_support_v = box_v + info.y;

          if (my_support_u < 0)
            my_support_u += supportSize.x;

          if (my_support_v < 0)
            my_support_v += supportSize.y;

#if MODE == MODE_SIMPLE || MODE == MODE_OVERSAMPLE
          unsigned index_u = my_support_u;
          unsigned index_v = my_support_v;

#if 0 && defined USE_SYMMETRY
          if (index_u > (supportSize.x - 1) / 2)
            index_u = supportSize.x - 1 - index_u;

          if (index_v > (supportSize.y - 1) / 2)
            index_v = supportSize.y - 1 - index_v;
#endif

#if MODE == MODE_SIMPLE || (MODE == MODE_OVERSAMPLE && ORDER == ORDER_W_OV_OU_V_U)
          unsigned supportIndex = index_u + SUPPORT_U * index_v + info.z;
#elif MODE == MODE_OVERSAMPLE && ORDER == ORDER_W_V_OV_U_OU
          unsigned supportIndex = OVERSAMPLE_U * index_u + OVERSAMPLE_V * SUPPORT_U * OVERSAMPLE_U * index_v + info.z;
#endif

#if defined USE_TEXTURE
          double2 supportPixel = fetch_double2(supportTexture, supportIndex);
#else
          double2 supportPixel = support[0][0][0][0][supportIndex];
#endif

#elif MODE == MODE_INTERPOLATE
          double u_frac = shared_frac[0][ch].x;
          double v_frac = shared_frac[0][ch].y;

          double index_u = scale.x * (my_support_u - u_frac) + offset.x;
          double index_v = scale.y * (my_support_v - v_frac) + offset.y;

#if defined USE_SYMMETRY
          if (index_u >= SUPPORT_U / 2)
            index_u = 2.f + SUPPORT_U - index_u;

          if (index_v >= SUPPORT_V / 2)
            index_v = 2.f + SUPPORT_V - index_v;
#endif

          double2 supportPixel = tex3D(supportTexture, index_u, index_v, __int_as_double(info.z));
#endif

          unsigned new_grid_point = my_support_u + GRID_U * my_support_v + info.w;

          if (new_grid_point != grid_point) {
//__prof_trigger(4);
            atomicAdd(&grid[0][grid_point][0], sumXX, sumXY, sumYX, sumYY);

            sumXX = make_double2(0, 0);
            sumXY = make_double2(0, 0);
            sumYX = make_double2(0, 0);
            sumYY = make_double2(0, 0);

            grid_point = new_grid_point;
          }

          addSupportPixel(sumXX, supportPixel, shared_vis[0][ch][0]);
          addSupportPixel(sumXY, supportPixel, shared_vis[0][ch][1]);
          addSupportPixel(sumYX, supportPixel, shared_vis[0][ch][2]);
          addSupportPixel(sumYY, supportPixel, shared_vis[0][ch][3]);
        }
      }

      atomicAdd(&grid[0][grid_point][0], sumXX, sumXY, sumYX, sumYY);
    }
  }
}


//#if MODE == MODE_SIMPLE || MODE == MODE_OVERSAMPLE
#define MIN(A,B)                        ((A) < (B) ? (A) : (B))
#define NR_THREADS_PER_BLOCK            MIN(SUPPORT_U * SUPPORT_V, 1024)
#define MIN_BLOCKS_PER_MULTIPROCESSOR   (2048 / NR_THREADS_PER_BLOCK)
__global__ __launch_bounds__(NR_THREADS_PER_BLOCK, MIN_BLOCKS_PER_MULTIPROCESSOR)
//#else
//__global__ __launch_bounds__(1024, 1)
//#endif
void addToGrid(GridType grid,
#if !defined USE_TEXTURE
               const SupportType support,
#endif
               const VisibilitiesType visibilities,
               const UVWtype uvw,
               const uint2 supportPixelsUsed[BASELINES])
{
  loadIntoSharedMem(visibilities, uvw, supportPixelsUsed);
  syncthreads();

#if defined USE_TEXTURE
  convolve(grid, supportPixelsUsed);
#else
  convolve(grid, support, supportPixelsUsed);
#endif
}


#if defined DEGRIDDING

__global__ void degrid(const GridType grid,
                       VisibilitiesType visibilities,
                       const UVWtype uvw,
                       const uint2 supportPixelsUsed[BASELINES])
{
  unsigned bl          = blockIdx.x;
  uint2    supportSize = supportPixelsUsed[bl];

  for (unsigned ch = threadIdx.x; ch < CHANNELS * TIMESTEPS; ch += blockDim.x) {
    double2 sumXX = make_double2(0, 0);
    double2 sumXY = make_double2(0, 0);
    double2 sumYX = make_double2(0, 0);
    double2 sumYY = make_double2(0, 0);

    double grid_u = uvw[bl][0][ch].x;
    double grid_v = uvw[bl][0][ch].y;

#if MODE == MODE_SIMPLE
    unsigned u_int  = __double2uint_rn(grid_u), v_int = __double2uint_rn(grid_v);
#elif MODE == MODE_OVERSAMPLE
    unsigned u_int  = (unsigned) grid_u, v_int = (unsigned) grid_v;
    unsigned w_int  = (unsigned) uvw[bl][0][ch].z;
    double    u_frac = grid_u - u_int;
    double    v_frac = grid_v - v_int;
    unsigned u_os   = SUPPORT_U * (unsigned) (OVERSAMPLE_U * u_frac);
    unsigned v_os   = SUPPORT_V * (unsigned) (OVERSAMPLE_V * v_frac);
#endif

    for (unsigned v = 0; v < supportSize.y; v ++) {
      const double2 *gridPtr = grid[v_int + v][u_int];

      for (unsigned u = 0; u < supportSize.x; u ++, gridPtr += POLARIZATIONS) {
#if MODE == MODE_SIMPLE
        double2 supportPixel = tex2D(supportTexture, u, v);
#elif MODE == MODE_OVERSAMPLE
        double2 supportPixel = tex3D(supportTexture, u + u_os, v + v_os, w_int);
#endif

        addSupportPixel(sumXX, supportPixel, gridPtr[0]);
        addSupportPixel(sumXY, supportPixel, gridPtr[1]);
        addSupportPixel(sumYX, supportPixel, gridPtr[2]);
        addSupportPixel(sumYY, supportPixel, gridPtr[3]);
      }
    }

    visibilities[bl][0][ch][0] = sumXX;
    visibilities[bl][0][ch][1] = sumXY;
    visibilities[bl][0][ch][2] = sumYX;
    visibilities[bl][0][ch][3] = sumYY;
  }
}

#endif // defined DEGRIDDING
#endif // defined CUDA

// The code *already uses a lot of static data,
// then one more would not change things significantly.
#ifdef USE_REAL_UVW
#ifdef ORIGINAL_EXE
extern double realUVW[2160][44 * 45 / 2][3];
#else
static const double * realUVW;
static const double2 * amps;
#endif
#endif

void initUVW(UVWtype uvw, uint2 supportPixelsUsed[BASELINES], const double frequencies[CHANNELS], unsigned block)
{
  double scale_u[CHANNELS], scale_v[CHANNELS], scale_w[CHANNELS];

  for (unsigned ch = 0; ch < CHANNELS; ch ++) {
    scale_u[ch] = frequencies[ch] / (CELL_SIZE_U * SPEED_OF_LIGHT);
    scale_v[ch] = frequencies[ch] / (CELL_SIZE_V * SPEED_OF_LIGHT);
    scale_w[ch] = frequencies[ch] / (CELL_SIZE_W * SPEED_OF_LIGHT);
  }

#if !defined USE_REAL_UVW
  drand48_data drandState;
  memset(&drandState, 0, sizeof drandState);

  for (unsigned bl = 0; bl < BASELINES; bl ++) {
    double randomNumber, r, phi;
    drand48_r(&drandState, &randomNumber);
    r = MAX_BASELINE_LENGTH * (.70 * randomNumber + .05);
#if 0
    supportPixelsUsed[bl].x = 62 * (.5 * randomNumber + .5);
    supportPixelsUsed[bl].y = 62 * (.5 * randomNumber + .5);
#else
    supportPixelsUsed[bl].x = X; // SUPPORT_U / 2;
    supportPixelsUsed[bl].y = X; // SUPPORT_V / 2;
#endif

    drand48_r(&drandState, &phi);

    for (unsigned time = 0; time < TIMESTEPS; time ++) {
      double angle = 2 * M_PI * ((block * TIMESTEPS + time) / 86400.0 + phi);

      for (unsigned ch = 0; ch < CHANNELS; ch ++) {
        uvw[bl][time][ch] = make_double3(
#if 0
          r * cos(angle) * scale_u[ch] + GRID_U / 2.0 - supportPixelsUsed[bl].x / 2.0,
          r * sin(angle) * scale_v[ch] + GRID_V / 2.0 - supportPixelsUsed[bl].y / 2.0,
          W_PLANES > 1 ? fmod(.1 * bl + .01 * (block * TIMESTEPS + time), W_PLANES - 1) : 0
#else
          98.9, 98, 1.9
#endif
        );

#if 0
        uvw[bl][time][ch].x = nearbyintf(8 * uvw[bl][time][ch].x) / 8;
        uvw[bl][time][ch].y = nearbyintf(8 * uvw[bl][time][ch].y) / 8;
        uvw[bl][time][ch].z = nearbyintf(1 * uvw[bl][time][ch].z) / 1;
#endif
      }
    }
  }
#else
  assert(NR_STATIONS <= 44);
  assert(TIMESTEPS <= 2160);

//#pragma omp parallel for num_threads(4)
  for (unsigned bl = 0; bl < BASELINES; bl ++) {
    // FIXME!!!
    // WTF is this? It goes beyond the end of data.
    // It is also perhaps related to that mystical 45 instead of 43 in the original code.
    // Ask people about this.
    // unsigned mappedBaseline = bl + (unsigned) ((sqrt((double) (bl * 8 + 1) + 1) / 2));
    unsigned mappedBaseline = bl;
    supportPixelsUsed[bl].x = X; // SUPPORT_U / 2;
    supportPixelsUsed[bl].y = X; // SUPPORT_V / 2;

    for (unsigned time = 0; time < TIMESTEPS; time ++) {
      for (unsigned ch = 0; ch < CHANNELS; ch ++) {
#ifdef ORIGINAL_EXE
        const double *currentUVW = realUVW[block * TIMESTEPS + time][mappedBaseline];
#else
        // const double *currentUVW = &realUVW[((block * TIMESTEPS + time) *  BASELINES + mappedBaseline) * 3];
        // OOPS! we have inverted(!) layout relatively to that of Romein
        const double *currentUVW = &realUVW[(mappedBaseline * TIMESTEPS * BLOCKS + (block * TIMESTEPS + time)) * 3];
#endif
        uvw[bl][time][ch] = make_double3(
          scale_u[ch] * (double) currentUVW[0] + GRID_U / 2.0f - supportPixelsUsed[bl].x / 2.0f,
          scale_v[ch] * (double) currentUVW[1] + GRID_V / 2.0f - supportPixelsUsed[bl].y / 2.0f,
          scale_w[ch] * (double) currentUVW[2] + W_PLANES / 2.0f
        );

        //if (ch == 0) std::cout << "bl = " << bl << ", t = " << time << ", ch = " << ch << ", uvw = " << uvw[bl][time][ch].x << ", " << uvw[bl][time][ch].y << ", " << uvw[bl][time][ch].z << ", r = " << sqrt(pow(scale_u[ch] * currentUVW[0], 2) + pow(scale_v[ch] * currentUVW[1], 2)) << std::endl;
        //std::cout << nearbyint(uvw[bl][time][ch].x) << ' ' << nearbyint(uvw[bl][time][ch].y) << std::endl;
#if 0
        assert(uvw[bl][time][ch].x >= 0);
        assert(uvw[bl][time][ch].x <  GRID_U - supportPixelsUsed[bl].x);
        assert(uvw[bl][time][ch].y >= 0);
        assert(uvw[bl][time][ch].y <  GRID_V - supportPixelsUsed[bl].y);
        assert(uvw[bl][time][ch].z >= 0);
        assert(uvw[bl][time][ch].z <  W_PLANES);
#endif
      }
    }
  }
#endif
}


void initSupport(SupportType support)
{
#if MODE == MODE_SIMPLE
  for (int v = 0; v < SUPPORT_V; v ++)
    for (int u = 0; u < SUPPORT_U; u ++)
      support[v][u] = make_double2(std::min(v + 1, SUPPORT_V - v) * std::min(u + 1, SUPPORT_U - u), 0);
#elif MODE == MODE_OVERSAMPLE
#if ORDER == ORDER_W_OV_OU_V_U
  for (int w = 0; w < W_PLANES; w ++)
    for (int ou = 0; ou < OVERSAMPLE_U; ou ++)
      for (int ov = 0; ov < OVERSAMPLE_V; ov ++)
        for (int v = 0; v < SUPPORT_V; v ++)
          for (int u = 0; u < SUPPORT_U; u ++)
            support[w][ov][ou][v][u] = make_double2(
              (w + 1) * (std::min)(v + 1, SUPPORT_V - v) * (std::min)(u + 1, SUPPORT_U - u),
              (w + 1) * (ov + 1) * (ou + 1));
#elif ORDER == ORDER_W_V_OV_U_OU
  for (int w = 0; w < W_PLANES; w ++)
    for (int v = 0; v < SUPPORT_V; v ++)
      for (int ov = 0; ov < OVERSAMPLE_V; ov ++)
        for (int u = 0; u < SUPPORT_U; u ++)
          for (int ou = 0; ou < OVERSAMPLE_U; ou ++)
            support[w][v][ov][u][ou] = make_double2(
              (w + 1) * std::min(v + 1, SUPPORT_V - v) * std::min(u + 1, SUPPORT_U - u),
              (w + 1) * (ov + 1) * (ou + 1));
#endif
#elif MODE == MODE_INTERPOLATE
  for (int w = 0; w < W_PLANES; w ++) {
    for (int v = 1; v < SUPPORT_V + 1; v ++) {
      for (int u = 1; u < SUPPORT_U + 1; u ++) {
        support[w][v][u] = make_double2(w * (std::min(v, SUPPORT_V + 1 - v) * std::min(u, SUPPORT_U + 1 - u)) / (double) (W_PLANES * (SUPPORT_V + 1) * (SUPPORT_U + 1) / 4) , 0);
      }

      support[w][v][0] = make_double2(0, 0);
      support[w][v][SUPPORT_U + 1] = make_double2(0, 0);
    }

    for (int u = 0; u < SUPPORT_U + 2; u ++) {
      support[w][0][u] = make_double2(0, 0);
      support[w][SUPPORT_V + 1][u] = make_double2(0, 0);
    }
  }
#endif
}


void initFrequencies(double frequencies[CHANNELS])
{
  for (unsigned ch = 0; ch < CHANNELS; ch ++)
    frequencies[ch] = 59908828.7353515625 + 12207.03125 * ch;
}


void initVisibilities(VisibilitiesType visibilities)
{
#if 0
  for (unsigned time = 0; time < TIMESTEPS; time ++)
    for (unsigned bl = 0; bl < BASELINES; bl ++)
      for (unsigned ch = 0; ch < CHANNELS; ch ++)
        for (unsigned pol = 0; pol < POLARIZATIONS; pol ++)
          visibilities[bl][time][ch][pol] = make_float2(2.0f, 1.0f);
#else
  double2 vis = make_double2(2.0f, 1.0f);

//#pragma omp parallel for num_threads(4)
  for (unsigned i = 0; i < BASELINES * TIMESTEPS * CHANNELS * POLARIZATIONS; i ++)
#ifdef ORIGINAL_EXE
    visibilities[0][0][0][i] = vis;
#else
    visibilities[0][0][0][i] = amps[i];
#endif
#endif
}


void printWorkLoad(uint2 supportPixelsUsed[BASELINES])
{
  unsigned long long gridPointUpdates = 0;

  for (unsigned bl = 0; bl < BASELINES; bl ++)
#if MODE == MODE_SIMPLE || MODE == MODE_OVERSAMPLE
    gridPointUpdates += TIMESTEPS * CHANNELS * POLARIZATIONS * supportPixelsUsed[bl].x * supportPixelsUsed[bl].y;
#elif MODE == MODE_INTERPOLATE
    gridPointUpdates += TIMESTEPS * CHANNELS * POLARIZATIONS * (supportPixelsUsed[bl].x + 1) * (supportPixelsUsed[bl].y + 1);
#endif

#pragma omp critical (cout)
  std::cout << "gridPointUpdates = " << gridPointUpdates << std::endl;
}


void plotGrid(const GridType grid)
{
#if 0
  float max = 0;

  for (int v = 0; v < GRID_V; v ++)
    for (int u = 0; u < GRID_U; u ++) {
      float val = sqrt(grid[v][u][0].x * grid[v][u][0].x + grid[v][u][0].y * grid[v][u][0].y);

      if (val > max)
        max = val;
    }

  float factor = 255 / max;
#endif

#pragma omp critical (cout)
  {
#if 0
    std::cout << "P2" << std::endl;
    std::cout << GRID_U << ' ' << GRID_V << std::endl;
    std::cout << "255" << std::endl;

    for (int v = GRID_V; -- v >= 0;)
      for (int u = 0; u < GRID_U; u ++)
        std::cout << 255 - (int) nearbyint(factor * sqrt(grid[0][v][u].x * grid[v[0]][u].x + grid[0][v][u].y * grid[0][v][u].y)) << std::endl;
#else
    std::cout << "P1" << std::endl;
    std::cout << GRID_U << ' ' << GRID_V << std::endl;

    for (int v = GRID_V; -- v >= 0;)
      for (int u = 0; u < GRID_U; u ++)
        std::cout << (grid[v][u][0].x != 0 || grid[v][u][0].y != 0 ? 1 : 0) << std::endl;
#endif
  }
}


void printVisibilities(const VisibilitiesType visibilities, const char *who)
{
  unsigned count = 0;

  for (unsigned bl = 0; bl < BASELINES; bl ++)
    for (unsigned time = 0; time < TIMESTEPS; time ++)
      for (unsigned ch = 0; ch < CHANNELS; ch ++)
        if (visibilities[bl][time][ch][0].x != 0 || visibilities[bl][time][ch][0].y != 0) {
#pragma omp critical (cout)
          std::cout << who << ": visibilities[" << bl << "][" << time << "][" << ch << "][0] = " << visibilities[bl][time][ch][0] << std::endl;

          if (++ count > 16)
            return;
        }
}


void printGrid(const GridType grid, const char *who)
{
//std::cout << * (unsigned long long *) &grid[4095][4095][0] << " out of " << * (unsigned long long *) &grid[4095][4095][1] << " (" << 100.0 * * (unsigned long long *) &grid[4095][4095][0] / * (unsigned long long *) &grid[4095][4095][1] << "%)" << std::endl;
  unsigned count_v = 0;
  double2  sum     = make_double2(0, 0);

  for (unsigned v = 0; v < GRID_V; v ++) {
    unsigned count_u = 0;

    for (unsigned u = 0; u < GRID_U; u ++) {
      if (grid[v][u][0].x != 0 || grid[v][u][0].y != 0) {
#if 1
        if (count_u ++ == 0)
          count_v ++;

        if (count_u < 5 && count_v < 5)
#pragma omp critical (cout)
          std::cout << who << ": (" << u << ", " << v << "): " << grid[v][u][0] << std::endl;
#endif

        sum += grid[v][u][0];
      }
    }
  }

#pragma omp critical (cout)
  std::cout << "sum = " << sum << std::endl;
}


#if defined __CUDA__

void initSupportOnHostAndDevice(SharedObject<SupportType> &support)
{
  initSupport(*support.hostPtr);
#if defined USE_TEXTURE
  checkCudaCall(cudaBindTexture(0, supportTexture, support.devPtr, sizeof(SupportType)));
#endif
  support.copyHostToDevice();
}


void initSupportOnHostAndDevice(SupportType *&hostSupport, cudaArray *&devSupport)
{
  cudaChannelFormatDesc channelDesc = cudaCreateChannelDesc<double2>();

  checkCudaCall(cudaHostAlloc(reinterpret_cast<void **>(&hostSupport), sizeof *hostSupport, 0));
  initSupport(*hostSupport);

#if MODE == MODE_SIMPLE
  //checkCudaCall(cudaMallocArray(&devSupport, &channelDesc, SUPPORT_U, SUPPORT_V));

  //checkCudaCall(cudaMemcpyToArray(devSupport, 0, 0, *hostSupport, SUPPORT_U * SUPPORT_V * sizeof(double2), cudaMemcpyHostToDevice));


  //supportTexture.filterMode     = cudaFilterModePoint;
#elif MODE == MODE_OVERSAMPLE
  //cudaExtent supportExtent = make_cudaExtent(SUPPORT_U * SUPPORT_V, OVERSAMPLE_U * OVERSAMPLE_V, W_PLANES);
  //checkCudaCall(cudaMalloc3DArray(&devSupport, &channelDesc, supportExtent));

  //cudaMemcpy3DParms copyParams = {0};
  //copyParams.srcPtr   = make_cudaPitchedPtr(*hostSupport, SUPPORT_U * SUPPORT_V * sizeof(double2), SUPPORT_U * SUPPORT_V, OVERSAMPLE_U * OVERSAMPLE_V);
  //copyParams.dstArray = devSupport;
  //copyParams.extent   = supportExtent;
  //copyParams.kind     = cudaMemcpyHostToDevice;
  //checkCudaCall(cudaMemcpy3D(&copyParams));

  //supportTexture.filterMode     = cudaFilterModePoint;
#elif MODE == MODE_INTERPOLATE
  cudaExtent supportExtent = make_cudaExtent(SUPPORT_U + 2, SUPPORT_V + 2, W_PLANES);
  checkCudaCall(cudaMalloc3DArray(&devSupport, &channelDesc, supportExtent));

  cudaMemcpy3DParms copyParams = {0};
  copyParams.srcPtr     = make_cudaPitchedPtr(*hostSupport, (SUPPORT_U + 2) * sizeof(double2), SUPPORT_U + 2, SUPPORT_V + 2);
  copyParams.dstArray   = devSupport;
  copyParams.extent     = supportExtent;
  copyParams.kind       = cudaMemcpyHostToDevice;
  checkCudaCall(cudaMemcpy3D(&copyParams));

  supportTexture.filterMode     = cudaFilterModeLinear;
#endif

#if defined USE_TEXTURE
  supportTexture.addressMode[0] = cudaAddressModeClamp;
  supportTexture.addressMode[1] = cudaAddressModeClamp;
  supportTexture.addressMode[2] = cudaAddressModeClamp;
  supportTexture.normalized     = false;

  cudaBindTextureToArray(supportTexture, devSupport, channelDesc);
#endif
}


SharedObject<GridType> * doCuda()
{
  int device = omp_get_thread_num();

#if defined _OPENMP
  omp_set_nested(true);
#endif

#if defined __linux__
  set_affinity(device);
#endif

#pragma omp critical (cout)
  std::cout << "device = " << device << std::endl;

  checkCudaCall(cudaSetDevice(device));
  checkCudaCall(cudaSetDeviceFlags(cudaDeviceMapHost));

  SharedObject<GridType> * grids = new SharedObject<GridType>[STREAMS];

#pragma omp barrier

  for (unsigned stream = 0; stream < STREAMS; stream ++)
    checkCudaCall(cudaMemset(grids[stream].devPtr, 0, sizeof(GridType)));

#if MODE == MODE_SIMPLE || MODE == MODE_OVERSAMPLE
  SharedObject<SupportType> supports[STREAMS];

  for (unsigned stream = 0; stream < STREAMS; stream ++)
    initSupportOnHostAndDevice(supports[stream]);
#else
  SupportType *hostSupport;
  cudaArray   *devSupport;
  initSupportOnHostAndDevice(hostSupport, devSupport);
#endif

  double frequencies[CHANNELS];
  initFrequencies(frequencies);

#if defined DEGRIDDING
  cudaFuncSetCacheConfig(degrid, cudaFuncCachePreferL1);
#else
  cudaFuncSetCacheConfig(addToGrid, cudaFuncCachePreferShared);
#endif

//#if MODE == MODE_INTERPOLATE
  //unsigned nrThreads = 1024;
  //unsigned bestNrThreads = optimalNrThreads((SUPPORT_U + 1) * (SUPPORT_V + 1), nrThreads);
//#else
  unsigned nrThreads = NR_THREADS_PER_BLOCK;
  //unsigned bestNrThreads = optimalNrThreads(SUPPORT_U * SUPPORT_V, nrThreads);
//#endif

  if (getenv("NR_THREADS") != 0)
    nrThreads = atoi(getenv("NR_THREADS"));

  double start = getTime();
  double  totalExecutionTime = 0;

//#pragma omp parallel num_threads(STREAMS)
  {
#if defined __linux__
    set_affinity(device);
#endif
    checkCudaCall(cudaSetDevice(device));

#if defined MAP_OBJECTS
    MappedObject<uint2 [BASELINES]> supportPixelsUsed[STREAMS];
    MappedObject<UVWtype> uvw[STREAMS];
    MappedObject<VisibilitiesType> visibilities[STREAMS];
#else
    SharedObject<uint2 [BASELINES]> supportPixelsUsed[STREAMS];
    SharedObject<UVWtype> uvw[STREAMS];
    SharedObject<VisibilitiesType> visibilities[STREAMS];
#endif

    Stream streams[STREAMS];
    //Event  startCopy[STREAMS], finishedCopy[STREAMS], startCompute[STREAMS], finishedCompute[STREAMS];

//#pragma omp critical (cout)
    std::cout << "using " << nrThreads << /*'/' << bestNrThreads <<*/ " threads" << std::endl;
    for (unsigned block = 0; block < BLOCKS; block += STREAMS) {
      for (unsigned stream = 0; stream < STREAMS; stream ++) {
        initUVW(*uvw[stream].hostPtr, *supportPixelsUsed[stream].hostPtr, frequencies, block + stream);
        initVisibilities(*visibilities[stream].hostPtr /*, block */);
      }

      for (unsigned stream = 0; stream < STREAMS; stream ++) {
        //startCopy[stream].record(streams[stream]);
        visibilities[stream].copyHostToDevice(streams[stream]);
        uvw[stream].copyHostToDevice(streams[stream]);
        supportPixelsUsed[stream].copyHostToDevice(streams[stream]);
        //finishedCopy[stream].record(streams[stream]);
      }

      for (unsigned stream = 0; stream < STREAMS; stream ++) {
        printWorkLoad(*supportPixelsUsed[stream].hostPtr);

        //startCompute[stream].record(streams[stream]);
#if defined USE_TEXTURE
        addToGrid<<<BASELINES, nrThreads, 0, streams[stream]>>>(*grids[stream].devPtr, *visibilities[stream].devPtr, *uvw[stream].devPtr, *supportPixelsUsed[stream].devPtr);
#else
        addToGrid<<<BASELINES, nrThreads, 0, streams[stream]>>>(*grids[stream].devPtr, *supports[stream].devPtr, *visibilities[stream].devPtr, *uvw[stream].devPtr, *supportPixelsUsed[stream].devPtr);
#endif
        checkCudaCall(cudaGetLastError());
        //finishedCompute[stream].record(streams[stream]);
      }

#if 0
      for (unsigned stream = 0; stream < STREAMS; stream ++) {
        streams[stream].synchronize();
        totalExecutionTime += finishedCompute[stream].elapsedTime(startCompute[stream]);
#pragma omp critical (cout)
        std::cout << "copy: " << finishedCopy[stream].elapsedTime(startCopy[stream]) << ", compute: " << finishedCompute[stream].elapsedTime(startCompute[stream]) << std::endl;
      }
#endif
    }
  }

  checkCudaCall(cudaThreadSynchronize());

  Event startCopy, finishedCopy;
  startCopy.record();

  if (device >= 4)
#pragma omp critical (PCIeBus1)
    grids[0].copyDeviceToHost();
  else
#pragma omp critical (PCIeBus0)
    grids[0].copyDeviceToHost();

  finishedCopy.record();
  finishedCopy.synchronize();

  double stop = getTime();

#pragma omp critical (cout)
  std::cout << "device " << device << ", dev->host copy = " << finishedCopy.elapsedTime(startCopy) << std::endl << "total exec time = " << (stop - start) << std::endl << "total kernel time = " << totalExecutionTime / 1000 << std::endl;

  if (device == 0) {
#if defined DEGRIDDING
    printVisibilities(*(visibilities[(BLOCKS - 1) % STREAMS]).hostPtr, "GPU - Cuda");
#else
    printGrid(*grids[0].hostPtr, "GPU - Cuda");
    //plotGrid(*grid.hostPtr);
#endif
  }

#if MODE == MODE_INTERPOLATE
  cudaFreeArray(devSupport);
  cudaFreeHost(*hostSupport);
#endif

  return grids;
}

#endif // defined __CUDA__


#if defined __OPENCL__

static cl::Context findDevices(std::vector<cl::Device> &devices)
{
  const char *platformName = getenv("PLATFORM");

#if defined __linux__
  if (platformName == 0)
#endif
    platformName = "AMD Accelerated Parallel Processing";

  cl_device_type type = CL_DEVICE_TYPE_DEFAULT;

  const char *deviceType = getenv("TYPE");

  if (deviceType != 0) {
    if (strcmp(deviceType, "GPU") == 0)
      type = CL_DEVICE_TYPE_GPU;
    else if (strcmp(deviceType, "CPU") == 0)
      type = CL_DEVICE_TYPE_CPU;
    else
      std::cerr << "warning: unrecognized device type" << std::endl;
  }

  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);

  for (std::vector<cl::Platform>::iterator platform = platforms.begin(); platform != platforms.end(); platform ++) {
    std::cout << "Platform profile: " << platform->getInfo<CL_PLATFORM_PROFILE>() << std::endl;
    std::cout << "Platform name: " << platform->getInfo<CL_PLATFORM_NAME>() << std::endl;
    std::cout << "Platform version: " << platform->getInfo<CL_PLATFORM_VERSION>() << std::endl;
    std::cout << "Platform extensions: " << platform->getInfo<CL_PLATFORM_EXTENSIONS>() << std::endl;
  }

  for (std::vector<cl::Platform>::iterator platform = platforms.begin(); platform != platforms.end(); platform ++) {
    if (platform->getInfo<CL_PLATFORM_NAME>() == platformName) {
      try {
        platform->getDevices(type, &devices);
      } catch (cl::Error &error) {
        std::cerr << "no GPU devices" << std::endl;
      }

      for (std::vector<cl::Device>::iterator device = devices.begin(); device != devices.end(); device ++) {
        std::cout << "device: " << device->getInfo<CL_DEVICE_NAME>() << std::endl;
        //std::cout << "max image size = " << device->getInfo<CL_DEVICE_IMAGE2D_MAX_WIDTH>() << 'x' << device->getInfo<CL_DEVICE_IMAGE2D_MAX_HEIGHT>() << std::endl;
      }

      cl_context_properties cps[3] = { CL_CONTEXT_PLATFORM, (cl_context_properties)(*platform)(), 0 };
      return cl::Context(type, cps);
    }
  }

  std::cerr << "Platform not found" << std::endl;
  exit(1);
}


cl::Program createProgram(cl::Context &context, std::vector<cl::Device> &devices)
{
  std::ifstream         kernelStream("Kernel.cl");
  std::string           kernelSource((std::istreambuf_iterator<char>(kernelStream)), std::istreambuf_iterator<char>());

  cl::Program::Sources  source(1, std::make_pair(kernelSource.data(), kernelSource.size()));

  cl::Program           program(context, source);

  try {
    std::stringstream args;
    args << "-cl-fast-relaxed-math -I.";

    std::vector<cl_context_properties> properties;
    context.getInfo(CL_CONTEXT_PROPERTIES, &properties);

    if (cl::Platform((cl_platform_id) properties[1]).getInfo<CL_PLATFORM_NAME>() == "NVIDIA CUDA") {
      args << " -cl-nv-verbose";
      args << " -cl-nv-opt-level=99";
      args << " -cl-nv-maxrregcount=32";
    } else if (cl::Platform((cl_platform_id) properties[1]).getInfo<CL_PLATFORM_NAME>() == "Intel(R) OpenCL") {
      args << " -DINTEL";
    }

    args << " -DMODE=" << MODE;
    args << " -DGRID_U=" << GRID_U << " -DGRID_V=" << GRID_V;
    args << " -DSUPPORT_U=" << SUPPORT_U << " -DSUPPORT_V=" << SUPPORT_V;
    args << " -DORDER=" << ORDER;
    args << " -DCHANNELS=" << CHANNELS;
    args << " -DTIMESTEPS=" << TIMESTEPS;
#if defined USE_TEXTURE
    args << " -DUSE_TEXTURE";
#endif
    program.build(devices, args.str().c_str());
    std::string msg;
    program.getBuildInfo(devices[0], CL_PROGRAM_BUILD_LOG, &msg);
#pragma omp critical (cout)
    std::cout << msg;
  } catch (cl::Error &error) {
    if (error.what() == "clBuildProgram") {
      std::string msg;
      program.getBuildInfo(devices[0], CL_PROGRAM_BUILD_LOG, &msg);
#pragma omp critical (cout)
      std::cerr << msg;
      exit(1);
    } else {
      throw;
    }
  }

#if 1
  std::vector<size_t> binarySizes = program.getInfo<CL_PROGRAM_BINARY_SIZES>();
#if 0
  // cl::Program::getInfo<> cl.hpp broken
  std::vector<char *> binaries    = program.getInfo<CL_PROGRAM_BINARIES>();

  std::cout << "sizes: " << binarySizes.size() << " " << binaries.size() << std::endl;
#else
  std::vector<char *> binaries(binarySizes.size());

  for (unsigned b = 0; b < binaries.size(); b ++)
    binaries[b] = new char[binarySizes[b]];

  cl_int error = clGetProgramInfo(program(), CL_PROGRAM_BINARIES, binaries.size() * sizeof(char *), &binaries[0], 0);

  if (error != CL_SUCCESS)
    throw cl::Error(error, "clGetProgramInfo"); // FIXME: cleanup binaries[*]

#endif
  for (unsigned i = 0; i < binaries.size(); i ++) {
    char filename[32];
    sprintf(filename, "Kernel-%u.ptx", i);
    std::ofstream(filename, std::ofstream::binary).write(binaries[i], binarySizes[i]);
  }
#endif

  return program;
}


void doOpenCL()
{
#if defined __linux__
  //int threadId = omp_get_thread_num();
  //set_affinity(threadId);

  if (setenv("DISPLAY", ":0.0", 1) < 0) {
    perror("error setting DISPLAY");
    exit(1);
  }
#endif

  try {
    std::vector<cl::Device> devices;
    cl::Context             context = findDevices(devices);
    cl::Program             program = createProgram(context, devices);

    if (getenv("NR_GPUS") != 0)
      devices.resize((std::min)(devices.size(), (size_t) atoi(getenv("NR_GPUS"))));

    double frequencies[CHANNELS];
    initFrequencies(frequencies);

#if defined ENABLE_PROFILING
    cl::CommandQueue globalQueue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);
#else
    cl::CommandQueue globalQueue(context, devices[0]);
#endif

#if defined USE_TEXTURE
    static SupportType supportHost;
    initSupport(supportHost);

    cl::ImageFormat format(CL_RG, CL_double);
    cl::size_t<3> origin, region;
    origin[0] = 0;
    origin[1] = 0;
    origin[2] = 0;
    cl::Image3D supportImage(context, CL_MEM_READ_ONLY | CL_MEM_USE_HOST_PTR, format, SUPPORT_U, SUPPORT_V, W_PLANES * OVERSAMPLE_V * OVERSAMPLE_U, 0, 0, supportHost);
    region[0] = SUPPORT_U;
    region[1] = SUPPORT_V;
    region[2] = W_PLANES * OVERSAMPLE_V * OVERSAMPLE_U;
    globalQueue.enqueueWriteImage(supportImage, CL_TRUE, origin, region, 0, 0, supportHost);
#else
    SharedObject<SupportType> support(context, CL_MEM_READ_WRITE);
    initSupport(*support.hostPtr);
    globalQueue.enqueueWriteBuffer(support.devPtr, CL_TRUE, 0, sizeof *support.hostPtr, support.hostPtr, 0, 0);
#endif

#if 0
    for  (int j = 0; j < 5; j ++)
    {
      const size_t size = 128 * 1024 * 1024;
      MappedObject<char [size]> buffer(context, globalQueue, false);
      memset(buffer.hostPtr, 0, size);

#if 0
      std::cout << buffer.hostPtr << ' ' << buffer.pinnedPtr() << ' ' << buffer.devPtr() << std::endl;
      char cmd[64];
      sprintf(cmd, "fgrep %llx- /proc/%d/maps", (long long) buffer.hostPtr - 4096, getpid());
      //sprintf(cmd, "cat /proc/%d/maps", getpid());
      system(cmd);
      memset(*buffer.hostPtr, 0, size);
#endif

      for (int i = 0 ; i < 3; i ++) {
        double startTime = getTime();
        globalQueue.enqueueWriteBuffer(buffer.devPtr, CL_TRUE, 0, size, (char *) *buffer.hostPtr, 0, 0);
        double stopTime = getTime();
        std::cout << "transfer speed is " << size / (stopTime - startTime) / 1e9 << std::endl;
      }
    }
#endif

    //SharedObject<GridType> grid(context, CL_MEM_READ_WRITE);
    MappedObject<GridType> grid(context, globalQueue, CL_MEM_READ_WRITE);

    cl::Kernel clearGrid(program, "clear_grid");
    clearGrid.setArg(0, grid.devPtr);

    unsigned nrSMs = devices[0].getInfo<CL_DEVICE_MAX_COMPUTE_UNITS>();
    unsigned nrThreads = clearGrid.getWorkGroupInfo<CL_KERNEL_WORK_GROUP_SIZE>(devices[0]);
    globalQueue.enqueueNDRangeKernel(clearGrid, cl::NullRange, cl::NDRange(nrThreads, nrSMs), cl::NDRange(nrThreads, 1), 0, 0);
    globalQueue.finish();

#if defined ENABLE_PROFILING
    unsigned long long totalExecutionTime = 0;
#endif

    double start = getTime();

#pragma omp parallel num_threads(STREAMS)
    {
#if defined ENABLE_PROFILING
      cl::CommandQueue queue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);
#else
      cl::CommandQueue queue(context, devices[0]);
#endif

      cl::Event event;

#if defined MAP_OBJECTS
      MappedObject<VisibilitiesType> visibilities(context, queue, true);
      MappedObject<uint2 [BASELINES]> supportPixelsUsed(context, queue, true);
      MappedObject<UVWtype> uvw(context, queue, true);
#else
      SharedObject<VisibilitiesType> visibilities(context, CL_MEM_READ_WRITE);
      SharedObject<uint2 [BASELINES]> supportPixelsUsed(context, CL_MEM_READ_WRITE);
      SharedObject<UVWtype> uvw(context, CL_MEM_READ_WRITE);
#endif

      cl::Kernel addToGrid(program, "add_to_grid");
      unsigned maxThreads = addToGrid.getWorkGroupInfo<CL_KERNEL_WORK_GROUP_SIZE>(devices[0]);
      //nrThreads = optimalNrThreads(SUPPORT_U * SUPPORT_V, maxThreads);
      nrThreads = std::min((unsigned) SUPPORT_U * SUPPORT_V, maxThreads);

#pragma omp single
#pragma omp critical (cout)
      std::cout << "using " << nrThreads << " threads per block" << std::endl;

#pragma omp for schedule(dynamic)
      for (unsigned block = 0; block < BLOCKS; block ++) {
//#pragma omp critical (cout)
        //std::cout << "thread " << omp_get_thread_num() << " starts init of blk " << block << " at " << getTime() - start << std::endl;
        //if (block < 2)
//#pragma omp critical (CPUtoGPU)
        {
          initUVW(*uvw.hostPtr, *supportPixelsUsed.hostPtr, frequencies, block);
          initVisibilities(*visibilities.hostPtr /*, block */); // should be done per block
        }

#if 1
//#pragma omp critical (CPUtoGPU)
        //{
//#pragma omp critical (cout)
          //std::cout << "thread " << omp_get_thread_num() << " starts transfer A of blk " << block << " at " << getTime() - start << std::endl;
//#pragma omp critical (cout)
          uvw.copyHostToDevice(queue);
          //std::cout << "thread " << omp_get_thread_num() << " starts transfer B of blk " << block << " at " << getTime() - start << std::endl;
          supportPixelsUsed.copyHostToDevice(queue);
//#pragma omp critical (cout)
          //std::cout << "thread " << omp_get_thread_num() << " starts transfer C of blk " << block << " at " << getTime() - start << std::endl;
          visibilities.copyHostToDevice(queue);
          //queue.flush();
//#pragma omp critical (cout)
          //std::cout << "thread " << omp_get_thread_num() << " flushed blk " << block << " at " << getTime() - start << std::endl;
          //queue.finish();
//#pragma omp critical (cout)
          //std::cout << "thread " << omp_get_thread_num() << " finishes transfer of blk " << block << " at " << getTime() - start << std::endl;
        //}
#endif

        addToGrid.setArg(0, grid.devPtr);
        addToGrid.setArg(1, visibilities.devPtr);
        addToGrid.setArg(2, uvw.devPtr);
        addToGrid.setArg(3, supportPixelsUsed.devPtr);
#if defined USE_TEXTURE
        addToGrid.setArg(4, supportImage);
#else
        addToGrid.setArg(4, support.devPtr);
#endif

//#pragma omp critical (cout)
          //std::cout << "thread " << omp_get_thread_num() << " starts compute of blk " << block << " at " << getTime() - start << std::endl;
        queue.enqueueNDRangeKernel(addToGrid, cl::NullRange, cl::NDRange(nrThreads, BASELINES), cl::NDRange(nrThreads, 1), 0, &event);
        //queue.finish();
//#pragma omp critical (cout)
          //std::cout << "thread " << omp_get_thread_num() << " finishes compute of blk " << block << " at " << getTime() - start << std::endl;

#if 0
        try {
          queue.finish();
        } catch (cl::Error &err) {
          std::cerr << "bah1!: " << err.what() << ": " << errorMessage(err) << std::endl;
        }
#endif

        event.wait();
#if defined ENABLE_PROFILING
        unsigned long long start, stop;
        //event.getProfilingInfo(CL_PROFILING_COMMAND_QUEUED, &queued);
        //event.getProfilingInfo(CL_PROFILING_COMMAND_SUBMIT, &submitted);
        event.getProfilingInfo(CL_PROFILING_COMMAND_START, &start);
        event.getProfilingInfo(CL_PROFILING_COMMAND_END, &stop);

//#pragma omp critical (cout)
        //std::cout << "kernel run time: " << (stop - start) / 1e9 << 's' << std::endl;
        totalExecutionTime += stop - start;
#endif
      }
    }

    double startRead = getTime();
    grid.copyDeviceToHost(globalQueue);
    globalQueue.finish();
    double stopRead = getTime();
    std::cout << "size = " << sizeof *grid.hostPtr / 1e9 << ", time = " << (stopRead - startRead) << ", BW = " << sizeof *grid.hostPtr / 1e9 / (stopRead - startRead) << std::endl;
#if defined ENABLE_PROFILING
    std::cout << "total kernel time = " << totalExecutionTime / 1e9 << std::endl;
#endif
    std::cout << "total exec time = " << (getTime() - start) << std::endl;

    printGrid(*grid.hostPtr, "GPU - OpenCL");
  } catch (cl::Error &err) {
    std::cerr << "ERROR: " << err.what() << ": " << errorMessage(err) << std::endl;
  }
}

#endif


#if !defined __CUDA__ && !defined __OPENCL__

void doCPUgridding(GridType grid[],
                   SupportType support,
                   VisibilitiesType visibilities,
                   UVWtype uvw,
                   uint2 supportPixelsUsed[BASELINES])
{
#pragma omp parallel
  {
    GridType *localGrid = &grid[omp_get_thread_num()];

#if MODE == MODE_INTERPOLATE
#pragma omp for schedule(dynamic)
    for (int bl = 0; bl < BASELINES; bl ++) {
      for (unsigned v = 0; v <= supportPixelsUsed[bl].x; v ++) {
        double scale_u = (double) SUPPORT_U / supportPixelsUsed[bl].x;
        double scale_v = (double) SUPPORT_V / supportPixelsUsed[bl].y;

        /*for (unsigned time = 0; time < TIMESTEPS; time ++)*/ {
          for (unsigned ch = 0; ch < CHANNELS * TIMESTEPS; ch ++) {
#if 0
            float grid_u = nearbyintf(256.f * uvw[bl][0][ch].x) / 256.0f;
            float grid_v = nearbyintf(256.f * uvw[bl][0][ch].y) / 256.0f;
            float w      = nearbyintf(256.f * uvw[bl][0][ch].z) / 256.0f;
#else
            double grid_u = uvw[bl][0][ch].x;
            double grid_v = uvw[bl][0][ch].y;
            double w      = uvw[bl][0][ch].z;
#endif

            unsigned grid_u_int  = (unsigned) nearbyintf(grid_u);
            double    grid_u_frac = grid_u - grid_u_int;
            unsigned grid_v_int  = (unsigned) nearbyintf(grid_v);
            double    grid_v_frac = grid_v - grid_v_int;

#if defined __AVX__
            // FIXME: Float -> Double
            __m256 vis = _mm256_load_ps((const double *) &visibilities[bl][0][ch][0]);
            __m256 *gridptr = (__m256 *) &(*localGrid)[grid_v_int + v][grid_u_int][0];
#endif

            double    support_v = .5f + scale_v * (v - grid_v_frac + .5f);
            unsigned support_v_int = (unsigned) support_v;
            double    v1 = support_v - support_v_int;
            double    v0 = 1 - v1;
            unsigned support_w_int = (unsigned) w;
            double    w1 = w - support_w_int;
            double    w0 = 1 - w1;

            double vw00 = v0 * w0;
            double vw10 = v1 * w0;
            double vw01 = v0 * w1;
            double vw11 = v1 * w1;

            for (unsigned u = 0; u <= supportPixelsUsed[bl].y; u ++) {
              double    support_u = .5f + scale_u * (u - grid_u_frac + .5f);
              unsigned support_u_int = (unsigned) support_u;
              double    u1 = support_u - support_u_int;
              double    u0 = 1 - u1;

              double2 weight = u0 * (vw00 * support[support_w_int    ][support_v_int    ][support_u_int    ] +
                                    vw01 * support[support_w_int    ][support_v_int + 1][support_u_int    ] +
                                    vw10 * support[support_w_int + 1][support_v_int    ][support_u_int    ] +
                                    vw11 * support[support_w_int + 1][support_v_int + 1][support_u_int    ]) +
                              u1 * (vw00 * support[support_w_int    ][support_v_int    ][support_u_int + 1] +
                                    vw01 * support[support_w_int    ][support_v_int + 1][support_u_int + 1] +
                                    vw10 * support[support_w_int + 1][support_v_int    ][support_u_int + 1] +
                                    vw11 * support[support_w_int + 1][support_v_int + 1][support_u_int + 1]);

#if defined __AVX__
#if 0
              __m128 sup00 = _mm_load_ps(&support[support_w_int    ][support_v_int    ][support_u_int]);
              __m128 sup10 = _mm_load_ps(&support[support_w_int    ][support_v_int + 1][support_u_int]);
              __m128 sup01 = _mm_load_ps(&support[support_w_int + 1][support_v_int    ][support_u_int]);
              __m128 sup11 = _mm_load_ps(&support[support_w_int + 1][support_v_int + 1][support_u_int]);
              sup00 = _mm_mul_ps(sup00, vw00);
              sup10 = _mm_mul_ps(sup10, vw10);
              sup01 = _mm_mul_ps(sup00, vw01);
              sup11 = _mm_mul_ps(sup10, vw11);
              //__m256 sup0 =_mm256_insertf128_ps((__m256) sup00, sup10, 1);
              //__m256 sup1 =_mm256_insertf128_ps((__m256) sup01, sup11, 1);
#endif

              // FIXME: Float -> Double
              __m256 weight_r = _mm256_set1_ps(weight.x);
              __m256 weight_i = _mm256_set1_ps(weight.y);
              __m256 t7 = _mm256_mul_ps(weight_r, vis);
              __m256 t6 = _mm256_mul_ps(weight_i, vis);
              __m256 t8 = _mm256_permute_ps(t6, 0xB1);
              __m256 t9 = _mm256_addsub_ps(t7, t8);
              gridptr[u] = _mm256_add_ps(gridptr[u], t9);
#else
              for (unsigned pol = 0; pol < POLARIZATIONS; pol ++) {
                double2 prod   = visibilities[bl][0][ch][pol] * weight;
                double2 &pixel = (*localGrid)[grid_v_int + v][grid_u_int + u][pol];
                pixel += prod;
              }
#endif
            }
          }
        }
      }
    }
#elif MODE == MODE_OVERSAMPLE
#pragma omp for schedule(dynamic)
    for (int bl = 0; bl < BASELINES; bl ++) {
      unsigned v_end  = supportPixelsUsed[bl].y;

      for (unsigned v = 0; v < v_end; v ++) {
        /*for (unsigned time = 0; time < TIMESTEPS; time ++)*/ {
          for (unsigned ch = 0; ch < CHANNELS * TIMESTEPS; ch ++) {
            double uc = uvw[bl][0][ch].x;
            double vc = uvw[bl][0][ch].y;
            unsigned wc = (unsigned) uvw[bl][0][ch].z;
            unsigned grid_u = (unsigned) uc;
            unsigned grid_v = (unsigned) vc;
            double          u_frac = uc - grid_u;
            double          v_frac = vc - grid_v;
            unsigned ou     = (unsigned) (OVERSAMPLE_U * u_frac);
            unsigned ov     = (unsigned) (OVERSAMPLE_V * v_frac);
            unsigned u_end  = supportPixelsUsed[bl].x;

#if defined __AVX__
            // FIXME: Float -> Double
            __m256 vis = _mm256_load_ps((const double *) &visibilities[bl][0][ch][0]);
            __m256 *gridptr = (__m256 *) &(*localGrid)[grid_v + v][grid_u][0];

#if ORDER == ORDER_W_OV_OU_V_U
            double2 *support_ptr = &support[wc][ov][ou][v][0];
#elif ORDER == ORDER_W_V_OV_U_OU
            double2 *support_ptr = &support[w][v][ov][0][ou];
#endif
            for (unsigned u = 0; u < u_end; u ++) {
#if ORDER == ORDER_W_OV_OU_V_U
              __m256 weight_r = _mm256_set1_ps(support_ptr[u].x);
              __m256 weight_i = _mm256_set1_ps(support_ptr[u].y);
#elif ORDER == ORDER_W_V_OV_U_OU
              __m256 weight_r = _mm256_set1_ps(support_ptr[OVERSAMPLE_U * u].x);
              __m256 weight_i = _mm256_set1_ps(support_ptr[OVERSAMPLE_U * u].y);
#endif
              __m256 t7 = _mm256_mul_ps(weight_r, vis);
              __m256 t6 = _mm256_mul_ps(weight_i, vis);
              __m256 t8 = _mm256_permute_ps(t6, 0xB1);
              __m256 t9 = _mm256_addsub_ps(t7, t8);
              gridptr[u] = _mm256_add_ps(gridptr[u], t9);
            }
#elif defined __SSE3__
            // FIXME: Float -> Double
            __m128 vis0 = _mm_load_ps((const double *) &visibilities[bl][0][ch][0]);
            __m128 vis1 = _mm_load_ps((const double *) &visibilities[bl][0][ch][2]);
            __m128 *gridptr = (__m128 *) &(*localGrid)[grid_v + v][grid_u][0];

#if ORDER == ORDER_W_OV_OU_V_U
            double2 *support_ptr = &support[wc][ov][ou][v][0];
#elif ORDER == ORDER_W_V_OV_U_OU
            double2 *support_ptr = &support[w][v][ov][0][ou];
#endif
            for (unsigned u = 0; u < u_end; u ++) {
#if ORDER == ORDER_W_OV_OU_V_U
              __m128 weight_r = _mm_set1_ps(support_ptr[u].x);
              __m128 weight_i = _mm_set1_ps(support_ptr[u].y);
#elif ORDER == ORDER_W_V_OV_U_OU
              __m128 weight_r = _mm_set1_ps(support_ptr[OVERSAMPLE_U * u].x);
              __m128 weight_i = _mm_set1_ps(support_ptr[OVERSAMPLE_U * u].y);
#endif
              __m128 t70 = _mm_mul_ps(weight_r, vis0);
              __m128 t60 = _mm_mul_ps(weight_i, vis0);
              __m128 t71 = _mm_mul_ps(weight_r, vis1);
              __m128 t61 = _mm_mul_ps(weight_i, vis1);
              __m128 t80 = _mm_shuffle_ps(t60, t60, 0xB1);
              __m128 t81 = _mm_shuffle_ps(t61, t61, 0xB1);
              __m128 t90 = _mm_addsub_ps(t70, t80);
              __m128 t91 = _mm_addsub_ps(t71, t81);
              gridptr[2 * u    ] = _mm_add_ps(gridptr[2 * u    ], t90);
              gridptr[2 * u + 1] = _mm_add_ps(gridptr[2 * u + 1], t91);
            }
#else
            double2 visXX = visibilities[bl][0][ch][0];
            double2 visXY = visibilities[bl][0][ch][1];
            double2 visYX = visibilities[bl][0][ch][2];
            double2 visYY = visibilities[bl][0][ch][3];

            for (unsigned u = 0; u < u_end; u ++) {
#if ORDER == ORDER_W_OV_OU_V_U
              double2 weight = support[wc][ov][ou][v][u];
#elif ORDER == ORDER_W_V_OV_U_OU
              double2 weight = support[wc][v][ov][u][ou];
#endif
              (*localGrid)[grid_v + v][grid_u + u][0] += visXX * weight;
              (*localGrid)[grid_v + v][grid_u + u][1] += visXY * weight;
              (*localGrid)[grid_v + v][grid_u + u][2] += visYX * weight;
              (*localGrid)[grid_v + v][grid_u + u][3] += visYY * weight;
            }
#endif
          }
        }
      }
    }
#elif MODE == MODE_SIMPLE
#pragma omp for schedule(dynamic)
    for (int bl = 0; bl < BASELINES; bl ++) {
      unsigned u_end  = supportPixelsUsed[bl].x;
      unsigned v_end  = supportPixelsUsed[bl].y;

      /*for (unsigned time = 0; time < TIMESTEPS; time ++)*/ {
        for (unsigned ch = 0; ch < CHANNELS * TIMESTEPS; ch ++) {
          unsigned grid_u = (unsigned) nearbyintf(uvw[bl][0][ch].x);
          unsigned grid_v = (unsigned) nearbyintf(uvw[bl][0][ch].y);

          for (unsigned v = 0; v < v_end; v ++) {
            for (unsigned u = 0; u < u_end; u ++) {
              double2 weight = support[v][u];

              for (unsigned pol = 0; pol < POLARIZATIONS; pol ++) {
                double2 prod   = visibilities[bl][0][ch][pol] * weight;
                double2 &pixel = (*localGrid)[grid_v + v][grid_u + u][pol];
                pixel += prod;
              }
            }
          }
        }
      }
    }
#endif
  }
}


#if defined DEGRIDDING

void doCPUdegridding(GridType grid,
                     SupportType support,
                     VisibilitiesType visibilities,
                     UVWtype uvw,
                     uint2 supportPixelsUsed[BASELINES])
{
#pragma omp parallel for schedule(dynamic)
  for (int bl = 0; bl < BASELINES; bl ++) {
    uint2 supportSize = supportPixelsUsed[bl];

    /*for (unsigned time = 0; time < TIMESTEPS; time ++)*/ {
      for (unsigned ch = 0; ch < CHANNELS * TIMESTEPS; ch ++) {
        double2   sumXX  = make_double2(0, 0);
        double2   sumXY  = make_double2(0, 0);
        double2   sumYX  = make_double2(0, 0);
        double2   sumYY  = make_double2(0, 0);

#if MODE == MODE_SIMPLE
        unsigned u_int = (unsigned) nearbyintf(uvw[bl][0][ch].x);
        unsigned v_int = (unsigned) nearbyintf(uvw[bl][0][ch].y);
#elif MODE == MODE_OVERSAMPLE
        double    grid_u = uvw[bl][0][ch].x;
        double    grid_v = uvw[bl][0][ch].y;
        double    grid_w = uvw[bl][0][ch].z;
        unsigned u_int  = (unsigned) grid_u;
        unsigned v_int  = (unsigned) grid_v;
        unsigned w_int  = (unsigned) grid_w;
        double    u_frac = grid_u - u_int;
        double    v_frac = grid_v - v_int;
        unsigned ou     = (unsigned) (OVERSAMPLE_U * u_frac);
        unsigned ov     = (unsigned) (OVERSAMPLE_V * v_frac);
#endif

        for (unsigned v = 0; v < supportSize.y; v ++) {
          for (unsigned u = 0; u < supportSize.x; u ++) {
#if MODE == MODE_SIMPLE
            double2 weight = support[v][u];
#elif MODE == MODE_OVERSAMPLE
#if ORDER == ORDER_W_OV_OU_V_U
            double2 weight = support[w_int][ov][ou][v][u];
#elif ORDER == ORDER_W_V_OV_U_OU
            double2 weight = support[w_int][v][ov][u][ou];
#endif
#endif
            sumXX += grid[v_int + v][u_int + u][0] * weight;
            sumXY += grid[v_int + v][u_int + u][1] * weight;
            sumYX += grid[v_int + v][u_int + u][2] * weight;
            sumYY += grid[v_int + v][u_int + u][3] * weight;
          }
        }

        visibilities[bl][0][ch][0] = sumXX;
        visibilities[bl][0][ch][1] = sumXY;
        visibilities[bl][0][ch][2] = sumYX;
        visibilities[bl][0][ch][3] = sumYY;
      }
    }
  }
}

#endif


void doCPU()
{
  static GridType grid;
  SupportType *support = (SupportType *) new SupportType;
  static VisibilitiesType visibilities;
  static UVWtype uvw;
  static double  frequencies[CHANNELS];
  static uint2  supportPixelsUsed[BASELINES];

  memset(grid, 0, sizeof grid);
  initSupport(*support);
  initFrequencies(frequencies);

  double time = 0;

#pragma omp parallel
#pragma omp single
  nrThreads = omp_get_num_threads();

  std::cout << "nrThreads = " << nrThreads << std::endl;

  GridType *tmpGrids = new GridType[nrThreads];
  memset(tmpGrids, 0, nrThreads * sizeof(GridType));

  for (unsigned block = 0; block < BLOCKS; block ++) {
#if defined DEGRIDDING
    grid[2000][2000][0] = make_double2(1.0f, 0.0f);
#else
    initVisibilities(visibilities /*, block */);
#endif
    initUVW(uvw, supportPixelsUsed, frequencies, block);

    time -= getTime();
#if defined DEGRIDDING
    doCPUdegridding(grid, *support, visibilities, uvw, supportPixelsUsed);
#else
    doCPUgridding(tmpGrids, *support, visibilities, uvw, supportPixelsUsed);
#endif
    time += getTime();
  }

  addGrids(grid, tmpGrids);

  std::cout << "total exec time = " << time << std::endl;
#if defined DEGRIDDING
  printVisibilities(visibilities, "CPU");
#else
  printGrid(grid, "CPU");
#endif

  delete [] tmpGrids;
  delete [] support;
}

#endif

#ifdef ORIGINAL_EXE

int main()
{
#if defined __CUDA__
  //printDeviceProperties();

  int deviceCount;
  checkCudaCall(cudaGetDeviceCount(&deviceCount));

  if (getenv("NR_GPUS") != 0)
    deviceCount = std::min(deviceCount, atoi(getenv("NR_GPUS")));

#pragma omp parallel num_threads(deviceCount)
  doCuda();
#elif defined __OPENCL__
  doOpenCL();
#else
  doCPU();
#endif

  return 0;
}

#else

#ifdef _MSC_VER
#define DLL_EXPORT __declspec(dllexport)
#else
#define DLL_EXPORT
#endif

extern "C" {

DLL_EXPORT
SharedObject<GridType> * romeinComputeGridOnCuda(const double * uvw, const double2 * amp) {
  realUVW = uvw;
  amps = amp;
  return doCuda();
}

DLL_EXPORT
GridType * romeinGetGridData(SharedObject<GridType> * grid) {
  return grid->hostPtr;
}

DLL_EXPORT
void romeinFinalizeGrid(SharedObject<GridType> * grid) {
  delete [] grid;
}

}

#endif
