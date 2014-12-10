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

#if !defined COMMON_H
#define COMMON_H

#include <cstdio>
#include <iostream>

#if defined __SSE__ && !defined __CUDA__ && !defined __OPENCL__
#include <xmmintrin.h>
#endif

#if defined __CUDA__
#include <cuda_runtime.h>
#endif

#if defined __OPENCL__
#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#endif

#if defined _OPENMP
#include <omp.h>
#else
#define omp_get_num_threads()	1
#define omp_get_thread_num()    0
#endif

#if defined __linux__
#include <sched.h>
#include <sys/time.h>
#endif

#if defined _WIN32 || defined __WIN32__ || defined _WIN64
#include <windows.h>
#ifdef _MSC_VER
#define _assert(m, f, l) _wassert(L ## m, L ## f, l)
#endif
#endif


#if defined __CUDA__
// Borrowed from CUDA C Programming guide

__device__ double atomicAdd(double* address, double val)
{
#ifdef FAKE_ATOMIC
  double orig = *address;
  *address = orig + val;
  return orig;
#else
  unsigned long long int* address_as_ull = (unsigned long long int*)address;
  unsigned long long int old = *address_as_ull, assumed;
  do {
       assumed = old;
       old = atomicCAS(address_as_ull, assumed,
                         __double_as_longlong(val +
                           __longlong_as_double(assumed)));
       // Note: uses integer comparison to avoid hang in case of NaN (since NaN != NaN)
  } while (assumed != old);
  return __longlong_as_double(old);
#endif
}

static __forceinline__ __device__ double2 fetch_double2(texture<int4, cudaTextureType1D, cudaReadModeElementType> t, int i)
{
  int4 v = tex1Dfetch(t, i);
  return make_double2(__hiloint2double(v.y, v.x), __hiloint2double(v.w, v.z));
}
#endif

#if defined __CUDA__
#elif 0 && defined __OPENCL__

typedef cl_double2 double2;
typedef cl_float2  float2;
typedef union { float s[3]; struct { float x, y, z; }; } float3;
typedef cl_float4  float4;
typedef cl_uint2   uint2;

inline float2 make_float2(float x, float y)
{
  float2 f2;
  f2.x = x;
  f2.y = y;
  return f2;
}

inline float3 make_float3(float x, float y, float z)
{
  float3 f3;
  f3.x = x;
  f3.y = y;
  f3.z = z;
  return f3;
}

inline double2 make_double2(double x, double y)
{
  double2 d2;
  d2.x = x;
  d2.y = y;
  return d2;
}

//#define make_float2(X,Y) float2((X),(Y))
//#define make_float3(X,Y,Z) (float3) { (X),(Y),(Z) }
//#define make_double2(X,Y) (double2) { (X),(Y) }
#define make_uint2(X,Y) (uint2) { (X),(Y) }

#else

typedef struct { float x, y; } float2;
typedef struct { float x, y, z; } float3;
typedef struct { double x, y, z; } double3;
typedef struct { double x, y; } double2;
typedef struct { unsigned x, y; } uint2;

inline float2 make_float2(float x, float y) { float2 f = { x, y }; return f; }
inline float3 make_float3(float x, float y, float z) { float3 f = { x, y, z}; return f; }
inline double3 make_double3(double x, double y, double z) { double3 f = { x, y, z}; return f; }
inline double2 make_double2(double x, double y) { double2 f = { x, y }; return f; }
inline uint2 make_uint2(unsigned x, unsigned y) { uint2 f = { x, y }; return f; }

#endif


double getTime()
{
  static double firstTime = 0.0;

#if defined __linux__
  struct timeval tv;

  if (gettimeofday(&tv, 0) < 0) {
    perror("gettimeofday");
    exit(1);
  }

  double now = tv.tv_sec + tv.tv_usec / 1e6;
#elif defined _WIN32 || defined __WIN32__ || defined _WIN64
  static LARGE_INTEGER freq;

  if (firstTime == 0 && !QueryPerformanceFrequency(&freq))
    std::cerr << "No high-resolution timer available" << std::endl;

  LARGE_INTEGER time;
  QueryPerformanceCounter(&time);

  double now = (double) time.QuadPart / (double) freq.QuadPart;
#endif

  if (firstTime == 0.0)
    firstTime = now;

  return now - firstTime;
}


inline float2 operator + (float2 a, float2 b)
{
  return make_float2(a.x + b.x, a.y + b.y);
}


inline float2 operator * (float a, float2 b)
{
  return make_float2(a * b.x, a * b.y);
}


inline float2 operator * (float2 a, float2 b)
{
  return make_float2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}


inline double2 operator * (double2 a, double2 b)
{
  return make_double2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}


inline float2 operator += (float2 &a, float2 b)
{
  return make_float2(a.x += b.x, a.y += b.y);
}


inline double2 operator += (double2 &a, float2 b)
{
  return make_double2(a.x += b.x, a.y += b.y);
}


inline double2 operator += (double2 &a, double2 b)
{
  return make_double2(a.x += b.x, a.y += b.y);
}


inline std::ostream &operator << (std::ostream &str, float2 z)
{
  return str << '(' << z.x << ", " << z.y << ')';
}


inline std::ostream &operator << (std::ostream &str, double2 z)
{
  return str << '(' << z.x << ", " << z.y << ')';
}


#if defined __SSE__ && !defined __CUDA__ && !defined __OPENCL__

inline std::ostream &operator << (std::ostream &str, __m128 v)
{
  union {
    __m128 m;
    float  a[4];
  } u;
  
  u.m = v;
  return str << '[' << u.a[0] << ", " << u.a[1] << ", " << u.a[2] << ", " << u.a[3] << ']';
}

#endif


#if defined __CUDA__

inline void checkCudaCallWithLineNumber(cudaError_t result, unsigned lineNumber)
{
  if (result != cudaSuccess) {
#pragma omp critical (cout)
    std::cerr << "cuda error (line #" << lineNumber << "): " << cudaGetErrorString(result) << std::endl;
    exit(1);
  }
}

#define checkCudaCall(X) checkCudaCallWithLineNumber((X), __LINE__)
#endif


#if defined __linux__

inline void set_affinity(unsigned device)
{
#if 0
  static const char mapping[1][12] = {
     0,  1,  2,  3,  8,  9, 10, 11,
  };
#else
  static const char mapping[8][12] = {
     0,  1,  2,  3,  4,  5, 12, 13, 14, 15, 16, 17,
     0,  1,  2,  3,  4,  5, 12, 13, 14, 15, 16, 17,
     0,  1,  2,  3,  4,  5, 12, 13, 14, 15, 16, 17,
     0,  1,  2,  3,  4,  5, 12, 13, 14, 15, 16, 17,
     6,  7,  8,  9, 10, 11, 18, 19, 20, 21, 22, 23,
     6,  7,  8,  9, 10, 11, 18, 19, 20, 21, 22, 23,
     6,  7,  8,  9, 10, 11, 18, 19, 20, 21, 22, 23,
     6,  7,  8,  9, 10, 11, 18, 19, 20, 21, 22, 23,
  };
#endif

  cpu_set_t set;

  CPU_ZERO(&set);

  for (unsigned coreIndex = 0; coreIndex < 12; coreIndex ++)
    CPU_SET(mapping[device][coreIndex], &set);

  if (sched_setaffinity(0, sizeof set, &set) < 0)
    perror("sched_setaffinity");
}

#endif


#if defined __CUDA__

inline void printDeviceProperties()
{
  int deviceCount;
  cudaGetDeviceCount(&deviceCount);
  std::cout << "deviceCount = " << deviceCount << std::endl;

#if 1
  for (int device = 0; device < deviceCount; device ++) {
    cudaDeviceProp p;
    cudaGetDeviceProperties(&p, device);

    std::cout << "device " << device << std::endl
	      << "  name: " << p.name << std::endl
	      << "  global mem: " << p.totalGlobalMem << std::endl
	      << "  sharedMemPerBlock: " << p.sharedMemPerBlock << std::endl
	      << "  regsPerBlock: " << p.regsPerBlock << std::endl
	      << "  warpSize: " << p.warpSize << std::endl
	      << "  memPitch: " << p.memPitch << std::endl
	      << "  maxThreadsPerBlock: " << p.maxThreadsPerBlock << std::endl
	      << "  maxThreadsPerMultiProcessor: " << p.maxThreadsPerMultiProcessor << std::endl
	      << "  maxBlockSize: (" << p.maxThreadsDim[0] << ", "<< p.maxThreadsDim[1] << ", "<< p.maxThreadsDim[2] << ")" << std::endl
	      << "  maxGridSize: (" << p.maxGridSize[0] << ", "<< p.maxGridSize[1] << ", "<< p.maxGridSize[2] << ")" << std::endl
	      << "  const mem: " << p.totalConstMem << std::endl
	      << "  version: " << p.major << "." << p.minor << std::endl
	      << "  clock rate: " << p.clockRate << std::endl
	      << "  tex alignment: " << p.textureAlignment << std::endl
	      << "  multiprocessors: " << p.multiProcessorCount << std::endl;
  }
#endif
}


class Stream
{
  public:
    Stream()
    {
      checkCudaCall(cudaStreamCreate(&stream));
    }

    ~Stream()
    {
      checkCudaCall(cudaStreamDestroy(stream));
    }

    void synchronize()
    {
      checkCudaCall(cudaStreamSynchronize(stream));
    }

    operator cudaStream_t & ()
    {
      return stream;
    }

  private:
    cudaStream_t stream;
};


class Event
{
  public:
    Event()
    {
      checkCudaCall(cudaEventCreate(&event));
    }

    ~Event()
    {
      checkCudaCall(cudaEventDestroy(event));
    }

    void record()
    {
      checkCudaCall(cudaEventRecord(event));
    }

    void record(Stream &stream)
    {
      checkCudaCall(cudaEventRecord(event, stream));
    }

    void synchronize()
    {
      checkCudaCall(cudaEventSynchronize(event));
    }

    float elapsedTime(Event &start)
    {
      float time;

      checkCudaCall(cudaEventElapsedTime(&time, start.event, this->event));
      return time;
    }

    operator cudaEvent_t & ()
    {
      return event;
    }

  private:
    cudaEvent_t event;
};


template <typename T> class SharedObject
{
  public:
    SharedObject(int hostAllocFlags = 0)
    {
      checkCudaCall(cudaMalloc(reinterpret_cast<void **>(&devPtr), sizeof(T)));
      checkCudaCall(cudaHostAlloc(reinterpret_cast<void **>(&hostPtr), sizeof(T), hostAllocFlags));
    }

    ~SharedObject()
    {
      checkCudaCall(cudaFree(devPtr));
      checkCudaCall(cudaFreeHost(hostPtr));
    }

    void copyHostToDevice()
    {
      checkCudaCall(cudaMemcpy(devPtr, hostPtr, sizeof(T), cudaMemcpyHostToDevice));
    }

    void copyHostToDevice(Stream &stream)
    {
      checkCudaCall(cudaMemcpyAsync(devPtr, hostPtr, sizeof(T), cudaMemcpyHostToDevice, stream));
    }

    void copyDeviceToHost()
    {
      checkCudaCall(cudaMemcpy(hostPtr, devPtr, sizeof(T), cudaMemcpyDeviceToHost));
    }

    void copyDeviceToHost(Stream &stream)
    {
      checkCudaCall(cudaMemcpyAsync(hostPtr, devPtr, sizeof(T), cudaMemcpyDeviceToHost, stream));
    }

    T *hostPtr, *devPtr;
};


template <typename T> class WriteCombiningSharedObject : public SharedObject<T>
{
  public:
    WriteCombiningSharedObject()
    :
      SharedObject<T>(cudaHostAllocWriteCombined)
    {
    }
};


template <typename T> class MappedObject
{
  public:
    MappedObject(int hostAllocFlags = 0)
    {
      checkCudaCall(cudaHostAlloc(reinterpret_cast<void **>(&hostPtr), sizeof(T), hostAllocFlags | cudaHostAllocMapped));
      checkCudaCall(cudaHostGetDevicePointer(reinterpret_cast<void **>(&devPtr), hostPtr, 0));
    }

    ~MappedObject()
    {
      checkCudaCall(cudaFreeHost(hostPtr));
    }

    void copyHostToDevice()
    {
    }

    void copyHostToDevice(Stream &stream)
    {
    }

    void copyDeviceToHost()
    {
    }

    void copyDeviceToHost(Stream &stream)
    {
      checkCudaCall(cudaMemcpyAsync(hostPtr, devPtr, sizeof(T), cudaMemcpyDeviceToHost, stream));
    }

    T *hostPtr, *devPtr;
};

#endif

#if defined __OPENCL__

template <typename T> class SharedObject
{
  public:
    SharedObject(cl::Context &context, int flags)
    :
      hostPtr(static_cast<T *>(malloc(sizeof(T)))),
      devPtr(context, flags | CL_MEM_USE_HOST_PTR, sizeof(T), hostPtr)
    {
    }

    ~SharedObject()
    {
      free(hostPtr);
    }

    void copyHostToDevice(cl::CommandQueue &queue)
    {
      queue.enqueueWriteBuffer(devPtr, CL_FALSE, 0, sizeof *hostPtr, *hostPtr, 0, 0);
    }

    void copyDeviceToHost(cl::CommandQueue &queue)
    {
      queue.enqueueReadBuffer(devPtr, CL_FALSE, 0, sizeof *hostPtr, *hostPtr);
    }

    T	       *hostPtr;
    cl::Buffer devPtr;
};


template <typename T> class MappedObject
{
  public:
    MappedObject(cl::Context &context, cl::CommandQueue &queue, bool hostToGPU)
    :
      pinnedPtr(context, (hostToGPU ? CL_MEM_WRITE_ONLY : CL_MEM_READ_ONLY) | CL_MEM_ALLOC_HOST_PTR, sizeof(T)),
      devPtr(context, (hostToGPU ? CL_MEM_READ_ONLY : CL_MEM_WRITE_ONLY), sizeof(T)),
      queue(queue)
    {
      hostPtr = (T *) queue.enqueueMapBuffer(pinnedPtr, CL_TRUE, hostToGPU ? CL_MAP_WRITE : CL_MAP_READ, 0, sizeof(T));
    }

    ~MappedObject()
    {
      queue.enqueueUnmapMemObject(pinnedPtr, hostPtr);
    }

    void copyHostToDevice(cl::CommandQueue &queue)
    {
      queue.enqueueWriteBuffer(devPtr, CL_FALSE, 0, sizeof *hostPtr, *hostPtr, 0, 0);
    }

    void copyDeviceToHost(cl::CommandQueue &queue)
    {
      queue.enqueueReadBuffer(devPtr, CL_FALSE, 0, sizeof *hostPtr, *hostPtr);
    }

    T	       *hostPtr;
    cl::Buffer pinnedPtr, devPtr;
    cl::CommandQueue &queue;
};


#if 1
// real mapping, only works on AMD

template <typename T> class MappedAMDObject
{
  public:
    MappedAMDObject(cl::Context &context, cl::CommandQueue &queue, bool hostToGPU)
    :
      devPtr(context, (!hostToGPU ? CL_MEM_READ_ONLY : CL_MEM_WRITE_ONLY) | CL_MEM_ALLOC_HOST_PTR, sizeof(T)),
      queue(queue)
    {
      hostPtr = (T *) queue.enqueueMapBuffer(devPtr, CL_TRUE, hostToGPU ? CL_MAP_WRITE : CL_MAP_READ, 0, sizeof(T));
    }

    ~MappedAMDObject()
    {
      queue.enqueueUnmapMemObject(devPtr, hostPtr);
    }

    void copyHostToDevice(cl::CommandQueue &queue)
    {
    }

    void copyDeviceToHost(cl::CommandQueue &queue)
    {
    }

    T	       *hostPtr;
    cl::Buffer devPtr;
    cl::CommandQueue &queue;
};

#else

// works only on small buffers
//
#if !defined CL_MEM_USE_PERSISTENT_MEM_AMD
#define CL_MEM_USE_PERSISTENT_MEM_AMD (1 << 6)
#endif

template <typename T> class MappedObject
{
  public:
    MappedObject(cl::Context &context, cl::CommandQueue &queue, bool hostToGPU)
    :
      devPtr(context, (!hostToGPU ? CL_MEM_READ_ONLY : CL_MEM_WRITE_ONLY) | CL_MEM_USE_PERSISTENT_MEM_AMD, sizeof(T)),
      queue(queue)
    {
    }

    void map(bool hostToGPU)
    {
      hostPtr = (T *) queue.enqueueMapBuffer(devPtr, CL_TRUE, hostToGPU ? CL_MAP_WRITE : CL_MAP_READ, 0, sizeof(T));
    }

    void unmap()
    {
      queue.enqueueUnmapMemObject(devPtr, hostPtr);
    }

    T	       *hostPtr;
    cl::Buffer devPtr;
    cl::CommandQueue &queue;
};

#endif

#endif
#endif
