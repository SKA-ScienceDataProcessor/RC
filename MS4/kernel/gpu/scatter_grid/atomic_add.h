#ifndef __ATOMIC_ADD_H
#define __ATOMIC_ADD_H

static __device__  __inline__ double atomicAdd(double* address, double val)
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

static __device__ __inline__ void atomicAdd(complexd *ptr, complexd sum)
{
  atomicAdd(&ptr->x, sum.x);
  atomicAdd(&ptr->y, sum.y);
}

#endif
