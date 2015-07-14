/*
 * Copyright 1993-2015 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

/*
  Copyright (C) 2015 Braam Research, LLC.
  This software contains source code provided by NVIDIA Corporation.
  It is modified to fulfill specific needs of GCF computations.
 */

/*
    Parallel reduction kernels
*/
#ifndef _REDUCE_KERNEL_H_
#define _REDUCE_KERNEL_H_

#ifdef __clang__
#include "cuda.h"
#include "cuda_builtin_vars.h"
extern __host__ cudaError_t cudaMemcpyToSymbol(const void *symbol, const void *src, size_t count, size_t offset, enum cudaMemcpyKind kind);
#define __SYM(a) (&(a))
#else
#define __SYM(a) a
#endif

#include <cuComplex.h>


/*
    Parallel reduction using shared memory
    - takes log(n) steps for n input elements
    - uses n/2 threads
    - only works for power-of-2 arrays

    This version adds multiple elements per thread sequentially.  This reduces the overall
    cost of the algorithm while keeping the work complexity O(n) and the step complexity O(log n).
    (Brent's Theorem optimization)

    See the CUDA SDK "reduction" sample for more information.
*/

template <
    unsigned int blockSize
  , typename typ
  , class task
  >
__device__ __inline__ void
reduceBlockGen(volatile typ *sdata, typ myAcc, const unsigned int tid)
{
    sdata[tid] = myAcc;
    __syncthreads();

    // do reduction in shared mem
    if (blockSize >= 512)
    {
        if (tid < 256) sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid + 256]);
        __syncthreads();
    }

    if (blockSize >= 256)
    {
        if (tid < 128) sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid + 128]);
        __syncthreads();
    }

    if (blockSize >= 128)
    {
        if (tid <  64) sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid +  64]);
        __syncthreads();
    }

    if (tid < 32)
    {
        sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid + 32]);
        sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid + 16]);
        sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid +  8]);
        sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid +  4]);
        sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid +  2]);
        sdata[tid] = myAcc = task::reduce(myAcc, sdata[tid +  1]);
    }
}

template <
    unsigned int blockSize
  , bool nIsPow2
  , typename typ
  , typename styp
  , class task
  >
__device__ __inline__ void
reduceBlocksGen(const styp *g_idata, typ *g_odata, unsigned int n)
{
    extern __shared__ typ sdata[];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = threadIdx.x;
    unsigned int i = blockIdx.x*(blockSize*2) + threadIdx.x;
    unsigned int gridSize = blockSize*2*gridDim.x;
    typ myAcc = task::init();

    // we reduce multiple elements per thread.  The number is determined by the
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < n)
    {
        myAcc = task::reduce(myAcc, task::f(i, g_idata[i]));

        // ensure we don't read out of bounds -- this is optimized away for powerOf2 sized arrays
        if (nIsPow2 || i + blockSize < n)
            myAcc = task::reduce(myAcc, task::f(i+blockSize, g_idata[i+blockSize]));

        i += gridSize;
    }

    // do reduction in shared mem
    reduceBlockGen<blockSize, typ, task>(sdata, myAcc, tid);

    // write result for this block to global mem
    if (tid == 0) g_odata[blockIdx.x] = sdata[0];
}

// Helper for determining the last finishing thread. Note that this
// will generate one copy per #include. As a result, every kernel
// including this will have to have its own call/export of
// resetRetirementCount!
__device__ unsigned int retirementCount = 0;
__host__ __inline__ cudaError_t resetRetirementCount()
{
  unsigned int retCnt = 0;
  return cudaMemcpyToSymbol(__SYM(retirementCount), &retCnt, sizeof(unsigned int), 0, cudaMemcpyHostToDevice);
}

// This reduction kernel reduces an arbitrary size array in a single kernel invocation
// It does so by keeping track of how many blocks have finished.  After each thread
// block completes the reduction of its own block of data, it "takes a ticket" by
// atomically incrementing a global counter.  If the ticket value is equal to the number
// of thread blocks, then the block holding the ticket knows that it is the last block
// to finish.  This last block is responsible for summing the results of all the other
// blocks.
//
// In order for this to work, we must be sure that before a block takes a ticket, all
// of its memory transactions have completed.  This is what __threadfence() does -- it
// blocks until the results of all outstanding memory transactions within the
// calling thread are visible to all other threads.
//
// For more details on the reduction algorithm (notably the multi-pass approach), see
// the "reduction" sample in the CUDA SDK.
template <
    unsigned int blockSize
  , bool nIsPow2
  , typename typ
  , typename styp
  , class task
  >
__device__  __inline__
void reduceSinglePass_devGen(const styp *g_idata, typ *g_odata, unsigned int n)
{

    //
    // PHASE 1: Process all inputs assigned to this block
    //

    reduceBlocksGen<blockSize, nIsPow2, typ, styp, task>(g_idata, g_odata, n);

    //
    // PHASE 2: Last block finished will process all partial sums
    //

    if (gridDim.x > 1)
    {
        const unsigned int tid = threadIdx.x;
        __shared__ bool amLast;
        extern typ __shared__ smem[];

        // wait until all outstanding memory instructions in this thread are finished
        __threadfence();

        // Thread 0 takes a ticket
        if (tid==0)
        {
            unsigned int ticket = atomicInc(&retirementCount, gridDim.x);
            // If the ticket ID is equal to the number of blocks, we are the last block!
            amLast = (ticket == gridDim.x-1);
        }

        __syncthreads();

        // The last block sums the results of all other blocks
        if (amLast)
        {
            int i = tid;
            typ myAcc = task::init();

            while (i < gridDim.x)
            {
                myAcc = task::reduce(myAcc, g_odata[i]);
                i += blockSize;
            }

            reduceBlockGen<blockSize, typ, task>(smem, myAcc, tid);

            if (tid==0)
            {
                g_odata[0] = smem[0];

                // reset retirement count so that next run succeeds
                retirementCount = 0;
            }
        }
    }
}

#define TASKCFG static __inline__ __device__

#endif // #ifndef _REDUCE_KERNEL_H_
