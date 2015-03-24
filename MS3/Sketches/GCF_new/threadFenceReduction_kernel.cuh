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

#include <device_functions.h>
#include <cuComplex.h>

/*
    Parallel sum reduction using shared memory
    - takes log(n) steps for n input elements
    - uses n/2 threads
    - only works for power-of-2 arrays

    This version adds multiple elements per thread sequentially.  This reduces the overall
    cost of the algorithm while keeping the work complexity O(n) and the step complexity O(log n).
    (Brent's Theorem optimization)

    See the CUDA SDK "reduction" sample for more information.
*/

template <unsigned int blockSize>
__device__ __inline__ void
reduceBlock(volatile double *sdata, double mySum, const unsigned int tid)
{
    sdata[tid] = mySum;
    __syncthreads();

    // do reduction in shared mem
    if (blockSize >= 512)
    {
        if (tid < 256) sdata[tid] = mySum = mySum + sdata[tid + 256];
        __syncthreads();
    }

    if (blockSize >= 256)
    {
        if (tid < 128) sdata[tid] = mySum = mySum + sdata[tid + 128];
        __syncthreads();
    }

    if (blockSize >= 128)
    {
        if (tid <  64) sdata[tid] = mySum = mySum + sdata[tid +  64];
        __syncthreads();
    }

    if (tid < 32)
    {
        sdata[tid] = mySum = mySum + sdata[tid + 32];
        sdata[tid] = mySum = mySum + sdata[tid + 16];
        sdata[tid] = mySum = mySum + sdata[tid +  8];
        sdata[tid] = mySum = mySum + sdata[tid +  4];
        sdata[tid] = mySum = mySum + sdata[tid +  2];
        sdata[tid] = mySum = mySum + sdata[tid +  1];
    }
}

template <unsigned int blockSize, bool nIsPow2>
__device__ __inline__ void
reduceBlocks(const cuDoubleComplex *g_idata, double *g_odata, unsigned int n)
{
    extern __shared__ double sdata[];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = threadIdx.x;
    unsigned int i = blockIdx.x*(blockSize*2) + threadIdx.x;
    unsigned int gridSize = blockSize*2*gridDim.x;
    double mySum = 0;

    // we reduce multiple elements per thread.  The number is determined by the
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < n)
    {
        mySum += g_idata[i].x;

        // ensure we don't read out of bounds -- this is optimized away for powerOf2 sized arrays
        if (nIsPow2 || i + blockSize < n)
            mySum += g_idata[i+blockSize].x;

        i += gridSize;
    }

    // do reduction in shared mem
    reduceBlock<blockSize>(sdata, mySum, tid);

    // write result for this block to global mem
    if (tid == 0) g_odata[blockIdx.x] = sdata[0];
}

// Global variable used by reduceSinglePass to count how many blocks have finished
__device__ unsigned int retirementCount = 0;

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
template <unsigned int blockSize, bool nIsPow2>
__device__  __inline__
void reduceSinglePass_dev(const cuDoubleComplex *g_idata, double *g_odata, unsigned int n)
{

    //
    // PHASE 1: Process all inputs assigned to this block
    //

    reduceBlocks<blockSize, nIsPow2>(g_idata, g_odata, n);

    //
    // PHASE 2: Last block finished will process all partial sums
    //

    if (gridDim.x > 1)
    {
        const unsigned int tid = threadIdx.x;
        __shared__ bool amLast;
        extern double __shared__ smem[];

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
            double mySum = 0;

            while (i < gridDim.x)
            {
                mySum += g_odata[i];
                i += blockSize;
            }

            reduceBlock<blockSize>(smem, mySum, tid);

            if (tid==0)
            {
                g_odata[0] = smem[0];

                // reset retirement count so that next run succeeds
                retirementCount = 0;
            }
        }
    }
}

template <unsigned int blockSize, bool nIsPow2>
__global__
void reduceSinglePass(const cuDoubleComplex *g_idata, double *g_odata, unsigned int n){
  reduceSinglePass_dev<blockSize, nIsPow2>(g_idata, g_odata, n);
}

#endif // #ifndef _REDUCE_KERNEL_H_
