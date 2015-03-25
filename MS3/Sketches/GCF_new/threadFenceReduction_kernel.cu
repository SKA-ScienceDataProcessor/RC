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

#include "threadFenceReduction_kernel.cuh"

bool isPow2(unsigned int x)
{
    return ((x&(x-1))==0);
}

template <unsigned int blockSize, bool nIsPow2>
__global__
void reduceSinglePass(const cuDoubleComplex *g_idata, double *g_odata, unsigned int n){
  reduceSinglePass_dev<blockSize, nIsPow2>(g_idata, g_odata, n);
}

// blocksize is >= 64 always
extern "C"
void reduceSinglePass(int size, int threads, int blocks, const cuDoubleComplex *d_idata, double *d_odata)
{
    dim3 dimBlock(threads, 1, 1);
    dim3 dimGrid(blocks, 1, 1);
    int smemSize = threads * sizeof(double);

    // choose which of the optimized versions of reduction to launch
    if (isPow2(size))
    {
        switch (threads)
        {
            case 512:
                reduceSinglePass<512, true><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;

            case 256:
                reduceSinglePass<256, true><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;

            case 128:
                reduceSinglePass<128, true><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;

            case 64:
                reduceSinglePass< 64, true><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;
        }
    }
    else
    {
        switch (threads)
        {
            case 512:
                reduceSinglePass<512, false><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;

            case 256:
                reduceSinglePass<256, false><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;

            case 128:
                reduceSinglePass<128, false><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;

            case 64:
                reduceSinglePass< 64, false><<< dimGrid, dimBlock, smemSize >>>(d_idata, d_odata, size);
                break;
        }
    }
}

/* How to use it
void getNumBlocksAndThreads(int n, int maxBlocks, int maxThreads, int &blocks, int &threads)
{
    if (n == 1)
    {
        threads = 1;
        blocks = 1;
    }
    else
    {
        threads = (n < maxThreads*2) ? nextPow2(n / 2) : maxThreads;
        blocks = max(1, n / (threads * 2));
    }

    blocks = min(maxBlocks, blocks);
}
*/
