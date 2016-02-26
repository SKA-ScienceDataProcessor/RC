#include <iostream>

#include "Defines.h"
#include "grid_gpu.cuh"

float getElapsed(cudaEvent_t start, cudaEvent_t stop);
void CUDA_CHECK_ERR(unsigned lineNumber, const char* fileName);

__global__ void vis2ints(double scale, double2 *vis_in, int2* vis_out, int npts);
__global__ void set_bookmarks(int2* vis_in, int npts, int blocksize, int blockgrid, int* bookmarks);
__global__ void __launch_bounds__(GCF_DIM*GCF_DIM/4/4/GCF_STRIPES/PTS, 12)
  grid_kernel_gather(CmplxType* out, int2* in, CmplxType* in_vals, size_t npts,
                              int img_dim, CmplxType* gcf, int* bookmarks, int yoff);

#define gcf_dim GCF_DIM

#ifdef __COMBINED

#ifdef __clang__
__device__ double __nvvm_floor_d(double f);
#define floor __nvvm_floor_d
#endif

void CUDA_CHECK_ERR(unsigned lineNumber, const char* fileName) {
   cudaError_t err = cudaGetLastError();
   if (err) std::cout << "Error " << err << " on line " << lineNumber << " of " << fileName << ": " << cudaGetErrorString(err) << std::endl;
}

float getElapsed(cudaEvent_t start, cudaEvent_t stop) {
   float elapsed;
   cudaEventRecord(stop);
   cudaEventSynchronize(stop);
   cudaEventElapsedTime(&elapsed, start, stop);
   return elapsed;
}

__global__ void vis2ints(double scale, double2 *vis_in, int2* vis_out, int npts) {
   for (int q=threadIdx.x+blockIdx.x*blockDim.x;
        q<npts;
        q+=gridDim.x*blockDim.x) {
      double2 inn = vis_in[q];
      inn.x *= scale;
      inn.y *= scale;
      int main_y = floor(inn.y);
      int sub_y = floor(GCF_GRID*(inn.y-main_y));
      int main_x = floor(inn.x);
      int sub_x = floor(GCF_GRID*(inn.x-main_x));
      vis_out[q].x = main_x*GCF_GRID+sub_x;
      vis_out[q].y = main_y*GCF_GRID+sub_y;
   }
}

//Make sure visibilities are sorted by  main_x/blocksize then main_y/blocksize
// blockgrid should be (img_dim+blocksize-1)/blocksize
__global__ void set_bookmarks(int2* vis_in, int npts, int blocksize, int blockgrid, int* bookmarks) {
   for (int q=threadIdx.x+blockIdx.x*blockDim.x;q<=npts;q+=gridDim.x*blockDim.x) {
      int2 this_vis = vis_in[q];
      int2 last_vis = vis_in[q-1];
      int main_x = this_vis.x/GCF_GRID/blocksize;
      int main_x_last = last_vis.x/GCF_GRID/blocksize;
      int main_y = this_vis.y/GCF_GRID/blocksize;
      int main_y_last = last_vis.y/GCF_GRID/blocksize;
      if (0==q) {
         main_y_last=0;
         main_x_last=-1;
      }
      if (npts==q) main_x = main_y = blockgrid;
      if (main_x != main_x_last || main_y != main_y_last)  {
         for (int z=main_y_last*blockgrid+main_x_last+1;
                  z<=main_y*blockgrid+main_x; z++) {
            bookmarks[z] = q;
         }
      }
   }
}

__global__ void
#if POLARIZATIONS == 1
__launch_bounds__(1024, 2)
#else
__launch_bounds__(GCF_DIM*GCF_DIM/4/4/GCF_STRIPES/PTS, 12)
#endif
grid_kernel_gather(CmplxType* out, int2* in, CmplxType* in_vals, size_t npts,
                              int img_dim, CmplxType* gcf, int* bookmarks, int yoff) {
   int2 __shared__ inbuff[32];
   CmplxType __shared__ invalbuff[POLARIZATIONS][32+32/POLARIZATIONS];
   const int bm_dim = (img_dim+gcf_dim-1)/gcf_dim*2;
   int left = blockIdx.x*blockDim.x;
   int top = blockIdx.y*blockDim.y*PTS*GCF_STRIPES;
   int this_x = left+threadIdx.x;
   int this_y = top+threadIdx.y+yoff;
   CmplxType sum[POLARIZATIONS][PTS];
   for (int p=0;p<PTS;p++) {
      for (int pz=0;pz<POLARIZATIONS;pz++) {
         sum[pz][p] = out[this_x + this_y*img_dim+p*blockDim.y*img_dim+pz*img_dim*img_dim];
      }
   }
   int half_gcf = gcf_dim/2;
   int bm_x = left/half_gcf-1;
   int bm_y = top/half_gcf-1;
   for (int y=bm_y<0?0:bm_y;(y<bm_y+2+(blockDim.y+half_gcf-1)/half_gcf)&&(y<(img_dim+half_gcf-1)/half_gcf);y++) {
   for (int x=bm_x<0?0:bm_x;(x<bm_x+2+(blockDim.x+half_gcf-1)/half_gcf)&&(x<(img_dim+half_gcf-1)/half_gcf);x++) {
   int bm_start = bookmarks[y*bm_dim+x];
   int bm_end = bookmarks[y*bm_dim+x+1];
   for (int n=bm_start; n<= bm_end; n+=32) {
      __syncthreads();
      int raw_idx = threadIdx.x+blockDim.x*threadIdx.y;
      if (raw_idx < 32) inbuff[raw_idx]= in[n+raw_idx];
      else {
         raw_idx -= 32;
         if (raw_idx < 32*POLARIZATIONS) invalbuff[raw_idx%POLARIZATIONS][raw_idx/POLARIZATIONS]= in_vals[n*POLARIZATIONS+raw_idx];
      }
      __syncthreads();
   for (int q = 0; q<32 && n+q < bm_end; q++) {
      int2 inn = inbuff[q];
      for (int p = 0; p < PTS; p++) {
      int main_y = inn.y/GCF_GRID;
      if (this_y + blockDim.y*p >= img_dim) continue;
      int b = this_y + blockDim.y*p - main_y;
      if (b > half_gcf || b <= - half_gcf) continue;
      int main_x = inn.x/GCF_GRID;
      int a = this_x - main_x;
      if (a > half_gcf || a <= - half_gcf) continue;
      int sub_x = inn.x%GCF_GRID;
      int sub_y = inn.y%GCF_GRID;
#if __CUDA_ARCH__ >= 350
#define ldg(a) __ldg(&(a))
#else
#define ldg(a) (a)
#endif
      CmplxType ctmp = ldg(gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                     gcf_dim*b+a]);
      auto r2 = ctmp.x;
      auto i2 = ctmp.y;
      for (int pz=0;pz<POLARIZATIONS;pz++) {
         CmplxType r1 = invalbuff[pz][q];
         sum[pz][p].x += r1.x*r2 - r1.y*i2;
         sum[pz][p].y += r1.x*i2 + r2*r1.y;
      }
   }
   }
   }
   }
   }
   for (int p=0;p<PTS;p++) {
      if (this_y + blockDim.y*p >= img_dim) continue;
      if (this_x >= img_dim) continue;
      for (int pz=0;pz<POLARIZATIONS;pz++) {
         out[this_x + img_dim * (this_y+blockDim.y*p) + pz*img_dim*img_dim] = sum[pz][p];
      }
   }
}

#define gridderFuncName gridGPUc
#else
#define gridderFuncName gridGPUs
#endif

void gridderFuncName (
               double scale
             , CmplxType* out
#ifdef __COMBINED
             , combined * in_c
#else
             , double3 * in, CmplxType* in_vals
#endif
             , size_t npts, size_t img_dim, CmplxType *gcf) {
   CmplxType *d_out, *d_in, *d_in_vals, *d_gcf;
   cudaEvent_t start, stop;
   cudaEventCreate(&start); cudaEventCreate(&stop);
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   cudaHostRegister(out, sizeof(CmplxType)*(img_dim*img_dim /* Unpadded data on host! +2*img_dim*gcf_dim+2*gcf_dim */)*POLARIZATIONS, 0x02);
   cudaHostRegister(gcf, sizeof(CmplxType)*GCF_GRID*GCF_GRID*gcf_dim*gcf_dim, 0x02);
#ifdef __COMBINED
   cudaHostRegister(in_c, sizeof(combined)*npts, 0x02);
#else
   cudaHostRegister(in, sizeof(double3)*npts, 0x02);
   cudaHostRegister(in_vals, sizeof(CmplxType)*npts*POLARIZATIONS, 0x02);
#endif
   cudaMalloc(&d_out, sizeof(CmplxType)*(img_dim*img_dim+2*img_dim*gcf_dim+2*gcf_dim)*POLARIZATIONS);
   cudaMalloc(&d_gcf, sizeof(CmplxType)*GCF_GRID*GCF_GRID*gcf_dim*gcf_dim);
   cudaMalloc(&d_in, sizeof(CmplxType)*npts);
   cudaMalloc(&d_in_vals, sizeof(CmplxType)*npts*POLARIZATIONS);
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   cudaEventRecord(start);
   cudaMemcpy(d_gcf, gcf, sizeof(CmplxType)*64*gcf_dim*gcf_dim,
              cudaMemcpyHostToDevice);
#ifdef __COMBINED
   cudaMemcpy2D(d_in, sizeof(double2), in_c, sizeof(combined), sizeof(double2), npts,
              cudaMemcpyHostToDevice);
   cudaMemcpy2D(d_in_vals, sizeof(CmplxType), &(in_c->a_re), sizeof(combined), sizeof(CmplxType), npts*POLARIZATIONS,
              cudaMemcpyHostToDevice);
#else
   cudaMemcpy2D(d_in, sizeof(double2), in, sizeof(double3), sizeof(double2), npts,
              cudaMemcpyHostToDevice);
   cudaMemcpy(d_in_vals, in_vals, sizeof(CmplxType)*npts*POLARIZATIONS,
              cudaMemcpyHostToDevice);
#endif
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   std::cout << "memcpy time: " << getElapsed(start, stop) << " ms." << std::endl;
   d_gcf += gcf_dim*(gcf_dim-1)/2-1;
   CmplxType* d_out_unpad = d_out + img_dim*gcf_dim+gcf_dim;
   int2* in_ints;
   int* bookmarks;
   cudaMalloc(&in_ints, sizeof(int2)*npts);
   cudaMalloc(&bookmarks, sizeof(int)*((img_dim/gcf_dim)*(img_dim/gcf_dim)*4+1));
   vis2ints<<<4,256>>>(scale, d_in, in_ints, npts);
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   set_bookmarks<<<4,256>>>(in_ints, npts, gcf_dim/2, (img_dim+gcf_dim/2-1)/(gcf_dim/2),
                               bookmarks);
   // int2* h_ints = (int2*)malloc(sizeof(int2)*npts);
   // cudaMemcpy(h_ints, in_ints, sizeof(int2)*npts, cudaMemcpyDeviceToHost);
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   cudaMemset(d_out, 0, sizeof(CmplxType)*(img_dim*img_dim+2*img_dim*gcf_dim+2*gcf_dim)*POLARIZATIONS);
   cudaEventRecord(start);
   for (int stripe=0;stripe<GCF_STRIPES;stripe++)
      grid_kernel_gather
            <<<dim3((img_dim+gcf_dim/4-1)/(gcf_dim/4), (img_dim+gcf_dim/4-1)/(gcf_dim/4)),
               dim3(gcf_dim/4, gcf_dim/4/PTS/GCF_STRIPES)>>>
                             (d_out_unpad,in_ints,d_in_vals,npts,img_dim,d_gcf,bookmarks,stripe*gcf_dim/4/GCF_STRIPES);
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   float kernel_time = getElapsed(start,stop);
   std::cout << "Processed " << npts << " complex points in " << kernel_time << " ms." << std::endl;
   std::cout << npts / 1000000.0 / kernel_time * gcf_dim * gcf_dim * 8 * POLARIZATIONS << " Gflops" << std::endl;
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   cudaMemcpy(out, d_out_unpad /* Unpadded data on host! d_out */,
              sizeof(CmplxType)*(img_dim*img_dim /* Unpadded data on host! +2*img_dim*gcf_dim+2*gcf_dim */)*POLARIZATIONS,
              cudaMemcpyDeviceToHost);
   CUDA_CHECK_ERR(__LINE__,__FILE__);
   cudaHostUnregister(gcf);
   cudaHostUnregister(out);
#ifdef __COMBINED
   cudaHostUnregister(in_c);
#else
   cudaHostUnregister(in);
   cudaHostUnregister(in_vals);
#endif
   d_gcf -= gcf_dim*(gcf_dim-1)/2-1;
   cudaFree(d_out);
   cudaFree(in_ints);
   cudaFree(bookmarks);
   cudaEventDestroy(start); cudaEventDestroy(stop);
   CUDA_CHECK_ERR(__LINE__,__FILE__);
}
