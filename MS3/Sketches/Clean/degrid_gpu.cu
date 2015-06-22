/*
   This code is a modified version of NVidia's degridding code:
   https://github.com/SKA-ScienceDataProcessor/GPUDegrid/blob/master/degrid_gpu.cu
 */

#include "common.h"
#include "metrix.h"
#include "OskarBinReader.h"

template <int over>
__global__ void
//__launch_bounds__(256, 6)
// double2 is for 'in'
degrid_kernel(complexd* out, const double3* in, size_t npts, const complexd* img, 
                              size_t img_dim, const complexd* gcf, int gcf_dim) {
   
   //TODO remove hard-coded 32
   for (int n = 32*blockIdx.x; n<npts; n+= 32*gridDim.x) {
   for (int q=threadIdx.y;q<32;q+=blockDim.y) {
      double3 inn = in[n+q];
      int sub_x = floorf(over*(inn.x-floorf(inn.x)));
      int sub_y = floorf(over*(inn.y-floorf(inn.y)));
      int main_x = floorf(inn.x); 
      int main_y = floorf(inn.y); 
      double sum_r = 0.0;
      double sum_i = 0.0;
      for(int a = threadIdx.x-gcf_dim/2;a<(gcf_dim+1)/2;a+=blockDim.x)
      for(int b = -gcf_dim/2;b<(gcf_dim+1)/2;b++)
      {
         //auto this_img = img[main_x+a+img_dim*(main_y+b)]; 
         //auto r1 = this_img.x;
         //auto i1 = this_img.y;
         auto r1 = img[main_x+a+img_dim*(main_y+b)].x; 
         auto i1 = img[main_x+a+img_dim*(main_y+b)].y; 
         if (main_x+a < 0 || main_y+b < 0 || 
             main_x+a >= img_dim  || main_y+b >= img_dim) {
            r1=i1=0.0;
         }
         //auto this_gcf = __ldg(&gcf[gcf_dim*gcf_dim*(OVER*sub_y+sub_x) + 
         //               gcf_dim*b+a]);
         //auto r2 = this_gcf.x;
         //auto i2 = this_gcf.y;
         auto r2 = __ldg(&gcf[gcf_dim*gcf_dim*(over*sub_y+sub_x) + 
                        gcf_dim*b+a].x);
         auto i2 = __ldg(&gcf[gcf_dim*gcf_dim*(over*sub_y+sub_x) + 
                        gcf_dim*b+a].y);
         sum_r += r1*r2 - i1*i2; 
         sum_i += r1*i2 + r2*i1;
      }

      for(unsigned int s = blockDim.x < 16 ? blockDim.x : 16; s>0;s/=2) {
         sum_r += __shfl_down(sum_r,s);
         sum_i += __shfl_down(sum_i,s);
      }
      complexd tmp;
      tmp.x = sum_r;
      tmp.y = sum_i;
      if (threadIdx.x == 0) {
         out[n+q] = tmp;
      }
   }
   }
}

template <int over>
__global__ void
//__launch_bounds__(256, 6)
degrid_kernel_small_gcf(complexd* out, const double3* in, size_t npts, const complexd* img, 
                              size_t img_dim, const complexd* gcf, int gcf_dim) {
   
   //TODO remove hard-coded 32
   for (int n = 32*blockIdx.x; n<npts; n+= 32*gridDim.x) {
   for (int q=threadIdx.y;q<32;q+=blockDim.y) {
      double3 inn = in[n+q];
      int sub_x = floorf(over*(inn.x-floorf(inn.x)));
      int sub_y = floorf(over*(inn.y-floorf(inn.y)));
      int main_x = floorf(inn.x); 
      int main_y = floorf(inn.y); 
      double sum_r = 0.0;
      double sum_i = 0.0;
      int a = -gcf_dim/2 + int(threadIdx.x)%gcf_dim;
      for(int b = -gcf_dim/2+int(threadIdx.x)/gcf_dim;b<gcf_dim/2;b+=blockDim.x/gcf_dim)
      {
         //auto this_img = img[main_x+a+img_dim*(main_y+b)]; 
         //auto r1 = this_img.x;
         //auto i1 = this_img.y;
         auto r1 = img[main_x+a+img_dim*(main_y+b)].x; 
         auto i1 = img[main_x+a+img_dim*(main_y+b)].y; 
         if (main_x+a < 0 || main_y+b < 0 || 
             main_x+a >= img_dim  || main_y+b >= img_dim) {
            r1=i1=0.0;
         }
         //auto this_gcf = __ldg(&gcf[gcf_dim*gcf_dim*(OVER*sub_y+sub_x) + 
         //               gcf_dim*b+a]);
         //auto r2 = this_gcf.x;
         //auto i2 = this_gcf.y;
         auto r2 = __ldg(&gcf[gcf_dim*gcf_dim*(over*sub_y+sub_x) + 
                        gcf_dim*b+a].x);
         auto i2 = __ldg(&gcf[gcf_dim*gcf_dim*(over*sub_y+sub_x) + 
                        gcf_dim*b+a].y);
         sum_r += r1*r2 - i1*i2; 
         sum_i += r1*i2 + r2*i1;
      }

      for(unsigned int s = blockDim.x < 16 ? blockDim.x : 16; s>0;s/=2) {
         sum_r += __shfl_down(sum_r,s);
         sum_i += __shfl_down(sum_i,s);
      }
      complexd tmp;
      tmp.x = sum_r;
      tmp.y = sum_i;
      if (threadIdx.x == 0) {
         out[n+q] = tmp;
      }
   }
   }
}

#define __PADAPI static __inline__ __host__ __device__

template <int max_gcf_size> struct degridPadder {
  __PADAPI int full_padded_size(int bare_lin_size){
    return bare_lin_size*(bare_lin_size+2*max_gcf_size)+2*max_gcf_size;
  }
  __PADAPI int bare_offset(int bare_lin_size){
    return (bare_lin_size+1)*max_gcf_size;
  }
};

// FIXME:
//  - add permutations vector to GPU kernels
//  - add another grid dimension for baselines
//  - assuming we sorted premutations vector,
//     make only 2 kernel launches -- one for small
//     gcf layer sizes, and the second -- for big ones
// All pointers are *device* pointers,
//   all buffer are already allocated on device,
//   necessary data are marshalled.
template <
    int max_gcf_dim
  , int over
  >
void degridGPU(
    double wstep
  , const BlWMap permutations[/* baselines */]
  , complexd* out_vis
  // Padded, points to the beginning
  // of physical data
  , const double3 * uvw
  // centered in 0 w-plane
  , const complexd * gcf[]
  , const complexd * img
  , int baselines
  , int timesteps_x_channels
  , int img_dim
  ) {
  for(int bl = 0; bl < baselines; bl++) {
    // Always use (sorted) permutations vector
    bl = permutations[bl].bl;
    int gcf_dim = get_supp(permutations[bl].wp);
    // No mirrored GCF yet
    if (gcf_dim <= 16) {
       degrid_kernel_small_gcf<over>
         <<<timesteps_x_channels/32,dim3(32,32)>>>(
             out_vis
           , uvw+bl*timesteps_x_channels, timesteps_x_channels
           , img+degridPadder<max_gcf_dim>::bare_offset(img_dim), img_dim
           , gcf[permutations[bl].wp], gcf_dim
           );
    } else {
       degrid_kernel<over>
         <<<timesteps_x_channels/32,dim3(32,8)>>>(
             out_vis
           , uvw+bl*timesteps_x_channels, timesteps_x_channels
           , img+degridPadder<max_gcf_dim>::bare_offset(img_dim), img_dim
           , gcf[permutations[bl].wp], gcf_dim
          );
    }
  }
}

// Test instantiation
extern "C" void testtest(
    double wstep
  , const BlWMap permutations[/* baselines */]
  , complexd* out_vis
  // Padded, points to the beginning
  // of physical data
  , const double3 * uvw
  // centered in 0 w-plane
  , const complexd * gcf[]
  , const complexd * img
  , int baselines
  , int timesteps_x_channels
  , int img_dim
){
  degridGPU<512, 8>(wstep, permutations, out_vis, uvw, gcf, img, baselines, timesteps_x_channels, img_dim);
}
