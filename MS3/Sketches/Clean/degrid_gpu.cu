/*
   This code is a modified version of NVidia's degridding code:
   https://github.com/SKA-ScienceDataProcessor/GPUDegrid/blob/master/degrid_gpu.cu
 */

#include "common.h"
#include "metrix.h"
#include "OskarBinReader.h"

template <int over, bool isbig> struct degridder {};

template <int over> struct degridder<over, true>{

static __device__ __inline__
//__launch_bounds__(256, 6)
// double2 is enough for 'in'
void degrid_kernel(complexd* out, const double3* in, size_t npts, const complexd* img, 
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
         // Add rotation
         out[n+q] = rotw(tmp, inn.z);
      }
   }
   }
}

};

template <int over> struct degridder<over, false>{

static __device__ __inline__
//__launch_bounds__(256, 6)
void degrid_kernel(complexd* out, const double3* in, size_t npts, const complexd* img, 
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
         // Add rotation
         out[n+q] = rotw(tmp, inn.z);
      }
   }
   }
}

};

#define __PADAPI static __inline__ __host__ __device__

template <int max_gcf_size> struct degridPadder {
  __PADAPI int full_padded_size(int bare_lin_size){
    return bare_lin_size*(bare_lin_size+2*max_gcf_size)+2*max_gcf_size;
  }
  __PADAPI int bare_offset(int bare_lin_size){
    return (bare_lin_size+1)*max_gcf_size;
  }
};

template <
    int max_gcf_dim
  , int over
  , bool isBig
  >
__global__
void degridGPU(
    const BlWMap permutations[/* baselines */]
  , complexd * out_vis
  // Padded, points to the beginning
  // of physical data
  , const double3 * uvw
  // centered in 0 w-plane
  , const complexd * gcf[]
  , const complexd * img
  , size_t timesteps_x_channels
  , size_t img_dim
  , int blOff
  ) {
  // Temporarily use the second treadblock-grid dimension as baseline dimension
  int bl = permutations[blockIdx.y + blOff].bl;
  int gcf_dim = get_supp(permutations[bl].wp);
  degridder<over, isBig>::degrid_kernel(
             out_vis + bl*timesteps_x_channels
           , uvw + bl*timesteps_x_channels
           , timesteps_x_channels
           , img+degridPadder<max_gcf_dim>::bare_offset(img_dim)
           , img_dim
           , gcf[permutations[bl].wp]
           , gcf_dim
           );
}


// We assume permutations vector
// is sorted against abs(w_plane_index) compare function
template <
    int max_gcf_dim
  , int over
  >
void degridGPU(
    const BlWMap permutations[/* baselines */]
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
  int bl;
  for(bl = 0; bl < baselines; bl++) {
    if(get_supp(permutations[bl].wp) > 16) break;
  }
  dim3 griddims_small = dim3(timesteps_x_channels/32, bl);
  degridGPU<max_gcf_dim, over, false><<<griddims_small, dim3(32,32)>>>(permutations, out_vis, uvw, gcf, img, timesteps_x_channels, img_dim, 0);
  if (bl != baselines - 1) {
    dim3 griddims_big = dim3(timesteps_x_channels/32, baselines - bl);
    degridGPU<max_gcf_dim, over, true><<<griddims_big, dim3(32,8)>>>(permutations, out_vis, uvw, gcf, img, timesteps_x_channels, img_dim, bl);
  }
}

extern "C" void test(
    const BlWMap permutations[/* baselines */]
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
  )
{
  degridGPU<256, 8>(permutations, out_vis, uvw, gcf, img, baselines, timesteps_x_channels, img_dim);
}
