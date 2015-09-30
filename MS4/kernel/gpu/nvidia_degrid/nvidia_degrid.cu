
#include <stdio.h>

// Oversampling
#define GCF_GRID 8

// We only use double complex values
typedef double2 CmplxType;
typedef double3 PosType;

extern "C" __global__
void subtractVis(CmplxType out[], CmplxType in[], int npts)
{
  for (int n = threadIdx.x; n < npts; n += blockDim.x) {
    out[n].x = out[n].x - in[n].x;
    out[n].y = out[n].y - in[n].y;
  }
}

__device__ double atomicAdd(double* address, double val)
{
    unsigned long long int* address_as_ull =
                             (unsigned long long int*)address;
    unsigned long long int old = *address_as_ull, assumed;
    do {
        assumed = old;
        old = atomicCAS(address_as_ull, assumed,
                        __double_as_longlong(val +
                                             __longlong_as_double(assumed)));
    } while (assumed != old);
    return __longlong_as_double(old);
}

__device__ double make_zero(double2* in) { return (double)0.0;}

extern "C" __global__ void
//__launch_bounds__(256, 6)
degrid_kernel(CmplxType* out, PosType* in, CmplxType* img, CmplxType* gcf,
              int npts, int img_dim, int gcf_dim) {
   //TODO remove hard-coded 32
   for (int n = 32*blockIdx.x; n<npts; n+= 32*gridDim.x) {
   for (int q=threadIdx.y;q<32;q+=blockDim.y) {
      if (n+q >= npts) break;
      PosType inn = in[n+q];
      int sub_x = floor(GCF_GRID*(inn.x-floor(inn.x)));
      int sub_y = floor(GCF_GRID*(inn.y-floor(inn.y)));
      int main_x = floor(inn.x);
      int main_y = floor(inn.y);
      double sum_r = make_zero(img);
      double sum_i = make_zero(img);
      for(int a = threadIdx.x-gcf_dim/2;a<(gcf_dim+1)/2;a+=blockDim.x)
      for(int b = -gcf_dim/2;b<(gcf_dim+1)/2;b++)
      {
         //auto this_img = img[main_x+a+img_dim*(main_y+b)];
         //auto r1 = this_img.x;
         //auto i1 = this_img.y;
         double r1;
         double i1;
         if (main_x+a < 0 || main_y+b < 0 ||
             main_x+a >= img_dim  || main_y+b >= img_dim) {
            r1=i1=0.0;
         } else {
             r1 = img[main_x+a+img_dim*(main_y+b)].x;
             i1 = img[main_x+a+img_dim*(main_y+b)].y;
         }
         //auto this_gcf = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
         //               gcf_dim*b+a]);
         //auto r2 = this_gcf.x;
         //auto i2 = this_gcf.y;
         double r2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                                gcf_dim*b+a].x);
         double i2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                                gcf_dim*b+a].y);
         sum_r += r1*r2 - i1*i2;
         sum_i += r1*i2 + r2*i1;
      }

      for(int s = blockDim.x < 16 ? blockDim.x : 16; s>0;s/=2) {
         sum_r += __shfl_down(sum_r,s);
         sum_i += __shfl_down(sum_i,s);
      }
      if (threadIdx.x == 0) {
          out[n+q].x = sum_r;
          out[n+q].y = sum_i;
      }
   }
   }
}
extern "C" __global__ void
//__launch_bounds__(256, 6)
degrid_kernel_small_gcf(CmplxType* out, PosType* in, CmplxType* img, CmplxType* gcf,
                        int npts, int img_dim, int gcf_dim) {

   //TODO remove hard-coded 32
#ifdef __COMPUTE_GCF
   double T = gcf[0].x;
   double w = gcf[0].y;
   float p1 = 2*3.1415926*w;
   float p2 = p1*T;
#endif
   for (int n = 32*blockIdx.x; n<npts; n+= 32*gridDim.x) {
   for (int q=threadIdx.y;q<32;q+=blockDim.y) {
      if (n+q >= npts) break;
      PosType inn = in[n+q];
      int sub_x = floor(GCF_GRID*(inn.x-floor(inn.x)));
      int sub_y = floor(GCF_GRID*(inn.y-floor(inn.y)));
      int main_x = floor(inn.x);
      int main_y = floor(inn.y);
      double sum_r = make_zero(img);
      double sum_i = make_zero(img);
      int a = -gcf_dim/2 + threadIdx.x%gcf_dim;
      for(int b = -gcf_dim/2+threadIdx.x/gcf_dim;b<gcf_dim/2;b+=blockDim.x/gcf_dim)
      {
         //auto this_img = img[main_x+a+img_dim*(main_y+b)];
         //auto r1 = this_img.x;
         //auto i1 = this_img.y;
         double r1;
         double i1;
         if (main_x+a < 0 || main_y+b < 0 ||
             main_x+a >= img_dim  || main_y+b >= img_dim) {
            r1=i1=0.0;
         } else {
             r1= img[main_x+a+img_dim*(main_y+b)].x;
             i1= img[main_x+a+img_dim*(main_y+b)].y;
         }
         //auto this_gcf = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
         //               gcf_dim*b+a]);
         //auto r2 = this_gcf.x;
         //auto i2 = this_gcf.y;
         double r2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                                gcf_dim*b+a].x);
         double i2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                                gcf_dim*b+a].y);
         sum_r += r1*r2 - i1*i2;
         sum_i += r1*i2 + r2*i1;
      }
      for(int s = blockDim.x < 16 ? blockDim.x : 16; s>0;s/=2) {
         sum_r += __shfl_down(sum_r,s);
         sum_i += __shfl_down(sum_i,s);
      }
      if (threadIdx.x == 0) {
          out[n+q].x = sum_r;
          out[n+q].y = sum_i;
      }
   }
   }
}
__device__ void warp_reduce(double &in, int sz = 16) {
   if (16<sz) sz=16;
   for(int s = sz; s>0;s/=2) {
      in += __shfl_down(in,s);
   }
}
__device__ void warp_reduce(float &in, int sz = 16) {
   if (16<sz) sz=16;
   for(int s = sz; s>0;s/=2) {
      in += __shfl_down(in,s);
   }
}
__device__ void warp_reduce2(float &in, int sz = 32) {
   if (32<sz) sz=32;
   for(int s=1; s<sz; s*=2) {
      in += __shfl_down(in,s);
   }
}
__device__ void warp_reduce2(double &in, int sz = 32) {
   if (32<sz) sz=32;
   for(int s=1; s<sz; s*=2) {
      in += __shfl_down(in,s);
   }
}
extern "C"__global__
void vis2ints(PosType *vis_in, int2* vis_out, int npts) {
   for (int q=threadIdx.x+blockIdx.x*blockDim.x;
        q<npts;
        q+=gridDim.x*blockDim.x) {
      PosType inn = vis_in[q];
      int main_y = floor(inn.y);
      int sub_y = floor(GCF_GRID*(inn.y-main_y));
      int main_x = floor(inn.x);
      int sub_x = floor(GCF_GRID*(inn.x-main_x));
      vis_out[q].x = main_x*GCF_GRID+sub_x;
      vis_out[q].y = main_y*GCF_GRID+sub_y;
   }
}
//Make sure visibilities are sorted by  main_x/blocksize then main_y/blocksize
// blockgrid should be img_dim/blocksize
extern "C"__global__
void set_bookmarks(int2* vis_in, int npts, int blocksize, int blockgrid, int* bookmarks) {
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
extern "C" __global__ void
__launch_bounds__(1024, 1)
degrid_kernel_scatter(CmplxType* out, int2* in, size_t npts, CmplxType* img,
                      int img_dim, CmplxType* gcf, int gcf_dim, int* bookmarks) {

   extern __shared__ int2 inbuff[];
   CmplxType *shm = (CmplxType *)&inbuff[32];
 
   int left = blockIdx.x*blockDim.x;
   int top = blockIdx.y*blockDim.y;
   int this_x = left+threadIdx.x;
   int this_y = top+threadIdx.y;
   double r1 = img[this_x + img_dim * this_y].x;
   double i1 = img[this_x + img_dim * this_y].y;
   double sum_r = make_zero(img);
   double sum_i = make_zero(img);
   int half_gcf = gcf_dim/2;

   int bm_x = left/half_gcf-1;
   int bm_y = top/half_gcf-1;
   for (int y=bm_y<0?0:bm_y;(y<bm_y+2+(blockDim.y+half_gcf-1)/half_gcf)&&(y<img_dim/half_gcf);y++) {
   for (int x=bm_x<0?0:bm_x;(x<bm_x+2+(blockDim.x+half_gcf-1)/half_gcf)&&(x<img_dim/half_gcf);x++) {
   for (int n=bookmarks[y*img_dim/half_gcf+x];
            n<bookmarks[y*img_dim/half_gcf+x+1]; n+=32) {
      if (threadIdx.x<32 && threadIdx.y==0) inbuff[threadIdx.x]=in[n+threadIdx.x];
      __syncthreads(); //1438

      //TODO remove
      //if (threadIdx.y==0 && threadIdx.x==22) shm[threadIdx.y][threadIdx.x].x = 4.44;
      shm[threadIdx.x*gcf_dim/4+threadIdx.y].x = 0.00;
      shm[threadIdx.x*gcf_dim/4+threadIdx.y].y = 0.00;
      //if (threadIdx.y==0 && threadIdx.x==22) shm[threadIdx.y][threadIdx.x].x = 4.04;
   for (int q = 0; q<32 && n+q < bookmarks[y*img_dim/half_gcf+x+1]; q++) {
      int2 inn = inbuff[q];
      //TODO Don't floor initially, just compare
      int main_y = inn.y/GCF_GRID;
      int b = this_y - main_y;
      //Skip the whole block?
      //if (top-main_y >= gcf_dim/2 || top-main_y+gcf_dim < -gcf_dim/2) continue;
      int main_x = inn.x/GCF_GRID;
      int a = this_x - main_x;
      //Skip the whole block?
      //if (left-main_x >= gcf_dim/2 || left-main_x+gcf_dim < -gcf_dim/2) continue;
      if (a >= half_gcf || a < -half_gcf ||
          b >= half_gcf || b < -half_gcf) {
         sum_r = 0.00;
         sum_i = 0.00;
      } else {
         int sub_x = inn.x%GCF_GRID;
         int sub_y = inn.y%GCF_GRID;
         double r2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                        gcf_dim*b+a].x);
         double i2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                        gcf_dim*b+a].y);
         //TODO remove
         //if (a!=0 || b!=0) {
         //   sum_r = sum_i = 0;
         //} else {
         //sum_r = (n+q)*1.0;//r2;
         //sum_i = (n+q)*1.0;//i2;
         //}
         //TODO restore
         sum_r = r1*r2 - i1*i2;
         sum_i = r1*i2 + r2*i1;
      }

     //reduce in two directions
      //WARNING: Adjustments must be made if blockDim.y and blockDim.x are no
      //         powers of 2
      //Reduce using shuffle first
#if 1
      warp_reduce2(sum_r);
      warp_reduce2(sum_i);
      int stripe_width_x = blockDim.x/32; //number of rows in shared memory that
                                          //contain data for a single visibility
      int n_stripe = blockDim.y/stripe_width_x; //the number of stripes that fit
      if (0 == threadIdx.x%32) {
         shm[threadIdx.x/32+stripe_width_x*(q%n_stripe)*gcf_dim/4+threadIdx.y].x = sum_r;
         shm[threadIdx.x/32+stripe_width_x*(q%n_stripe)*gcf_dim/4+threadIdx.y].y = sum_i;
      }
      //Once we've accumulated a full set, or this is the last q, reduce more
      if (q+1==n_stripe || q==31 || n+q == bookmarks[y*img_dim/half_gcf+x+1]-1) {
         int stripe_width_y = blockDim.y/32;
         if (stripe_width_y < 1) stripe_width_y=1;
         __syncthreads();
         if (threadIdx.y<(q+1)*stripe_width_x) {
            sum_r = shm[threadIdx.y*gcf_dim/4+threadIdx.x].x;
            sum_i = shm[threadIdx.y*gcf_dim/4+threadIdx.x].y;
            warp_reduce2(sum_r, blockDim.y<32 ? blockDim.y:32);
            warp_reduce2(sum_i, blockDim.y<32 ? blockDim.y:32);
            if (0 == threadIdx.x%32) {
               //shm[0][threadIdx.y*stripe_width_y + (threadIdx.x/32)].x = sum_r;
               //shm[0][threadIdx.y*stripe_width_y + (threadIdx.x/32)].y = sum_i;
               int idx = threadIdx.y*stripe_width_y + (threadIdx.x/32);
               atomicAdd(&(out[n+idx/(stripe_width_x*stripe_width_y)].x), sum_r);
               atomicAdd(&(out[n+idx/(stripe_width_x*stripe_width_y)].y), sum_i);
            }
         }
#if 0
         __syncthreads();
         //Warning: trouble if gcf_dim > sqrt(32*32*32) = 128
         int idx = threadIdx.x + threadIdx.y*blockDim.x;
         if (idx < stripe_width_x*stripe_width_y*(q+1)) {
            sum_r = shm[idx].x;
            sum_i = shm[idx].y;
            warp_reduce2(sum_r, stripe_width_x*stripe_width_y);
            warp_reduce2(sum_i, stripe_width_x*stripe_width_y);
            if (0 == idx%(stripe_width_x*stripe_width_y)) {
               atomicAdd(&(out[n+idx/(stripe_width_x*stripe_width_y)].x),sum_r);
               atomicAdd(&(out[n+idx/(stripe_width_x*stripe_width_y)].y),sum_i);
            }
         }
#endif
      }

#else

      shm[threadIdx.y*gcf_dim/4+threadIdx.x].x = sum_r;
      shm[threadIdx.y*gcf_dim/4+threadIdx.x].y = sum_i;
      __syncthreads();
      //Reduce in y
      for(int s = blockDim.y/2;s>0;s/=2) {
         if (threadIdx.y < s) {
           shm[threadIdx.y*gcf_dim/4+threadIdx.x].x += shm[(threadIdx.y+s)*gcf_dim/4+threadIdx.x].x;
           shm[threadIdx.y*gcf_dim/4+threadIdx.x].y += shm[(threadIdx.y+s)*gcf_dim/4+threadIdx.x].y;
         }
         __syncthreads();
      }

      //Reduce the top row
      for(int s = blockDim.x/2;s>16;s/=2) {
         if (0 == threadIdx.y && threadIdx.x < s)
                    shm[threadIdx.x].x += shm[threadIdx.x+s].x;
         if (0 == threadIdx.y && threadIdx.x < s)
                    shm[threadIdx.x].y += shm[threadIdx.x+s].y;
         __syncthreads();
      }
      if (threadIdx.y == 0) {
         //Reduce the final warp using shuffle
         CmplxType tmp = shm[threadIdx.x];
         for(int s = blockDim.x < 16 ? blockDim.x : 16; s>0;s/=2) {
            tmp.x += __shfl_down(tmp.x,s);
            tmp.y += __shfl_down(tmp.y,s);
         }
         if (threadIdx.x == 0) {
            atomicAdd(&(out[n+q].x),tmp.x);
            atomicAdd(&(out[n+q].y),tmp.y);
         }
      }
#endif
   }
   } //n
   } //x
   } //y
}

extern "C" __global__ void
__launch_bounds__(1024, 1)
degrid_kernel_window(CmplxType* out, int2* in, size_t npts, CmplxType* img,
                     int img_dim, CmplxType* gcf, int gcf_dim) {

#ifdef __COMPUTE_GCF
   double T = gcf[0].x;
   double w = gcf[0].y;
   float p1 = 2*3.1415926*w;
   float p2 = p1*T;
#endif
   extern int2 __shared__ inbuff[];
   CmplxType *shm = (CmplxType *)&inbuff[32];

   double sum_r = make_zero(img);
   double sum_i = make_zero(img);
   double r1 = sum_r;
   double i1 = sum_r;
   int half_gcf = gcf_dim/2;
   in += npts/gridDim.x*blockIdx.x;
   out += npts/gridDim.x*blockIdx.x;
   int last_idx = -INT_MAX;
   size_t gcf_y = threadIdx.y + blockIdx.y*blockDim.y;
   int end_pt = npts/gridDim.x;
   if (blockIdx.x==gridDim.x-1) end_pt = npts-npts/gridDim.x*blockIdx.x;

   for (int n=0; n<end_pt; n+=32) {

      if (threadIdx.x<32 && threadIdx.y==0) inbuff[threadIdx.x]=in[n+threadIdx.x];

      //shm[threadIdx.x][threadIdx.y].x = 0.00;
      //shm[threadIdx.x][threadIdx.y].y = 0.00;
      __syncthreads();
   for (int q = 0; q<32 && n+q < end_pt; q++) {
      int2 inn = inbuff[q];
      int main_y = inn.y/GCF_GRID;
      int main_x = inn.x/GCF_GRID;
      int this_x = gcf_dim*((main_x+half_gcf-threadIdx.x-1)/gcf_dim)+threadIdx.x;
      int this_y;
      this_y = gcf_dim*((main_y+half_gcf-gcf_y-1)/gcf_dim)+gcf_y;
      if (this_x < 0 || this_x >= img_dim ||
          this_y < 0 || this_y >= img_dim) {
          //TODO pad instead?
          sum_r = 0.0;
          sum_i = 0.0;
      } else {
      //TODO is this the same as last time?
          int this_idx = this_x + img_dim * this_y;
          prof_trigger(0);
          if (last_idx != this_idx) {
             prof_trigger(1);
             r1 = img[this_idx].x;
             i1 = img[this_idx].y;
             last_idx = this_idx;
          }
          int sub_x = inn.x%GCF_GRID;
          int sub_y = inn.y%GCF_GRID;
          int b = this_y - main_y;
          int a = this_x - main_x;
          double r2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                                 gcf_dim*b+a].x);
          double i2 = __ldg(&gcf[gcf_dim*gcf_dim*(GCF_GRID*sub_y+sub_x) +
                                 gcf_dim*b+a].y);
          sum_r = r1*r2 - i1*i2;
          sum_i = r1*i2 + r2*i1;
      }

     //reduce in two directions
      //WARNING: Adjustments must be made if blockDim.y and blockDim.x are no
      //         powers of 2
      //Reduce using shuffle first
#if 1
      warp_reduce2(sum_r);
      warp_reduce2(sum_i);
#if 0
      //Write immediately
      if (0 == threadIdx.x%32) {
         atomicAdd(&(out[n+q].x),sum_r);
         atomicAdd(&(out[n+q].y),sum_i);
      }
#else
      //Reduce again in shared mem
      if (0 == threadIdx.x%32) {
         //Save results as if shared memory were blockDim.y*32 by blockDim.x/32
         //Each q writes a unique set of blockDim.y rows
         shm[(threadIdx.y+q*blockDim.y)*blockDim.x/32+threadIdx.x/32].x = sum_r;
         shm[(threadIdx.y+q*blockDim.y)*blockDim.x/32+threadIdx.x/32].y = sum_i;
      }
      if (q==31 || n+q == npts/gridDim.x-1) {
         //Once we have filled all of shared memory, reduce further
         //and write using atomicAdd
         __syncthreads();
         sum_r=shm[threadIdx.y*gcf_dim/4+threadIdx.x].x;
         sum_i=shm[threadIdx.y*gcf_dim/4+threadIdx.x].y;
         if (blockDim.x*blockDim.y>1024) {
            warp_reduce2(sum_r);
            warp_reduce2(sum_i);
            if (0==(threadIdx.x + threadIdx.y*blockDim.x)%32) {
               atomicAdd(&(out[n+(threadIdx.x+threadIdx.y*blockDim.x)/(blockDim.x*blockDim.y/32)].x), sum_r);
               atomicAdd(&(out[n+(threadIdx.x+threadIdx.y*blockDim.x)/(blockDim.x*blockDim.y/32)].y), sum_i);
            }
         } else {
            warp_reduce2(sum_r,blockDim.x*blockDim.y/32);
            warp_reduce2(sum_i,blockDim.x*blockDim.y/32);
            if (0==(threadIdx.x + threadIdx.y*blockDim.x)%(blockDim.x*blockDim.y/32)) {
               atomicAdd(&(out[n+(threadIdx.x+threadIdx.y*blockDim.x)/(blockDim.x*blockDim.y/32)].x), sum_r);
               atomicAdd(&(out[n+(threadIdx.x+threadIdx.y*blockDim.x)/(blockDim.x*blockDim.y/32)].y), sum_i);
            }
         }
      }
#endif
#else

      //Simple reduction

      shm[threadIdx.y*gcf_dim/4+threadIdx.x].x = sum_r;
      shm[threadIdx.y*gcf_dim/4+threadIdx.x].y = sum_i;
      __syncthreads();
      //Reduce in y
      for(int s = blockDim.y/2;s>0;s/=2) {
         if (threadIdx.y < s) {
           shm[threadIdx.y*gcf_dim/4+threadIdx.x].x += shm[(threadIdx.y+s)*gcf_dim/4+threadIdx.x].x;
           shm[threadIdx.y*gcf_dim/4+threadIdx.x].y += shm[(threadIdx.y+s)*gcf_dim/4+threadIdx.x].y;
         }
         __syncthreads();
      }

      //Reduce the top row
      for(int s = blockDim.x/2;s>16;s/=2) {
         if (0 == threadIdx.y && threadIdx.x < s)
                    shm[threadIdx.x].x += shm[threadIdx.x+s].x;
         if (0 == threadIdx.y && threadIdx.x < s)
                    shm[threadIdx.x].y += shm[threadIdx.x+s].y;
         __syncthreads();
      }
      if (threadIdx.y == 0) {
         //Reduce the final warp using shuffle
         CmplxType tmp = shm[threadIdx.x];
         for(int s = blockDim.x < 16 ? blockDim.x : 16; s>0;s/=2) {
            tmp.x += __shfl_down(tmp.x,s);
            tmp.y += __shfl_down(tmp.y,s);
         }
         if (threadIdx.x == 0) {
            atomicAdd(&(out[n+q].x),tmp.x);
            atomicAdd(&(out[n+q].y),tmp.y);
            //out[n+q].x=tmp.x;
            //out[n+q].y=tmp.y;
         }
      }
      __syncthreads();
#endif
   } //q
   __syncthreads();
   } //n
}

