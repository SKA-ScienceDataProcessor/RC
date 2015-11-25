#include <complex>
typedef std::complex<double> complexd;

const int over = 8;

const int
    num_baselines = 32131
  , num_times = 200
  ;

struct visData {
  double u, v, w;
  complexd amp;
};

template <bool chunked> struct dlayout;
template<> struct dlayout<true> {typedef const visData type[num_baselines][num_times];};
template<> struct dlayout<false>{typedef const visData type[num_baselines *num_times];};

template <bool chunked, int gcf_size> using kern_type = void (&)(
    double
  , complexd[]
  , const complexd[over][over][gcf_size][gcf_size]
  , typename dlayout<chunked>::type data
  , int
  );

// We still keep them usable with FFI.
#define __KERNEL_CHUNKED_DECL(GCF_SIZE)                \
extern "C"                                             \
void gridKernel_scatter_full_chunked##GCF_SIZE(        \
    double scale                                       \
  , complexd grid[]                                    \
  , const complexd gcf[over][over][GCF_SIZE][GCF_SIZE] \
  , const visData data[num_baselines][num_times]       \
  , int grid_size                                      \
  )
__KERNEL_CHUNKED_DECL(16);
__KERNEL_CHUNKED_DECL(32);

#define __KERNEL_DECL(GCF_SIZE)                        \
extern "C"                                             \
void gridKernel_scatter_full##GCF_SIZE(                \
    double scale                                       \
  , complexd grid[]                                    \
  , const complexd gcf[over][over][GCF_SIZE][GCF_SIZE] \
  , const visData data[num_baselines*num_times]        \
  , int grid_size                                      \
  )
__KERNEL_DECL(16);
__KERNEL_DECL(32);
