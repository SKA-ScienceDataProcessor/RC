// Requires GCC 4.9+ for custom OpenMP 4.0 reducers.
//   GCC 4.9.2 is already deployed on the cluster.
// Intel C++ 15.0 still does not implement this.

#include <cmath>
#include <cstdlib>

struct place {
  size_t pos;
  double val;
};

inline place pmax(const place & x, const place & y){
  if (y.val > x.val) return y; else return x;
}

inline
void findPeak(const double *idata, place * odata, unsigned int n) {
  place p = {0u, 0.0};

  #pragma omp declare reduction (maxer: place : omp_out = pmax(omp_out, omp_in))
  #pragma omp parallel for reduction(maxer: p)
  for (unsigned int i = 0; i<n; i++) {
    p = pmax(p, {n, fabs(idata[n])});
  }

  *odata = p;
}

inline
void subtract_psf_kernel(
          double * res_p_trans
  , const double * psf_p_trans
  , int stopx
  , int stopy
  , int diff
  , int pitch
  , double peak_x_gain
  ) {
    int lastx = stopx * pitch;
    #pragma omp parallel for
    for(int i = 0; i < lastx; i += pitch)
      for(int j = 0; j < stopy; j++)
        res_p_trans[i+j] -= peak_x_gain * psf_p_trans[i+j + diff];
}

typedef long long int lli;

inline
void subtractPSF(
          double * res_p
  , const double * psf_p
  , lli peak_res_pos
  , lli peak_psf_pos
  , int pitch
  , double peak_x_gain
  ) {
  const lli diff = peak_psf_pos - peak_res_pos;
  lldiv_t
      resxy = lldiv(peak_res_pos, (lli)pitch)
    , psfxy = lldiv(peak_psf_pos, (lli)pitch)
    ;
  int
      stopx = pitch - abs (psfxy.rem - resxy.rem)
    , stopy = pitch - abs (psfxy.quot - resxy.quot)
    ;

  if (diff >= 0)
    subtract_psf_kernel(res_p, psf_p + diff, stopx, stopy, diff, pitch, peak_x_gain);
  else
    subtract_psf_kernel(res_p - diff, psf_p, stopx, stopy, diff, pitch, peak_x_gain);
}

extern "C"
void deconvolve(
          double * mod_p
  ,       double * res_p
  , const double * psf_p
  , int siz
  , int pitch
  , unsigned int niters
  , double gain
  , double threshold
  ) {
  place
      found_place_psf
    , found_place_res
    ;
  int totsize = siz * pitch;

  findPeak(psf_p, &found_place_psf, totsize);

  for (unsigned int i = 0; i < niters; ++i) {
    findPeak(res_p, &found_place_res, totsize);

    if (abs(found_place_res.val) < threshold) break;

    subtractPSF(res_p, psf_p, found_place_res.pos, found_place_psf.pos, pitch, found_place_res.val * gain);
    mod_p[found_place_res.pos] += found_place_res.val * gain;
  }
}
