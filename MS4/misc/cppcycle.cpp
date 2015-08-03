#include <vector>

#include "common.h"
#include "GCF.h"
#include "fft_dyn_padded.h"
#include "herm_padded.h"
#include "OskarBinReader.h"
#include "scatter_gridder_w_dependent_dyn_1p.h"
#include "hogbom.h"
#include "stats_n_utils.h"

// Config
const double wstep = 10000.0;
const double t2 = 0.02/2.0;
const int over = 8;
const int over2 = over*over;
const int pad = 2;
const int gcfGrowth = 16;
const int gcfMinSize = 3;
const int gcfMaxSize = 128;
const int src_size = over * gcfMaxSize * (over * gcfMaxSize + pad);
const int gridSize = 2048;
const int gridPad = 2;

const int gridPitch = gridSize + gridPad;
const int fullSize = gridPitch * gridSize;

template <typename T>
void writeImgToDisk(const char * fname, T * out){
  FILE * f = fopen(fname, "wb");
  if (f == NULL) {
    printf("Can't open %s\n", fname);
    return;
  }
  printf("Writing %s ...\n", fname);
  for (int r = 0; r < gridSize; r++, out += gridPitch)
    fwrite(out, sizeof(T), gridSize, f);
  fclose(f);
}


#define __CK(a) if (res != 0) return -a

int main(int argc, char * argv[])
{
  VisData vd;
  int res;
  // res = mkFromFile(&vd, argv[1]);
  res = mkFromFile(&vd, "G:\\BR\\$from_cluster\\test_p00_s00_f00.vis");
  __CK(1);
  printf("Oskar binary is inited!\n");

  typedef std::vector<double> dv;
  typedef std::vector<Double3> d3v;
  typedef std::vector<Double3*> d3pv;
  typedef std::vector<complexd> cdv;
  typedef std::vector<complexd*> cdpv;
  typedef std::vector<int> iv;

  Metrix m;
  d3v uvwvec = d3v(vd.num_points);
  cdv pol0vec = cdv(vd.num_points);
  auto bwvec = std::vector<BlWMap>(vd.num_baselines);
  { // Need no mmvec and ampvec after bwvec map is created and pol0vec is filled.
    cdv ampvec = cdv(vd.num_points * 4);
    auto mmvec = std::vector<WMaxMin>(vd.num_baselines);
    res = readAndReshuffle(&vd, reinterpret_cast<double*>(ampvec.data()), reinterpret_cast<double*>(uvwvec.data()), &m, mmvec.data());
    mkBlWpMap(mmvec.data(), vd.num_baselines, wstep, bwvec.data());
    auto dst = pol0vec.begin();
    auto src = ampvec.begin();
    while (src != ampvec.end()) {
      *dst = *src;
      src+=4;
      dst++;
    }
    freeBinHandler(&vd);
    __CK(2);
  }
  printf("Oskar binary is read and mapped!\n");

  double maxx = std::max(m.maxu, m.maxv);

#if 0
  double scale = double(gridSize/2 - 1) / maxx * 16.0;

  // printf("Rotate!\n");
  // rotateCPU(uvwvec.data(), pol0vec.data(), vd.num_points, scale);

  printf("Reweight!\n");
  reweight(uvwvec.data(), pol0vec.data(), scale, vd.num_points, gridSize);

  std::vector<complexd> gridVec(gridSize * gridPitch, complexd(0.0, 0.0));
  complexd * grid = gridVec.data() + gridSize/2*(gridPitch+1);

  grid0(uvwvec.data(), pol0vec.data(), grid, scale, vd.num_points, gridPitch, gridSize);
  printf("Gridded!\n");

  fftw_plan plan = NULL;
  fftInitThreading();
#else
  int maxWPlane = int(std::max(round(m.maxw / wstep), round(-m.minw / wstep)));
  int numOfPlanes = 2 * maxWPlane + 1;
  auto lsize = [maxWPlane](int i) {
    return std::min(gcfMaxSize, gcfMinSize + gcfGrowth * (abs(i - maxWPlane)));
  };
  auto layerOff = [lsize](int l) {
    int off = 0;
    for (int i = 0; i < l; i++) off += lsize(i) * lsize(i);
    return off;
  };
  // int layerSizesSum = 0;
  // for (int i = 0; i < numOfPlanes; i++) layerSizesSum += lsize(i);
  int layerSizesSum = layerOff(numOfPlanes);
  int gcfDataSize = over2 * layerSizesSum;
  int gcfTableSize = over2 * numOfPlanes;

  // double scale = double((gridSize - gcfMaxSize)/2) / maxx;
  double scale = double(gridSize/2 - 1) / maxx * 16.0;
  dv avgs = dv(numOfPlanes);
  { // Need no npts after finishing accums
    iv npts = iv(numOfPlanes);
    calcAccums(
      uvwvec.data()
      , avgs.data()
      , npts.data()
      , wstep
      , vd.num_points
      , numOfPlanes
      );
    for (int n = 0; n < numOfPlanes; n++) {
      if (npts[n] > 0)
        avgs[n] /= double(npts[n]);
      else
        avgs[n] = wstep * (n - maxWPlane);
      // IMPORTANT! We scale w also !!!
      avgs[n] *= scale;
    }
  }

  printf("Reweight!\n");
  reweight(uvwvec.data(), pol0vec.data(), scale, vd.num_points, gridSize);

  // Oskar data are rotated already!
  // rotateCPU(uvwvec.data(), pol0vec.data(), vd.num_points, scale);

  fftw_plan plan = NULL;
  fftInitThreading();

  cdv gcfData = cdv(gcfDataSize);
  cdpv gcfTable = cdpv(gcfTableSize);
  { // Don't need the arena after GCF calculation
    cdv arena = cdv(src_size);

    complexd * dptr = gcfData.data();
    complexd ** tptr = gcfTable.data();
    for (int l = 0; l < numOfPlanes; l++) {
      plan = mkGCFLayer(
        plan
        , dptr
        , tptr
        , arena.data()
        , lsize(l)
        , gcfMaxSize
        , pad
        , t2
        , avgs[l]
        );
      printf("W-plane %3d is created!\n", l-maxWPlane);
      dptr += over2 * lsize(l) * lsize(l);
      tptr += over2;
    }
  }
  fftw_destroy_plan(plan);

  /*
  auto extractLayer = [=](int l) {
    char name[32];
    FILE * f;
    for (int i = 0; i < 64; i++) {
      sprintf(name, "o%02d%02d.dat", l, i);
      f = fopen(name, "wb");
      fwrite(gcfTable.data()[over2 * l], sizeof(complexd), lsize(l) * lsize(l), f);
      fclose(f);
    }
  };
  extractLayer(15);
  */

  printf("Start gridding preps!\n");
  iv blSuppvec(vd.num_baselines);
  d3pv uvwpvec(vd.num_baselines); Double3 * uvwp = uvwvec.data();
  cdpv pol0pvec(vd.num_baselines); complexd * pol0p = pol0vec.data();
  int ts_ch = vd.num_times * vd.num_channels;
  for (int i = 0; i < vd.num_baselines; i++) {
    // bwvec is unsorted, thus we directly use w-plane number
    blSuppvec[i] = lsize(bwvec[i].wp + maxWPlane); // lsize waits maxWPlane-centered w-plane
    uvwpvec[i] = uvwp; uvwp += ts_ch;
    pol0pvec[i] = pol0p; pol0p += ts_ch;
  }

  iv gcfSuppvec(numOfPlanes);
  for (int i = 0; i < numOfPlanes; i++) gcfSuppvec[i] = lsize(i);

  int
      gridPitch = gridSize + gridPad
    , fullSize = gridSize * gridPitch
    ;
  cdv gridVec(fullSize);
  printf("Start gridding!\n");
  gridKernelCPUFullGCF(
    scale
    , wstep
    , vd.num_baselines
    , blSuppvec.data()
    , gridVec.data()
    , const_cast<const complexd **>(gcfTable.data()+maxWPlane*over2) // Center at 0
    , const_cast<const Double3 **>(uvwpvec.data())
    , const_cast<const complexd **>(pol0pvec.data())
    , ts_ch
    , gridPitch
    , gridSize
    , gcfSuppvec.data() + maxWPlane // Center at 0
    );
#endif

  printf("Start normalizing!\n");
  normalizeCPU(
    gridVec.data()
    , gridPitch
    , gridSize
    );

  writeImgToDisk("grid.dat", gridVec.data());

  printf("Save the data to be destroyed by hermitianizing them!");
  herm_padded_inplace(gridVec.data(), gridSize, gridPitch);
  printf("FFTing ...\n");
  plan = NULL;
  // plan = fft_inplace_even(plan, FFTW_FORWARD, gridVec.data(), gridSize, gridPitch);
  plan = fft_inplace_even(plan, 2, gridVec.data(), gridSize, gridPitch);
  fftw_destroy_plan(plan);

  writeImgToDisk("image.dat", reinterpret_cast<double*>(gridVec.data()));

  printf("Calculating PSF grid ...\n");
  cdv psfGridVec(fullSize);
  { cdv psfVis(vd.num_points, complexd(1.0, 0.0));
    cdpv psfpvec(vd.num_baselines);
    complexd * psfp = psfVis.data();
    for (int i = 0; i < vd.num_baselines; i++)
      psfpvec[i] = psfp; psfp += vd.num_times * vd.num_channels;
    gridKernelCPUFullGCF(
      scale
      , wstep
      , vd.num_baselines
      , blSuppvec.data()
      , psfGridVec.data()
      , const_cast<const complexd **>(gcfTable.data() + maxWPlane*over2) // Center at 0
      , const_cast<const Double3 **>(uvwpvec.data())
      , const_cast<const complexd **>(psfpvec.data())
      , ts_ch
      , gridPitch
      , gridSize
      , gcfSuppvec.data() + maxWPlane // Center at 0
      );
  }
  printf("Start normalizing PSF!\n");
  normalizeCPU(
    psfGridVec.data()
    , gridPitch
    , gridSize
    );
  printf("Save the data to be destroyed by hermitianizing them!");
  herm_padded_inplace(psfGridVec.data(), gridSize, gridPitch);
  printf("FFTing PSF ...\n");
  plan = NULL;
  plan = fft_inplace_even(plan, 2, psfGridVec.data(), gridSize, gridPitch);
  fftw_destroy_plan(plan);

  writeImgToDisk("psf.dat", reinterpret_cast<double*>(psfGridVec.data()));

  dv modVec(fullSize);
  deconvolve(
    modVec.data()
    , reinterpret_cast<double*>(gridVec.data())
    , reinterpret_cast<double*>(psfGridVec.data())
    , gridSize
    , gridPitch
    , 1000
    , 1.0
    , 0.001
    );

  writeImgToDisk("model.dat", modVec.data());
  writeImgToDisk("residual.dat", reinterpret_cast<const double*>(gridVec.data()));

  return 0;
}
