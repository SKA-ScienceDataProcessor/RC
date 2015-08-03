/* binner.cpp
  Copyright (C) 2015 Braam Research, LLC.
 */

#define __BINNER
#define __DYN_GRID_SIZE
#include "binner.h"
#include "metrix.h"

#include <cassert>
#include <cuda.h>
#include <cuda_runtime_api.h>

using namespace std;

typedef unsigned int uint;

struct binTable {
  int dataSizeInBytes; // device allocator must know this
  int dataOff;
  int visOffs[DIVIDERS][DIVIDERS];
  int preOffs[DIVIDERS][DIVIDERS];
  int nOfItems[DIVIDERS][DIVIDERS];
};

// Not sure if I need this alignment but still ...
const int dataoff = (sizeof(binTable)+31)/32*32;

template <int divs>
struct streamset {

  void * flush() {
    // CUDA host alloc/free is very expensive, thus we made a single allocation.
    // hence count elements first.
    size_t presiz = 0;
    for(int i = 0; i < divs; i++)
      for(int j = 0; j < divs; j++)
        presiz += pre_streams[i][j].tellp();
    size_t
        num_of_pts = presiz / sizeof(Pregridded)
      , vissiz = num_of_pts * sizeof(complexd)
      , totsiz = dataoff + vissiz + presiz
      ;

    void * datap0 = NULL;
    int res = cudaHostAlloc(&datap0, totsiz, 0);
    if (res != CUDA_SUCCESS) return NULL;

    binTable *tp = reinterpret_cast<binTable*>(datap0);
    tp->dataSizeInBytes = totsiz;
    tp->dataOff = dataoff;

    int
        visoff = 0
      , preoff = vissiz
      ;
    char * datap = reinterpret_cast<char*>(datap0) + dataoff;
    for(int i = 0; i < divs; i++) {
      for(int j = 0; j < divs; j++) {
        int plen, n, vlen;
        plen = pre_streams[i][j].tellp();
        if (plen > 0) {
           tp->preOffs[i][j] = preoff;
           pre_streams[i][j].read(datap+preoff, plen);
           preoff += plen;
           //
           n = plen / sizeof(Pregridded);
           tp->nOfItems[i][j] = n;
           //
           vlen = n * sizeof(complexd);
           tp->visOffs[i][j] = visoff;
           vis_streams[i][j].read(datap+visoff, vlen);
           visoff += vlen;
        } else {
           tp->nOfItems[i][j] = 0;
        }
      }
    }
    return datap;
  }

  void put_point(uint x, uint y, const Pregridded & p, const complexd & vis){
    pre_streams[x][y].write((char*)&p, sizeof(p));
    vis_streams[x][y].write((char*)&vis, sizeof(vis));
  }

private:
  stringstream pre_streams[divs][divs];
  stringstream vis_streams[divs][divs];
};

template <
    int over
  , int divs
  , bool do_mirror
  >
// We perform no baselines sorting yet.
// But it is not so hard to implement it.
// We assume we have the data for a single channel laid continuously in memory!
inline void * doit(int num_points, double scale, double wstep, complexd* amps, Double3 * uvws, int gcfsupps[], int grid_size) {
  const int div_size = grid_size / divs;

  streamset<divs> streams;

  complexd
      * amps_end = amps + num_points
    , * amp_curr = amps
    ;
  Double3 * uvw_curr = uvws;

  while(amp_curr < amps_end){
    Pregridded p;
    pregridPoint<over, do_mirror>(scale, wstep, *uvw_curr, p); // p is passed as reference and updated!
    div_t us, vs;
    us = div(p.u, div_size);
    vs = div(p.v, div_size);

    // NOTE: We use the pregridder with no translation, see __BINNER macro defined
    // in the begginning of this file.
    // Thus make the translation here.
    int gcf_layer_supp;
    gcf_layer_supp = gcfsupps[p.w_plane];

    // Then finally make the translation
    // and replace w_plane with gcf_layer_supp
    p.u += short(grid_size / 2 - gcf_layer_supp / 2);
    p.v += short(grid_size / 2 - gcf_layer_supp / 2);
    p.gcf_layer_supp = gcf_layer_supp;

    assert(us.quot >= 0 && vs.quot >= 0
        && us.quot < divs && vs.quot < divs
        );

    complexd amp_rot;
    amp_rot = rotw(*amp_curr, uvw_curr->w * scale);
    streams.put_point(us.quot, vs.quot, p, amp_rot);

    int margin;
    margin = gcf_layer_supp / 2;
    // Optimize slightly for most inhabited w-plane
    if (margin > 0) {
      bool leftm, rightm, topm, botm;
      leftm = false; rightm = false; topm = false; botm = false;

      if (us.rem < margin && us.quot >= 1) {
        streams.put_point(us.quot-1, vs.quot, p, amp_rot);
        leftm = true;
      }
      else if (us.rem > div_size - margin && us.quot < divs-1) {
        streams.put_point(us.quot+1, vs.quot, p, amp_rot);
        rightm = true;
      }

      if (vs.rem < margin && vs.quot >= 1) {
        streams.put_point(us.quot, vs.quot-1, p, amp_rot);
        botm = true;
      }
      else if (vs.rem > div_size - margin && vs.quot < divs-1) {
        streams.put_point(us.quot, vs.quot+1, p, amp_rot);
        topm = true;
      }

      if (leftm && botm) {
        streams.put_point(us.quot-1, vs.quot-1, p, amp_rot);
      }
      else if (leftm && topm) {
        streams.put_point(us.quot-1, vs.quot+1, p, amp_rot);
      }
      else if (rightm && topm) {
        streams.put_point(us.quot+1, vs.quot+1, p, amp_rot);
      }
      else if (rightm && botm) {
        streams.put_point(us.quot+1, vs.quot-1, p, amp_rot);
      }
    }

    amp_curr++;
    uvw_curr++;
  }
  return streams.flush();
}

void * bin(int num_points, double scale, double wstep, complexd* amps, Double3 * uvws, int gcfsupps[], int grid_size){
  return doit<OVER, DIVIDERS, false>(num_points, scale, wstep, amps, uvws, gcfsupps, grid_size);
}

void * binm(int num_points, double scale, double wstep, complexd* amps, Double3 * uvws, int gcfsupps[], int grid_size){
  return doit<OVER, DIVIDERS, true>(num_points, scale, wstep, amps, uvws, gcfsupps, grid_size);
}
