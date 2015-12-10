#include "degrid_halide.h"
#include "utils.h"
#include "cfg.h"

#define Q(a) a(#a)
#define F(a) a = Func(#a)
#define __INP(n,dim) n = ImageParam(type_of<double>(), dim, #n)

Var Q(cmplx), Q(x), Q(y), Q(uvdim), Q(tdim);

SDeGridder::SDeGridder(
    int cpos
  , int xpos
  , int ypos
  ) {
  // ** Input
  scale = Param<double>("scale");
  // FIXME: it clashes with uvg.min/extent usage below
  grid_size = Param<int>("grid_size");
  __INP(uvw, 2);
  __INP(uvg, 3);

  // GCF: Array of OxOxSxS complex numbers. We "fuse" two dimensions
  // as Halide only supports up to 4 dimensions.
  __INP(gcf_fused, 4);

  // ** Output

  // Output visibilites are zeroed from the outside
  F(vis);
  vis(cmplx, tdim) = undef<double>();

  // Get grid limits. This limits the uv pixel coordinates we accept
  // for the top-left corner of the GCF.
  Expr min_u = uvg.min(1);
  Expr max_u = uvg.min(1) + uvg.extent(1) - GCF_SIZE - 1;
  Expr min_v = uvg.min(2);
  Expr max_v = uvg.min(2) + uvg.extent(2) - GCF_SIZE - 1;

  // ** Helpers

  // Coordinate preprocessing
  Func Q(uvs);
  F(uv), F(overc);
  uvs(uvdim, tdim) = uvw(uvdim, tdim) * scale;
  overc(uvdim, tdim) = clamp(cast<int>(round(OVER * (uvs(uvdim, tdim) - floor(uvs(uvdim, tdim))))), 0, OVER-1);
  uv(uvdim, tdim) = cast<int>(round(uvs(uvdim, tdim)) + grid_size / 2 - GCF_SIZE / 2);

  // Visibilities to ignore due to being out of bounds
  F(inBound);
  inBound(tdim) =
    uv(_U, tdim) >= min_u && uv(_U, tdim) <= max_u &&
    uv(_V, tdim) >= min_v && uv(_V, tdim) <= max_v;

  // GCF lookup for a given visibility
  Func Q(gcf);
  Var Q(suppx), Q(suppy), Q(overx), Q(overy);
  gcf(suppx, suppy, tdim)
      = Complex(gcf_fused(_REAL, suppx, suppy, overc(_U, tdim) + OVER * overc(_V, tdim)),
                gcf_fused(_IMAG, suppx, suppy, overc(_U, tdim) + OVER * overc(_V, tdim)));

  // Reduction domain.
  typedef std::pair<Expr, Expr> rType;
  rType
      cRange = {0, _CPLX_FIELDS}
    , gRange = {0, GCF_SIZE}
    ;

  std::vector<rType> rVec(3);
  rVec[cpos] = cRange;
  rVec[xpos] = gRange;
  rVec[ypos] = gRange;

  RDom red(rVec);
    rcmplx = red[cpos]
  , rgcfx  = red[xpos]
  , rgcfy  = red[ypos]
  ;

  // Compute the result
  vis(rcmplx, tdim) =
  uvg(rcmplx,
      rgcfx + clamp(uv(_U, tdim), min_u, max_u),
      rgcfy + clamp(uv(_V, tdim), min_v, max_v))
    * select(inBound(tdim),
              (Complex(gcf(rgcfx, rgcfy, tdim))).unpack(rcmplx),
              undef<double>());

  // Compute UV & oversampling coordinates per visibility
  overc.compute_at(vis, tdim);
  uv.compute_at(vis, tdim);
  inBound.compute_at(vis, tdim);
}
