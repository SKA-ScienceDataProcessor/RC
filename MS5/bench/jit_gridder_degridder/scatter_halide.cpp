#include "degrid_halide.h"
#include "scatter_halide.h"
#include "utils.h"
#include "cfg.h"

#define Q(a) a(#a)
#define F(a) a = Func(#a)
#define __INP(n,dim) n = ImageParam(type_of<double>(), dim, #n)

static Var Q(cmplx), Q(x), Q(y), Q(uvdim), Q(t);

SGridder::SGridder(int cpos, int xpos, int ypos, int vpos) {
  // ** Input
  scale = Param<double>("scale");
  __INP(vis, 2);

  // GCF: Array of OxOxSxS complex numbers. We "fuse" two dimensions
  // as Halide only supports up to 4 dimensions.
  __INP(gcf_fused, 4);

  // ** Output

  // Grid starts out undefined so we can update the output buffer
  F(uvg);
  uvg(cmplx, x, y) = undef<double>();

  // Get grid limits. This limits the uv pixel coordinates we accept
  // for the top-left corner of the GCF.
  Expr min_u = uvg.output_buffer().min(1);
  Expr max_u = uvg.output_buffer().min(1) + uvg.output_buffer().extent(1) - GCF_SIZE - 1;
  Expr min_v = uvg.output_buffer().min(2);
  Expr max_v = uvg.output_buffer().min(2) + uvg.output_buffer().extent(2) - GCF_SIZE - 1;

  // FIXME: assert grid is square
  Expr grid_size = uvg.output_buffer().extent(1);
  // ** Helpers

  // Coordinate preprocessing
  Func Q(uvs);
  F(uv), F(overc);
  uvs(uvdim, t) = vis(uvdim, t) * scale;
  overc(uvdim, t) = clamp(cast<int>(round(OVER * (uvs(uvdim, t) - floor(uvs(uvdim, t))))), 0, OVER-1);
  uv(uvdim, t) = cast<int>(round(uvs(uvdim, t)) + grid_size / 2 - GCF_SIZE / 2);

  // Visibilities to ignore due to being out of bounds
  F(inBound);
  inBound(t) = uv(_U, t) >= min_u && uv(_U, t) <= max_u &&
               uv(_V, t) >= min_v && uv(_V, t) <= max_v;

  // GCF lookup for a given visibility
  Func Q(gcf);
  Var suppx("suppx"), suppy("suppy"), overx("overx"), overy("overy");
  gcf(suppx, suppy, t)
      = Complex(gcf_fused(_REAL, suppx, suppy, overc(_U, t) + OVER * overc(_V, t)),
                gcf_fused(_IMAG, suppx, suppy, overc(_U, t) + OVER * overc(_V, t)));

  // ** Definition

  // Reduction domain. Note that we iterate over time steps before
  // switching the GCF row in order to increase locality (Romein).
  typedef std::pair<Expr, Expr> rType;
  rType
      cRange = {0, _CPLX_FIELDS}
    , gRange = {0, GCF_SIZE}
    , vRange = {vis.top(), vis.height()}
    ;

  std::vector<rType> rVec(4);
  rVec[cpos] = cRange;
  rVec[xpos] = gRange;
  rVec[ypos] = gRange;
  rVec[vpos] = vRange;

  RDom red(rVec);
    rcmplx = red[cpos]
  , rgcfx  = red[xpos]
  , rgcfy  = red[ypos]
  , rvis   = red[vpos]
  ;

  // Get visibility as complex number
  Complex visC(vis(_R, rvis), vis(_I, rvis));

  // Update grid
  uvg(rcmplx,
      rgcfx + clamp(uv(_U, rvis), min_u, max_u),
      rgcfy + clamp(uv(_V, rvis), min_v, max_v))
    += select(inBound(rvis),
              (visC * Complex(gcf(rgcfx, rgcfy, rvis))).unpack(rcmplx),
              undef<double>());

  overc.compute_at(uvg, rvis).vectorize(uvdim);
  uv.compute_at(uvg,rvis).vectorize(uvdim);
  inBound.compute_at(uvg,rvis);

  RVar rgcfxc;
  uvg.update()
    .allow_race_conditions()
    .fuse(rgcfx, rcmplx, rgcfxc)
    .vectorize(rgcfxc, 2)
    .unroll(rgcfxc, GCF_SIZE/2);
}
