#include "scatter_halide.h"

struct Complex {
  Expr real, imag;
  Complex(Expr r, Expr i) : real(r), imag(i) {}
  Complex(FuncRefExpr f) : real(Tuple(f)[0]), imag(Tuple(f)[1]) {}
  Complex operator+(const Complex &other) const {
    return{ real + other.real, imag + other.imag };
  }
  Complex operator*(const Complex &other) const {
    return{
        real * other.real - imag * other.imag
      , real * other.imag + imag * other.real
      };
  }
  Expr unpack(Expr c) { return select(c == _REAL, real, imag); }
  operator Tuple() { return Tuple(real, imag); }
};

#define Q(a) a(#a)
#define F(a) a = Func(#a)

Var Q(cmplx), Q(x), Q(y), Q(uvdim), Q(t);

SGridder::SGridder(SGridderConfig cfg) {
  // ** Input
  scale = Param<double>("scale");
  grid_size = Param<int>("grid_size");
  vis = ImageParam(type_of<double>(), 2, "vis");

  // GCF: Array of OxOxSxS complex numbers. We "fuse" two dimensions
  // as Halide only supports up to 4 dimensions.
  gcf_fused = ImageParam(type_of<double>(), 4, "gcf");

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
  rVec[cfg.cpos] = cRange;
  rVec[cfg.xpos] = gRange;
  rVec[cfg.ypos] = gRange;
  rVec[cfg.vpos] = vRange;

  RDom red(rVec);
    rcmplx = red[cfg.cpos]
  , rgcfx  = red[cfg.xpos]
  , rgcfy  = red[cfg.ypos]
  , rvis   = red[cfg.vpos]
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

  if (cfg.dim & (1 << _VIS0)) vis.set_min(0,0).set_stride(0,1).set_extent(0,_VIS_FIELDS);
  if (cfg.dim & (1 << _VIS1)) vis.set_stride(1,_VIS_FIELDS);

  if (cfg.dim & (1 << _GCF0)) gcf_fused.set_min(0,0).set_stride(0,1).set_extent(0,_CPLX_FIELDS);
  if (cfg.dim & (1 << _GCF1)) gcf_fused.set_min(1,0).set_stride(1,_CPLX_FIELDS).set_extent(1,GCF_SIZE);
  if (cfg.dim & (1 << _GCF2)) gcf_fused.set_min(2,0).set_stride(2,_CPLX_FIELDS*GCF_SIZE).set_extent(2,GCF_SIZE);
  if (cfg.dim & (1 << _GCF3)) gcf_fused.set_min(3,0).set_stride(3,_CPLX_FIELDS*GCF_SIZE*GCF_SIZE).set_extent(3,OVER*OVER);

  if (cfg.dim & (1 << _UVG0)) uvg.output_buffer().set_stride(0,1).set_extent(0,_CPLX_FIELDS);
  if (cfg.dim & (1 << _UVG1)) uvg.output_buffer().set_stride(1,_CPLX_FIELDS);

  // Compute UV & oversampling coordinates per visibility
  overc.compute_at(uvg, rvis).vectorize(uvdim);
  uv.compute_at(uvg,rvis).vectorize(uvdim);
  inBound.compute_at(uvg,rvis);

  RVar rgcfxc("rgcfxc");
  switch(cfg.upd)
  {
  case _UPD_NONE: break;
  case _UPD_VECT:
      uvg.update()
          .allow_race_conditions()
          .vectorize(rcmplx);
      break;
  case _UPD_FUSE:
      uvg.update()
          .allow_race_conditions()
          .fuse(rgcfx, rcmplx, rgcfxc)
          .vectorize(rgcfxc, cfg.vector);
      break;
  case _UPD_FUSE_UNROLL:
      uvg.update()
          .allow_race_conditions()
          .fuse(rgcfx, rcmplx, rgcfxc)
          .vectorize(rgcfxc, cfg.vector)
          .unroll(rgcfxc, GCF_SIZE * 2 / cfg.vector);
      break;
  }
}
