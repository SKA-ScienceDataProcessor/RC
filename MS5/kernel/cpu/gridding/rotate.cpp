
#include "Halide.h"
#include "utils.h"
using namespace Halide;

int main(int argc, char **argv) {
  if (argc < 2) return 1;

  // ** Input

  Param<double> in_lon("in_lon"), in_lat("in_lat");
  Param<double> out_lon0("out_lon0"), out_lat0("out_lat0");
  Param<double> out_lon_incr("out_lon_incr"), out_lat_incr("out_lat_incr");
  Param<int> uvproj("uvproj");

  // Visibilities: Array of 5-pairs, packed together with UVW
  enum VisFields { _U=0, _V, _W, _R, _I,  _VIS_FIELDS };
  ImageParam vis(type_of<double>(), 2, "vis");
  vis.set_min(0,0).set_stride(0,1).set_extent(0,_VIS_FIELDS)
     .set_stride(1,_VIS_FIELDS);

  std::vector<Halide::Argument> args = {
      in_lon, in_lat,
      out_lon0, out_lat0,
      out_lon_incr, out_lat_incr,
      uvproj, vis
  };

  // ** Output

  // Create bogus definition so we can access output extents, then
  // calculate out longitude/latitude from it
  Var uvdim("uvdim"); Var t("t");
  Func rot("rot");
  rot(uvdim, t) = undef<double>();
  Expr lmin = rot.output_buffer().min(2);
  Expr mmin = rot.output_buffer().min(3);
  Expr out_lon = out_lon0 + lmin * out_lon_incr;
  Expr out_lat = out_lat0 + mmin * out_lat_incr;
  // Calculate reprojection matrixes
  Matrix rotMtx = xyz2uvw(out_lon, out_lat) * uvw2xyz(in_lon, in_lat);
  Matrix projMtx = rotMtx.inverse2x2().transpose();
  Matrix invMtx = rotMtx.transpose(); // Rotation matrix: transpose=inverse

  // UVW vector for a given visibility
  Func uvw("uvw"), newVector("newVector"), mtx("mtx");
  Expr d0 = cast<double>(0), d1 = cast<double>(1);
  uvw(t) = Vector3(vis(_U,t), vis(_V,t), vis(_W,t));
  mtx() = selectMtx(uvproj != 0,
                    invMtx * (projMtx * rotMtx),
                    Matrix(d1,d0,d0, d0,d1,d0, d0,d0,d1));
  newVector(t) = Matrix(mtx()) * Vector3(uvw(t));

  // Determine path difference, rotate visibility
  Vector3 wvec = { cast<double>(0), cast<double>(0), cast<double>(1) };
  Func posChange("posChange"), newVis("newVis");
  posChange() = wvec - rotMtx * wvec;
  Expr pathDiff = Vector3(posChange()) * uvw(t);
  Complex visRot = polar(cast<double>(1), 2*pi()*pathDiff);
  newVis(t) = visRot * Complex(vis(_R,t), vis(_I,t));

  // Generate output
  rot(uvdim, t) =
    select(uvdim < _R,
           Vector3(newVector(t)).unpack(uvdim),
           Complex(newVis(t)).unpack(uvdim-_R));

  // ** Strategy

  // Pre-compute constants. We do not use compute_root because we want
  // to specialise mtx, posChange and rot together. This way LLVM
  // hopefully realises that with mtx() the identity matrix the UVW do
  // not change at all.
  mtx.specialize(uvproj != 0);
  mtx.compute_at(rot, Var::outermost());
  posChange.compute_at(rot, Var::outermost());

  rot.unroll(uvdim).unroll(t,2).specialize(uvproj != 0);
  rot.output_buffer()
     .set_min(0,0).set_stride(0,1).set_extent(0,_VIS_FIELDS)
     .set_stride(1,_VIS_FIELDS);

  Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
  Module mod = rot.compile_to_module(args, "kern_rotate", target);
  compile_module_to_object(mod, argv[1]);
  return 0;
}
