#include <algorithm>

#ifdef _MSC_VER
#pragma warning(push, 0)
#endif
#include "Halide.h"
#ifdef _MSC_VER
#pragma warning(pop)
#endif

using namespace Halide;
using namespace Halide::Internal;
using namespace std;

#include "../../cpu/gridding/cfg.h"
#include "../../cpu/gridding/utils.h"

enum VisFields {
    _U=0
  , _V
  , _W
  , _R
  , _I
  ,  _VIS_FIELDS
  };

struct SGridder {
  SGridder(
      int cpos
    , int xpos
    , int ypos
    , int vpos
    );

  Param<double> scale;
  Param<int>    grid_size;
  ImageParam
      vis
    , gcf_fused
    ;
  RVar
      rcmplx
    , rgcfx
    , rgcfy
    , rvis
    ;
  Func
      uvg
    , uv
    , inBound
    , overc
    ;
};


#define Q(a) a(#a)
#define F(a) a = Func(#a)

Var Q(cmplx), Q(x), Q(y), Q(uvdim), Q(t);

SGridder::SGridder(
    int cpos
  , int xpos
  , int ypos
  , int vpos
  ) {
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
  uvg(cmplx, x, y) = /* cast<double>(0.0f); */ undef<double>();

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
  Var Q(suppx), Q(suppy), Q(overx), Q(overy);
  gcf(suppx, suppy, t)
      = Complex(gcf_fused(_REAL, suppx, suppy, overc(_U, t) + OVER * overc(_V, t)),
                gcf_fused(_IMAG, suppx, suppy, overc(_U, t) + OVER * overc(_V, t)));

  // ** Definition

  // Reduction domain.
  typedef std::pair<Expr, Expr> rType;
  rType
      cRange = {0, _CPLX_FIELDS}
    , gRange = {0, GCF_SIZE}
    , vRange = {vis.top(), (vis.height() / 200) * 200}
    // , vRange = {0, num_baselines}
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

   vis.set_min(0,0).set_stride(0,1).set_extent(0,_VIS_FIELDS);
   vis.set_stride(1,_VIS_FIELDS);

   gcf_fused.set_min(0,0).set_stride(0,1).set_extent(0,_CPLX_FIELDS);
   gcf_fused.set_min(1,0).set_stride(1,_CPLX_FIELDS).set_extent(1,GCF_SIZE);
   gcf_fused.set_min(2,0).set_stride(2,_CPLX_FIELDS*GCF_SIZE).set_extent(2,GCF_SIZE);
   gcf_fused.set_min(3,0).set_stride(3,_CPLX_FIELDS*GCF_SIZE*GCF_SIZE).set_extent(3,OVER*OVER);

   uvg.output_buffer().set_stride(0,1).set_extent(0,_CPLX_FIELDS);
   uvg.output_buffer().set_stride(1,_CPLX_FIELDS);

   RVar Q(rvis_outer), Q(rvis_inner);

   overc.compute_at(uvg, rvis_inner);
   overc.gpu_threads(t);
   uv.compute_at(uvg, rvis_inner);
   uv.gpu_threads(t);
   inBound.compute_at(uvg, rvis_inner);
   inBound.gpu_threads(t);

   RVar rgcfxc, rall;
   uvg.update().allow_race_conditions()
      .fuse(rgcfx, rcmplx, rgcfxc)
      .fuse(rgcfy, rgcfxc, rall)
      .split(rvis, rvis_outer, rvis_inner, 200)
      .gpu_blocks(rvis_outer)
      // .gpu_threads(rgcfx)
      .gpu_threads(rall)
      // .unroll(rcmplx)
      ;
}

// Quick util to find node type info
struct TInfoAssoc {
   IRNodeType * tinfo;
   const char * tname;
 };

#define __TIA(t) {& t::_type_info, #t}

const TInfoAssoc tiassocs[] =
  { __TIA(IntImm)
  , __TIA(UIntImm)
  , __TIA(FloatImm)
  , __TIA(StringImm)
  , __TIA(Cast)
  , __TIA(Add)
  , __TIA(Sub)
  , __TIA(Mul)
  , __TIA(Div)
  , __TIA(Mod)
  , __TIA(Min)
  , __TIA(Max)
  , __TIA(EQ)
  , __TIA(NE)
  , __TIA(LT)
  , __TIA(LE)
  , __TIA(GT)
  , __TIA(GE)
  , __TIA(And)
  , __TIA(Or)
  , __TIA(Not)
  , __TIA(Select)
  , __TIA(Load)
  , __TIA(Ramp)
  , __TIA(Broadcast)
  , __TIA(Let)
  , __TIA(Call)
  , __TIA(Variable)
  };

// Useful util
#define __HAS_TYPE(e,et) ((e).ptr->type_info() == &et::_type_info)

const int tiassocs_n = sizeof(tiassocs)/sizeof(TInfoAssoc);

const char * getType(const Expr & e){
  auto pred = [=](const TInfoAssoc & tia){
    if (e.ptr->type_info() == tia.tinfo) return true;
    else return false;
  };
  auto rit = find_if(tiassocs, tiassocs+tiassocs_n, pred);
  if (rit != tiassocs+tiassocs_n)
    return rit->tname;
  else
    return "NOT_FOUND";
}

struct RewriteLoadStore2Atomic : public IRMutator {

  Expr
      addOp
    , lastLet
    , addend
    , uvgLoad
    ;

  void visit(const Store *op) {
    if (! (op->name == "uvg")) {
      stmt = op;
      return;
    }
    // Ignore Store with immediate float (this is an initial pure definition)
    else if (__HAS_TYPE(op->value, FloatImm)) {
      cout << "Store to uvg with FloatImm is ignored\n";
      stmt = op;
      return;
    }
    cout << "Store to uvg is found with " << getType(op->value) << " at " << getType(op->index) << endl;
    Expr dev_ptr = Call::make( Handle()
                             , Call::address_of
                             , {Load::make(Float(64), "uvg", op->index, Buffer(), Parameter())}
                             , Call::Intrinsic);
    // We have 'void' external function return. They usually choose UInt(8) for irrelevant results.
    findLoad(op->value);
    Expr val = mutate(op->value);
    Expr ext_call = Call::make(UInt(8), "atomicAddDouble", {dev_ptr, val}, Call::Extern);
    stmt = Evaluate::make(ext_call);
  }

  void findLoad(const Expr & op){
    // Drill down inline scheduled computations lets
    const Let * plet = op.as<Let>();
    if (plet != nullptr) {
      lastLet = op;
      findLoad(plet->body);
    }
    // Should be add with Load and addend value
    else {
      const Add * padd = op.as<Add>();
      if (padd != nullptr) {
         const Load * pload = padd->a.as<Load>();
         if(pload != nullptr && pload->name == "uvg") {
           cout << "Found final Add and Load from uvg!\n";
           cout << getType(padd->a) << " + " << getType(padd->b) << endl;
           addOp = op;
           uvgLoad = padd->a;
           addend = padd->b;
         }
      }
    }
  }

  Expr mutate(Expr e){
    if (e.same_as(lastLet)) {
      cout << "Let rewrite!\n";
      return Let::make(e.as<Let>()->name, e.as<Let>()->value, addend);
    }
    if(e.same_as(addOp)) {
      cout << "Add eliminated!\n";
      return Expr();
    }
    if (e.same_as(uvgLoad)){
      cout << "Load eliminated!\n";
      return Expr();
    }
    return IRMutator::mutate(e);
  }

};

int main(int argc, char * argv[])
{
  if (argc < 2) return 1;

  Target target_cuda(get_target_from_environment().os, Target::X86, 64,
           { Target::SSE41, Target::AVX, Target::CUDA, Target::CUDACapability35 });

  SGridder
      gridderGPU = SGridder(0,1,2,3)
    ;

  RewriteLoadStore2Atomic rewriter;
  gridderGPU.uvg.add_custom_lowering_pass(&rewriter, nullptr);

  auto comp = [](SGridder & sg, const char * fn, Target target) {
      vector<Halide::Argument> args = {
          sg.scale
        , sg.grid_size
        , sg.vis
        , sg.gcf_fused
        };
      return sg.uvg.compile_to_module(args, fn, target);
    };

  Module mGPU = comp(gridderGPU, "kern_scatter_gpu", target_cuda);

  compile_module_to_object(mGPU, argv[1]);
  return 0;
}
