#include <algorithm>

#include "scatter_halide.h"

using namespace Halide;
using namespace Halide::Internal;
using namespace std;

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

  Target
      target_plain = get_target_from_environment()
    , target_cuda = target_plain.with_feature(Target::CUDA)// .with_feature(Target::Debug)
    , target_cuda35 = target_cuda.with_feature(Target::CUDACapability35)
    ;

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

  compile_module_to_c_header(mGPU, string(argv[1]) + "_gpu.h");
  compile_module_to_c_source(mGPU, string(argv[1]) + "_gpu.c");
  compile_module_to_llvm_assembly(mGPU, string(argv[1]) + "_gpu.ll");
  compile_module_to_html(mGPU, string(argv[1]) + "_gpu.html");
  compile_module_to_object(mGPU, string(argv[1]) + "_gpu.obj");

}
