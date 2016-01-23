#include "Halide.h"
using namespace Halide;

#ifdef _TEST
typedef int test_t;
#define __FMT "%d"
#else
typedef double test_t;
#define __FMT "%6.1f"
#endif

#define Q(a) a(#a)
#define F(a) a = Func(#a)
#define __INP(n,dim) n = ImageParam(type_of<test_t>(), dim, #n)

typedef Expr __int;

enum Coordinates {X, Y, val};
enum Components {RES, MOD};

struct Hogbom {
  Hogbom();
  ImageParam
      res
    , psf
    ;
  Param<test_t> gain;
  Param<int> pPeakx;
  Param<int> pPeaky;

  Func findPeak;
  Func psfBounded;
  Func peakVal;
  Func residual;
  Func model;
  Pipeline pres;
  Pipeline pmodel;
  Pipeline presmodel;

  RVar rdx;
};

Hogbom::Hogbom() :
    Q(findPeak)
  , Q(psfBounded)
  , Q(peakVal)
  , Q(residual)
  , Q(model)
  {

  __INP(res, 2);
  __INP(psf, 2);

  psfBounded = BoundaryConditions::constant_exterior(psf, Expr(0.0));

  RDom rdr(res);
  findPeak() = argmax(rdr, abs(res(rdr.x, rdr.y)), "findPeakAm");

  peakVal() = res( clamp(findPeak()[X], 0, res.extent(X)-1)
                 , clamp(findPeak()[Y], 0, res.extent(Y)-1)
                 );

  __int
      boxx = ((psf.extent(X) + 3)/4)*4
    , boxy = psf.extent(Y)
    ;
  RDom r(0, boxx, 0, boxy, "r");
  rdx = r.x;

  __int
      diffx = clamp(findPeak()[X] - pPeakx, -psf.extent(X), res.extent(X))
    , diffy = clamp(findPeak()[Y] - pPeaky, -psf.extent(Y), res.extent(Y))
    , startx = max(0, diffx)
    , starty = max(0, diffy)
    , rx = clamp(startx + r.x, 0, res.extent(X)-1)
    , ry = clamp(starty + r.y, 0, res.extent(Y)-1)
    , psfStartx = max(0, -diffx)
    , psfStarty = max(0, -diffy)
    ;

  Var Q(i), Q(j);
  residual(i,j) = undef<double>();
  residual(rx, ry) -= gain * peakVal() * psfBounded(psfStartx + r.x, psfStarty + r.y);

  model(i,j) = undef<double>();
  model(
     clamp(findPeak()[X], 0, res.extent(X)-1)
   , clamp(findPeak()[Y], 0, res.extent(Y)-1)
   ) += gain * peakVal();

  pres = Pipeline({residual, peakVal});
  pmodel = Pipeline({model, peakVal});
  presmodel = Pipeline({residual, model, peakVal});
}

void basicStrategy(Hogbom & h){
  h.findPeak.compute_root();
  h.peakVal.compute_root();
  h.residual.update().allow_race_conditions().vectorize(h.rdx, 4);
}

int main(){
  Var Q(x), Q(y);

  // Test images
  Func residual_cap("residual_cap");
  residual_cap(x, y) = cast<test_t>(300 - (x-10)*(x-10) - (y-10)*(y-10));

  Func psf_cap("psf_cap");
  psf_cap(x, y) = cast<test_t>(150 - (x-9)*(x-9) - (y-11)*(y-11));

  Image<test_t> res(20,20,"res");
  residual_cap.realize(res);

  Image<test_t> psf(20,20,"psf");
  psf_cap.realize(psf);
  //

  printf("Res:\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", res(i,j));
    printf("\n");
  }
  printf("\nPSF:\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", psf(i,j));
    printf("\n");
  }

  printf("\nCompiling functions...\n");
  Hogbom c;
  basicStrategy(c);
  c.findPeak.compile_jit();
  c.presmodel.compile_jit();

  printf("\nRun ...\n");
  c.res.set(psf);
  Realization ppkr = c.findPeak.realize();
  Image<int> px = ppkr[X];
  Image<int> py = ppkr[Y];
  printf("\nPSF peak is found at (%d,%d)\n", px(0), py(0));

  c.psf.set(psf);
  c.res.set(res);
  c.gain.set(0.01);
  c.pPeakx.set(px(0));
  c.pPeaky.set(py(0));

  Image<test_t> mod(20,20,"mod");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) mod(i,j) = 0;
  }

  Image<test_t> peakv(0);
  c.presmodel.realize({res, mod, peakv});

  printf("\nResidual ...\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", res(i,j));
    printf("\n");
  }
  printf("\nModel ...\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", mod(i,j));
    printf("\n");
  }

  printf("\nNow generate AOT code ...\n");

  Target
      target_plain = get_target_from_environment()
    , target_cuda = target_plain.with_feature(Target::CUDA)
    // , target_cuda35 = target_cuda.with_feature(Target::CUDACapability35)
    ;

  auto comp = [&](const std::string & suff, const Target & target){
      Module m = link_modules(
        "hogbom_kernels" + suff
       , std::vector<Module>({
      	   c.findPeak.compile_to_module({c.res}, "find_peak" + suff , target)
      	 , c.pres.     compile_to_module({c.res, c.psf, c.gain, c.pPeakx, c.pPeaky}, "res"      + suff , target)
      	 // We, perhaps, don't need psf for model, but I make things simpler for now ...
      	 , c.pmodel.   compile_to_module({c.res, c.psf, c.gain, c.pPeakx, c.pPeaky}, "model"    + suff , target)
      	 , c.presmodel.compile_to_module({c.res, c.psf, c.gain, c.pPeakx, c.pPeaky}, "resmodel" + suff , target)
         })
      );
      compile_module_to_object(m, "hogbom" + suff + ".obj");
      compile_module_to_c_header(m, "hogbom" + suff + ".h");
    };

  comp("_cpu", target_plain);
  comp("_gpu", target_cuda);
}
