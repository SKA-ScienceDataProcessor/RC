#include "scatter_halide.h"

int main(int argc, char * argv[])
{
  if (argc < 2) return 1;

  Target
      target_plain = get_target_from_environment()
    // , target_cuda = target_plain.with_feature(Target::CUDA)
    // , target_cuda35 = target_cuda.with_feature(Target::CUDACapability35)
    ;

  SGridder
      gridderCPUbare   = SGridder(0,1,3,2)
    , gridderCPUbare1  = SGridder(0,1,3,2).strategy1()
    , gridderCPUbare2  = SGridder(0,1,3,2).strategy2()
    , gridderCPUbare12 = SGridder(0,1,3,2).strategy1().strategy2()

    , gridderCPUdims   = SGridder(0,1,3,2).setDims()
    , gridderCPUdims1  = SGridder(0,1,3,2).setDims().strategy1()
    , gridderCPUdims2  = SGridder(0,1,3,2).setDims().strategy2()
    , gridderCPUdims12 = SGridder(0,1,3,2).setDims().strategy1().strategy2()

    // , gridderGPU = genScatter(0,1,2,3);
    ;

  auto comp = [](SGridder & sg, const char * fn, Target target) {
      std::vector<Halide::Argument> args = {
      	  sg.scale
      	, sg.grid_size
      	, sg.vis
      	, sg.gcf_fused
      	};
  	  return sg.uvg.compile_to_module(args, fn, target);
  	};

    Module mCPU = link_modules(
        "cpu_kernels"
      ,	std::vector<Module>({
      	  comp(gridderCPUbare  , "kern_scatter_bare"  , target_plain)
      	, comp(gridderCPUbare1 , "kern_scatter_bare1" , target_plain)
      	, comp(gridderCPUbare2 , "kern_scatter_bare2" , target_plain)
      	, comp(gridderCPUbare12, "kern_scatter_bare12", target_plain)
      	, comp(gridderCPUdims  , "kern_scatter_dims"  , target_plain)
      	, comp(gridderCPUdims1 , "kern_scatter_dims1" , target_plain)
      	, comp(gridderCPUdims2 , "kern_scatter_dims2" , target_plain)
      	, comp(gridderCPUdims12, "kern_scatter_dims12", target_plain)
        })
      );
    compile_module_to_object(mCPU, std::string(argv[1]) + ".obj");
    compile_module_to_c_header(mCPU, std::string(argv[1]) + ".h");
    
    /*
    Module mGPU = link_modules(
        "gpu_kernels"
      ,	std::vector<Module>({
      	  comp(gridderGPUf, "kern_scatter", target_cuda)
        })
      );
    compile_module_to_object(mGPU, std::string(argv[1]) + "_gpu.obj");
     */
}
