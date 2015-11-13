#include "scatter_halide.h"

int main(int argc, char * argv[])
{
  if (argc < 2) return 1;

  Target
      target_plain = get_target_from_environment()
    , target_cuda = target_plain.with_feature(Target::CUDA)
    // , target_cuda35 = target_cuda.with_feature(Target::CUDACapability35)
    ;

  SGridder
    //  gridderCPUbare   = SGridder(0,1,3,2)
      gridderCPUbare0123  = SGridder(0,1,2,3).strategy1()
    , gridderCPUbare0132  = SGridder(0,1,3,2).strategy1()
    // , gridderCPUbare0213  = SGridder(0,2,1,3).strategy1()
    // , gridderCPUbare0231  = SGridder(0,2,3,1).strategy1()
    // , gridderCPUbare0312  = SGridder(0,3,1,2).strategy1()
    // , gridderCPUbare0321  = SGridder(0,3,2,1).strategy1()
    // SLOW!! , gridderCPUbare1230  = SGridder(1,2,3,0).strategy1()
    // , gridderCPUbare2  = SGridder(0,1,3,2).strategy2()
    // , gridderCPUbare12 = SGridder(0,1,3,2).strategy1().strategy2()
    // , gridderCPUdims   = SGridder(0,1,3,2).setDims()
    , gridderCPUdims0123  = SGridder(0,1,2,3).setDims().strategy1()
    , gridderCPUdims0132  = SGridder(0,1,3,2).setDims().strategy2()
    // , gridderCPUdims12 = SGridder(0,1,3,2).setDims().strategy1().strategy2()
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
      	//  comp(gridderCPUbare  , "kern_scatter_bare"  , target_plain)
      	  comp(gridderCPUbare0123, "kern_scatter_bare0123" , target_plain)
      	, comp(gridderCPUbare0132, "kern_scatter_bare0132" , target_plain)
      	// , comp(gridderCPUbare0213, "kern_scatter_bare0213" , target_plain)
      	// , comp(gridderCPUbare0231, "kern_scatter_bare0231" , target_plain)
      	// , comp(gridderCPUbare0312, "kern_scatter_bare0312" , target_plain)
      	// , comp(gridderCPUbare0321, "kern_scatter_bare0321" , target_plain)
      	// , comp(gridderCPUbare1230, "kern_scatter_bare1230" , target_plain)
      	// , comp(gridderCPUbare2 , "kern_scatter_bare2" , target_plain)
      	// , comp(gridderCPUbare12, "kern_scatter_bare12", target_plain)
      	// , comp(gridderCPUdims  , "kern_scatter_dims"  , target_plain)
      	, comp(gridderCPUdims0123 , "kern_scatter_dims0123" , target_plain)
      	, comp(gridderCPUdims0132 , "kern_scatter_dims0132" , target_plain)
      	// , comp(gridderCPUdims12, "kern_scatter_dims12", target_plain)
        })
      );
    compile_module_to_object(mCPU, std::string(argv[1]) + ".obj");
    compile_module_to_c_header(mCPU, std::string(argv[1]) + ".h");
    compile_module_to_assembly(mCPU, std::string(argv[1]) + ".s");
    
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
