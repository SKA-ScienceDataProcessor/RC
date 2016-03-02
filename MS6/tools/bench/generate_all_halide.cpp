#include "scatter_halide.h"

int main(int argc, char * argv[])
{
  if (argc < 2) return 1;

  Target
      target_plain = get_target_from_environment()
    , target_cuda = target_plain.with_feature(Target::CUDA) // .with_feature(Target::Debug)
    // , target_cuda35 = target_cuda.with_feature(Target::CUDACapability35)
    ;

  SGridder
      gridderGPU = SGridder(0,1,2,3) // .gpu()
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

    Module mGPU = link_modules(
        "gpu_kernels"
      ,	std::vector<Module>({
      	  comp(gridderGPU, "kern_scatter_gpu", target_cuda)
        })
      );
    compile_module_to_c_header(mGPU, std::string(argv[1]) + "_gpu.h");
    compile_module_to_object(mGPU, std::string(argv[1]) + "_gpu.o");
}
