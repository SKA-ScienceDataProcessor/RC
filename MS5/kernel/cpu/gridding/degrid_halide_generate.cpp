#include "degrid_halide.h"

int main(int argc, char * argv[])
{
  if (argc < 2) return 1;

  Target
      target_plain = get_target_from_environment()
    , target_cuda = target_plain.with_feature(Target::CUDA)
    // , target_cuda35 = target_cuda.with_feature(Target::CUDACapability35)
    ;

  SDeGridder
      deGridder = SDeGridder(0,1,2)
    ;

  auto comp = [](SDeGridder & sdg, const char * fn, Target target) {
      std::vector<Halide::Argument> args = {
      	  sdg.scale
      	, sdg.uvw
      	, sdg.uvg
      	, sdg.gcf_fused
      	};
  	  return sdg.vis.compile_to_module(args, fn, target);
  	};

    Module mCPU = link_modules(
        "cpu_kernels"
      ,	std::vector<Module>({
      	  comp(deGridder, "deGridder" , target_plain)
        })
      );
    compile_module_to_object(mCPU, std::string(argv[1]) + ".obj");
    compile_module_to_c_header(mCPU, std::string(argv[1]) + ".h");
    compile_module_to_assembly(mCPU, std::string(argv[1]) + ".s");
    
    Module mGPU = link_modules(
        "gpu_kernels"
      ,	std::vector<Module>({
      	  comp(deGridder, "deGridder" , target_cuda)
        })
      );
    compile_module_to_object(mGPU, std::string(argv[1]) + "_gpu.obj");
}
