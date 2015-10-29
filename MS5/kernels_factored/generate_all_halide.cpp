#include "kernels_halide.h"

int main(int argc, char * argv[])
{
  if (argc < 2) return 1;

  Target
      target_plain = get_target_from_environment()
    , target_cuda = target_plain.with_feature(Target::CUDA)
    , target_cuda35 = target_cuda.with_feature(Target::CUDACapability35)
    ;

  FullFunc
      init = genInit()
    // Recreate all funcs because we have no deep copy for them
    //  (is this correct?)
    , fftCPU  = genFFTO0()
    , fftGPU = genFFTO0()
    , gridderCPU  = genScatter(false)
    , gridderGPU = genScatter(true) // reorder delivers slightly better GPU numbers
    ;

    #define VAR(f,n) f.second.args()[n]
    // FFT for CPU
    Var
        u = VAR(fftCPU, 0)
      , v = VAR(fftCPU, 1)
      , ui, uo, vi, vo
      ;
    fftCPU.second
      .split(v, vo, vi, HEIGHT/2)
      .unroll(vo)
      .split(u, uo, ui, WIDTH/2)
      .unroll(uo)
      .vectorize(ui,4)
      ;

    /* CUDA goes out of memory with this
    Var ug, vg;
    ug = VAR(fftGPU, 0);
    vg = VAR(fftGPU, 1);
    fftGPU.second
      .gpu_tile(ug, vg, 8, 8)
      ;
     */

    RDom rd = gridderCPU.second.reduction_domain();
    RVar
        rcmplx = rd.x
      , rgcfx  = rd.y
      , rgcfxc
      ;
    gridderCPU.second.update()
      .allow_race_conditions()
      .fuse(rgcfx, rcmplx, rgcfxc)
      .vectorize(rgcfxc, 8)
      .unroll(rgcfxc, GCF_SIZE * 2 / 8)
      ;

    /* Doesn't work
    RDom rdg = gridderGPU.second.reduction_domain();
    RVar
        rcmplxg = rdg.x
      , rgcfxg = rdg.y
      // reordered!
      , rgcfyg = rdg.z
      // , rvisg  = rdg.w
      , fused
      ;
    gridderGPU.second.update()
      .fuse(rcmplxg, rgcfxg, fused)
      .gpu_tile(rgcfxg, rgcfyg, 16, 16)
      ;
     */

    auto comp = [](FullFunc ff, const char * fn, Target t) { return ff.second.compile_to_module(ff.first, fn, t); };

    Module mCPU = link_modules(
        "cpu_kernels"
      ,	std::vector<Module>({
      	  comp(init, "kern_init", target_plain)
        , comp(fftCPU, "kern_fft", target_plain)
      	, comp(gridderCPU, "kern_scatter", target_plain)
        })
      );
    compile_module_to_object(mCPU, std::string(argv[1]) + ".obj");
    
    Module mGPU = link_modules(
        "gpu_kernels"
      ,	std::vector<Module>({
      	  comp(init, "kern_init", target_cuda)
        , comp(fftGPU, "kern_fft", target_cuda)
      	, comp(gridderGPU, "kern_scatter", target_cuda)
        })
      );
    compile_module_to_object(mGPU, std::string(argv[1]) + "_gpu.obj");
}
