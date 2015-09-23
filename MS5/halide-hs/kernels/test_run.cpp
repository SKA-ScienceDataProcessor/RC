#include "kern_generate_f.h"
#include "kern_dotp.h"
#include "kern_sum.h"
#include <iostream>
#include <string.h>
#include <HalideRuntime.h>
// #include <Argument.h>

void dump_meta(const halide_filter_metadata_t& meta) {
    int n = meta.num_arguments;
    std::cout
        << "META: "
        << meta.name << '\t' << meta.target << "\n";
        for(int i = 0; i<n; i++) {
            const halide_filter_argument_t& arg = meta.arguments[i];
            std::cout << "  ARG: " << arg.name << std::endl;
            std::cout << "    kind      : " << arg.kind       << std::endl;
            std::cout << "    dim       : " << arg.dimensions << std::endl;
            std::cout << "    type code : " << arg.type_code  << std::endl;
            std::cout << "    type bits : " << arg.type_bits  << std::endl;
            std::cout << "    def       : " << (void*)arg.def << std::endl;
            std::cout << "    min       : " << (void*)arg.min << std::endl;
            std::cout << "    max       : " << (void*)arg.max << std::endl;
        }
}

int main() {
    const int off = 0;
    const int N   = 10;
    // Allocate buffers for vector
    float buf_f[N]  = {0};
    float buf_g[N]  = {0};
    float buf_pp[N] = {0};
    float buf_s[N]  = {0};
    
    // Initialize halide buffers
    buffer_t hbuf_f  = {0};
    hbuf_f.host      = reinterpret_cast<uint8_t*>(buf_f);
    hbuf_f.min[0]    = off;
    hbuf_f.stride[0] = 1;
    hbuf_f.extent[0] = N;
    hbuf_f.elem_size = sizeof(float);
    // 
    buffer_t hbuf_g  = {0};
    hbuf_g.host      = reinterpret_cast<uint8_t*>(buf_g);
    hbuf_g.min[0]    = off;
    hbuf_g.stride[0] = 1;
    hbuf_g.extent[0] = N;
    hbuf_g.elem_size = sizeof(float);
    // 
    buffer_t hbuf_pp  = {0};
    hbuf_pp.host      = reinterpret_cast<uint8_t*>(buf_pp);
    hbuf_pp.min[0]    = off;
    hbuf_pp.stride[0] = 1;
    hbuf_pp.extent[0] = N;
    hbuf_pp.elem_size = sizeof(float);
    // 
    buffer_t hbuf_s  = {0};
    hbuf_s.host      = reinterpret_cast<uint8_t*>(buf_s);
    hbuf_s.min[0]    = off;
    hbuf_s.stride[0] = 1;
    hbuf_s.extent[0] = N;
    hbuf_s.elem_size = sizeof(float);
    // 

    int err_f  = kern_generate_f(&hbuf_f);
    int err_g  = kern_generate_f(&hbuf_g);
    int err_pp = kern_dotp(&hbuf_f, &hbuf_g, &hbuf_pp);
    int err_s  = kern_sum(&hbuf_pp, off, N, &hbuf_s);


    std::cout << err_f << std::endl;
    std::cout << err_g << std::endl;
    std::cout << err_pp << std::endl;
    std::cout << err_s << std::endl;
    std::cout << "Array : \n";
    for(int i  =0 ; i<N; i++) {
        std::cout << "    "
                  << buf_f[i] << '\t'
                  << buf_g[i] << '\t'
                  << buf_pp[i] << '\t'
                  << buf_s[i] << '\t'
                  << std::endl;
    }
    // Write metadata
    dump_meta(kern_generate_f_metadata);
    dump_meta(kern_dotp_metadata);
    dump_meta(kern_sum_metadata);
    return 0;
}
