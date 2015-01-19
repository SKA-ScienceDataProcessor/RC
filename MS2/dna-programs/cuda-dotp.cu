
/* Global variable for returning result of computation */
__device__ float g_res;


/* CUDA kernel for dot product. Optimized for brevity
 */
__global__ void dotp_kernel(double xs[], double ys[]) {
    const unsigned int idx = threadIdx.x + blockIdx.x*blockDim.x;
    double mine = xs[idx] * ys[idx];
    atomicAdd(&g_res, mine);
}

/* Execute kernel on GPU */
extern "C"
double calculate_dot_p(double xs[], double ys[], int n) {
    // Deeply magical constant
    const int block_size = 512; 
    // size padded to nearest multiple of block size
    int n_padded = ((n + block_size - 1)/block_size)*block_size;
    dim3 block(block_size);
    dim3 grid (n_padded/block_size);
    // Buffers for CUDA data
    double *buf_xs = NULL;
    double *buf_ys = NULL;
    // Allocate memory and copy to device
    cudaMalloc( &buf_xs, n*sizeof(double) );
    cudaMalloc( &buf_ys, n*sizeof(double) );
    cudaMemcpy(buf_xs, xs, n*sizeof(double), cudaMemcpyHostToDevice);
    cudaMemcpy(buf_ys, ys, n*sizeof(double), cudaMemcpyHostToDevice);
    // Execute kernel
    dotp_kernel<<<grid, block>>>(buf_xs, buf_ys);
    // Free buffers & obtain data
    cudaFree( &buf_xs );
    cudaFree( &buf_ys );
    double res;
    cudaMemcpyFromSymbol(&res, g_res, sizeof(double), 0, cudaMemcpyDeviceToHost);
    return res;
}
