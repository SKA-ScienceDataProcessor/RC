
#include "xmmintrin.h"
#include "smmintrin.h"
#include "emmintrin.h"
#include "immintrin.h"

#include "stdio.h"

float sse_ddp(float *in1, float *in2, int cnt) {
    int i;

    const int stride = 4;
    const int prefetch_dist = 512;
    __m128 sum = _mm_setzero_ps();
    for (i = 0; i < cnt / stride; i++) {
        __m128 r;
        r = _mm_mul_ps(_mm_load_ps(in1 + i*stride),
                       _mm_load_ps(in2 + i*stride));
        sum = _mm_add_ps(sum, r);

        if (i*stride+prefetch_dist<cnt) {
            _mm_prefetch(in1 + i*stride + prefetch_dist, 0);
            _mm_prefetch(in2 + i*stride + prefetch_dist, 0);
        }
    }
    __attribute__((aligned (128))) float sum_f[4];
    _mm_store_ps(sum_f, sum);
    return sum_f[0] + sum_f[1] + sum_f[2] + sum_f[3];
}

double sse_ddp_d(double *in1, double *in2, int cnt) {
    int i;

    const int stride = 2;
    const int prefetch_dist = 256;
    __m128d sum = _mm_setzero_pd();
    for (i = 0; i < cnt / stride; i++) {
        __m128d r;
        r = _mm_mul_pd(_mm_load_pd(in1 + i*stride),
                       _mm_load_pd(in2 + i*stride));
        sum = _mm_add_pd(sum, r);

        if (i*stride+prefetch_dist<cnt) {
            _mm_prefetch(in1 + i*stride + prefetch_dist, 0);
            _mm_prefetch(in2 + i*stride + prefetch_dist, 0);
        }
    }

    __attribute__((aligned (128))) double sum_d[2];
    _mm_store_pd(sum_d, sum);
    return sum_d[0] + sum_d[1];
}

float avx_ddp(float *in1, float *in2, int cnt) {
    int i;

    const int stride = 8;
    const int prefetch_dist = 512;
    __m256 sum = _mm256_setzero_ps();
    for (i = 0; i < cnt / stride; i++) {
        __m256 r;
        r = _mm256_mul_ps(_mm256_loadu_ps(in1 + i*stride),
                          _mm256_loadu_ps(in2 + i*stride));
        sum = _mm256_add_ps(sum, r);

        if (i*stride+prefetch_dist<cnt) {
            _mm_prefetch(in1 + i*stride + prefetch_dist, 0);
            _mm_prefetch(in2 + i*stride + prefetch_dist, 0);
        }
    }

    __attribute__((aligned (256))) float sum_f[8];
    _mm256_store_ps(sum_f, sum);
    return sum_f[0] + sum_f[1] + sum_f[2] + sum_f[3] +
           sum_f[4] + sum_f[5] + sum_f[6] + sum_f[7];
}

double avx_ddp_d(double *in1, double *in2, int cnt) {
    int i;

    const int stride = 4;
    const int prefetch_dist = 256;
    __m256d sum = _mm256_setzero_pd();
    for (i = 0; i < cnt / stride; i++) {
        __m256d r;
        r = _mm256_mul_pd(_mm256_loadu_pd(in1 + i*stride),
                          _mm256_loadu_pd(in2 + i*stride));
        sum = _mm256_add_pd(sum, r);

        if (i*stride+prefetch_dist<cnt) {
            _mm_prefetch(in1 + i*stride + prefetch_dist, 0);
            _mm_prefetch(in2 + i*stride + prefetch_dist, 0);
        }
    }

    __attribute__((aligned (256))) double sum_d[8];
    _mm256_store_pd(sum_d, sum);
    return sum_d[0] + sum_d[1] + sum_d[2] + sum_d[3];
}
