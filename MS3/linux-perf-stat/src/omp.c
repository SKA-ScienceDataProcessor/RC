
#include <omp.h>

static long num_steps = 1000000;
double step;

double omp_pi()
{
    int i;
    double sum = 0.0;
    step = 1.0 / (double)num_steps;
    omp_set_num_threads(8);
    #pragma omp parallel
    {
        double x;
#pragma omp for reduction(+:sum)
        for (i = 0; i < num_steps; i++) {
            x = ((double)i+0.5) * step;
            sum = sum + 4.0 / (1.0 + x * x);
        }
    }
    return step * sum;
}
