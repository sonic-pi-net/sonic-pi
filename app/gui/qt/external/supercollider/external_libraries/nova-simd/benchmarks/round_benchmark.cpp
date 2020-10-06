#include "benchmark_helpers.hpp"
#include "../simd_unary_arithmetic.hpp"

nova::aligned_array<float, 1024> in;
nova::aligned_array<float, 1024> out;


void __noinline__ bench_1(unsigned int numSamples)
{
    nova::round_vec(out.data(), in.data(), numSamples);
}

void __noinline__ bench_2(unsigned int numSamples)
{
    nova::round_vec_simd(out.data(), in.data(), numSamples);
}


int main(void)
{
    /* touch buffers */
    out.assign(0);
    fill_container(in);

    const unsigned int iterations = 50000000;

    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_2, 64), iterations);
}
