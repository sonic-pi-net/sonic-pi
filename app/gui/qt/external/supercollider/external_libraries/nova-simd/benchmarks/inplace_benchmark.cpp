#include "benchmark_helpers.hpp"
#include "../simd_unary_arithmetic.hpp"

const int size = 16384;

nova::aligned_array<float, size> in;
nova::aligned_array<float, size> out;

using namespace nova;

void __noinline__ bench_1(unsigned int numSamples)
{
    abs_vec_simd(out.begin(), in.begin(), numSamples);
}

void __noinline__ bench_2(unsigned int numSamples)
{
    abs_vec_simd(in.begin(), in.begin(), numSamples);
}

int main(void)
{
    /* touch buffers */
    out.assign(0);
    fill_container(in);

    const unsigned int iterations = 20000000;

    run_bench(boost::bind(bench_1, size), iterations);
    run_bench(boost::bind(bench_2, size), iterations);
}
