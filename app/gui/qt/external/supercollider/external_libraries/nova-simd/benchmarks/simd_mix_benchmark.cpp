#include "benchmark_helpers.hpp"

#include "../simd_mix.hpp"

void __noinline__ bench_1(float * out0, const float * in0, float factor0,
                          const float * in1, float factor1, unsigned int n)
{
    nova::mix_vec(out0, in0, factor0, in1, factor1, n);
}

void __noinline__ bench_2(float * out0, const float * in0, float factor0,
                          const float * in1, float factor1, unsigned int n)
{
    nova::mix_vec_simd(out0, in0, factor0, in1, factor1, n);
}

void __noinline__ bench_3(float * out0, const float * in0, float factor0, float slope0,
                          const float * in1, float factor1, float slope1, unsigned int n)
{
    nova::mix_vec(out0, in0, factor0, slope0, in1, factor1, slope1, n);
}

void __noinline__ bench_4(float * out0, const float * in0, float factor0, float slope0,
                          const float * in1, float factor1, float slope1, unsigned int n)
{
    nova::mix_vec_simd(out0, in0, factor0, slope0, in1, factor1, slope1, n);
}


int main(void)
{
    using namespace std;
    nova::aligned_array<float, 64> __attribute__((aligned(64))) out;
    nova::aligned_array<float, 64> __attribute__((aligned(64))) in0;
    nova::aligned_array<float, 64> __attribute__((aligned(64))) in1;

    fill_container(out);
    fill_container(in0);
    fill_container(in1);

    const int iterations = 50000000;

    run_bench(boost::bind(bench_1, out.begin(), in0.begin(), 0.1f, in1.begin(), 0.9f, 64), iterations);
    run_bench(boost::bind(bench_2, out.begin(), in0.begin(), 0.1f, in1.begin(), 0.9f, 64), iterations);

    run_bench(boost::bind(bench_3, out.begin(), in0.begin(), 0.1f, 0.01f, in1.begin(), 0.9f, 0.01f, 64), iterations);
    run_bench(boost::bind(bench_4, out.begin(), in0.begin(), 0.1f, 0.01f, in1.begin(), 0.9f, 0.01f, 64), iterations);
}
