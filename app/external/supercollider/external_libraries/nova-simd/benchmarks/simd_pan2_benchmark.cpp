#include "benchmark_helpers.hpp"

#include "../simd_pan.hpp"

void __noinline__ bench_1(float * out0, float * out1, const float * in,
                          float factor0, float factor1, unsigned int n)
{
    nova::pan2_vec(out0, out1, in, factor0, factor1, n);
}

void __noinline__ bench_2(float * out0, float * out1, const float * in,
                          float factor0, float factor1, unsigned int n)
{
    nova::pan2_vec_simd(out0, out1, in, factor0, factor1, n);
}

void __noinline__ bench_3(float * out0, float * out1, const float * in,
                          float factor0, float slope0, float factor1, float slope1, unsigned int n)
{
    nova::pan2_vec(out0, out1, in, factor0, slope0, factor1, slope1, n);
}

void __noinline__ bench_4(float * out0, float * out1, const float * in,
                          float factor0, float slope0, float factor1, float slope1, unsigned int n)
{
    nova::pan2_vec_simd(out0, out1, in, factor0, slope0, factor1, slope1, n);
}

int main(void)
{
    using namespace std;
    nova::aligned_array<float, 66> __attribute__((aligned(64))) in;
    nova::aligned_array<float, 66> __attribute__((aligned(64))) out0;
    nova::aligned_array<float, 66> __attribute__((aligned(64))) out1;

    fill_container(in);
    fill_container(out0);
    fill_container(out1);

    const int iterations = 50000000;

    run_bench(boost::bind(bench_1, out0.begin(), out1.begin(), in.begin(), 0.4f, 0.6f, 64), iterations);
    run_bench(boost::bind(bench_2, out0.begin(), out1.begin(), in.begin(), 0.4f, 0.6f, 64), iterations);

    run_bench(boost::bind(bench_3, out0.begin(), out1.begin(), in.begin(), 0.4f, 0.6f, 0.01f, 0.01f, 64), iterations);
    run_bench(boost::bind(bench_4, out0.begin(), out1.begin(), in.begin(), 0.4f, 0.6f, 0.01f, 0.01f, 64), iterations);
}
