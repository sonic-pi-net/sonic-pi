#include "benchmark_helpers.hpp"
#include "../simd_unary_arithmetic.hpp"

#define sc_abs(a)  ((a)>=0?(a) : -(a))

nova::aligned_array<float, 1024> in;
nova::aligned_array<float, 1024> out;


void __noinline__ bench_1(unsigned int numSamples)
{
    for (unsigned int i = 0; i != numSamples; ++i)
        out[i] = sc_abs(in[i]);
}

void __noinline__ bench_2(unsigned int numSamples)
{
    for (unsigned int i = 0; i != numSamples; ++i)
        out[i] = std::fabs(in[i]);
}

void __noinline__ bench_3(unsigned int numSamples)
{
    typedef float afloat __attribute__ ((__aligned__(16)));

    float * __restrict__ o = out.data();
    float * __restrict__ i = in.data();

    for (unsigned int n = 0; n != numSamples; n += 8)
    {
        o[n] = std::fabs(i[n]);
        o[n+1] = std::fabs(i[n+1]);
        o[n+2] = std::fabs(i[n+2]);
        o[n+3] = std::fabs(i[n+3]);
        o[n+4] = std::fabs(i[n+4]);
        o[n+5] = std::fabs(i[n+5]);
        o[n+6] = std::fabs(i[n+6]);
        o[n+7] = std::fabs(i[n+7]);
    }
}

using namespace nova;

void __noinline__ bench_4(unsigned int numSamples)
{
    abs_vec_simd<64>(out.begin(), in.begin());
}

void __noinline__ bench_5(unsigned int numSamples)
{
    abs_vec_simd(out.begin(), in.begin(), numSamples);
}


int main(void)
{
    /* touch buffers */
    out.assign(0);
    fill_container(in);

    const unsigned int iterations = 50000000;

    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_2, 64), iterations);
    run_bench(boost::bind(bench_3, 64), iterations);
    run_bench(boost::bind(bench_4, 64), iterations);
    run_bench(boost::bind(bench_5, 64), iterations);
}
