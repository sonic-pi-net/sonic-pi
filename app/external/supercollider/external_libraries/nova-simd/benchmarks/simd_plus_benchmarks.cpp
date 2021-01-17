#include "benchmark_helpers.hpp"
#include "../simd_binary_arithmetic.hpp"

#include <cmath>

using namespace nova;
using namespace std;

namespace {

aligned_array<float, 64> out, in, in2;

typedef float afloat __attribute__ ((__aligned__(16)));

afloat * __restrict__ o = out.begin();
afloat * __restrict__ i1 = in.begin();
afloat * __restrict__ i2 = in2.begin();

void __noinline__ bench_1(unsigned int n)
{
    plus_vec_simd(o, i1, i2, n);
}

void __noinline__ bench_2(unsigned int n)
{
    plus_vec_simd<64>(out.begin(), in.begin(), in2.begin());
}

// void __noinline__ bench_2a(unsigned int n)
// {
//     n /= 8;
//     float * i = in.begin();
//     float * i2 = in2.begin();
//     float * o = out.begin();
//     do
//     {
//         plus_vec_simd<8>(o, i, i2);
//         i += 8;
//         i2 += 8;
//         o += 8;
//     }
//     while (--n);
// }

void __noinline__ bench_2b(unsigned int n)
{
    n /= 16;
    float * i = in.begin();
    float * i2 = in2.begin();
    float * o = out.begin();
    do
    {
        plus_vec_simd<16>(o, i, i2);
        i += 16;
        i2 += 16;
        o += 16;
    }
    while (--n);
}

void __noinline__ bench_2c(unsigned int n)
{
    plus_vec_simd<64>(out.begin(), in.begin(), 1.f);
}

void __noinline__ bench_2d(unsigned int n)
{
    n /= 32;
    float * i = in.begin();
    float * o = out.begin();
    do
    {
        plus_vec_simd<32>(o, i, 1.f);
        i += 32;
        o += 32;
    }
    while (--n);
}

void __noinline__ bench_2e(unsigned int n)
{
    n /= 16;
    float * i = in.begin();
    float * o = out.begin();
    do
    {
        plus_vec_simd<16>(o, i, 1.f);
        i += 16;
        o += 16;
    }
    while (--n);
}


void __noinline__ bench_3(unsigned int n)
{
    for (int i = 0; i != n; ++i)
        o[i] = i1[i] + i2[i];
}

void __noinline__ bench_3a(unsigned int n)
{
    for (std::size_t i = 0; i != n; ++i)
        out[i] = in[i] + in2[i];
}


void __noinline__ bench_4(unsigned int n)
{
    for (int i = 0; i != n; i+=4)
    {
        o[i]   = i1[i]   + i2[i];
        o[i+1] = i1[i+1] + i2[i+1];
        o[i+2] = i1[i+2] + i2[i+2];
        o[i+3] = i1[i+3] + i2[i+3];
    }
}

void __noinline__ bench_4a(unsigned int n)
{
    for (std::size_t i = 0; i != n; i+=4)
    {
        out[i]   = in[i]   + in2[i];
        out[i+1] = in[i+1] + in2[i+1];
        out[i+2] = in[i+2] + in2[i+2];
        out[i+3] = in[i+3] + in2[i+3];
    }
}

void __noinline__ bench_5(unsigned int n)
{
    for (int i = 0; i != n; i+=8)
    {
        o[i]   = i1[i]   + i2[i];
        o[i+1] = i1[i+1] + i2[i+1];
        o[i+2] = i1[i+2] + i2[i+2];
        o[i+3] = i1[i+3] + i2[i+3];
        o[i+4] = i1[i+4] + i2[i+4];
        o[i+5] = i1[i+5] + i2[i+5];
        o[i+6] = i1[i+6] + i2[i+6];
        o[i+7] = i1[i+7] + i2[i+7];
    }
}

void __noinline__ bench_5a(float * out, const float * in, const float * in2, unsigned int n)
{
    for (std::size_t i = 0; i != n; i+=8)
    {
        out[i]   = in[i]   + in2[i];
        out[i+1] = in[i+1] + in2[i+1];
        out[i+2] = in[i+2] + in2[i+2];
        out[i+3] = in[i+3] + in2[i+3];
        out[i+4] = in[i+4] + in2[i+4];
        out[i+5] = in[i+5] + in2[i+5];
        out[i+6] = in[i+6] + in2[i+6];
        out[i+7] = in[i+7] + in2[i+7];
    }
}

void __noinline__ bench_5b(float * out, const float * in, const float * in2, unsigned int n)
{
    for (std::size_t i = 0; i != n; i+=4)
    {
        float i01 = in[i];
        float i02 = in2[i];

        float i11 = in[i+1];
        float i12 = in2[i+1];
        float i21 = in[i+2];
        float i22 = in2[i+2];
        float i31 = in[i+3];
        float i32 = in2[i+3];
/*         float i41 = in[i+4]; */
/*         float i42 = in2[i+4]; */
/*         float i51 = in[i+5]; */
/*         float i52 = in2[i+5]; */
/*         float i61 = in[i+6]; */
/*         float i62 = in2[i+6]; */
/*         float i71 = in[i+7]; */
/*         float i72 = in2[i+7]; */

        out[i]   = i01 + i02;
        out[i+1] = i11 + i12;
        out[i+2] = i21 + i22;
        out[i+3] = i31 + i32;
/*         out[i+4] = i41 + i42; */
/*         out[i+5] = i51 + i52; */
/*         out[i+6] = i61 + i62; */
/*         out[i+7] = i71 + i72; */
    }
}

void __noinline__ bench_5c(float * __restrict__ out, const float * __restrict__ in,
                           const float * __restrict__ in2, unsigned int n)
{
    for (std::size_t i = 0; i != n; i+=8)
    {
        out[i]   = in[i]   + in2[i];
        out[i+1] = in[i+1] + in2[i+1];
        out[i+2] = in[i+2] + in2[i+2];
        out[i+3] = in[i+3] + in2[i+3];
        out[i+4] = in[i+4] + in2[i+4];
        out[i+5] = in[i+5] + in2[i+5];
        out[i+6] = in[i+6] + in2[i+6];
        out[i+7] = in[i+7] + in2[i+7];
    }
}

}

int main(void)
{
    out.assign(0.f);
    in.assign(0.2f);
    in2.assign(0.3f);

    const unsigned int iterations = 100000000;

    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_2, 64), iterations);
/*    run_bench(boost::bind(bench_2a, 64), iterations);*/
    run_bench(boost::bind(bench_2b, 64), iterations);
    run_bench(boost::bind(bench_2c, 64), iterations);
    run_bench(boost::bind(bench_2d, 64), iterations);
    run_bench(boost::bind(bench_2e, 64), iterations);
/*     run_bench(boost::bind(bench_3, 64), iterations); */
/*     run_bench(boost::bind(bench_3a, 64), iterations); */
/*     run_bench(boost::bind(bench_4, 64), iterations); */
/*     run_bench(boost::bind(bench_4a, 64), iterations); */
/*     run_bench(boost::bind(bench_5, 64), iterations); */
/*     run_bench(boost::bind(bench_5a, out.begin(), in.begin(), in2.begin(), 64), iterations); */
/*     run_bench(boost::bind(bench_5b, out.begin(), in.begin(), in2.begin(), 64), iterations); */
/*     run_bench(boost::bind(bench_5c, out.begin(), in.begin(), in2.begin(), 64), iterations); */
}
