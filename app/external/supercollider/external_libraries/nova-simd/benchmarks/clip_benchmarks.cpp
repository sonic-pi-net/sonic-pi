#include "benchmark_helpers.hpp"
#include "../simd_ternary_arithmetic.hpp"

#include <algorithm>

#define sc_clip(x, lo, hi) ((x) > (hi) ? (hi) : ((x) < (lo) ? (lo) : (x)))

using namespace nova;

nova::aligned_array<float, 1024> in;
nova::aligned_array<float, 1024> out;

void __noinline__ bench_1(unsigned int numSamples)
{
    for (unsigned int i = 0; i != numSamples; ++i)
        out[i] = sc_clip(in[i], -0.5f, 0.5f);
}

/* #define direct_clip(x, lo, hi) std::max (lo, std::min(hi, x)); */

/* void __noinline__ bench_2(unsigned int numSamples) */
/* { */
/*     for (unsigned int i = 0; i != numSamples; ++i) */
/*         out[i] = direct_clip(in[i], -0.5f, 0.5f); */
/* } */

#ifdef __SSE__

#include <xmmintrin.h>
inline float sse_clip(float x, float lo, float hi)
{
    __m128 xx = _mm_load_ss(&x);
    __m128 xlo = _mm_load_ss(&lo);
    __m128 xhi = _mm_load_ss(&hi);
    __m128 clipped = _mm_max_ss(xlo, _mm_min_ss(xx, xhi));
    float ret;
    _mm_store_ss(&ret, clipped);
    return ret;
}


void __noinline__ bench_3(unsigned int numSamples)
{
    for (unsigned int i = 0; __builtin_expect((i != numSamples), 1); ++i)
        out[i] = sse_clip(in[i], -0.5f, 0.5f);
}
#endif

/* void __noinline__ bench_4(unsigned int numSamples) */
/* { */
/*     clip_vec_simd<64>(out.begin(), in.begin(), -0.5f, 0.5f); */
/* } */

void __noinline__ bench_5(unsigned int numSamples)
{
    clip_vec_simd<float>(out.begin(), wrap_argument(in.begin()), wrap_argument(-0.5f), wrap_argument(0.5f), numSamples);
}


int main(void)
{
    /* touch buffers */
    out.assign(0);

    fill_container(in);

    const int iterations = 50000000;

    run_bench(boost::bind(bench_1, 64), iterations);
/*     run_bench(boost::bind(bench_2, 64), iterations); */
#ifdef __SSE__
    run_bench(boost::bind(bench_3, 64), iterations);
#endif
/*     run_bench(boost::bind(bench_4, 64), iterations); */
    run_bench(boost::bind(bench_5, 64), iterations);
}
