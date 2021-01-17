#include "benchmark_helpers.hpp"
#include <cmath>

#ifdef __SSE__
#include <xmmintrin.h>
#endif

using namespace std;

nova::aligned_array<float, 64> out, in, in2;

typedef float afloat __attribute__ ((__aligned__(16)));

void __noinline__ bench_1(float * out, float * in1, float in2, float slope, unsigned int n)
{
    for (unsigned int i = 0; i != n; ++i)
    {
        out[i] = in1[i] + in2;
        in2 += slope;
    }
}

void __noinline__ bench_2(float * out, float * in1, float in2, float slope, unsigned int n)
{
    for (unsigned int i = 0; i != n; i += 4)
    {
        out[i] = in1[i] + in2; in2 += slope;
        out[i+1] = in1[i+1] + in2; in2 += slope;
        out[i+2] = in1[i+2] + in2; in2 += slope;
        out[i+3] = in1[i+3] + in2; in2 += slope;
    }
}

#ifdef __SSE__
void __noinline__ bench_3(float * out, float * in1, float in2, float slope, unsigned int n)
{
    __m128 arg2 = _mm_set_ps(in2, in2+slope, in2+slope+slope, in2+slope+slope+slope);
    const __m128 vslope = _mm_set_ps1(slope+slope+slope+slope);

    std::size_t loops = n / 4;

    do {
        __m128 arg1 = _mm_load_ps(in1);
        __m128 result = _mm_add_ps(arg1, arg2);
        arg2 = _mm_add_ps(arg2, vslope);
        _mm_store_ps(out, result);
        in1+=4;
        out+=4;
    } while (--loops);
}

void __noinline__ bench_3a(float * out, float * in1, float in2, float slope, unsigned int n)
{
    __m128 arg2 = _mm_set_ps(in2, in2+slope, in2+ 2*slope, in2+3*slope);
    const __m128 vslope = _mm_set_ps1(4 * slope);

    std::size_t loops = n / 4;

    do {
        __m128 arg1 = _mm_load_ps(in1);
        __m128 result = _mm_add_ps(arg1, arg2);
        arg2 = _mm_add_ps(arg2, vslope);
        _mm_store_ps(out, result);
        in1+=4;
        out+=4;
    } while (--loops);
}
#endif

void __noinline__ bench_4(float * out, float * in1, float in2, float slope, unsigned int n)
{
    for (unsigned int i = 0; i != n; ++i)
    {
        out[i] = in1[i] * in2;
        in2 += slope;
    }
}

void __noinline__ bench_5(float * out, float * in1, float in2, float slope, unsigned int n)
{
    for (unsigned int i = 0; i != n; i += 4)
    {
        out[i] = in1[i] * in2; in2 += slope;
        out[i+1] = in1[i+1] * in2; in2 += slope;
        out[i+2] = in1[i+2] * in2; in2 += slope;
        out[i+3] = in1[i+3] * in2; in2 += slope;
    }
}

#ifdef __SSE__
void __noinline__ bench_6(float * out, float * in1, float in2, float slope, unsigned int n)
{
    __m128 arg2 = _mm_set_ps(in2, in2+slope, in2+slope+slope, in2+slope+slope+slope);
    const __m128 vslope = _mm_set_ps1(slope+slope+slope+slope);

    std::size_t loops = n / 4;

    do {
        __m128 arg1 = _mm_load_ps(in1);
        __m128 result = _mm_mul_ps(arg1, arg2);
        arg2 = _mm_add_ps(arg2, vslope);
        _mm_store_ps(out, result);
        in1+=4;
        out+=4;
    } while (--loops);
}

void __noinline__ bench_6a(float * out, float * in1, float in2, float slope, unsigned int n)
{
    __m128 arg2 = _mm_set_ps(in2, in2+slope, in2+2*slope, in2+3*slope);
    const __m128 vslope = _mm_set_ps1(4*slope);

    std::size_t loops = n / 4;

    do {
        __m128 arg1 = _mm_load_ps(in1);
        __m128 result = _mm_mul_ps(arg1, arg2);
        arg2 = _mm_add_ps(arg2, vslope);
        _mm_store_ps(out, result);
        in1+=4;
        out+=4;
    } while (--loops);
}
#endif

int main(void)
{
    out.assign(0.f);
    in.assign(0.2f);
    in2.assign(0.3f);

    const unsigned int iterations = 50000000;

    run_bench(boost::bind(bench_1, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);
    run_bench(boost::bind(bench_2, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);
#ifdef __SSE__
    run_bench(boost::bind(bench_3, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);
    run_bench(boost::bind(bench_3a, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);
#endif

    run_bench(boost::bind(bench_4, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);
    run_bench(boost::bind(bench_5, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);

#ifdef __SSE__
    run_bench(boost::bind(bench_6, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);
    run_bench(boost::bind(bench_6a, out.begin(), in.begin(), 0.1f, 0.001f, 64), iterations);
#endif
}
