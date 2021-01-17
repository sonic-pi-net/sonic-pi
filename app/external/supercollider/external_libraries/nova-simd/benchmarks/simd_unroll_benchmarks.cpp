#include "benchmark_helpers.hpp"

#ifdef __SSE__
#include <xmmintrin.h>
#endif

#include "../simd_binary_arithmetic.hpp"

using namespace nova;
using namespace std;

aligned_array<float, 64> out, in;

#ifdef __SSE__
void __noinline__ bench_1(float * out, float * in, float f, unsigned int n)
{
    n /= 4;

    __m128 scalar = _mm_set_ps1(f);

    do
    {
        __m128 arg = _mm_load_ps(in);
        __m128 result = _mm_add_ps(arg, scalar);
        _mm_store_ps(out, result);
        in += 4;
        out += 4;
    }
    while (--n);
}

void __noinline__ bench_2(float * out, float * in, float f, unsigned int n)
{
    n /= 8;
    __m128 scalar = _mm_set_ps1(f);
    do
    {
        __m128 arg = _mm_load_ps(in);
        __m128 result = _mm_add_ps(arg, scalar);
        _mm_store_ps(out, result);

        arg = _mm_load_ps(in+4);
        result = _mm_add_ps(arg, scalar);
        _mm_store_ps(out+4, result);
        in += 8;
        out += 8;
    }
    while (--n);
}

void  __noinline__ bench_3(float * __restrict__ out, float * __restrict__ in, float f, unsigned int n)
{
    n /= 8;
    __m128 scalar = _mm_set_ps1(f);
    do
    {
        __m128 arg = _mm_load_ps(in);
        __m128 result = _mm_add_ps(arg, scalar);
        _mm_store_ps(out, result);

        arg = _mm_load_ps(in+4);
        result = _mm_add_ps(arg, scalar);
        _mm_store_ps(out+4, result);
        in += 8;
        out += 8;
    }
    while (--n);
}


void __noinline__ bench_4(float * out, float * in, float f, unsigned int n)
{
    n /= 8;

    __m128 scalar = _mm_set_ps1(f);

    do
    {
        __m128 arg  = _mm_load_ps(in);
        __m128 arg2 = _mm_load_ps(in+4);
        __m128 result  = _mm_add_ps(arg, scalar);
        __m128 result2 = _mm_add_ps(arg2, scalar);
        _mm_store_ps(out, result);
        _mm_store_ps(out+4, result2);
        in += 8;
        out += 8;
    }
    while (--n);
}

void __noinline__ bench_5(float * out, float * in, float f, unsigned int n)
{
    n /= 16;

    __m128 scalar = _mm_set_ps1(f);

    do
    {
        __m128 arg  = _mm_load_ps(in);
        __m128 arg2 = _mm_load_ps(in+4);
        __m128 arg3 = _mm_load_ps(in+8);
        __m128 arg4 = _mm_load_ps(in+12);
        __m128 result  = _mm_add_ps(arg, scalar);
        __m128 result2 = _mm_add_ps(arg2, scalar);
        __m128 result3 = _mm_add_ps(arg3, scalar);
        __m128 result4 = _mm_add_ps(arg4, scalar);
        _mm_store_ps(out, result);
        _mm_store_ps(out+4, result2);
        _mm_store_ps(out+8, result3);
        _mm_store_ps(out+12, result4);
        in += 16;
        out += 16;
    }
    while (--n);
}
#endif

/*void __noinline__ bench_6(float * out, float * in, float f, unsigned int n)
{
    n /= 8;

    do
    {
        nova::plus_vec_simd<8>(out, in, f);
        in += 8;
        out += 8;
    }
    while (--n);
}
*/
void __noinline__ bench_7(float * out, float * in, float f, unsigned int n)
{
    n /= 16;

    do
    {
        nova::plus_vec_simd<16>(out, in, f);
        in += 16;
        out += 16;
    }
    while (--n);
}

void __noinline__ bench_8(float * out, float * in, float f, unsigned int n)
{
    n /= 32;

    do
    {
        nova::plus_vec_simd<32>(out, in, f);
        in += 32;
        out += 32;
    }
    while (--n);
}


int main(void)
{
    out.assign(0.f);
    in.assign(0.f);

    const unsigned int iterations = 100000000;

#ifdef __SSE__
    run_bench(boost::bind(bench_1, out.begin(), in.begin(), 1.f, 64), iterations);
    run_bench(boost::bind(bench_2, out.begin(), in.begin(), 1.f, 64), iterations);
    run_bench(boost::bind(bench_3, out.begin(), in.begin(), 1.f, 64), iterations);
    run_bench(boost::bind(bench_4, out.begin(), in.begin(), 1.f, 64), iterations);
    run_bench(boost::bind(bench_5, out.begin(), in.begin(), 1.f, 64), iterations);
#endif
 /*   run_bench(boost::bind(bench_6, out.begin(), in.begin(), 1.f, 64), iterations);*/
    run_bench(boost::bind(bench_7, out.begin(), in.begin(), 1.f, 64), iterations);
    run_bench(boost::bind(bench_8, out.begin(), in.begin(), 1.f, 64), iterations);
}
