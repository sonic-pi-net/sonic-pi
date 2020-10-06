#include "benchmark_helpers.hpp"

#ifdef __SSE__
#include <xmmintrin.h>
#endif

#include "../simd_binary_arithmetic.hpp"

using namespace nova;
using namespace std;

aligned_array<float, 64> out, in1, in2;

#ifdef __SSE__
void __noinline__ bench_1(float * out, float * in1, float * in2, unsigned int n)
{
    n /= 4;

    do
    {
        __m128 lhs = _mm_load_ps(in1);
        __m128 rhs = _mm_load_ps(in2);
        __m128 result = _mm_add_ps(lhs, rhs);
        _mm_store_ps(out, result);
        in1 += 4;
        in2 += 4;
        out += 4;
    }
    while (--n);
}

void __noinline__ bench_2(float * out, float * in1, float * in2, unsigned int n)
{
    n /= 8;

    do
    {
        __m128 lhs1 = _mm_load_ps(in1);
        __m128 lhs2 = _mm_load_ps(in1+4);
        __m128 rhs1 = _mm_load_ps(in2);
        __m128 rhs2 = _mm_load_ps(in2+4);
        __m128 result1 = _mm_add_ps(lhs1, rhs1);
        __m128 result2 = _mm_add_ps(lhs2, rhs2);
        _mm_store_ps(out, result1);
        _mm_store_ps(out+4, result2);
        in1 += 8;
        in2 += 8;
        out += 8;
    }
    while (--n);
}

void __noinline__ bench_3(float * out, float * in1, float * in2, unsigned int n)
{
    n /= 16;

    do
    {
        __m128 lhs1 = _mm_load_ps(in1);
        __m128 lhs2 = _mm_load_ps(in1+4);
        __m128 lhs3 = _mm_load_ps(in1+8);
        __m128 lhs4 = _mm_load_ps(in1+12);
        __m128 rhs1 = _mm_load_ps(in2);
        __m128 rhs2 = _mm_load_ps(in2+4);
        __m128 rhs3 = _mm_load_ps(in2+8);
        __m128 rhs4 = _mm_load_ps(in2+12);
        __m128 result1 = _mm_add_ps(lhs1, rhs1);
        __m128 result2 = _mm_add_ps(lhs2, rhs2);
        __m128 result3 = _mm_add_ps(lhs3, rhs3);
        __m128 result4 = _mm_add_ps(lhs4, rhs4);
        _mm_store_ps(out, result1);
        _mm_store_ps(out+4, result2);
        _mm_store_ps(out+8, result3);
        _mm_store_ps(out+12, result4);
        in1 += 16;
        in2 += 16;
        out += 16;
    }
    while (--n);
}
#endif


void __noinline__ bench_4(float * out, float * in1, float * in2, unsigned int n)
{
    n /= 8;

    do
    {
        nova::plus_vec_simd<8>(out, in1, in2);
        in1 += 8;
        in2 += 8;
        out += 8;
    }
    while (--n);
}

void __noinline__ bench_5(float * out, float * in1, float * in2, unsigned int n)
{
    n /= 16;

    do
    {
        nova::plus_vec_simd<16>(out, in1, in2);
        in1 += 16;
        in2 += 16;
        out += 16;
    }
    while (--n);
}



int main(void)
{
    out.assign(0.f);
    in1.assign(0.f);
    in2.assign(0.f);

    const unsigned int iterations = 100000000;

#ifdef __SSE__
    run_bench(boost::bind(bench_1, out.begin(), in1.begin(), in2.begin(), 64), iterations);
    run_bench(boost::bind(bench_2, out.begin(), in1.begin(), in2.begin(), 64), iterations);
    run_bench(boost::bind(bench_3, out.begin(), in1.begin(), in2.begin(), 64), iterations);
#endif
    run_bench(boost::bind(bench_4, out.begin(), in1.begin(), in2.begin(), 64), iterations);
    run_bench(boost::bind(bench_5, out.begin(), in1.begin(), in2.begin(), 64), iterations);
}
