#include "benchmark_helpers.hpp"
#include "../simd_memory.hpp"

#ifdef __SSE2__
#include <xmmintrin.h>
#include <emmintrin.h>
#endif

const int unroll = 8;

using namespace nova;

void __noinline__ bench_1(float * out, float * in, unsigned int numSamples)
{
    copyvec(out, in, numSamples);
}

#ifdef __SSE2__
void __noinline__ bench_2(float * out, float * in, unsigned int numSamples)
{
    int loops = numSamples / unroll;
    do {
        __m128 data = _mm_load_ps(in);
        _mm_store_ps(out, data);

        __m128 data2 = _mm_load_ps(in+4);
        _mm_store_ps(out+4, data2);

        in += 8;
        out += 8;
    } while (--loops);
}

void __noinline__ bench_3(float * out, float * in, unsigned int numSamples)
{
    int loops = numSamples / unroll;
    do {
        __m128 data = _mm_load_ps(in);
        __m128 data2 = _mm_load_ps(in+4);
        _mm_store_ps(out, data);
        _mm_store_ps(out+4, data2);

        in += 8;
        out += 8;
    } while (--loops);
}

void __noinline__ bench_4(float * out, float * in, unsigned int numSamples)
{
    int loops = numSamples / unroll;
    do {
        __m128d data = _mm_load_pd((double*)in);
        _mm_store_pd((double*)out, data);

        __m128d data2 = _mm_load_pd((double*)in+4);
        _mm_store_pd((double*)out+4, data2);

        in += 8;
        out += 8;
    } while (--loops);
}

void __noinline__ bench_5(float * out, float * in, unsigned int numSamples)
{
    int loops = numSamples / unroll;
    do {
        __m128i data = _mm_load_si128((__m128i*)in);
        _mm_store_si128((__m128i*)out, data);

        __m128i data2 = _mm_load_si128((__m128i*)in+4);
        _mm_store_si128((__m128i*)out+4, data2);

        in += 8;
        out += 8;
    } while (--loops);
}



/*
 * sse implementation of partially aligned copy operations
 * this seems to be more efficient than memcpy on x86, but not on x86_64
 *
 */


#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

const int samples_per_loop = 8;

template <int n>
always_inline void copyvec_naa_simd_mp(float *dest, const float *src)
{
    __m128 data = _mm_loadu_ps(src);
    _mm_store_ps(dest, data);

    copyvec_naa_simd_mp<n-4>(dest+4, src+4);
}

template <>
always_inline void copyvec_naa_simd_mp<0>(float *dest, const float *src)
{}

template <int n>
always_inline void copyvec_ana_simd_mp(float *dest, const float *src)
{
    __m128 data = _mm_load_ps(src);
    _mm_storeu_ps(dest, data);

    copyvec_ana_simd_mp<n-4>(dest+4, src+4);
}

template <>
always_inline void copyvec_ana_simd_mp<0>(float *dest, const float *src)
{}

template <int n>
always_inline void copyvec_nana_simd_mp(float *dest, const float *src)
{
    __m128 data = _mm_loadu_ps(src);
    _mm_storeu_ps(dest, data);

    copyvec_nana_simd_mp<n-4>(dest+4, src+4);
}

template <>
always_inline void copyvec_nana_simd_mp<0>(float *dest, const float *src)
{}

inline void copyvec_naa_simd(float * dest, const float * src, uint n)
{
    n = n / samples_per_loop;

    do
    {
        copyvec_naa_simd_mp<samples_per_loop>(dest, src);
        dest += samples_per_loop;
        src += samples_per_loop;
    }
    while(--n);
}

inline void copyvec_ana_simd(float * dest, const float * src, uint n)
{
    n = n / samples_per_loop;

    do
    {
        copyvec_ana_simd_mp<samples_per_loop>(dest, src);
        dest += samples_per_loop;
        src += samples_per_loop;
    }
    while(--n);
}

inline void copyvec_nana_simd(float * dest, const float * src, uint n)
{
    n = n / samples_per_loop;

    do
    {
        copyvec_nana_simd_mp<samples_per_loop>(dest, src);
        dest += samples_per_loop;
        src += samples_per_loop;
    }
    while(--n);
}

void __noinline__ bench_6(float * out, float * in, unsigned int numSamples)
{
    copyvec_nana_simd(out, in, numSamples);
}

void __noinline__ bench_7(float * out, float * in, unsigned int numSamples)
{
    copyvec_ana_simd(out, in, numSamples);
}

void __noinline__ bench_8(float * out, float * in, unsigned int numSamples)
{
    copyvec_naa_simd(out, in, numSamples);
}
#endif

int main(void)
{
    using namespace std;

    const int iterations = 50000000;
    nova::aligned_array<float, 66> __attribute__((aligned(64))) in;
    nova::aligned_array<float, 66> __attribute__((aligned(64))) out;

    fill_container(in);
    fill_container(out);

    cout << "memcpy" << endl;
    run_bench(boost::bind(bench_1, out.begin(), in.begin(), 64), iterations);
    run_bench(boost::bind(bench_1, out.begin()+2, in.begin(), 64), iterations);
    run_bench(boost::bind(bench_1, out.begin(), in.begin()+2, 64), iterations);
    run_bench(boost::bind(bench_1, out.begin()+2, in.begin()+2, 64), iterations);
    cout << endl;

#ifdef __SSE2__
    cout << "float/double/int (sse/sse2 instructions)" << endl;
    run_bench(boost::bind(bench_2, out.begin(), in.begin(), 64), iterations);
    run_bench(boost::bind(bench_3, out.begin(), in.begin(), 64), iterations);
    run_bench(boost::bind(bench_4, out.begin(), in.begin(), 64), iterations);
    run_bench(boost::bind(bench_5, out.begin(), in.begin(), 64), iterations);
    cout << endl;

    cout << "nonaligned" << endl;
    run_bench(boost::bind(bench_6, out.begin(), in.begin(), 64), iterations);
    run_bench(boost::bind(bench_6, out.begin()+2, in.begin()+2, 64), iterations);
    cout << endl;

    cout << "partially aligned" << endl;
    run_bench(boost::bind(bench_7, out.begin()+2, in.begin(), 64), iterations);
    run_bench(boost::bind(bench_8, out.begin(), in.begin()+2, 64), iterations);
    cout << endl;
#endif
}
