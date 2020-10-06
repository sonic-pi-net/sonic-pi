#include "benchmark_helpers.hpp"
#include "../simd_memory.hpp"

#define LOOP(length, stmt) for (int xxi=0; xxi<(length); ++xxi) { stmt; }

#define ZXP(z) (*++(z))
#define ZOFF (1)

void __noinline__ bench_1(float * out, unsigned int numSamples)
{
    if ((numSamples & 1) == 0) {
        double *outd = (double*)out - ZOFF;
        LOOP(numSamples >> 1, ZXP(outd) = 0.; );
    } else {
        out -= ZOFF;
        LOOP(numSamples, ZXP(out) = 0.f; );
    }
}

void __noinline__ bench_2(float * out, unsigned int numSamples)
{
    int i;
    for(i = 0; i < numSamples; ++i)
        out[i] = 0.f;
}

void __noinline__ bench_3(float * out, unsigned int numSamples)
{
    int i;
    float *loc = out;
    for(i = 0; i < numSamples; ++i)
        *(loc++) = 0.f;
}

void __noinline__ bench_4(float * out, unsigned int numSamples)
{
    int i;
    float *loc = out - 1;
    for(i = 0; i < numSamples; ++i)
        *(++loc) = 0.f;
}

void __noinline__ bench_5(float * out, unsigned int numSamples)
{
    int i = numSamples;
    do{
        out[--i] = 0.f;
    }while(i != 0);
}

#define DUFF_DEVICE_8(aCount, aAction) \
{ \
int count_ = (aCount); \
int times_ = (count_ + 7) >> 3; \
switch (count_ & 7){ \
case 0: do { aAction; \
case 7: aAction; \
case 6: aAction; \
case 5: aAction; \
case 4: aAction; \
case 3: aAction; \
case 2: aAction; \
case 1: aAction; \
} while (--times_ > 0); \
} \
}

void __noinline__ bench_6(float * out, unsigned int numSamples)
{
    float *loc = out;
    DUFF_DEVICE_8(numSamples, *(loc++)=0.f;);
}


void __noinline__ bench_7(float * out, unsigned int numSamples)
{
    float *loc = out - 1;
    DUFF_DEVICE_8(numSamples, *(++loc)=0.f;);
}

void __noinline__ bench_8(float * out, unsigned int numSamples)
{
    memset(out, 0, numSamples * sizeof(float));
}

void __noinline__ bench_9(float * out, unsigned int numSamples)
{
    bzero(out, numSamples * sizeof(float));
}

void __noinline__ bench_10(float * out, unsigned int numSamples)
{
    int i;
    float *loc = out;
    for(i = numSamples >> 2; i != 0; --i){ // Unroll into blocks of four
        *(loc++) = 0.f;
        *(loc++) = 0.f;
        *(loc++) = 0.f;
        *(loc++) = 0.f;
    }
// These two "if"s handle the remainder, if not divisible exactly by four
    if(numSamples & 1){
        *(loc++) = 0.f;
    }
    if(numSamples & 2){
        *(loc++) = 0.f;
        *(loc++) = 0.f;
    }
}

using namespace nova;

void __noinline__ bench_11(float * out, unsigned int numSamples)
{
    zerovec_simd<64>(out);
}

void __noinline__ bench_12(float * out, unsigned int numSamples)
{
    zerovec_simd<64>(out);
    if (numSamples-64)
        zerovec(out+64, numSamples-64);
}

void __noinline__ bench_13(float * out, unsigned int numSamples)
{
    zerovec_simd(out, numSamples);
}

void __noinline__ bench_14(float * out, unsigned int numSamples)
{
    unsigned int unrolled = numSamples & ~8;
    unsigned int remain = numSamples - unrolled;
    if (unrolled)
        zerovec_simd(out, unrolled);

    if (remain)
        zerovec(out, remain);
}


template <typename F>
inline void zerovec_8(F * dest, uint n)
{
    for (uint i = 0; i != n; i+=8)
    {
        dest[i+0] = dest[i+1] = dest[i+2] = dest[i+3] = dest[i+4] =
            dest[i+5] = dest[i+6] = dest[i+7] = 0;
    }
}

void __noinline__ bench_15(float * out, unsigned int numSamples)
{
    zerovec_8(out, numSamples);
}

void __noinline__ bench_16(float * out, unsigned int numSamples)
{
    unsigned int unrolled = numSamples & ~8;
    unsigned int remain = numSamples - unrolled;
    if (unrolled)
        zerovec_8(out, unrolled);
    if (remain)
        zerovec(out, remain);
}

void __noinline__ bench_17(float * out, unsigned int numSamples)
{
    zerovec(out, numSamples);
}

void __noinline__ bench_18(float * out, unsigned int numSamples)
{
    unsigned int unrolled = numSamples & ~8;
    unsigned int remain = numSamples - unrolled;
    if (unrolled)
        zerovec_na_simd<float>(out, unrolled);
    if (remain)
        zerovec(out, remain);
}


int main(void)
{
    const int iterations = 50000000;
    nova::aligned_array<float, 1024> buffer;
    fill_container(buffer);

    run_bench(boost::bind(bench_1, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_2, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_3, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_4, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_5, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_6, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_7, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_8, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_9, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_10, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_11, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_12, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_13, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_14, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_15, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_16, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_17, buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_18, buffer.begin(), 64), iterations);
}
