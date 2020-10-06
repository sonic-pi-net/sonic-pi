#include <cmath>
#include "benchmark_helpers.hpp"

#include "../simd_peakmeter.hpp"

#define LOOP(length, stmt) for (int xxi=0; xxi<(length); ++xxi) { stmt; }

#define ZXP(z) (*++(z))
#define ZOFF (1)


void __noinline__ bench_1(float * out, float * in, unsigned int numSamples)
{
    float level = 0;
    using namespace std;
    for (int i = 0; i != numSamples; i += 8, out += 8, in += 8)
    {
        float level0 = max(abs(in[0]), level);
        float level1 = max(abs(in[1]), level0);
        float level2 = max(abs(in[2]), level1);
        float level3 = max(abs(in[3]), level2);
        float level4 = max(abs(in[4]), level3);
        float level5 = max(abs(in[5]), level4);
        float level6 = max(abs(in[6]), level5);
        float level7 = max(abs(in[7]), level6);
        out[0] = level0;
        out[1] = level1;
        out[2] = level2;
        out[3] = level3;
        out[4] = level4;
        out[5] = level5;
        out[6] = level6;
        out[7] = level7;
        level = level7;
    }
}


void __noinline__ bench_2(float * out, float * in, unsigned int numSamples)
{
    float inlevel;
    float level = 0;
    LOOP(numSamples/8,
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
        inlevel = std::abs(ZXP(in));
        level = std::max(inlevel, level);
        ZXP(out) = level;
    );
}

void __noinline__ bench_3(float * in, unsigned int numSamples)
{
    float level = 0;
    float inlevel = nova::peak_vec(in, &level, numSamples);
}

void __noinline__ bench_4(float * in, unsigned int numSamples)
{
    float level = 0;
    float inlevel = nova::peak_vec_simd<float>(in, &level, numSamples);
}

int main(void)
{
    const int iterations = 50000000;
    nova::aligned_array<float, 1024> in_buffer;
    nova::aligned_array<float, 1024> out_buffer;
    fill_container(in_buffer);
    fill_container(out_buffer);

    run_bench(boost::bind(bench_1, out_buffer.begin(), in_buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_2, out_buffer.begin(), in_buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_3, in_buffer.begin(), 64), iterations);
    run_bench(boost::bind(bench_4, in_buffer.begin(), 64), iterations);
}
