#include "benchmark_helpers.hpp"
#include "cache_aligned_array.hpp"
#include "../softclip.hpp"

using namespace nova;
using namespace std;

aligned_array<float, 64> out, args_best_case, args_worst_case, args_avg_case;

void __noinline__ bench_simd_best(unsigned int n)
{
    softclip_vec_simd(out.begin(), args_best_case.begin(), n);
}

void __noinline__ bench_best(unsigned int n)
{
    softclip_vec(out.begin(), args_best_case.begin(), n);
}

void __noinline__ bench_simd_worst(unsigned int n)
{
    softclip_vec_simd(out.begin(), args_worst_case.begin(), n);
}

void __noinline__ bench_worst(unsigned int n)
{
    softclip_vec(out.begin(), args_worst_case.begin(), n);
}

void __noinline__ bench_simd_avg(unsigned int n)
{
    softclip_vec_simd(out.begin(), args_avg_case.begin(), n);
}

void __noinline__ bench_avg(unsigned int n)
{
    softclip_vec(out.begin(), args_avg_case.begin(), n);
}

int main(void)
{
    out.assign(0.f);

    const unsigned int iterations = 10000000;

    for (int i = 0; i != 64; ++i)
    {
        args_best_case[i] = float(i) / 128.0;
        args_worst_case[i] = float(i) / 64.0 + 0.5;
        args_avg_case[i] = float(i) / 64.0;
    }

    cout << "simd:" << endl;
    run_bench(boost::bind(bench_simd_best, 64), iterations);
    run_bench(boost::bind(bench_simd_avg, 64), iterations);
    run_bench(boost::bind(bench_simd_worst, 64), iterations);

    cout << "\nnormal:" << endl;
    run_bench(boost::bind(bench_best, 64), iterations);
    run_bench(boost::bind(bench_avg, 64), iterations);
    run_bench(boost::bind(bench_worst, 64), iterations);
}
