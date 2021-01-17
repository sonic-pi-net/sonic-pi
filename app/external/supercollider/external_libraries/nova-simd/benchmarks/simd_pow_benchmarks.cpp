#include "benchmark_helpers.hpp"
#include "cache_aligned_array.hpp"
#include "../simd_math.hpp"

#include <cmath>

using namespace nova;
using namespace std;

aligned_array<float, 64> out, in, in2;

void __noinline__ bench_1(unsigned int n)
{
    spow_vec(out.begin(), in.begin(), 2.3f, n);
}

void __noinline__ bench_1_simd(unsigned int n)
{
    spow_vec_simd(out.begin(), in.begin(), 2.3f, n);
}

void __noinline__ bench_2(unsigned int n)
{
    spow_vec(out.begin(), in.begin(), in2.begin(), n);
}

void __noinline__ bench_2_simd(unsigned int n)
{
    spow_vec_simd(out.begin(), in.begin(), in2.begin(), n);
}


int main(void)
{
    out.assign(0);

    in.assign(0.2f);
    in2.assign(2.3f);

    const unsigned int iterations = 500000;

    cout << "pow(0.2, 2.3):" << endl;
    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_1_simd, 64), iterations);

    run_bench(boost::bind(bench_2, 64), iterations);
    run_bench(boost::bind(bench_2_simd, 64), iterations);
}
