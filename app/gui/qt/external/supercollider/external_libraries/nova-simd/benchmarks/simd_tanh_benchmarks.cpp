#include "benchmark_helpers.hpp"
#include "cache_aligned_array.hpp"
#include "../simd_math.hpp"

#include <cmath>

using namespace nova;
using namespace std;

aligned_array<float, 64> out, in;

void __noinline__ bench_1_simd(unsigned int n)
{
    tanh_vec_simd(out.begin(), in.begin(), n);
}

void __noinline__ bench_1(unsigned int n)
{
    for (int i = 0; i != n; ++i)
        out[i] = tanh(in[i]);
}

int main(void)
{
    out.assign(0.f);

    const unsigned int iterations = 5000000;

    in.assign(0.2f);
    cout << "tanh(0.2):" << endl;
    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_1_simd, 64), iterations);

    in.assign(2.f);
    cout << "tanh(2):" << endl;
    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_1_simd, 64), iterations);

    in.assign(200.f);
    cout << "tanh(200):" << endl;
    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_1_simd, 64), iterations);

    fill_container(in);
    cout << "tanh([-1..1]):" << endl;
    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_1_simd, 64), iterations);
}
