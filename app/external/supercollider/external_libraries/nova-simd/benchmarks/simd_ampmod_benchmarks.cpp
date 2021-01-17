#include "benchmark_helpers.hpp"
#include "cache_aligned_array.hpp"
#include "../simd_ternary_arithmetic.hpp"

using namespace nova;
using namespace std;

aligned_array<float, 64> out, in1, in2, in3;

void __noinline__ bench_1(unsigned int n)
{
    ampmod_vec(out.begin(), wrap_argument(in1.begin()), wrap_argument(in2.begin()), wrap_argument(0.1f), n);
}

void __noinline__ bench_1_simd(unsigned int n)
{
    ampmod_vec_simd(out.begin(), wrap_argument(in1.begin()), wrap_argument(in2.begin()), wrap_argument(0.1f), n);
}

void __noinline__ bench_2(unsigned int n)
{
    ampmod_vec(out.begin(), wrap_argument(in1.begin()), wrap_argument(in2.begin()), wrap_argument(in3.begin()), n);
}

void __noinline__ bench_2_simd(unsigned int n)
{
    ampmod_vec_simd(out.begin(), wrap_argument(in1.begin()), wrap_argument(in2.begin()), wrap_argument(in3.begin()), n);
}

void __noinline__ bench_3(unsigned int n)
{
    ampmod_vec(out.begin(), wrap_argument(in1.begin()), wrap_argument(in2.begin()), wrap_argument(0.1f, 0.001f), n);
}

void __noinline__ bench_3_simd(unsigned int n)
{
    ampmod_vec_simd(out.begin(), wrap_argument(in1.begin()), wrap_argument(in2.begin()), wrap_argument(0.1f, 0.001f), n);
}

int main(void)
{
    out.assign(0.f);
    in1.assign(0.f);
    in2.assign(0.f);
    in3.assign(0.f);

    const unsigned int iterations = 50000000;

    cout << "simd:" << endl;
    run_bench(boost::bind(bench_1_simd, 64), iterations);
    run_bench(boost::bind(bench_2_simd, 64), iterations);
    run_bench(boost::bind(bench_3_simd, 64), iterations);

    cout << "\nnormal:" << endl;
    run_bench(boost::bind(bench_1, 64), iterations);
    run_bench(boost::bind(bench_2, 64), iterations);
    run_bench(boost::bind(bench_3, 64), iterations);
}
