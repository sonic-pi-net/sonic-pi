#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../softclip.hpp"
#include "../benchmarks/cache_aligned_array.hpp"
#include "test_helper.hpp"

using namespace nova;
using namespace std;

BOOST_AUTO_TEST_CASE( softclip_tests_float )
{
    aligned_array<float, 10000> simd_val, normal_val, arguments;

    float init = 0;
    for (int i = 0; i != 10000; ++i)
    {
        arguments[i] = init;
        init += 10/10000.f;
    }

    softclip_vec_simd(simd_val.begin(), arguments.begin(), 10000);
    softclip_vec (normal_val.begin(), arguments.begin(), 10000);

    compare_buffers(normal_val.begin(), simd_val.begin(), 10000, 1e-8f);
}
