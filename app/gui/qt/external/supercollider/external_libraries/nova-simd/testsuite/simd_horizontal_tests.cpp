#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../simd_horizontal_functions.hpp"
#include "../benchmarks/cache_aligned_array.hpp"
#include "test_helper.hpp"

using namespace nova;
using namespace std;

BOOST_AUTO_TEST_CASE( horizontal_tests_float )
{
    aligned_array<float, 10000> arguments;

    float init = 0;
    for (int i = 0; i != 10000; ++i)
    {
        arguments[i] = init;
        init += 10/10000.f;
    }

    float non_simd_min = horizontal_min_vec(arguments.begin(), 10000);
    float simd_min = horizontal_min_vec_simd(arguments.begin(), 10000);
    BOOST_REQUIRE_CLOSE_FRACTION(non_simd_min, simd_min, 0);

    float non_simd_max = horizontal_max_vec(arguments.begin(), 10000);
    float simd_max = horizontal_max_vec_simd(arguments.begin(), 10000);
    BOOST_REQUIRE_CLOSE_FRACTION(non_simd_max, simd_max, 0);

    float non_simd_sum = horizontal_sum_vec(arguments.begin(), 10000);
    float simd_sum = horizontal_sum_vec_simd(arguments.begin(), 10000);
    BOOST_REQUIRE_CLOSE_FRACTION(non_simd_sum, simd_sum, 1e-6);

    horizontal_maxsum_vec(non_simd_max, non_simd_sum, arguments.begin(), 10000);
    horizontal_maxsum_vec_simd(simd_max, simd_sum, arguments.begin(), 10000);
    BOOST_REQUIRE_CLOSE_FRACTION(non_simd_max, simd_max, 0);
    BOOST_REQUIRE_CLOSE_FRACTION(non_simd_sum, simd_sum, 1e-6);

    horizontal_minmax_vec(non_simd_min, non_simd_max, arguments.begin(), 10000);
    horizontal_minmax_vec_simd(simd_min, simd_max, arguments.begin(), 10000);
    BOOST_REQUIRE_CLOSE_FRACTION(non_simd_min, simd_min, 0);
    BOOST_REQUIRE_CLOSE_FRACTION(non_simd_max, simd_max, 0);
}
