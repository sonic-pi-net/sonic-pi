#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#ifdef __SSE__
#include "../simd_utils.hpp"

using namespace nova;
using namespace std;


void test_register(__m128 arg, float f)
{
    float data[4];
    _mm_storeu_ps(data, arg);
    for (int i = 0; i != 4; ++ i)
        BOOST_REQUIRE_EQUAL(data[i], f);
}


BOOST_AUTO_TEST_CASE( utils_test )
{
    test_register(detail::gen_one(), 1.f);
    test_register(detail::gen_05(), 0.5f);
    test_register(detail::gen_025(), 0.25f);
}

#endif

BOOST_AUTO_TEST_CASE( dummy_test )
{
}
