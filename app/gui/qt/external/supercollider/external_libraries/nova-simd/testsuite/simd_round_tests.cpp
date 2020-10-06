#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../simd_unary_arithmetic.hpp"
#include "test_helper.hpp"
#include "../benchmarks/cache_aligned_array.hpp"

using namespace nova;
using namespace std;

static const int size = 1024;

#define COMPARE_TEST(name)                                              \
template <typename float_type>                                          \
void test_##name(void)                                                  \
{                                                                       \
    aligned_array<float_type, size> sseval, mpval, generic, args;       \
    randomize_buffer<float_type>(args.c_array(), size);                 \
                                                                        \
    name##_vec(generic.c_array(), args.c_array(), size);                \
    name##_vec_simd(sseval.c_array(), args.c_array(), size);            \
    name##_vec_simd<size>(mpval.c_array(), args.c_array());             \
                                                                        \
    for (int i = 0; i != size; ++i)                                     \
        BOOST_CHECK_CLOSE( sseval[i], generic[i], 0.0001 );             \
    for (int i = 0; i != size; ++i)                                     \
        BOOST_CHECK_CLOSE( mpval[i], generic[i], 0.0001 );              \
}                                                                       \
                                                                        \
BOOST_AUTO_TEST_CASE( name##_tests_float )                              \
{                                                                       \
    test_##name<float>();                                               \
}                                                                       \
                                                                        \
BOOST_AUTO_TEST_CASE( name##_tests_double )                             \
{                                                                       \
    test_##name<double>();                                              \
}

COMPARE_TEST(round)
COMPARE_TEST(ceil)
COMPARE_TEST(floor)
COMPARE_TEST(frac)
COMPARE_TEST(trunc)