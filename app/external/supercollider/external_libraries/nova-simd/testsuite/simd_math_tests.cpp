#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../simd_math.hpp"
#include "../benchmarks/cache_aligned_array.hpp"
#include "test_helper.hpp"

using namespace nova;
using namespace std;


static const int size = 10000;


#define COMPARE_TEST(name, low, high)                                   \
template <typename float_type>                                          \
void test_##name(void)                                                  \
{                                                                       \
    aligned_array<float_type, size> sseval, mpval, libmval, args;       \
                                                                        \
    float_type init = low;                                              \
    float_type diff = (float_type(high) - float_type(low)) / float_type(size); \
    \
    for (int i = 0; i != size; ++i)                                     \
    {                                                                   \
        args[i] = init;                                                 \
        init += diff;                                                   \
    }                                                                   \
                                                                        \
    name##_vec(libmval.begin(), args.begin(), size);                    \
    name##_vec_simd(sseval.begin(), args.begin(), size);                \
    /*name##_vec_simd<size>(mpval.begin(), args.begin())*/;                 \
                                                                        \
    compare_buffers(sseval.begin(), libmval.begin(), size, 5e-6f);      \
    /*compare_buffers(mpval.begin(), libmval.begin(), size, 5e-6f)*/;       \
}                                                                       \
                                                                        \
BOOST_AUTO_TEST_CASE( name##_tests)                                     \
{                                                                       \
    test_##name<float>();                                               \
    test_##name<double>();                                              \
}

COMPARE_TEST(sin, -3.2, 3.2)
COMPARE_TEST(cos, -3.2, 3.2)
COMPARE_TEST(tan, -1.5, 1.5)
COMPARE_TEST(asin, -1, 1)
COMPARE_TEST(acos, -0.9, 0.9)
COMPARE_TEST(atan, -10, 10)
COMPARE_TEST(tanh, -10, 10)
COMPARE_TEST(signed_sqrt, -20, 20)


/* test range: 0, 20 */
BOOST_AUTO_TEST_CASE( pow_tests_float_1 )
{
    for (float exponent = 0.1; exponent < 2; exponent += 0.1)
    {
        aligned_array<float, size> sseval, libmval, args;

        float init = 0;
        for (int i = 0; i != size; ++i)
        {
            args[i] = init;
            init += 20.f/size;
        }

        pow_vec(libmval.begin(), args.begin(), exponent, size);
        pow_vec_simd(sseval.begin(), args.begin(), exponent, size);

        compare_buffers(sseval.begin(), libmval.begin(), 1e-4f);
    }
}


/* test range: -10, 10 */
BOOST_AUTO_TEST_CASE( spow_tests_float_1 )
{
    for (float exponent = 0.1; exponent < 2; exponent += 0.1)
    {
        aligned_array<float, size> sseval, libmval, args;

        aligned_array<float, 4> exparray;
        exparray.assign(exponent);

        float init = -10;
        for (int i = 0; i != size; ++i)
        {
            args[i] = init;
            init += 20.f/size;
        }

        spow_vec(libmval.begin(), args.begin(), exponent, size);
        spow_vec_simd(sseval.begin(), args.begin(), exponent, size);

        compare_buffers(sseval.begin(), libmval.begin(), 1e-4f);
    }

	vec<float> zero(0.f);
	vec<float> exponent(1.1f);

	vec<float> result = pow(zero, exponent);
	BOOST_REQUIRE_EQUAL(result.get(0), 0.f);
}

COMPARE_TEST(log, 0.01, 100)
COMPARE_TEST(log2, 0.01, 100)
COMPARE_TEST(log10, 0.01, 100)
COMPARE_TEST(exp, -10, 10)
