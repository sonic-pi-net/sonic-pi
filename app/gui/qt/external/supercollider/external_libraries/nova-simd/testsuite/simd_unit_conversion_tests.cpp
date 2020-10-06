#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../simd_unit_conversion.hpp"
#include "test_helper.hpp"
#include "../benchmarks/cache_aligned_array.hpp"

using namespace nova;
using namespace std;

static const int size = 1024;

#define CONVERSION_TEST(name, offset, scale)                            \
template <typename float_type>                                          \
void test_##name(void)                                                  \
{                                                                       \
    aligned_array<float_type, size> sseval, generic, args0;             \
    randomize_buffer<float_type>(args0.c_array(), size, offset, scale); \
                                                                        \
    name##_vec(generic.c_array(), args0.c_array(), size);               \
    name##_vec_simd(sseval.c_array(), args0.c_array(), size);           \
                                                                        \
    for (int i = 0; i != size; ++i) {                                   \
        BOOST_CHECK_CLOSE( sseval[i], generic[i], 0.001 );              \
    }                                                                   \
}                                                                       \
                                                                        \
BOOST_AUTO_TEST_CASE( name##_tests )                                    \
{                                                                       \
    test_##name<float>();                                               \
    test_##name<double>();                                              \
}

CONVERSION_TEST(midi2freq, 127, 0)
CONVERSION_TEST(freq2midi, 16000, 0)
CONVERSION_TEST(midi2ratio, 127, 0)
CONVERSION_TEST(ratio2midi, 1, 0)
CONVERSION_TEST(oct2freq, 10, 0)
CONVERSION_TEST(freq2oct, 16000, 0)
CONVERSION_TEST(db2amp, 127, -127)
CONVERSION_TEST(amp2db, 1, 0)
