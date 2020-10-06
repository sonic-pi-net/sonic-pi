#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../simd_pan.hpp"
#include "test_helper.hpp"
#include "../benchmarks/cache_aligned_array.hpp"

using namespace nova;
using namespace std;

static const int size = 512;

template <typename float_type>
void test_pan2(void)
{
    aligned_array<float_type, size> sseval0, mpval0, generic0, args;
    aligned_array<float_type, size> sseval1, mpval1, generic1;
    randomize_buffer<float_type>(args.c_array(), size);

    float_type factor0 = 0.4;
    float_type factor1 = 0.6;

    pan2_vec(generic0.c_array(), generic1.c_array(), args.c_array(), factor0, factor1, size);
    pan2_vec_simd(sseval0.c_array(), sseval1.c_array(), args.c_array(), factor0, factor1, size);
    pan2_vec_simd<size>(mpval0.c_array(), mpval1.c_array(), args.c_array(), factor0, factor1);

    for (int i = 0; i != size; ++i) {
        BOOST_CHECK_CLOSE( sseval0[i], generic0[i], 0.0001 );
        BOOST_CHECK_CLOSE( mpval0[i], generic0[i], 0.0001 );
        BOOST_CHECK_CLOSE( sseval1[i], generic1[i], 0.0001 );
        BOOST_CHECK_CLOSE( mpval1[i], generic1[i], 0.0001 );
    }
}

BOOST_AUTO_TEST_CASE( pan2_tests )
{
    test_pan2<float>();
    test_pan2<double>();
}

template <typename float_type>
void test_pan2_ramp(void)
{
    aligned_array<float_type, size> sseval0, mpval0, generic0, args;
    aligned_array<float_type, size> sseval1, mpval1, generic1;
    randomize_buffer<float_type>(args.c_array(), size);

    float_type factor0 = 0.4;
    float_type factor1 = 0.6;

    pan2_vec(generic0.c_array(), generic1.c_array(), args.c_array(), factor0,
             float_type(size*0.1), factor1, -float_type(size*0.1), size);
    pan2_vec_simd(sseval0.c_array(), sseval1.c_array(), args.c_array(), factor0,
                  float_type(size*0.1), factor1, -float_type(size*0.1), size);
    pan2_vec_simd<size>(mpval0.c_array(), mpval1.c_array(), args.c_array(), factor0,
                                    float_type(size*0.1), factor1, -float_type(size*0.1));

    for (int i = 0; i != size; ++i) {
        BOOST_CHECK_CLOSE( sseval0[i], generic0[i], 0.01 );
        BOOST_CHECK_CLOSE( mpval0[i],  generic0[i], 0.01 );
        BOOST_CHECK_CLOSE( sseval1[i], generic1[i], 0.01 );
        BOOST_CHECK_CLOSE( mpval1[i],  generic1[i], 0.01 );
    }
}

BOOST_AUTO_TEST_CASE( pan2_ramp_tests )
{
    test_pan2_ramp<float>();
    test_pan2_ramp<double>();
}
