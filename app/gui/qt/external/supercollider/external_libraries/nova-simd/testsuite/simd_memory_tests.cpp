#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../simd_memory.hpp"
#include "../benchmarks/cache_aligned_array.hpp"
#include "test_helper.hpp"

using namespace nova;
using namespace std;


static const unsigned int size = 64;

template <typename float_type>
void zero_tests(void)
{
    aligned_array<float_type, size> out, out_simd, out_mp;
    randomize_buffer<float_type>(out.c_array(), size);
    randomize_buffer<float_type>(out_simd.c_array(), size);
    randomize_buffer<float_type>(out_mp.c_array(), size);

    zerovec<float_type>(out.c_array(), size);
    zerovec_simd<float_type>(out_simd.c_array(), size);
    zerovec_simd<size>(out_mp.c_array());

    compare_buffers(out.c_array(), out_simd.c_array(), size);
    compare_buffers(out.c_array(), out_mp.c_array(), size);
}

BOOST_AUTO_TEST_CASE( zero_test )
{
    zero_tests<float>();
    zero_tests<double>();
}


template <typename float_type>
void set_tests(void)
{
    aligned_array<float_type, size> out, out_simd, out_mp;
    randomize_buffer<float_type>(out.c_array(), size);
    randomize_buffer<float_type>(out_simd.c_array(), size);
    randomize_buffer<float_type>(out_mp.c_array(), size);

    float_type f = randomize_float<float_type>();

    setvec<float_type>(out.c_array(), f, size);
    setvec_simd<float_type>(out_simd.c_array(), f, size);
    setvec_simd<size>(out_mp.c_array(), f);

    compare_buffers(out.c_array(), out_simd.c_array(), size);
    compare_buffers(out.c_array(), out_mp.c_array(), size);
}

BOOST_AUTO_TEST_CASE( set_test )
{
    set_tests<float>();
    set_tests<double>();
}

template <typename float_type>
void add_tests(void)
{
    aligned_array<float_type, size> out, out_simd, out_mp, in;

    randomize_buffer<float_type>(in.c_array(), size);
    randomize_buffer<float_type>(out.c_array(), size);
    out_simd = out;
    out_mp = out;

    addvec<float_type>(out.c_array(), in.c_array(), size);
    addvec_simd<float_type>(out_simd.c_array(), in.c_array(), size);
    addvec_simd<size>(out_mp.c_array(), in.c_array());

    compare_buffers(out.c_array(), out_simd.c_array(), size);
    compare_buffers(out.c_array(), out_mp.c_array(), size);
}

BOOST_AUTO_TEST_CASE( add_test )
{
    add_tests<float>();
    add_tests<double>();
}

template <typename float_type>
void slope_tests(void)
{
    aligned_array<float_type, size> out, out_simd;

    float_type base = randomize_float<float_type>();
    float_type slope = randomize_float<float_type>() * 0.01;

    set_slope_vec<float_type>(out.c_array(), base, slope, size);
    set_slope_vec_simd<float_type>(out_simd.c_array(), base, slope, size);

    compare_buffers(out.c_array(), out_simd.c_array(), size);
}

BOOST_AUTO_TEST_CASE( slope_test )
{
    slope_tests<float>();
    slope_tests<double>();
}

template <typename float_type>
void exp_tests(void)
{
    aligned_array<float_type, size> out, out_simd;

    float_type base = std::abs(randomize_float<float_type>());
    float_type slope = std::abs(randomize_float<float_type>() * 0.01) + 1;

    set_exp_vec<float_type>(out.c_array(), base, slope, size);
    set_exp_vec_simd<float_type>(out_simd.c_array(), base, slope, size);

    compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-5);
}

BOOST_AUTO_TEST_CASE( exp_test )
{
    exp_tests<float>();
    exp_tests<double>();
}
