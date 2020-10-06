#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "test_helper.hpp"

#include "../benchmarks/cache_aligned_array.hpp"
#include "../simd_unary_arithmetic.hpp"

using namespace nova;
using namespace std;

static const unsigned int size = 64;

#define COMPARE_TEST(function)                                          \
    template <typename float_type>                                      \
    void function##_compare(void)                                       \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp, in;      \
        randomize_buffer<float_type>(in.c_array(), size);               \
                                                                        \
        nova::function##_vec<float_type>(out.c_array(), in.c_array(), size); \
        nova::function##_vec_simd<float_type>(out_simd.c_array(), in.c_array(), size); \
        nova::function##_vec_simd<size>(out_mp.c_array(), in.c_array());\
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-6f);\
        compare_buffers(out.c_array(), out_mp.c_array(), size, 1e-6f);  \
    }                                                                   \
                                                                        \
    BOOST_AUTO_TEST_CASE( function##_comparer )                         \
    {                                                                   \
        function##_compare<float>();                                    \
        function##_compare<double>();                                   \
    }


template <typename float_type>
void run_sign_test(void)
{
    aligned_array<float_type, size> in;
    aligned_array<float_type, size> out;
    in[0] = -2;
    in[1] = in[2] = 0;
    in[3] = 2;

    nova::sgn_vec_simd(out.begin(), in.begin(), size);

    BOOST_REQUIRE_EQUAL( out[0], -1 );
    BOOST_REQUIRE_EQUAL( out[1], 0 );
    BOOST_REQUIRE_EQUAL( out[2], 0 );
    BOOST_REQUIRE_EQUAL( out[3], 1 );
}

BOOST_AUTO_TEST_CASE( sign_test2 )
{
    run_sign_test<float>();
    run_sign_test<double>();
}

COMPARE_TEST(sgn)

template <typename float_type>
void run_abs_test(void)
{
    aligned_array<float_type, size> in;
    aligned_array<float_type, size> out;
    in[0] = -2;
    in[1] = in[2] = 0;
    in[3] = 2;

    nova::abs_vec_simd(out.begin(), in.begin(), size);

    BOOST_REQUIRE_EQUAL( out[0], 2 );
    BOOST_REQUIRE_EQUAL( out[1], 0 );
    BOOST_REQUIRE_EQUAL( out[2], 0 );
    BOOST_REQUIRE_EQUAL( out[3], 2 );

}

BOOST_AUTO_TEST_CASE( abs_test2 )
{
    run_abs_test<float>();
    run_abs_test<double>();
}

COMPARE_TEST(abs)

template <typename float_type>
void run_square_test(void)
{
    aligned_array<float_type, size> in;
    aligned_array<float_type, size> out;
    in[0] = -2;
    in[1] = in[2] = 0;
    in[3] = 2;

    nova::square_vec_simd(out.begin(), in.begin(), size);

    BOOST_REQUIRE_EQUAL( out[0], 4 );
    BOOST_REQUIRE_EQUAL( out[1], 0 );
    BOOST_REQUIRE_EQUAL( out[2], 0 );
    BOOST_REQUIRE_EQUAL( out[3], 4 );

}

BOOST_AUTO_TEST_CASE( square_test2 )
{
    run_square_test<float>();
    run_square_test<double>();
}

COMPARE_TEST(square)

template <typename float_type>
void run_cube_test(void)
{
    aligned_array<float_type, size> in;
    aligned_array<float_type, size> out;
    in[0] = -2;
    in[1] = in[2] = 0;
    in[3] = 2;

    nova::cube_vec_simd(out.begin(), in.begin(), size);

    BOOST_REQUIRE_EQUAL( out[0], -8 );
    BOOST_REQUIRE_EQUAL( out[1], 0 );
    BOOST_REQUIRE_EQUAL( out[2], 0 );
    BOOST_REQUIRE_EQUAL( out[3], 8 );

}

BOOST_AUTO_TEST_CASE( cube_test2 )
{
    run_cube_test<float>();
    run_cube_test<double>();
}

COMPARE_TEST(cube)

template <typename float_type>
void undenormalize_compare(void) {
    aligned_array<float_type, size> out, out_simd, out_mp, in;

    const float_type min_positive_value = std::numeric_limits<float_type>::min();
    for (int i = 0; i != size; ++i)
        in [i] = (i * min_positive_value * 2.0)/(float_type)size;

    nova::undenormalize_vec<float_type>(out.c_array(), in.c_array(), size);
    nova::undenormalize_vec_simd<float_type>(out_simd.c_array(), in.c_array(), size);
    nova::undenormalize_vec_simd<size>(out_mp.c_array(), in.c_array());
    compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-6f);
    compare_buffers(out.c_array(), out_mp.c_array(), size, 1e-6f);
}

BOOST_AUTO_TEST_CASE( undenormalize_tester )
{
    undenormalize_compare<float>();
    undenormalize_compare<double>();
}

template <typename float_type>
void reciprical_compare(void)
{
    aligned_array<float_type, size> out, out_simd, out_mp, in;

    for (int i = 0; i != size; ++i)
        in [i] = (i * 100)/(float_type)size + 1;

    nova::reciprocal_vec<float_type>(out.c_array(), in.c_array(), size);
    nova::reciprocal_vec_simd<float_type>(out_simd.c_array(), in.c_array(), size);
    nova::reciprocal_vec_simd<size>(out_mp.c_array(), in.c_array());
    compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-6f);
    compare_buffers(out.c_array(), out_mp.c_array(), size, 1e-6f);
}

BOOST_AUTO_TEST_CASE( reciprocal_tester )
{
    reciprical_compare<float>();
    reciprical_compare<double>();
}
