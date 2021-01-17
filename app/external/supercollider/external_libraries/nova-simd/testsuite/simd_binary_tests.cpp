#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "test_helper.hpp"

#include "../benchmarks/cache_aligned_array.hpp"
#include "../simd_binary_arithmetic.hpp"

using namespace nova;
using namespace std;

static const unsigned int size = 64;

#define COMPARE_TEST(function)                                          \
    template <typename float_type>                                      \
    void function##_compare_vv(void)                                    \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp,          \
            in0, in1;                                                   \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        randomize_buffer<float_type>(in1.c_array(), size);              \
                                                                        \
        function##_vec<float_type>(out.c_array(), in0.c_array(),        \
                                   in1.c_array(), size);                \
        function##_vec_simd<float_type>(out_simd.c_array(),             \
                                        in0.c_array(),                  \
                                        in1.c_array(), size);           \
        function##_vec_simd<size>(out_mp.c_array(), in0.c_array(),      \
                                        in1.c_array());                 \
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size);       \
        compare_buffers(out.c_array(), out_mp.c_array(), size);         \
    }                                                                   \
                                                                        \
    template <typename float_type>                                      \
    void function##_compare_vs(void)                                    \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp, in0;     \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        float_type in1 = randomize_float<float_type>();                 \
                                                                        \
        function##_vec<float_type>(out.c_array(), in0.c_array(),        \
                                   in1, size);                          \
        function##_vec_simd<float_type>(out_simd.c_array(),             \
                                        in0.c_array(),                  \
                                        in1, size);                     \
        function##_vec_simd<size>(out_mp.c_array(), in0.c_array(),      \
                                  in1);                                 \
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size);       \
        compare_buffers(out.c_array(), out_mp.c_array(), size);         \
    }                                                                   \
                                                                        \
    template <typename float_type>                                      \
    void function##_compare_sv(void)                                    \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp, in0;     \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        float_type in1 = randomize_float<float_type>();                 \
                                                                        \
        function##_vec<float_type>(out.c_array(), in1,                  \
                                   in0.c_array(), size);                \
        function##_vec_simd<float_type>(out_simd.c_array(), in1,        \
                                        in0.c_array(), size);           \
        function##_vec_simd<size>(out_mp.c_array(), in1, in0.c_array()); \
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size);       \
        compare_buffers(out.c_array(), out_mp.c_array(), size);         \
    }                                                                   \
                                                                        \
    template <typename float_type>                                      \
    void function##_compare_vr(void)                                    \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp, in0;     \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        float_type in1 = randomize_float<float_type>();                 \
        float_type in1_slope = randomize_float_slope<float_type>();     \
                                                                        \
        function##_vec<float_type>(out.c_array(), in0.c_array(),        \
                                   slope_argument(in1, in1_slope), size); \
        function##_vec_simd<float_type>(out_simd.c_array(),             \
                                        in0.c_array(),                  \
                                        slope_argument(in1, in1_slope), size); \
        function##_vec_simd<size>(out_mp.c_array(), in0.c_array(),      \
                                  slope_argument(in1, in1_slope));      \
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size, 5e-4); \
        compare_buffers(out.c_array(), out_mp.c_array(), size, 5e-4);   \
    }                                                                   \
                                                                        \
    template <typename float_type>                                      \
    void function##_compare_rv(void)                                    \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp, in0;     \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        float_type in1 = randomize_float<float_type>();                 \
        float_type in1_slope = randomize_float_slope<float_type>();     \
                                                                        \
        function##_vec<float_type>(out.c_array(), slope_argument(in1, in1_slope), \
                                   in0.c_array(), size);                \
        function##_vec_simd<float_type>(out_simd.c_array(),             \
                                        slope_argument(in1, in1_slope), \
                                        in0.c_array(), size);           \
        function##_vec_simd<size>(out_mp.c_array(), slope_argument(in1, in1_slope), \
                                  in0.c_array());                       \
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size, 5e-4); \
        compare_buffers(out.c_array(), out_mp.c_array(), size, 5e-4);   \
    }                                                                   \
                                                                        \
    BOOST_AUTO_TEST_CASE( function##_comparer_vv )                      \
    {                                                                   \
        function##_compare_vv<float>();                                 \
        function##_compare_vv<double>();                                \
    }                                                                   \
                                                                        \
    BOOST_AUTO_TEST_CASE( function##_comparer_vs )                      \
    {                                                                   \
        function##_compare_vs<float>();                                 \
        function##_compare_vs<double>();                                \
    }                                                                   \
                                                                        \
    BOOST_AUTO_TEST_CASE( function##_comparer_vr )                      \
    {                                                                   \
        function##_compare_vr<float>();                                 \
        function##_compare_vr<double>();                                \
    }                                                                   \
                                                                        \
    BOOST_AUTO_TEST_CASE( function##_comparer_sv )                      \
    {                                                                   \
        function##_compare_sv<float>();                                 \
        function##_compare_sv<double>();                                \
    }                                                                   \
                                                                        \
    BOOST_AUTO_TEST_CASE( function##_comparer_rv )                      \
    {                                                                   \
        function##_compare_rv<float>();                                 \
        function##_compare_rv<double>();                                \
    }


COMPARE_TEST(plus)
COMPARE_TEST(minus)
COMPARE_TEST(times)
COMPARE_TEST(over)
COMPARE_TEST(min)
COMPARE_TEST(max)
COMPARE_TEST(less)
COMPARE_TEST(less_equal)
COMPARE_TEST(greater)
COMPARE_TEST(greater_equal)
COMPARE_TEST(equal)
COMPARE_TEST(notequal)
COMPARE_TEST(clip2)
