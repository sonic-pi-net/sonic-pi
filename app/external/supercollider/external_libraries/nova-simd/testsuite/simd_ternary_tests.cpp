#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "test_helper.hpp"

#include "../benchmarks/cache_aligned_array.hpp"
#include "../simd_ternary_arithmetic.hpp"

using namespace nova;
using namespace std;

static const int unsigned size = 64;

#define COMPARE_TEST(function)                                          \
    template <typename float_type>                                      \
    void function##_compare_vvv(void)                                   \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp,          \
            in0, in1, in2;                                              \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        randomize_buffer<float_type>(in1.c_array(), size, -1);          \
        randomize_buffer<float_type>(in2.c_array(), size, +1);          \
                                                                        \
        function##_vec<float_type>(out.c_array(),                       \
                                   wrap_argument(in0.c_array()),        \
                                   wrap_argument(in1.c_array()),        \
                                   wrap_argument(in2.c_array()),        \
                                   size);                               \
                                                                        \
        function##_vec_simd<float_type>(out_simd.c_array(),             \
                                        wrap_argument(in0.c_array()),   \
                                        wrap_argument(in1.c_array()),   \
                                        wrap_argument(in2.c_array()),   \
                                        size);                          \
                                                                        \
        function##_vec_simd<size>(out_mp.c_array(),                     \
                                  wrap_argument(in0.c_array()),         \
                                  wrap_argument(in1.c_array()),         \
                                  wrap_argument(in2.c_array()));        \
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-4f); \
        compare_buffers(out.c_array(), out_mp.c_array(), size, 1e-4f);  \
    }                                                                   \
    template <typename float_type>                                      \
    void function##_compare_vvs(void)                                   \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp,          \
            in0, in1;                                                   \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        randomize_buffer<float_type>(in1.c_array(), size, -1);          \
        float_type in2 = randomize_float<float_type>();                 \
                                                                        \
        function##_vec<float_type>(out.c_array(),                       \
                                   wrap_argument(in0.c_array()),        \
                                   wrap_argument(in1.c_array()),        \
                                   wrap_argument(in2), size);           \
        function##_vec_simd<float_type>(out_simd.c_array(),             \
                                        wrap_argument(in0.c_array()),   \
                                        wrap_argument(in1.c_array()),   \
                                        wrap_argument(in2),             \
                                        size);                          \
        function##_vec_simd<size>(out_mp.c_array(),                     \
                                  wrap_argument(in0.c_array()),         \
                                  wrap_argument(in1.c_array()),         \
                                  wrap_argument(in2));                  \
                                                                        \
        compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-4f); \
        compare_buffers(out.c_array(), out_mp.c_array(), size, 1e-4f);  \
    }                                                                   \
    template <typename float_type>                                      \
    void function##_compare_vvr(void)                                   \
    {                                                                   \
        aligned_array<float_type, size> out, out_simd, out_mp,           \
            in0, in1;                                                   \
        randomize_buffer<float_type>(in0.c_array(), size);              \
        randomize_buffer<float_type>(in1.c_array(), size, -1);          \
        float_type in2 = randomize_float<float_type>() + 2;             \
        float_type in2_slope = randomize_float<float_type>() * 0.0001;  \
                                                                        \
        function##_vec<float_type>(out.c_array(),                       \
                                   wrap_argument(in0.c_array()),        \
                                   wrap_argument(in1.c_array()),        \
                                   wrap_argument(in2, in2_slope),       \
                                   size);                               \
        function##_vec_simd<float_type>(out_simd.c_array(),             \
                                        wrap_argument(in0.c_array()),   \
                                        wrap_argument(in1.c_array()),   \
                                        wrap_argument(in2, in2_slope),  \
                                        size);                          \
        function##_vec_simd<size>(out_mp.c_array(),                     \
                                  wrap_argument(in0.c_array()),         \
                                  wrap_argument(in1.c_array()),         \
                                  wrap_argument(in2, in2_slope));       \
                                                                        \
        compare_buffers_relative(out.c_array(), out_simd.c_array(),     \
                                 size);                                 \
        compare_buffers_relative(out.c_array(), out_mp.c_array(),       \
                                 size);                                 \
    }                                                                   \
                                                                        \
    BOOST_AUTO_TEST_CASE( function##_comparer )                         \
    {                                                                   \
        function##_compare_vvv<float>();                                \
        function##_compare_vvv<double>();                               \
        function##_compare_vvs<float>();                                \
        function##_compare_vvs<double>();                               \
        function##_compare_vvr<float>();                                \
        function##_compare_vvr<double>();                               \
    }

COMPARE_TEST(clip)
COMPARE_TEST(muladd)
