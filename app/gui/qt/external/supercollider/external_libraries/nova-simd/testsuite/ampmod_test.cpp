#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../simd_ternary_arithmetic.hpp"
#include "../benchmarks/cache_aligned_array.hpp"
#include "test_helper.hpp"

using namespace nova;
using namespace std;

static const unsigned int size = 64;

template <typename float_type>
void vv_tests(void)
{
    aligned_array<float_type, size> out, out_simd, in0, in1, amount;
    randomize_buffer<float_type>(in0.c_array(), size);
    randomize_buffer<float_type>(in1.c_array(), size);
    randomize_buffer<float_type>(amount.c_array(), size);

    ampmod_vec<float_type>(out.c_array(), wrap_argument(in0.c_array()),
                        wrap_argument(in1.c_array()),
                        wrap_argument(amount.c_array()), size);

    ampmod_vec_simd<float_type>(out_simd.c_array(), wrap_argument(in0.c_array()),
                             wrap_argument(in1.c_array()), wrap_argument(amount.c_array()), size);

    compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-6);
}

BOOST_AUTO_TEST_CASE( vv_test )
{
    vv_tests<float>();
    vv_tests<double>();
}

template <typename float_type>
void vs_tests(void)
{
    aligned_array<float_type, size> out, out_simd, in0, in1;
    float_type amount;
    randomize_buffer<float_type>(in0.c_array(), size);
    randomize_buffer<float_type>(in1.c_array(), size);
    amount = randomize_float<float_type>();

    ampmod_vec<float_type>(out.c_array(), wrap_argument(in0.c_array()),
                        wrap_argument(in1.c_array()),
                        wrap_argument(amount), size);
    ampmod_vec_simd<float_type>(out_simd.c_array(), wrap_argument(in0.c_array()),
                             wrap_argument(in1.c_array()), wrap_argument(amount), size);

    compare_buffers(out.c_array(), out_simd.c_array(), size, 1e-6);
}

BOOST_AUTO_TEST_CASE( vs_test )
{
    vs_tests<float>();
    vs_tests<double>();
}
