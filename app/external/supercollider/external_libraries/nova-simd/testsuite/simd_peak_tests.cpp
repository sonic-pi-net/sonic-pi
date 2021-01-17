#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../benchmarks/cache_aligned_array.hpp"
#include "../simd_peakmeter.hpp"

using namespace nova;
using namespace std;

static const int size = 64;

template <typename F>
void run_peak(void)
{
    aligned_array<F, size> in;
    in.assign(0);

    in[63] = -0.5;
    in[40] = 1;
    {
        F peak = 0;
        F last = nova::peak_vec_simd<F>(in.begin(), &peak, size);

        BOOST_REQUIRE_EQUAL( peak, 1 );
        BOOST_REQUIRE_EQUAL( last, 0.5 );
    }

    {
        F peak = 0;
        F last = nova::peak_vec<F>(in.begin(), &peak, size);

        BOOST_REQUIRE_EQUAL( peak, 1 );
        BOOST_REQUIRE_EQUAL( last, 0.5 );
    }

    {
        F peak = 0;
        F rms = 0;
        nova::peak_rms_vec_simd<F>(in.begin(), &peak, &rms, size);

        BOOST_REQUIRE_EQUAL( peak, 1 );
        BOOST_REQUIRE_EQUAL( rms, 1.25 );
    }

    {
        F peak = 0;
        F rms = 0;
        nova::peak_rms_vec<F>(in.begin(), &peak, &rms, size);

        BOOST_REQUIRE_EQUAL( peak, 1 );
        BOOST_REQUIRE_EQUAL( rms, 1.25 );
    }

}

BOOST_AUTO_TEST_CASE( peak_test_float )
{
    run_peak<float>();
}

BOOST_AUTO_TEST_CASE( peak_test_double )
{
    run_peak<double>();
}
