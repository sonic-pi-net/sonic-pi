#include <iostream>
#define BOOST_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include <cmath>

#include "../vec.hpp"

using namespace nova;
using namespace std;

template <typename F>
void test_get_set(void)
{
    vec<F> v;
    for (int i = 0; i != v.size; ++i)
        v.set(i, (F)1.0*i);

    for (int i = 0; i != v.size; ++i)
        BOOST_REQUIRE_EQUAL( v.get(i), F(1.0*i));

    vec<F> w;
    w.set_slope(0, 1.0);
    for (int i = 0; i != v.size; ++i)
        BOOST_REQUIRE_EQUAL( w.get(i), F(1.0*i));
}

template <typename VecType>
void compare_float_mask(VecType const & v, unsigned int mask)
{
   for (int i = 0; i != v.size; ++i) {
        union {
            float f;
            unsigned int i;
        } x;
        x.f = v.get(i);
        BOOST_REQUIRE_EQUAL( x.i, mask);
    }
}

void test_gen_float(void)
{
    typedef vec<float> vec_t;
#if defined(__SSE__) || defined (__AVX__)
    vec_t zero = vec_t::gen_zero();
    for (int i = 0; i != zero.size; ++i)
        BOOST_REQUIRE_EQUAL( zero.get(i), 0.f);

    vec_t one = vec_t::gen_one();
    for (int i = 0; i != one.size; ++i)
        BOOST_REQUIRE_EQUAL( one.get(i), 1.f);

    vec_t half = vec_t::gen_05();
    for (int i = 0; i != one.size; ++i)
        BOOST_REQUIRE_EQUAL( half.get(i), 0.5f);

    vec_t sign_mask = vec_t::gen_sign_mask();
    compare_float_mask(sign_mask, 0x80000000);

    vec_t abs_mask = vec_t::gen_abs_mask();
    compare_float_mask(abs_mask, 0x7fffffff);

    vec_t exp_mask = vec_t::gen_exp_mask();
    compare_float_mask(exp_mask, 0x7F800000);

    vec_t exp1_mask = vec_t::gen_exp_mask_1();
    compare_float_mask(exp1_mask, 0x3F000000);
#endif
}

BOOST_AUTO_TEST_CASE( get_set )
{
    test_get_set<float>();
    test_get_set<double>();
    test_gen_float();
}

template <typename T>
void test_slope(void)
{
    typedef vec<T> vec_t;
    vec_t vec;
    T increment = vec.set_slope(0, 1);
    T expected_increment = vec_t::size * 1;
    BOOST_REQUIRE_CLOSE_FRACTION( increment, expected_increment, 0.001);

    for (int i = 0; i != vec_t::size; ++i)
        BOOST_REQUIRE_CLOSE_FRACTION( vec.get(i), i, 0.001);
}

BOOST_AUTO_TEST_CASE( slope )
{
    test_slope<float>();
    test_slope<double>();
}

template <typename T>
void test_select(void)
{
    typedef vec<T> vec_t;
    vec_t a(1.0), b(2.0);
    vec_t zero = vec_t::gen_zero();
    vec_t ones = vec_t::gen_ones();

    vec_t select_a = select(a, b, zero);
    vec_t select_b = select(a, b, ones);

    BOOST_REQUIRE(select_a.get(0) == 1);
    BOOST_REQUIRE(select_b.get(0) == 2);
}

BOOST_AUTO_TEST_CASE( select_tester )
{
    test_select<float>();
    test_select<double>();
}

BOOST_AUTO_TEST_CASE( align_ )
{
    BOOST_REQUIRE(vec<double>::is_aligned(NULL));
    BOOST_REQUIRE(vec<float>::is_aligned(NULL));
    BOOST_REQUIRE(!vec<double>::is_aligned((double*)(1)));
    BOOST_REQUIRE(!vec<float>::is_aligned((float*)(1)));
}

template <typename T>
void test_undenormalize(void)
{
    const T min_positive_value = std::numeric_limits<T>::min();
    typedef vec<T> vec_t;
    vec_t denormal(min_positive_value/2);

    vec_t fixed = undenormalize(denormal);
    BOOST_REQUIRE(fixed.get(0) == 0);

    vec_t normal(min_positive_value);
    BOOST_REQUIRE(normal.get(0) == min_positive_value);
}

BOOST_AUTO_TEST_CASE( undenormalize_tester )
{
    test_undenormalize<float>();
    test_undenormalize<double>();
}

template <typename T>
void test_reciprocal(void)
{
    typedef vec<T> vec_t;
    vec_t four(4.0);
    T result = reciprocal(four).get(0);
    BOOST_REQUIRE_CLOSE(result, 0.25, 1e-5);
}


BOOST_AUTO_TEST_CASE( reciprocal_tester )
{
    test_reciprocal<float>();
    test_reciprocal<double>();
}
