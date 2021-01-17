//  avx single-precision vector class
//
//  Copyright (C) 2011 Tim Blechmann
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
//  Boston, MA 02110-1301, USA.

#ifndef VEC_AVX_FLOAT_HPP
#define VEC_AVX_FLOAT_HPP

#include <immintrin.h>

#include "../detail/vec_math.hpp"
#include <numeric>

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

#include "vec_int_avx.hpp"
#include "vec_base.hpp"

namespace nova
{


template <>
struct vec<float>:
    vec_base<float, __m256, 8>
{
private:
    typedef vec_base<float, __m256, 8> base;

public:
    static const bool has_compare_bitmask = true;

    typedef __m256 internal_vector_type;
    typedef float float_type;

    /* SSE fallback */
    static inline __m256 gen_sign_mask(void)
    {
        return set_bitmask(0x80000000);
    }

    static inline __m256 gen_abs_mask(void)
    {
        return set_bitmask(0x7fffffff);
    }

    static inline __m256 gen_one(void)
    {
        return _mm256_set1_ps(1.f);
    }

    static inline __m256 gen_05(void)
    {
        return _mm256_set1_ps(0.5f);
    }

    static inline __m256 set_bitmask(unsigned int mask)
    {
        union {
            unsigned int i;
            float f;
        } u;
        u.i = mask;
        return _mm256_set1_ps(u.f);
    }

    static inline __m256 gen_exp_mask(void)
    {
        return set_bitmask(0x7F800000);
    }

    static inline __m256 gen_exp_mask_1(void)
    {
        return set_bitmask(0x3F000000);
    }

    static inline __m256 gen_ones(void)
    {
        __m256 x = gen_zero();
        __m256 ones = _mm256_cmp_ps(x, x, _CMP_EQ_OQ);
        return ones;
    }

    static inline __m256 gen_zero(void)
    {
        return _mm256_setzero_ps();
    }

    vec(__m256 const & arg):
        base(arg)
    {}

public:
    static const int size = 8;
    static const int objects_per_cacheline = 64/sizeof(float);

    static bool is_aligned(float* ptr)
    {
        return ((intptr_t)(ptr) & (intptr_t)(size * sizeof(float) - 1)) == 0;
    }

    /* @{ */
    /** constructors */
    vec(void)
    {}

    vec(float f)
    {
        set_vec(f);
    }

    vec(vec const & rhs)
    {
        data_ = rhs.data_;
    }
    /* @} */

    /* @{ */
    /** io */
    void load(const float * data)
    {
        data_ = _mm256_loadu_ps(data);
    }

    void load_aligned(const float * data)
    {
        data_ = _mm256_load_ps(data);
    }

    void load_first(const float * data)
    {
        clear();
        data_ = _mm256_castps128_ps256(_mm_load_ss(data));
    }

    void store(float * dest) const
    {
        _mm256_storeu_ps(dest, data_);
    }

    void store_aligned(float * dest) const
    {
        _mm256_store_ps(dest, data_);
    }

    void store_aligned_stream(float * dest) const
    {
        _mm256_stream_ps(dest, data_);
    }

    void clear(void)
    {
        data_ = gen_zero();
    }

    operator __m256(void) const
    {
        return data_;
    }

    /* @} */

    /* @{ */
    /** element access */
    void set_vec (float value)
    {
        data_ = _mm256_set1_ps(value);
    }

    float set_slope(float start, float slope)
    {
        float v1 = start + slope;
        float v2 = v1 + slope;
        float v3 = v2 + slope;
        float v4 = v3 + slope;
        float v5 = v4 + slope;
        float v6 = v5 + slope;
        float v7 = v6 + slope;
        data_ = _mm256_set_ps(v7, v6, v5, v4, v3, v2, v1, start);
        return slope * 8;
    }

    float set_exp(float start, float curve)
    {
        float v1 = start * curve;
        float v2 = v1 * curve;
        float v3 = v2 * curve;
        float v4 = v3 * curve;
        float v5 = v4 * curve;
        float v6 = v5 * curve;
        float v7 = v6 * curve;
        data_ = _mm256_set_ps(v7, v6, v5, v4,v3, v2, v1, start);
        return v7 * curve;
    }
    /* @} */

    /* @{ */
    /** arithmetic operators */
#define OPERATOR_ASSIGNMENT(op, opcode) \
    vec & operator op(vec const & rhs) \
    { \
        data_ = opcode(data_, rhs.data_);\
        return *this;\
    }

    OPERATOR_ASSIGNMENT(+=, _mm256_add_ps)
    OPERATOR_ASSIGNMENT(-=, _mm256_sub_ps)
    OPERATOR_ASSIGNMENT(*=, _mm256_mul_ps)
    OPERATOR_ASSIGNMENT(/=, _mm256_div_ps)

#undef OPERATOR_ASSIGNMENT

#define ARITHMETIC_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        return opcode(data_, rhs.data_); \
    } \
 \
    friend vec operator op(vec const & lhs, float f)  \
    { \
        return opcode(lhs.data_, vec(f).data_); \
    } \
    \
    friend vec operator op(float f, vec const & rhs)  \
    { \
        return opcode(vec(f).data_, rhs.data_); \
    }

    ARITHMETIC_OPERATOR(+, _mm256_add_ps)
    ARITHMETIC_OPERATOR(-, _mm256_sub_ps)
    ARITHMETIC_OPERATOR(*, _mm256_mul_ps)
    ARITHMETIC_OPERATOR(/, _mm256_div_ps)

#undef ARITHMETIC_OPERATOR

    friend vec operator -(const vec & arg)
    {
        return _mm256_xor_ps(arg.data_, gen_sign_mask());
    }

    friend vec fast_reciprocal(const vec & arg)
    {
        return _mm256_rcp_ps(arg.data_);
    }

    friend vec reciprocal(const vec & arg)
    {
        return detail::vec_reciprocal_newton(arg);
    }

#ifndef __FMA__
    NOVA_SIMD_DEFINE_MADD
#else
    inline friend vec madd(vec const & arg1, vec const & arg2, vec const & arg3)
    {
        return _mm256_fmadd_ps(arg1.data_, arg2.data_, arg3.data_);
    }
#endif

#define RELATIONAL_OPERATOR(op, RELATION) \
    vec operator op(vec const & rhs) const \
    { \
        const __m256 one = gen_one(); \
        return _mm256_and_ps(_mm256_cmp_ps(data_, rhs.data_, RELATION), one); \
    }

    RELATIONAL_OPERATOR(<, _CMP_LT_OS)
    RELATIONAL_OPERATOR(<=, _CMP_LE_OS)
    RELATIONAL_OPERATOR(>, _CMP_NLE_US)
    RELATIONAL_OPERATOR(>=, _CMP_NLT_US)
    RELATIONAL_OPERATOR(==, _CMP_EQ_OQ)
    RELATIONAL_OPERATOR(!=, _CMP_NEQ_UQ)
#undef RELATIONAL_OPERATOR

    /* @{ */
#define BITWISE_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        return opcode(data_, rhs.data_); \
    }

    BITWISE_OPERATOR(&, _mm256_and_ps)
    BITWISE_OPERATOR(|, _mm256_or_ps)
    BITWISE_OPERATOR(^, _mm256_xor_ps)

#undef BITWISE_OPERATOR

    friend inline vec andnot(vec const & lhs, vec const & rhs)
    {
        return _mm256_andnot_ps(lhs.data_, rhs.data_);
    }

#define RELATIONAL_MASK_OPERATOR(op, RELATION) \
    friend vec mask_##op(vec const & lhs, vec const & rhs) \
    { \
        return _mm256_cmp_ps(lhs.data_, rhs.data_, RELATION); \
    }

    RELATIONAL_MASK_OPERATOR(lt, _CMP_LT_OS)
    RELATIONAL_MASK_OPERATOR(le, _CMP_LE_OS)
    RELATIONAL_MASK_OPERATOR(gt, _CMP_NLE_US)
    RELATIONAL_MASK_OPERATOR(ge, _CMP_NLT_US)
    RELATIONAL_MASK_OPERATOR(eq, _CMP_EQ_OQ)
    RELATIONAL_MASK_OPERATOR(neq, _CMP_NEQ_UQ)

#undef RELATIONAL_MASK_OPERATOR

    friend inline vec select(vec lhs, vec rhs, vec bitmask)
    {
        /* if bitmask is set, return value in rhs, else value in lhs */
        return _mm256_blendv_ps(lhs.data_, rhs.data_, bitmask.data_);
    }

    /* @} */

    /* @{ */
    /** unary functions */
    friend inline vec abs(vec const & arg)
    {
        return _mm256_and_ps(gen_abs_mask(), arg.data_);
    }

    friend always_inline vec sign(vec const & arg)
    {
        return detail::vec_sign(arg);
    }

    friend inline vec square(vec const & arg)
    {
        return _mm256_mul_ps(arg.data_, arg.data_);
    }

    friend inline vec sqrt(vec const & arg)
    {
        return _mm256_sqrt_ps(arg.data_);
    }

    friend inline vec cube(vec const & arg)
    {
        return arg * arg * arg;
    }
    /* @} */

    /* @{ */
    /** binary functions */
    friend inline vec max_(vec const & lhs, vec const & rhs)
    {
        return _mm256_max_ps(lhs.data_, rhs.data_);
    }

    friend inline vec min_(vec const & lhs, vec const & rhs)
    {
        return _mm256_min_ps(lhs.data_, rhs.data_);
    }
    /* @} */

    /* @{ */
    /** rounding functions */
    friend inline vec round(vec const & arg)
    {
        return _mm256_round_ps(arg.data_, _MM_FROUND_TO_NEAREST_INT);
    }

    friend inline vec frac(vec const & arg)
    {
        vec floor_result = floor(arg);
        return arg - floor_result;
    }

    friend inline vec floor(vec const & arg)
    {
        return _mm256_round_ps(arg.data_, _MM_FROUND_TO_NEG_INF);
    }

    friend inline vec ceil(vec const & arg)
    {
        return _mm256_round_ps(arg.data_, _MM_FROUND_TO_POS_INF);
    }

    friend inline vec trunc(vec const & arg)
    {
        return _mm256_round_ps(arg.data_, _MM_FROUND_TO_ZERO);
    }
    /* @} */


    /* @{ */
    /** mathematical functions */
    friend inline vec exp(vec const & arg)
    {
        return detail::vec_exp_float(arg);
    }

    friend inline vec log(vec const & arg)
    {
        return detail::vec_log_float(arg);
    }

    friend inline vec pow(vec const & arg1, vec const & arg2)
    {
        return detail::vec_pow(arg1, arg2);
    }

    friend inline vec sin(vec const & arg)
    {
        return detail::vec_sin_float(arg);
    }

    friend inline vec cos(vec const & arg)
    {
        return detail::vec_cos_float(arg);
    }

    friend inline vec tan(vec const & arg)
    {
        return detail::vec_tan_float(arg);
    }

    friend inline vec asin(vec const & arg)
    {
        return detail::vec_asin_float(arg);
    }

    friend inline vec acos(vec const & arg)
    {
        return detail::vec_acos_float(arg);
    }

    friend inline vec atan(vec const & arg)
    {
        return detail::vec_atan_float(arg);
    }

    friend inline vec tanh(vec const & arg)
    {
        return detail::vec_tanh_float(arg);
    }

    friend inline vec signed_pow(vec const & lhs, vec const & rhs)
    {
        return detail::vec_signed_pow(lhs, rhs);
    }

    friend inline vec signed_sqrt(vec const & arg)
    {
        return detail::vec_signed_sqrt(arg);
    }

    friend inline vec log2(vec const & arg)
    {
        return detail::vec_log2(arg);
    }

    friend inline vec log10(vec const & arg)
    {
        return detail::vec_log10(arg);
    }

    friend inline vec undenormalize(vec const & arg)
    {
        return detail::vec_undenormalize(arg);
    }
    /* @} */

    /* @{ */
    typedef nova::detail::int_vec_avx int_vec;

    vec (int_vec const & rhs):
        base(_mm256_castsi256_ps(rhs.data_))
    {}

    int_vec truncate_to_int(void) const
    {
        __m256i int_val = _mm256_cvttps_epi32(data_);
        return int_vec(int_val);
    }
    /* @} */
};

} /* namespace nova */


#undef always_inline

#endif /* VEC_AVX_FLOAT_HPP */
