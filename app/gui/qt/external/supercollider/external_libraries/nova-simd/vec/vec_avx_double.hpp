//  avx double-precision vector class
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

#ifndef VEC_AVX_DOUBLE_HPP
#define VEC_AVX_DOUBLE_HPP

#include <algorithm>

#include <immintrin.h>

#include "../detail/vec_math.hpp"
#include "vec_base.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

#include "stdint.h"

namespace nova
{



template <>
struct vec<double>:
    vec_base<double, __m256d, 4>
{
private:
    typedef vec_base<double, __m256d, 4> base;
    static const bool has_compare_bitmask = true;

public:
    typedef double float_type;

    static inline __m256d gen_sign_mask(void)
    {
        return set_bitmask(0x8000000000000000);
    }

    static inline __m256d gen_abs_mask(void)
    {
        return set_bitmask(0x7fffffffffffffff);
    }

    static inline __m256d set_bitmask(uint64_t mask)
    {
        union {
            uint64_t i;
            double d;
        } u;
        u.i = mask;
        return _mm256_set1_pd(u.d);
    }

    static inline __m256d gen_one(void)
    {
        return _mm256_set1_pd(1.f);
    }

    static inline __m256d gen_05(void)
    {
        return _mm256_set1_pd(0.5f);
    }

    static inline __m256d gen_zero(void)
    {
        return _mm256_setzero_pd();
    }

    static inline __m256d gen_ones(void)
    {
        __m256d x = gen_zero();
        __m256d ones = _mm256_cmp_pd(x, x, _CMP_EQ_OQ);
        return ones;
    }

    vec(__m256d const & arg):
        base(arg)
    {}

public:
    static const int size = 4;
    static const int objects_per_cacheline = 64/sizeof(double);

    static bool is_aligned(double* ptr)
    {
        return ((intptr_t)(ptr) & (intptr_t)(size * sizeof(double) - 1)) == 0;
    }

    /* @{ */
    /** constructors */
    vec(void)
    {}

    vec(double f)
    {
        set_vec(f);
    }

    vec(float f)
    {
        set_vec((double)f);
    }

    vec(vec const & rhs)
    {
        data_ = rhs.data_;
    }
    /* @} */

    /* @{ */
    /** io */
    void load(const double * data)
    {
        data_ = _mm256_loadu_pd(data);
    }

    void load_aligned(const double * data)
    {
        data_ = _mm256_load_pd(data);
    }

    void load_first(const double * data)
    {
        clear();
        data_ = _mm256_castpd128_pd256(_mm_load_sd(data));
    }

    void store(double * dest) const
    {
        _mm256_storeu_pd(dest, data_);
    }

    void store_aligned(double * dest) const
    {
        _mm256_store_pd(dest, data_);
    }

    void store_aligned_stream(double * dest) const
    {
        _mm256_stream_pd(dest, data_);
    }

    void clear(void)
    {
        data_ = gen_zero();
    }

    /* @} */

    /* @{ */
    /** element access */
    void set_vec (double value)
    {
        data_ = _mm256_set1_pd(value);
    }

    double set_slope(double start, double slope)
    {
        double v1 = start + slope;
        double v2 = v1 + slope;
        double v3 = v2 + slope;
        data_ = _mm256_set_pd(v3, v2, v1, start);
        return slope + slope + slope + slope;
    }

    double set_exp(double start, double curve)
    {
        double v1 = start * curve;
        double v2 = v1 * curve;
        double v3 = v2 * curve;
        data_ = _mm256_set_pd(v3, v2, v1, start);
        return v3 * curve;
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

    OPERATOR_ASSIGNMENT(+=, _mm256_add_pd)
    OPERATOR_ASSIGNMENT(-=, _mm256_sub_pd)
    OPERATOR_ASSIGNMENT(*=, _mm256_mul_pd)
    OPERATOR_ASSIGNMENT(/=, _mm256_div_pd)

#undef OPERATOR_ASSIGNMENT

#define ARITHMETIC_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        return opcode(data_, rhs.data_); \
    }

    ARITHMETIC_OPERATOR(+, _mm256_add_pd)
    ARITHMETIC_OPERATOR(-, _mm256_sub_pd)
    ARITHMETIC_OPERATOR(*, _mm256_mul_pd)
    ARITHMETIC_OPERATOR(/, _mm256_div_pd)

#undef ARITHMETIC_OPERATOR

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(reciprocal)


#ifndef __FMA__
    NOVA_SIMD_DEFINE_MADD
#else
    inline friend vec madd(vec const & arg1, vec const & arg2, vec const & arg3)
    {
        return _mm256_fmadd_pd(arg1.data_, arg2.data_, arg3.data_);
    }
#endif

#define RELATIONAL_OPERATOR(op, RELATION) \
    vec operator op(vec const & rhs) const \
    { \
        const __m256d one = gen_one(); \
        return _mm256_and_pd(_mm256_cmp_pd(data_, rhs.data_, RELATION), one); \
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

    BITWISE_OPERATOR(&, _mm256_and_pd)
    BITWISE_OPERATOR(|, _mm256_or_pd)
    BITWISE_OPERATOR(^, _mm256_xor_pd)

#undef BITWISE_OPERATOR

#define RELATIONAL_MASK_OPERATOR(op, RELATION) \
    friend vec mask_##op(vec const & lhs, vec const & rhs) \
    { \
        return _mm256_cmp_pd(lhs.data_, rhs.data_, RELATION); \
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
        return _mm256_blendv_pd(lhs.data_, rhs.data_, bitmask.data_);
    }

    /* @} */

    /* @{ */
    /** unary functions */
    friend inline vec abs(vec const & arg)
    {
        return _mm256_and_pd(gen_abs_mask(), arg.data_);
    }

    friend always_inline vec sign(vec const & arg)
    {
        return detail::vec_sign(arg);
    }

    friend inline vec square(vec const & arg)
    {
        return _mm256_mul_pd(arg.data_, arg.data_);
    }

    friend inline vec sqrt(vec const & arg)
    {
        return _mm256_sqrt_pd(arg.data_);
    }

    friend inline vec cube(vec const & arg)
    {
        return _mm256_mul_pd(arg.data_, _mm256_mul_pd(arg.data_, arg.data_));
    }
    /* @} */

    /* @{ */
    /** binary functions */
    friend inline vec max_(vec const & lhs, vec const & rhs)
    {
        return _mm256_max_pd(lhs.data_, rhs.data_);
    }

    friend inline vec min_(vec const & lhs, vec const & rhs)
    {
        return _mm256_min_pd(lhs.data_, rhs.data_);
    }
    /* @} */

    /* @{ */
    /** rounding functions */
    friend inline vec round(vec const & arg)
    {
        return _mm256_round_pd(arg.data_, _MM_FROUND_TO_NEAREST_INT);
    }

    friend inline vec frac(vec const & arg)
    {
        vec floor_result = floor(arg);
        return arg - floor_result;
    }

    friend inline vec floor(vec const & arg)
    {
        return _mm256_round_pd(arg.data_, _MM_FROUND_TO_NEG_INF);
    }

    friend inline vec ceil(vec const & arg)
    {
        return _mm256_round_pd(arg.data_, _MM_FROUND_TO_POS_INF);
    }

    friend inline vec trunc(vec const & arg)
    {
        return _mm256_round_pd(arg.data_, _MM_FROUND_TO_ZERO);
    }
    /* @} */


    /* @{ */
    /** mathematical functions */
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(pow)
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(signed_pow)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log2)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log10)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(exp)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(sin)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(cos)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(tan)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(asin)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(acos)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(atan)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(tanh)

    friend inline vec signed_sqrt(vec const & arg)
    {
        return detail::vec_signed_sqrt(arg);
    }
    friend inline vec undenormalize(vec const & arg)
    {
        return detail::vec_undenormalize(arg);
    }
    /* @} */
};

} /* namespace nova */


#undef OPERATOR_ASSIGNMENT
#undef ARITHMETIC_OPERATOR
#undef RELATIONAL_OPERATOR
#undef always_inline

#endif /* VEC_AVX_DOUBLE_HPP */
