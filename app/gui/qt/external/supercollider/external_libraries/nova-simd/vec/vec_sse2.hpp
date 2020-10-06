//  sse2 vector class
//
//  Copyright (C) 2010 Tim Blechmann
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

#ifndef VEC_SSE2_HPP
#define VEC_SSE2_HPP

#include <algorithm>

#include <xmmintrin.h>
#include <emmintrin.h>

#ifdef __SSE4_1__
#include <smmintrin.h>
#endif

#ifdef __FMA__
#include <immintrin.h>
#endif

#include "../detail/vec_math.hpp"
#include "vec_base.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif


namespace nova
{

template <>
struct vec<double>:
    vec_base<double, __m128d, 2>
{
    typedef vec_base<double, __m128d, 2> base;

    typedef double float_type;
    typedef __m128d internal_vector_type;

    static inline __m128d gen_sign_mask(void)
    {
        __m128i x = _mm_setzero_si128();
#ifdef __SSE4_1__
        __m128i ones = _mm_cmpeq_epi64(x, x);
#else
        __m128i ones = _mm_cmpeq_epi32(x, x);
#endif
        return (__m128d)_mm_slli_epi64 (_mm_srli_epi64(ones, 63), 63);
    }

    static inline __m128d gen_abs_mask(void)
    {
        __m128i x = _mm_setzero_si128();
#ifdef __SSE4_1__
        __m128i ones = _mm_cmpeq_epi64(x, x);
#else
        __m128i ones = _mm_cmpeq_epi32(x, x);
#endif
        return (__m128d)_mm_srli_epi64 (_mm_slli_epi64(ones, 1), 1);
    }

    static inline __m128d gen_one(void)
    {
        return _mm_set1_pd(1.f);
    }

    static inline __m128d gen_05(void)
    {
        return _mm_set1_pd(0.5f);
    }

    static inline __m128d gen_zero(void)
    {
        return _mm_setzero_pd();
    }

    static inline __m128d gen_ones(void)
    {
        __m128d x = gen_zero();
        __m128d ones = _mm_cmpeq_pd(x, x);
        return ones;
    }


    vec(__m128d const & arg):
        base(arg)
    {}

public:
    static const int size = 2;
    static const int objects_per_cacheline = 64/sizeof(double);
    static const bool has_compare_bitmask = true;

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
        data_ = _mm_loadu_pd(data);
    }

    void load_aligned(const double * data)
    {
        data_ = _mm_load_pd(data);
    }

    void load_first(const double * data)
    {
        data_ = _mm_load_sd(data);
    }

    void store(double * dest) const
    {
        _mm_storeu_pd(dest, data_);
    }

    void store_aligned(double * dest) const
    {
        _mm_store_pd(dest, data_);
    }

    void store_aligned_stream(double * dest) const
    {
        _mm_stream_pd(dest, data_);
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
        data_ = _mm_set1_pd(value);
    }

    double set_slope(double start, double slope)
    {
        double v1 = start + slope;
        data_ = _mm_set_pd(v1, start);
        return slope + slope;
    }

    double set_exp(double start, double curve)
    {
        double v1 = start * curve;
        data_ = _mm_set_pd(v1, start);
        return v1 * curve;
    }

    double get (std::size_t index) const
    {
        __m128d ret;
        switch (index)
        {
        case 0:
            ret = data_;
            break;

        case 1:
            ret = _mm_shuffle_pd(data_, data_, _MM_SHUFFLE2(1, 1));
            break;
        }

        return _mm_cvtsd_f64(ret);
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

    OPERATOR_ASSIGNMENT(+=, _mm_add_pd)
    OPERATOR_ASSIGNMENT(-=, _mm_sub_pd)
    OPERATOR_ASSIGNMENT(*=, _mm_mul_pd)
    OPERATOR_ASSIGNMENT(/=, _mm_div_pd)

#define ARITHMETIC_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        return opcode(data_, rhs.data_); \
    }

    ARITHMETIC_OPERATOR(+, _mm_add_pd)
    ARITHMETIC_OPERATOR(-, _mm_sub_pd)
    ARITHMETIC_OPERATOR(*, _mm_mul_pd)
    ARITHMETIC_OPERATOR(/, _mm_div_pd)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(reciprocal)

#ifndef __FMA__
    NOVA_SIMD_DEFINE_MADD
#else
    inline friend vec madd(vec const & arg1, vec const & arg2, vec const & arg3)
    {
        return _mm_fmadd_pd(arg1.data_, arg2.data_, arg3.data_);
    }
#endif

#define RELATIONAL_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        const __m128d one = gen_one(); \
        return _mm_and_pd(opcode(data_, rhs.data_), one); \
    }

    RELATIONAL_OPERATOR(<, _mm_cmplt_pd)
    RELATIONAL_OPERATOR(<=, _mm_cmple_pd)
    RELATIONAL_OPERATOR(>, _mm_cmpgt_pd)
    RELATIONAL_OPERATOR(>=, _mm_cmpge_pd)
    RELATIONAL_OPERATOR(==, _mm_cmpeq_pd)
    RELATIONAL_OPERATOR(!=, _mm_cmpneq_pd)

    /* @{ */
#define BITWISE_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        return opcode(data_, rhs.data_); \
    }

    BITWISE_OPERATOR(&, _mm_and_pd)
    BITWISE_OPERATOR(|, _mm_or_pd)
    BITWISE_OPERATOR(^, _mm_xor_pd)

    friend vec andnot(vec const & lhs, vec const & rhs)
    {
        return _mm_andnot_pd(lhs.data_, rhs.data_);
    }

    #define RELATIONAL_MASK_OPERATOR(op, opcode) \
    friend vec mask_##op(vec const & lhs, vec const & rhs) \
    { \
        return opcode(lhs.data_, rhs.data_); \
    }

    RELATIONAL_MASK_OPERATOR(lt, _mm_cmplt_pd)
    RELATIONAL_MASK_OPERATOR(le, _mm_cmple_pd)
    RELATIONAL_MASK_OPERATOR(gt, _mm_cmpgt_pd)
    RELATIONAL_MASK_OPERATOR(ge, _mm_cmpge_pd)
    RELATIONAL_MASK_OPERATOR(eq, _mm_cmpeq_pd)
    RELATIONAL_MASK_OPERATOR(neq, _mm_cmpneq_pd)

    #undef RELATIONAL_MASK_OPERATOR

    friend inline vec select(vec lhs, vec rhs, vec bitmask)
    {
        /* if bitmask is set, return value in rhs, else value in lhs */
#ifdef __SSE4_1__
        return _mm_blendv_pd(lhs.data_, rhs.data_, bitmask.data_);
#else
        return detail::vec_select(lhs, rhs, bitmask);
#endif
    }

    /* @} */

    /* @{ */
    /** unary functions */
    friend inline vec abs(vec const & arg)
    {
        return _mm_and_pd(gen_abs_mask(), arg.data_);
    }

    friend always_inline vec sign(vec const & arg)
    {
        return detail::vec_sign(arg);
    }

    friend inline vec square(vec const & arg)
    {
        return _mm_mul_pd(arg.data_, arg.data_);
    }

    friend inline vec sqrt(vec const & arg)
    {
        return _mm_sqrt_pd(arg.data_);
    }

    friend inline vec cube(vec const & arg)
    {
        return _mm_mul_pd(arg.data_, _mm_mul_pd(arg.data_, arg.data_));
    }
    /* @} */

    /* @{ */
    /** binary functions */
    friend inline vec max_(vec const & lhs, vec const & rhs)
    {
        return _mm_max_pd(lhs.data_, rhs.data_);
    }

    friend inline vec min_(vec const & lhs, vec const & rhs)
    {
        return _mm_min_pd(lhs.data_, rhs.data_);
    }
    /* @} */

    /* @{ */
    /** rounding functions */
    friend inline vec round(vec const & arg)
    {
#ifdef __SSE4_1__
        return _mm_round_pd(arg.data_, _MM_FROUND_TO_NEAREST_INT);
#else
        return vec::round(arg);
#endif
    }

    friend inline vec frac(vec const & arg)
    {
        vec floor_result = floor(arg);
        return arg - floor_result;
    }

    friend inline vec floor(vec const & arg)
    {
#ifdef __SSE4_1__
        return _mm_round_pd(arg.data_, _MM_FROUND_TO_NEG_INF);
#else
        return vec::floor(arg);
#endif
    }

    friend inline vec ceil(vec const & arg)
    {
#ifdef __SSE4_1__
        return _mm_round_pd(arg.data_, _MM_FROUND_TO_POS_INF);
#else
        return vec::ceil(arg);
#endif
    }

    friend inline vec trunc(vec const & arg)
    {
#ifdef __SSE4_1__
        return _mm_round_pd(arg.data_, _MM_FROUND_TO_ZERO);
#else
        return vec::trunc(arg);
#endif
    }
    /* @} */


    /* @{ */
    /** mathematical functions */
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(pow)
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(signed_pow)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(sin)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(cos)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(tan)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(asin)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(acos)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(atan)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log2)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log10)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(exp)

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

    /* @{ */
    /** horizontal functions */
#define HORIZONTAL_OP(OP)                                        \
        __m128d data = data_;                       /* [0, 1] */ \
        __m128d high = _mm_unpackhi_pd(data, data); /* [1, 1] */ \
        __m128d accum = OP(data, high);                          \
        return _mm_cvtsd_f64(accum);


    inline double horizontal_min(void) const
    {
        HORIZONTAL_OP(_mm_min_sd);
    }

    inline double horizontal_max(void) const
    {
        HORIZONTAL_OP(_mm_max_sd);
    }

    inline double horizontal_sum(void) const
    {
        HORIZONTAL_OP(_mm_add_sd);
    }
    /* @} */

#undef HORIZONTAL_OP

};

} /* namespace nova */


#undef OPERATOR_ASSIGNMENT
#undef ARITHMETIC_OPERATOR
#undef RELATIONAL_OPERATOR
#undef always_inline

#endif /* VEC_SSE2_HPP */
