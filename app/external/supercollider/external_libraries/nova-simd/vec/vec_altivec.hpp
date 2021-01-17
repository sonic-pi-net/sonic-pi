//  altivec vector class
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

#ifndef VEC_ALTIVEC_HPP
#define VEC_ALTIVEC_HPP

#include <altivec.h>
#undef bool
#undef pixel
#undef vector

#include "../detail/vec_math.hpp"
#include "vec_int_altivec.hpp"
#include "../detail/math.hpp"
#include "vec_base.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova
{

template <>
struct vec<float>:
    vec_base<float, __vector float, 4>
{
    typedef __vector float internal_vector_type;
    typedef float float_type;

private:
    typedef vec_base<float, __vector float, 4> base;

    static internal_vector_type set_vector(float f0, float f1, float f2, float f3)
    {
        union {
            float f[4];
            internal_vector_type v;
        } ret;

        ret.f[0] = f0;
        ret.f[1] = f1;
        ret.f[2] = f2;
        ret.f[3] = f3;
        return ret.v;
    }

    static internal_vector_type set_vector(float f)
    {
        return set_vector(f, f, f, f);
    }

public:
    static const bool has_compare_bitmask = true;

    static inline internal_vector_type gen_sign_mask(void)
    {
        return set_bitmask(0x80000000);
    }

    static inline internal_vector_type gen_abs_mask(void)
    {
        return set_bitmask(0x7fffffff);
    }

    static inline internal_vector_type gen_one(void)
    {
        return set_vector(1.f);
    }

    static inline internal_vector_type gen_05(void)
    {
        return set_vector(0.5f);
    }

    static inline internal_vector_type set_bitmask(unsigned int mask)
    {
        union {
            unsigned int i;
            float f;
        } u;
        u.i = mask;
        return set_vector(u.f);
    }

    static inline internal_vector_type gen_exp_mask(void)
    {
        return set_bitmask(0x7F800000);
    }

    static inline internal_vector_type gen_exp_mask_1(void)
    {
        return set_bitmask(0x3F000000);
    }

    static inline internal_vector_type gen_ones(void)
    {
        return set_bitmask(0xFFFFFFFF);
    }

    static inline internal_vector_type gen_zero(void)
    {
        return (internal_vector_type)vec_splat_u32(0);
    }

    vec(internal_vector_type const & arg):
        base(arg)
    {}

public:
    static const int size = 4;
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

    vec(vec const & rhs):
        base(rhs.data_)
    {}
    /* @} */

    /* @{ */
    /** io */
    void load(const float * data)
    {
        base::data_ = vec_ld(0, data);
    }

    void load_aligned(const float * data)
    {
        base::data_ = vec_ld(0, data);
    }

    void load_first(const float * data)
    {
        clear();
        base::set(0, *data);
    }

    void store(float * dest) const
    {
        vec_st(base::data_, 0, dest);
    }

    void store_aligned(float * dest) const
    {
        vec_st(base::data_, 0, dest);
    }

    void store_aligned_stream(float * dest) const
    {
        vec_st(base::data_, 0, dest);
    }

    void clear(void)
    {
        base::data_ = gen_zero();
    }

    operator internal_vector_type (void) const
    {
        return base::data_;
    }

    /* @} */

    /* @{ */
    /** element access */
    void set_vec (float value)
    {
        data_ = set_vector(value, value, value, value);
    }

    float set_slope(float start, float slope)
    {
        float v1 = start + slope;
        float v2 = start + slope + slope;
        float v3 = start + slope + slope + slope;
        data_ = set_vector(start, v1, v2, v3);
        return slope + slope + slope + slope;
    }

    float set_exp(float start, float curve)
    {
        float v1 = start * curve;
        float v2 = v1 * curve;
        float v3 = v2 * curve;
        data_ = set_vector(start, v1, v2, v3);
        return v3 * curve;
    }
    /* @} */

    /* @{ */

private:
    static internal_vector_type vec_mul(internal_vector_type const & lhs, internal_vector_type const & rhs)
    {
        return vec_madd(lhs, rhs, gen_zero());
    }

    static internal_vector_type vec_reciprocal(internal_vector_type const & arg)
    {
        // adapted from http://developer.apple.com/hardwaredrivers/ve/algorithms.html

        // Get the reciprocal estimate
        __vector float estimate = vec_re(arg);

        // One round of Newton-Raphson refinement
        return vec_madd(vec_nmsub(estimate, arg, gen_one()), estimate, estimate);
    }

    static internal_vector_type vec_div(internal_vector_type const & lhs, internal_vector_type const & rhs)
    {
        return vec_mul(lhs, vec_reciprocal(rhs));
    }

public:
    /** arithmetic operators */
#define OPERATOR_ASSIGNMENT(op, opcode) \
    vec & operator op(vec const & rhs) \
    { \
        data_ = opcode(data_, rhs.data_);\
        return *this;\
    }

    OPERATOR_ASSIGNMENT(+=, vec_add)
    OPERATOR_ASSIGNMENT(-=, vec_sub)
    OPERATOR_ASSIGNMENT(*=, vec_mul)
    OPERATOR_ASSIGNMENT(/=, vec_div)

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

    ARITHMETIC_OPERATOR(+, vec_add)
    ARITHMETIC_OPERATOR(-, vec_sub)
    ARITHMETIC_OPERATOR(*, vec_mul)
    ARITHMETIC_OPERATOR(/, vec_div)

    friend vec operator -(const vec & arg)
    {
        return vec_xor(arg.data_, gen_sign_mask());
    }

    friend vec fast_reciprocal(const vec & arg)
    {
        __vector float estimate = vec_re(arg.data_);
        return estimate;
    }

    friend vec reciprocal(const vec & arg)
    {
        return vec_reciprocal(arg.data_);
    }

    friend vec madd(vec const & arg1, vec const & arg2, vec const & arg3)
    {
        return vec_madd(arg1.data_, arg2.data_, arg3.data_);
    }

private:
    static internal_vector_type vec_not(internal_vector_type const & arg)
    {
        return vec_nor(arg, arg);
    }

    static internal_vector_type vec_cmpneq(internal_vector_type const & lhs, internal_vector_type const & rhs)
    {
        internal_vector_type equal = (internal_vector_type)vec_cmpeq(lhs, rhs);
        return vec_not(equal);
    }

public:

#define RELATIONAL_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        const internal_vector_type one = gen_one(); \
        __vector unsigned int mask = (__vector unsigned int)opcode(data_, rhs.data_); \
        return (internal_vector_type)vec_and(mask, (__vector unsigned int)one); \
    }

#define vec_cmple_(a, b) vec_cmpge(b, a)

    RELATIONAL_OPERATOR(<, vec_cmplt)
    RELATIONAL_OPERATOR(<=, vec_cmple_)
    RELATIONAL_OPERATOR(>, vec_cmpgt)
    RELATIONAL_OPERATOR(>=, vec_cmpge)
    RELATIONAL_OPERATOR(==, vec_cmpeq)
    RELATIONAL_OPERATOR(!=, vec_cmpneq)


#undef RELATIONAL_OPERATOR

    /* @{ */
#define BITWISE_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        return opcode(data_, rhs.data_); \
    }

    BITWISE_OPERATOR(&, vec_and)
    BITWISE_OPERATOR(|, vec_or)
    BITWISE_OPERATOR(^, vec_xor)

    friend inline vec andnot(vec const & lhs, vec const & rhs)
    {
        return vec_andc(lhs.data_, rhs.data_);
    }

#undef BITWISE_OPERATOR

#define RELATIONAL_MASK_OPERATOR(op, opcode) \
    friend vec mask_##op(vec const & lhs, vec const & rhs) \
    { \
        return internal_vector_type(opcode(lhs.data_, rhs.data_)); \
    }

    RELATIONAL_MASK_OPERATOR(lt, vec_cmplt)
    RELATIONAL_MASK_OPERATOR(le, vec_cmple_)
    RELATIONAL_MASK_OPERATOR(gt, vec_cmpgt)
    RELATIONAL_MASK_OPERATOR(ge, vec_cmpge)
    RELATIONAL_MASK_OPERATOR(eq, vec_cmpeq)
    RELATIONAL_MASK_OPERATOR(neq, vec_cmpneq)

#undef RELATIONAL_MASK_OPERATOR

    friend inline vec select(vec lhs, vec rhs, vec bitmask)
    {
        return vec_sel(lhs.data_, rhs.data_, (__vector unsigned int)bitmask.data_);
    }

    /* @} */

    /* @{ */
    /** unary functions */
    friend inline vec abs(vec const & arg)
    {
        return vec_abs(arg.data_);
    }

    friend always_inline vec sign(vec const & arg)
    {
        return detail::vec_sign(arg);
    }

    friend inline vec square(vec const & arg)
    {
        return vec_mul(arg.data_, arg.data_);
    }

private:
    static internal_vector_type vec_rsqrt(internal_vector_type const & arg)
    {
        // adapted from http://developer.apple.com/hardwaredrivers/ve/algorithms.html

        //Get the square root reciprocal estimate
        __vector float zero =    gen_zero();
        __vector float oneHalf = gen_05();
        __vector float one =     gen_one();
        __vector float estimate = vec_rsqrte(arg);

        //One round of Newton-Raphson refinement
        __vector float estimateSquared = vec_madd(estimate, estimate, zero);
        __vector float halfEstimate = vec_madd(estimate, oneHalf, zero);
        return vec_madd(vec_nmsub(arg, estimateSquared, one), halfEstimate, estimate);
    }

    static internal_vector_type vec_sqrt(internal_vector_type const & arg)
    {
        // adapted from http://developer.apple.com/hardwaredrivers/ve/algorithms.html
        return vec_mul(arg, vec_rsqrt(arg));
    }

public:
    friend inline vec sqrt(vec const & arg)
    {
        return vec_sqrt(arg.data_);
    }

    friend inline vec cube(vec const & arg)
    {
        return vec_mul(arg.data_, vec_mul(arg.data_, arg.data_));
    }
    /* @} */

    /* @{ */
    /** binary functions */
    friend inline vec max_(vec const & lhs, vec const & rhs)
    {
        return vec_max(lhs.data_, rhs.data_);
    }

    friend inline vec min_(vec const & lhs, vec const & rhs)
    {
        return vec_min(lhs.data_, rhs.data_);
    }
    /* @} */

    /* @{ */
    /** rounding functions */
    friend inline vec round(vec const & arg)
    {
        return detail::vec_round_float(arg);
        // return vec_round(arg.data_); testsuite fails: seems to round differently than we do?
    }

    friend inline vec frac(vec const & arg)
    {
        vec floor_result = floor(arg);
        return arg - floor_result;
    }

    friend inline vec floor(vec const & arg)
    {
        return vec_floor(arg.data_);
    }

    friend inline vec ceil(vec const & arg)
    {
        return vec_ceil(arg.data_);
    }

    friend inline vec trunc(vec const & arg)
    {
        return arg.truncate_to_int().convert_to_float();
    }

    typedef detail::int_vec_altivec int_vec;

    vec (int_vec const & rhs):
        base((internal_vector_type)rhs.data_)
    {}

    int_vec truncate_to_int(void) const
    {
        return int_vec(vec_ctu(data_, 0));
    }
    /* @} */


    /* @{ */
    /** mathematical functions */

#if 0
    // FIXME: vector math support seems to be broken
    typedef nova::detail::int_vec_altivec int_vec;

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



#else

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

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(signed_sqrt)

#endif
    /* @} */
};

} /* namespace nova */

#undef always_inline
#undef vec_cmplt_

#endif /* VEC_ALTIVEC_HPP */
