//  neon vector class
//
//  Copyright (c) 2010 Tim Blechmann and Dan Stowell
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

#ifndef VEC_NEON_HPP
#define VEC_NEON_HPP

#include <arm_neon.h>

#include "vec_base.hpp"
#include "vec_int_neon.hpp"
#include "../detail/vec_math.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova
{

template <>
struct vec<float>:
    vec_base<float, float32x4_t, 4>
{
    typedef float float_type;

private:
    typedef float32x4_t internal_vector_type;
    typedef vec_base<float, float32x4_t, 4> base;

    static float32x4_t set_vector(float f0, float f1, float f2, float f3)
    {
        float32x4_t ret;
        ret = vsetq_lane_f32(f0, ret, 0);
        ret = vsetq_lane_f32(f1, ret, 1);
        ret = vsetq_lane_f32(f2, ret, 2);
        ret = vsetq_lane_f32(f3, ret, 3);
        return ret;
    }

    static float32x4_t set_vector(float f)
    {
        return vdupq_n_f32(f);
    }

public:
    static inline float32x4_t gen_sign_mask(void)
    {
        static const int sign_mask = 0x80000000;
        float * casted = (float*)(&sign_mask);
        return vdupq_n_f32(*casted);
    }

    static inline float32x4_t gen_abs_mask(void)
    {
        static const int abs_mask = 0x7fffffff;
        float * casted = (float*)(&abs_mask);
        return vdupq_n_f32(*casted);
    }

    static inline float32x4_t gen_one(void)
    {
        return vdupq_n_f32(1.f);
    }

    static inline float32x4_t gen_05(void)
    {
        return vdupq_n_f32(0.5f);
    }

    static inline float32x4_t gen_zero(void)
    {
        return vdupq_n_f32(0.f);
    }

    static inline internal_vector_type gen_exp_mask(void)
    {
        return set_bitmask(0x7F800000);
    }

    static inline internal_vector_type gen_exp_mask_1(void)
    {
        return set_bitmask(0x3F000000);
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

    vec(float32x4_t const & arg):
        base(arg)
    {}

public:
    static const int size = 4;
    static const int objects_per_cacheline = 64/sizeof(float);
    static const bool has_compare_bitmask = true;

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

    vec(double f)
    {
        set_vec((float)f);
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
        base::data_ = vld1q_f32((const float32_t*)data);
    }

    void load_aligned(const float * data)
    {
        load(data);
    }

    void store(float * dest) const
    {
        vst1q_f32((float32_t*)dest, data_);
    }

    void store_aligned(float * dest) const
    {
        store(dest);
    }

    void store_aligned_stream(float * dest) const
    {
        store(dest);
    }

    // no particular setzero intrinsic
    void clear(void)
    {
        set_vec(0);
    }

    /* @} */

    /* @{ */
    /** element access */
    void set (std::size_t index, float value)
    {
        switch (index) {
            case 0:
                data_ = vsetq_lane_f32(value, data_, 0);
                return;
            case 1:
                data_ = vsetq_lane_f32(value, data_, 1);
                return;
            case 2:
                data_ = vsetq_lane_f32(value, data_, 2);
                return;
            case 3:
                data_ = vsetq_lane_f32(value, data_, 3);
                return;
        }
        assert(false);
    }

    void set_vec (float value)
    {
        data_ = vdupq_n_f32(value);
    }

    float set_slope(float start, float slope)
    {
        data_ = set_vector(start,
                           start + slope,
                           start + slope + slope,
                           start + slope + slope + slope);

        return slope + slope + slope + slope;
    }

    float set_exp(float start, float curve)
    {
        data_ = set_vector(start,
                           start * curve,
                           start * curve * curve,
                           start * curve * curve * curve);

        return start * curve * curve * curve * curve;
    }

    float get (std::size_t index) const
    {
        switch (index) {
            case 0:
                return vgetq_lane_f32(data_, 0);
            case 1:
                return vgetq_lane_f32(data_, 1);
            case 2:
                return vgetq_lane_f32(data_, 2);
            case 3:
                return vgetq_lane_f32(data_, 3);
        }
        assert(false);
    }
    /* @} */

    /* @{ */

private:
    static float32x4_t vdivq_f32(float32x4_t lhs, float32x4_t rhs)
    {
        float32x4_t reciprocal = vrecpeq_f32(rhs);
        reciprocal = vmulq_f32(reciprocal, vrecpsq_f32(rhs, reciprocal));
        return vmulq_f32(lhs, reciprocal);
    }

public:
    friend vec fast_reciprocal(vec const & arg)
    {
        float32x4_t reciprocal = vrecpeq_f32(arg);
        return reciprocal;
    }

    friend vec reciprocal(vec const & arg)
    {
        float32x4_t reciprocal = vrecpeq_f32(arg);
        reciprocal = vmulq_f32(reciprocal, vrecpsq_f32(arg, reciprocal));
        return reciprocal;
    }

    /** arithmetic operators */
#define OPERATOR_ASSIGNMENT(op, opcode) \
    vec & operator op(vec const & rhs) \
    { \
        data_ = opcode(data_, rhs.data_);\
        return *this;\
    }

    OPERATOR_ASSIGNMENT(+=, vaddq_f32)
    OPERATOR_ASSIGNMENT(-=, vsubq_f32)
    OPERATOR_ASSIGNMENT(*=, vmulq_f32)
    OPERATOR_ASSIGNMENT(/=, vdivq_f32)

#undef OPERATOR_ASSIGNMENT

#define ARITHMETIC_OPERATOR(op, opcode) \
    friend vec operator op(vec const & lhs, vec const & rhs) \
    { \
        return opcode(lhs.data_, rhs.data_); \
    }

    ARITHMETIC_OPERATOR(+, vaddq_f32)
    ARITHMETIC_OPERATOR(-, vsubq_f32)
    ARITHMETIC_OPERATOR(*, vmulq_f32)
    ARITHMETIC_OPERATOR(/, vdivq_f32)

#undef ARITHMETIC_OPERATOR

    friend vec madd(vec const & arg1, vec const & arg2, vec const & arg3)
    {
        return vmlaq_f32(arg3.data_, arg2.data_, arg1.data_);
    }

private:
    static uint32x4_t vcneqq_f32(float32x4_t a, float32x4_t b)
    {
        return vmvnq_u32(vceqq_f32(a, b));
    }

public:
#define RELATIONAL_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        const uint32x4_t one = vreinterpretq_u32_f32(gen_one()); \
        uint32x4_t mask = opcode(data_, rhs.data_); \
        return vreinterpretq_f32_u32(vandq_u32(mask, one)); \
    }

     RELATIONAL_OPERATOR(<, vcltq_f32)
     RELATIONAL_OPERATOR(<=, vcleq_f32)
     RELATIONAL_OPERATOR(>, vcgtq_f32)
     RELATIONAL_OPERATOR(>=, vcgeq_f32)
     RELATIONAL_OPERATOR(==, vceqq_f32)
     RELATIONAL_OPERATOR(!=, vcneqq_f32)

#undef RELATIONAL_OPERATOR

    /* @{ */
#define BITWISE_OPERATOR(op, opcode) \
    vec operator op(vec const & rhs) const \
    { \
        return vreinterpretq_f32_u32(opcode( \
            vreinterpretq_u32_f32(data_), vreinterpretq_u32_f32(rhs.data_))); \
    }

    BITWISE_OPERATOR(&, vandq_u32)
    BITWISE_OPERATOR(|, vorrq_u32)
    BITWISE_OPERATOR(^, veorq_u32)

#undef BITWISE_OPERATOR

    friend inline vec andnot(vec const & lhs, vec const & rhs)
    {
        return  vreinterpretq_f32_u32(vandq_u32(vreinterpretq_u32_f32(lhs.data_),
                                                vmvnq_u32(vreinterpretq_u32_f32(rhs.data_))));;
    }


#define RELATIONAL_MASK_OPERATOR(op, opcode) \
    friend vec mask_##op(vec const & lhs, vec const & rhs) \
    { \
        return vreinterpretq_f32_u32(opcode( \
            lhs.data_, rhs.data_)); \
    }

    RELATIONAL_MASK_OPERATOR(lt, vcltq_f32)
    RELATIONAL_MASK_OPERATOR(le, vcleq_f32)
    RELATIONAL_MASK_OPERATOR(gt, vcgtq_f32)
    RELATIONAL_MASK_OPERATOR(ge, vcgeq_f32)
    RELATIONAL_MASK_OPERATOR(eq, vceqq_f32)
    RELATIONAL_MASK_OPERATOR(neq, vcneqq_f32)

#undef RELATIONAL_MASK_OPERATOR

public:
    friend inline vec select(vec lhs, vec rhs, vec bitmask)
    {
        return vbslq_f32(vreinterpretq_u32_f32(bitmask.data_), lhs.data_, rhs.data_);
    }

    /* @} */

    /* @{ */
    /** unary functions */
    friend inline vec abs(vec const & arg)
    {
        return vabsq_f32(arg.data_);
    }

    friend inline vec square(vec const & arg)
    {
        return vmulq_f32(arg.data_, arg.data_);
    }

    friend inline vec cube(vec const & arg)
    {
        return vmulq_f32(arg.data_, vmulq_f32(arg.data_, arg.data_));
    }
    /* @} */

    /* @{ */
    /** binary functions */
    friend inline vec max_(vec const & lhs, vec const & rhs)
    {
        return vmaxq_f32(lhs.data_, rhs.data_);
    }

    friend inline vec min_(vec const & lhs, vec const & rhs)
    {
        return vminq_f32(lhs.data_, rhs.data_);
    }
    /* @} */

    /* @{ */
    /** rounding functions */
    friend inline vec round(vec const & arg)
    {
        return detail::vec_round_float(arg);
    }

    friend inline vec frac(vec const & arg)
    {
        vec floor_result = floor(arg);
        return arg - floor_result;
    }

    friend inline vec floor(vec const & arg)
    {
        return detail::vec_floor_float(arg);
    }

    friend inline vec ceil(vec const & arg)
    {
        return detail::vec_ceil_float(arg);
    }

/*  FIXME: this is broken
    friend inline vec trunc(vec const & arg)
    {
        return arg.truncate_to_int().convert_to_float();
    }
*/
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(trunc)
    /* @} */

    /* @{ */
    /** mathematical functions */
    friend inline vec sign(vec const & arg)
    {
        return detail::vec_sign(arg);
    }

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

private:
    static float32x4_t vsqrtq_f32(float32x4_t arg)
    {
        float32x4_t reciprocal = vrsqrteq_f32(arg);

        // TODO: maybe we should do another newton-raphson iteration (see: qvrsqrtsq_f32)?
        return vmulq_f32(arg, reciprocal);
    }


public:
    friend vec sqrt(vec const & arg)
    {
        return vsqrtq_f32(arg);
    }

    friend inline vec signed_sqrt(vec const & arg)
    {
        return detail::vec_signed_sqrt(arg);
    }
    /* @} */

    typedef detail::int_vec_neon int_vec;

    vec (int_vec const & rhs):
        base(vreinterpretq_f32_u32(rhs.data_))
    {}

    int_vec truncate_to_int(void) const
    {
        return int_vec(vreinterpretq_u32_s32(vcvtq_s32_f32(data_)));
    }

    float horizontal_min(void) const
    {
        float32x2_t high = vget_high_f32(data_);
        float32x2_t low = vget_low_f32(data_);

        float32x2_t pmin = vmin_f32(low, high);
        float pmin0 = vget_lane_f32(pmin, 0);
        float pmin1 = vget_lane_f32(pmin, 1);

        return std::min(pmin0, pmin1);
    }

    float horizontal_max(void) const
    {
        float32x2_t high = vget_high_f32(data_);
        float32x2_t low = vget_low_f32(data_);

        float32x2_t pmax = vmax_f32(low, high);
        float pmax0 = vget_lane_f32(pmax, 0);
        float pmax1 = vget_lane_f32(pmax, 1);

        return std::max(pmax0, pmax1);
    }

    float horizontal_sum(void) const
    {
        float32x2_t high = vget_high_f32(data_);
        float32x2_t low = vget_low_f32(data_);

        float32x2_t psum = vpadd_f32(low, high);
        return vget_lane_f32(psum, 0) + vget_lane_f32(psum, 1);
    }
};

} /* namespace nova */


#undef always_inline

#endif /* VEC_NEON_HPP */
