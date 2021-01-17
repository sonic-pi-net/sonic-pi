//  neon integer vector helper class
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

#ifndef VEC_INT_NEON_HPP
#define VEC_INT_NEON_HPP

#include <arm_neon.h>

namespace nova {
namespace detail {

struct int_vec_neon
{
    uint32x4_t data_;

    explicit int_vec_neon(int arg):
        data_(vdupq_n_u32(arg))
    {}

    int_vec_neon(float32x4_t arg):
        data_(vreinterpretq_u32_f32(arg))
    {}

    int_vec_neon(uint32x4_t arg):
        data_(arg)
    {}

    int_vec_neon(int_vec_neon const & arg):
        data_(arg.data_)
    {}

    int_vec_neon(void)
    {}

    operator uint32x4_t (void) const
    {
        return data_;
    }

    friend int_vec_neon operator+(int_vec_neon const & lhs, int_vec_neon const & rhs)
    {
        return vaddq_u32(lhs.data_, rhs.data_);
    }

    friend int_vec_neon operator-(int_vec_neon const & lhs, int_vec_neon const & rhs)
    {
        return vsubq_u32(lhs.data_, rhs.data_);
    }

#define RELATIONAL_MASK_OPERATOR(op, opcode)                                                    \
    friend inline int_vec_neon mask_##op(int_vec_neon const & lhs, int_vec_neon const & rhs)    \
    {                                                                                           \
        return opcode(lhs.data_, rhs.data_);                                                    \
    }

    RELATIONAL_MASK_OPERATOR(lt, vcltq_u32)
    RELATIONAL_MASK_OPERATOR(gt, vcgtq_u32)
    RELATIONAL_MASK_OPERATOR(eq, vceqq_u32)

#undef RELATIONAL_MASK_OPERATOR

    friend int_vec_neon operator&(int_vec_neon const & lhs, int_vec_neon const & rhs)
    {
        return vandq_u32(lhs.data_, rhs.data_);
    }

    friend inline int_vec_neon andnot(int_vec_neon const & lhs, int_vec_neon const & rhs)
    {
        return vandq_u32(lhs.data_, vmvnq_u32(rhs.data_));
    }

    // shift in zeros
    friend inline int_vec_neon slli(int_vec_neon const & arg, int count)
    {
        return vshlq_u32(arg.data_, vdupq_n_s32(count));
    }

    // shift in zeros
    friend inline int_vec_neon srli(int_vec_neon const & arg, int count)
    {
        return vshlq_u32(arg.data_, vdupq_n_s32(-count));
    }

    inline float32x4_t convert_to_float(void) const
    {
        return vcvtq_f32_s32(vreinterpretq_s32_u32(data_));
    }
};

}
}

#endif /* VEC_INT_NEON_HPP */
