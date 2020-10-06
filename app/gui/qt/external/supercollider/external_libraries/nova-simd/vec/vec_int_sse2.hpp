//  sse integer vector helper class
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

#ifndef VEC_INT_SSE_HPP
#define VEC_INT_SSE_HPP

#include <emmintrin.h>


namespace nova {
namespace detail {

struct int_vec_sse2
{
    __m128i data_;

    explicit int_vec_sse2(int arg):
        data_(_mm_set1_epi32(arg))
    {}

    int_vec_sse2(__m128 arg):
        data_((__m128i)arg)
    {}

    int_vec_sse2(__m128i arg):
        data_(arg)
    {}

    int_vec_sse2(int_vec_sse2 const & arg):
        data_(arg.data_)
    {}

    int_vec_sse2(void)
    {}

    operator __m128i (void) const
    {
        return data_;
    }

    friend int_vec_sse2 operator+(int_vec_sse2 const & lhs, int_vec_sse2 const & rhs)
    {
        return _mm_add_epi32(lhs.data_, rhs.data_);
    }

    friend int_vec_sse2 operator-(int_vec_sse2 const & lhs, int_vec_sse2 const & rhs)
    {
        return _mm_sub_epi32(lhs.data_, rhs.data_);
    }

    #define RELATIONAL_MASK_OPERATOR(op, opcode) \
    friend inline int_vec_sse2 mask_##op(int_vec_sse2 const & lhs, int_vec_sse2 const & rhs) \
    { \
        return opcode(lhs.data_, rhs.data_); \
    }

    RELATIONAL_MASK_OPERATOR(lt, _mm_cmplt_epi32)
    RELATIONAL_MASK_OPERATOR(gt, _mm_cmpgt_epi32)
    RELATIONAL_MASK_OPERATOR(eq, _mm_cmpeq_epi32)

    #undef RELATIONAL_MASK_OPERATOR

    friend int_vec_sse2 operator&(int_vec_sse2 const & lhs, int_vec_sse2 const & rhs)
    {
        int_vec_sse2 ret = int_vec_sse2 (_mm_and_si128(lhs.data_, rhs.data_));
        return ret;
    }

    friend inline int_vec_sse2 andnot(int_vec_sse2 const & lhs, int_vec_sse2 const & rhs)
    {
        return int_vec_sse2(_mm_andnot_si128(lhs.data_, rhs.data_));
    }

    // shift in zeros
    friend inline int_vec_sse2 slli(int_vec_sse2 const & arg, int count)
    {
        int_vec_sse2 ret (_mm_slli_epi32(arg.data_, count));
        return ret;
    }

    // shift in zeros
    friend inline int_vec_sse2 srli(int_vec_sse2 const & arg, int count)
    {
        int_vec_sse2 ret (_mm_srli_epi32(arg.data_, count));
        return ret;
    }

    inline __m128 convert_to_float(void) const
    {
        return _mm_cvtepi32_ps(data_);
    }
};

}
}

#endif /* VEC_INT_SSE_HPP */
