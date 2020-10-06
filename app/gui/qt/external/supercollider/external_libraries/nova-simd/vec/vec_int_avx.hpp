//  avx int vector class
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

#ifndef VEC_AVX_INT_HPP
#define VEC_AVX_INT_HPP

#include <pmmintrin.h>

#include "vec_int_sse2.hpp"
#include <functional>

namespace nova   {
namespace detail {

struct int_vec_avx
{
    __m256i data_;

    /* cast */
    explicit int_vec_avx(int arg):
        data_(_mm256_set1_epi32(arg))
    {}

    int_vec_avx(__m256i arg):
        data_(arg)
    {}

    int_vec_avx(__m256 arg):
        data_(_mm256_castps_si256(arg))
    {}

    int_vec_avx(int_vec_avx const & arg):
        data_(arg.data_)
    {}

    int_vec_avx(void)
    {}

#define APPLY_SSE_FUNCTION(op, function) \
    friend int_vec_avx op(int_vec_avx const & lhs, int_vec_avx const & rhs) \
    { \
        __m256 lhs_data = _mm256_castsi256_ps(lhs.data_);       \
        __m256 rhs_data = _mm256_castsi256_ps(rhs.data_);       \
        __m128 lhs_low =  _mm256_castps256_ps128(lhs_data);    \
        __m128 lhs_hi =   _mm256_extractf128_ps(lhs_data, 1);  \
        __m128 rhs_low =  _mm256_castps256_ps128(rhs_data);    \
        __m128 rhs_hi =   _mm256_extractf128_ps(rhs_data, 1);  \
\
        __m128i newlow = function(int_vec_sse2(lhs_low), int_vec_sse2(rhs_low)); \
        __m128i newhi  = function(int_vec_sse2(lhs_hi),  int_vec_sse2(rhs_hi)); \
\
        __m256i result = _mm256_castsi128_si256(newlow);  \
        result = _mm256_insertf128_si256(result,  newhi, 1);   \
        return result;   \
    }

    APPLY_SSE_FUNCTION(operator +, std::plus<int_vec_sse2>());
    APPLY_SSE_FUNCTION(operator -, std::minus<int_vec_sse2>());


    APPLY_SSE_FUNCTION(mask_lt, mask_lt)
    APPLY_SSE_FUNCTION(mask_gt, mask_gt)
    APPLY_SSE_FUNCTION(mask_eq, mask_eq)

#undef APPLY_SSE_FUNCTION

    friend int_vec_avx operator&(int_vec_avx const & lhs, int_vec_avx const & rhs)
    {
        return int_vec_avx(_mm256_and_ps(_mm256_castsi256_ps(lhs.data_),
                                        _mm256_castsi256_ps(rhs.data_)));
    }

    friend inline int_vec_avx andnot(int_vec_avx const & lhs, int_vec_avx const & rhs)
    {
        return int_vec_avx(_mm256_andnot_ps(_mm256_castsi256_ps(lhs.data_),
                                        _mm256_castsi256_ps(rhs.data_)));
    }


    // shift in zeros
    friend inline int_vec_avx slli(int_vec_avx const & arg, int count)
    {
        __m256 arg_data = _mm256_castsi256_ps(arg.data_);
        __m128 arg_low =  _mm256_castps256_ps128(arg_data);
        __m128 arg_hi =   _mm256_extractf128_ps(arg_data, 1);

        __m128 newlow = (__m128)_mm_slli_epi32((__m128i)arg_low, count);
        __m128 newhi  = (__m128)_mm_slli_epi32((__m128i)arg_hi,  count);

        __m256 result = _mm256_castps128_ps256(newlow);
        result = _mm256_insertf128_ps(result,  newhi, 1);
        return result;
    }

    // shift in zeros
    friend inline int_vec_avx srli(int_vec_avx const & arg, int count)
    {
        __m256 arg_data = _mm256_castsi256_ps(arg.data_);
        __m128 arg_low =  _mm256_castps256_ps128(arg_data);
        __m128 arg_hi =   _mm256_extractf128_ps(arg_data, 1);

        __m128 newlow = (__m128)_mm_srli_epi32((__m128i)arg_low, count);
        __m128 newhi  = (__m128)_mm_srli_epi32((__m128i)arg_hi,  count);

        __m256 result = _mm256_castps128_ps256(newlow);
        result = _mm256_insertf128_ps(result,  newhi, 1);
        return result;
    }

    __m256 convert_to_float(void) const
    {
        return _mm256_cvtepi32_ps(data_);
    }
};


}
}

#endif /* VEC_AVX_INT_HPP */
