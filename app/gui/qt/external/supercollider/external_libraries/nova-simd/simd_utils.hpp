//  utilities for the simd implementation
//  Copyright (C) 2008, 2009 Tim Blechmann
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

#ifndef SIMD_UTILS_HPP
#define SIMD_UTILS_HPP

#include <xmmintrin.h>

#ifdef __SSE2__
#include <emmintrin.h>
#endif /* __SSE2__ */

#ifdef __SSE4_1__
#include <smmintrin.h>
#endif /* __SSE41__ */


namespace nova
{
namespace detail
{

#ifdef __SSE2__
inline __m128 gen_sign_mask(void)
{
    __m128i x = _mm_setzero_si128();
    __m128i ones = _mm_cmpeq_epi32(x, x);
    return (__m128)_mm_slli_epi32 (_mm_srli_epi32(ones, 31), 31);
}

inline __m128 gen_abs_mask(void)
{
    __m128i x = _mm_setzero_si128();
    __m128i ones = _mm_cmpeq_epi32(x, x);
    return (__m128)_mm_srli_epi32 (_mm_slli_epi32(ones, 1), 1);
}

inline __m128 gen_one(void)
{
    __m128i x = _mm_setzero_si128();
    __m128i ones = _mm_cmpeq_epi32(x, x);
    return (__m128)_mm_slli_epi32 (_mm_srli_epi32(ones, 25), 23);
}

inline __m128 gen_05(void)
{
    __m128i x = _mm_setzero_si128();
    __m128i ones = _mm_cmpeq_epi32(x, x);
    return (__m128)_mm_slli_epi32 (_mm_srli_epi32(ones, 26), 24);
}

#else

/* SSE fallback */

inline __m128 gen_sign_mask(void)
{
    static const int sign_mask = 0x80000000;
    float * casted = (float*)(&sign_mask);
    return _mm_set_ps1(*casted);
}

inline __m128 gen_abs_mask(void)
{
    static const int abs_mask = 0x7fffffff;
    float * casted = (float*)(&abs_mask);
    return _mm_set_ps1(*casted);
}

inline __m128 gen_one(void)
{
    return _mm_set_ps1(1.f);
}

inline __m128 gen_05(void)
{
    return _mm_set_ps1(0.5f);
}

#endif

inline __m128 gen_025(void)
{
    return _mm_set_ps1(0.25f);
}

inline float extract_0(__m128 arg)
{
    float r;
    _mm_store_ss(&r, arg);
    return r;
}

inline float extract_3(__m128 arg)
{
    __m128 last = _mm_shuffle_ps(arg, arg, _MM_SHUFFLE(2, 1, 0, 3));
    float r;
    _mm_store_ss(&r, last);
    return r;
}

inline float horizontal_min(__m128 args)
{
    __m128 xmm0, xmm1;
    xmm0 = args;
    xmm1 = _mm_shuffle_ps(xmm0, xmm0, _MM_SHUFFLE(2,2,2,2));
    xmm0 = _mm_min_ps(xmm0, xmm1);
    xmm1 = _mm_shuffle_ps(xmm0, xmm0, _MM_SHUFFLE(1,1,1,1));
    xmm0 = _mm_min_ss(xmm0, xmm1);
    return extract_0(xmm0);
}

inline float horizontal_max(__m128 args)
{
    __m128 xmm0, xmm1;
    xmm0 = args;
    xmm1 = _mm_shuffle_ps(xmm0, xmm0, _MM_SHUFFLE(2,2,2,2));
    xmm0 = _mm_max_ps(xmm0, xmm1);
    xmm1 = _mm_shuffle_ps(xmm0, xmm0, _MM_SHUFFLE(1,1,1,1));
    xmm0 = _mm_max_ss(xmm0, xmm1);
    return extract_0(xmm0);
}

#ifdef __SSE4_1__

inline __m128 select_vector(__m128 val0, __m128 val1, __m128 sel)
{
    /* if bitmask is set, return value in val1, else value in val0 */
    return _mm_blendv_ps(val0, val1, sel);
}

#else

inline __m128 select_vector(__m128 val0, __m128 val1, __m128 sel)
{
    /* if bitmask is set, return value in val1, else value in val0 */
    return _mm_or_ps(_mm_andnot_ps(sel, val0),
                     _mm_and_ps(val1, sel));
}

#endif

} /* namespace detail */
} /* namespace nova */

#endif /* SIMD_UTILS_HPP */
