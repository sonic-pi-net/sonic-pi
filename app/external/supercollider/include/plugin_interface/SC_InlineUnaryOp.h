/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
    http://www.audiosynth.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#pragma once

#include "SC_Types.h"
#include "SC_Constants.h"

#include <cmath>
#include <limits>

#ifdef __SSE__
#    include <xmmintrin.h>
#endif

#ifdef __SSE4_1__
#    include <smmintrin.h>
#endif


///////////////////////////////////////////////////////////////////////////////////////

inline bool sc_isnan(float x) { return std::isnan(x); }

inline bool sc_isnan(double x) { return std::isnan(x); }

inline bool sc_isfinite(float x) { return std::isfinite(x); }

inline bool sc_isfinite(double x) { return std::isfinite(x); }

///////////////////////////////////////////////////////////////////////////////////////

// versions provided for float32 and float64
// did not supply template because do not want to instantiate for integers.
// all constants explicitly cast to prevent PowerPC frsp instruction generation.

///////////////////////////////////////////////////////////////////////////////////////

// this is a function for preventing pathological math operations in ugens.
// can be used at the end of a block to fix any recirculating filter values.
inline float32 zapgremlins(float32 x) {
    float32 absx = std::abs(x);
    // very small numbers fail the first test, eliminating denormalized numbers
    //    (zero also fails the first test, but that is OK since it returns zero.)
    // very large numbers fail the second test, eliminating infinities
    // Not-a-Numbers fail both tests and are eliminated.
    return (absx > (float32)1e-15 && absx < (float32)1e15) ? x : (float32)0.;
}

inline float32 sc_log2(float32 x) { return std::log2(x); }

inline float32 sc_log10(float32 x) { return std::log10(std::abs(x)); }

inline float32 sc_midicps(float32 note) {
    return (float32)440. * std::pow((float32)2., (note - (float32)69.) * (float32)0.083333333333);
}

inline float32 sc_cpsmidi(float32 freq) {
    return sc_log2(freq * (float32)0.0022727272727) * (float32)12. + (float32)69.;
}

inline float32 sc_midiratio(float32 midi) { return std::pow((float32)2., midi * (float32)0.083333333333); }

inline float32 sc_ratiomidi(float32 ratio) { return (float32)12. * sc_log2(ratio); }

inline float32 sc_octcps(float32 note) { return (float32)440. * std::pow((float32)2., note - (float32)4.75); }

inline float32 sc_cpsoct(float32 freq) { return sc_log2(freq * (float32)0.0022727272727) + (float32)4.75; }

inline float32 sc_ampdb(float32 amp) { return std::log10(amp) * (float32)20.; }

inline float32 sc_dbamp(float32 db) { return std::pow((float32)10., db * (float32).05); }

inline float32 sc_squared(float32 x) { return x * x; }

inline float32 sc_cubed(float32 x) { return x * x * x; }

inline float32 sc_sqrt(float32 x) { return x < (float32)0. ? -sqrt(-x) : sqrt(x); }


inline float32 sc_hanwindow(float32 x) {
    if (x < (float32)0. || x > (float32)1.)
        return (float32)0.;
    return (float32)0.5 - (float32)0.5 * static_cast<float32>(cos(x * (float32)twopi));
}

inline float32 sc_welwindow(float32 x) {
    if (x < (float32)0. || x > (float32)1.)
        return (float32)0.;
    return static_cast<float32>(sin(x * pi));
}

inline float32 sc_triwindow(float32 x) {
    if (x < (float32)0. || x > (float32)1.)
        return (float32)0.;
    if (x < (float32)0.5)
        return (float32)2. * x;
    else
        return (float32)-2. * x + (float32)2.;
}

inline float32 sc_bitriwindow(float32 x) {
    float32 ax = (float32)1. - std::abs(x);
    if (ax <= (float32)0.)
        return (float32)0.;
    return ax;
}

inline float32 sc_rectwindow(float32 x) {
    if (x < (float32)0. || x > (float32)1.)
        return (float32)0.;
    return (float32)1.;
}

inline float32 sc_scurve(float32 x) {
    if (x <= (float32)0.)
        return (float32)0.;
    if (x >= (float32)1.)
        return (float32)1.;
    return x * x * ((float32)3. - (float32)2. * x);
}

inline float32 sc_scurve0(float32 x) {
    // assumes that x is in range
    return x * x * ((float32)3. - (float32)2. * x);
}

inline float32 sc_ramp(float32 x) {
    if (x <= (float32)0.)
        return (float32)0.;
    if (x >= (float32)1.)
        return (float32)1.;
    return x;
}

inline float32 sc_sign(float32 x) {
    return x < (float32)0. ? (float32)-1. : (x > (float32)0. ? (float32)1.f : (float32)0.f);
}

inline float32 sc_distort(float32 x) { return x / ((float32)1. + std::abs(x)); }

inline float32 sc_distortneg(float32 x) {
    if (x < (float32)0.)
        return x / ((float32)1. - x);
    else
        return x;
}

inline float32 sc_softclip(float32 x) {
    float32 absx = std::abs(x);
    if (absx <= (float32)0.5)
        return x;
    else
        return (absx - (float32)0.25) / x;
}

// Taylor expansion out to x**9/9! factored  into multiply-adds
// from Phil Burk.
inline float32 taylorsin(float32 x) {
    // valid range from -pi/2 to +3pi/2
    x = static_cast<float32>((float32)pi2 - std::abs(pi2 - x));
    float32 x2 = x * x;
    return static_cast<float32>(
        x * (x2 * (x2 * (x2 * (x2 * (1.0 / 362880.0) - (1.0 / 5040.0)) + (1.0 / 120.0)) - (1.0 / 6.0)) + 1.0));
}

inline float32 sc_trunc(float32 x) { return std::trunc(x); }


inline float32 sc_ceil(float32 x) {
#ifdef __SSE4_1__
    __m128 a = _mm_set_ss(x);
    __m128 b = _mm_round_ss(a, a, _MM_FROUND_TO_POS_INF);
    return _mm_cvtss_f32(b);
#else
    return std::ceil(x);
#endif
}

inline float32 sc_floor(float32 x) {
#ifdef __SSE4_1__
    __m128 a = _mm_set_ss(x);
    __m128 b = _mm_round_ss(a, a, _MM_FROUND_TO_NEG_INF);
    return _mm_cvtss_f32(b);
#else
    return std::floor(x);
#endif
}

inline float32 sc_reciprocal(float32 x) {
#ifdef __SSE__
    // adapted from AP-803 Newton-Raphson Method with Streaming SIMD Extensions
    // 23 bit accuracy (out of 24bit)
    const __m128 arg = _mm_set_ss(x);
    const __m128 approx = _mm_rcp_ss(arg);
    const __m128 muls = _mm_mul_ss(_mm_mul_ss(arg, approx), approx);
    const __m128 doubleApprox = _mm_add_ss(approx, approx);
    const __m128 result = _mm_sub_ss(doubleApprox, muls);
    return _mm_cvtss_f32(result);
#else
    return 1.f / x;
#endif
}

inline float32 sc_frac(float32 x) { return x - sc_floor(x); }


////////////////////////////////

inline float32 sc_bitNot(float32 x) { return (float32) ~(int)x; }

inline float32 sc_lg3interp(float32 x1, float32 a, float32 b, float32 c, float32 d) {
    // cubic lagrange interpolator
    float32 x0 = x1 + 1.f;
    float32 x2 = x1 - 1.f;
    float32 x3 = x1 - 2.f;

    float32 x03 = x0 * x3 * 0.5f;
    float32 x12 = x1 * x2 * 0.16666666666666667f;

    return x12 * (d * x0 - a * x3) + x03 * (b * x2 - c * x1);
}

inline float32 sc_CalcFeedback(float32 delaytime, float32 decaytime) {
    if (delaytime == 0.f || decaytime == 0.f)
        return 0.f;

    float32 absret = static_cast<float32>(std::exp(log001 * delaytime / std::abs(decaytime)));
    float32 ret = std::copysign(absret, decaytime);
    return ret;
}

inline float32 sc_wrap1(float32 x) {
    if (x >= (float32)1.)
        return x + (float32)-2.;
    if (x < (float32)-1.)
        return x + (float32)2.;
    return x;
}

inline float32 sc_fold1(float32 x) {
    if (x >= (float32)1.)
        return (float32)2. - x;
    if (x < (float32)-1.)
        return (float32)-2. - x;
    return x;
}


///////////////////////////////////////////////////////////////////////////////////////

inline float64 zapgremlins(float64 x) {
    float64 absx = std::abs(x);
    // very small numbers fail the first test, eliminating denormalized numbers
    //    (zero also fails the first test, but that is OK since it returns zero.)
    // very large numbers fail the second test, eliminating infinities
    // Not-a-Numbers fail both tests and are eliminated.
    return (absx > (float64)1e-15 && absx < (float64)1e15) ? x : (float64)0.;
}

inline float64 sc_log2(float64 x) { return std::log2(std::abs(x)); }

inline float64 sc_log10(float64 x) { return std::log10(std::abs(x)); }

inline float64 sc_midicps(float64 note) {
    return (float64)440. * std::pow((float64)2., (note - (float64)69.) * (float64)0.08333333333333333333333333);
}

inline float64 sc_cpsmidi(float64 freq) {
    return sc_log2(freq * (float64)0.002272727272727272727272727) * (float64)12. + (float64)69.;
}

inline float64 sc_midiratio(float64 midi) { return std::pow((float64)2., midi * (float64)0.083333333333); }

inline float64 sc_ratiomidi(float64 ratio) { return (float64)12. * sc_log2(ratio); }

inline float64 sc_octcps(float64 note) { return (float64)440. * std::pow((float64)2., note - (float64)4.75); }

inline float64 sc_cpsoct(float64 freq) { return sc_log2(freq * (float64)0.0022727272727) + (float64)4.75; }

inline float64 sc_ampdb(float64 amp) { return std::log10(amp) * (float64)20.; }

inline float64 sc_dbamp(float64 db) { return std::pow((float64)10., db * (float64).05); }

inline float64 sc_squared(float64 x) { return x * x; }

inline float64 sc_cubed(float64 x) { return x * x * x; }

inline float64 sc_sqrt(float64 x) { return x < (float64)0. ? -sqrt(-x) : sqrt(x); }

inline float64 sc_hanwindow(float64 x) {
    if (x < (float64)0. || x > (float64)1.)
        return (float64)0.;
    return (float64)0.5 - (float64)0.5 * cos(x * twopi);
}

inline float64 sc_welwindow(float64 x) {
    if (x < (float64)0. || x > (float64)1.)
        return (float64)0.;
    return sin(x * pi);
}

inline float64 sc_triwindow(float64 x) {
    if (x < (float64)0. || x > (float64)1.)
        return (float64)0.;
    if (x < (float64)0.5)
        return (float64)2. * x;
    else
        return (float64)-2. * x + (float64)2.;
}

inline float64 sc_bitriwindow(float64 x) {
    float64 ax = std::abs(x);
    if (ax > (float64)1.)
        return (float64)0.;
    return (float64)1. - ax;
}

inline float64 sc_rectwindow(float64 x) {
    if (x < (float64)0. || x > (float64)1.)
        return (float64)0.;
    return (float64)1.;
}

inline float64 sc_scurve(float64 x) {
    if (x <= (float64)0.)
        return (float64)0.;
    if (x >= (float64)1.)
        return (float64)1.;
    return x * x * ((float64)3. - (float64)2. * x);
}

inline float64 sc_scurve0(float64 x) {
    // assumes that x is in range
    return x * x * ((float64)3. - (float64)2. * x);
}

inline float64 sc_ramp(float64 x) {
    if (x <= (float64)0.)
        return (float64)0.;
    if (x >= (float64)1.)
        return (float64)1.;
    return x;
}

inline float64 sc_sign(float64 x) {
    return x < (float64)0. ? (float64)-1. : (x > (float64)0. ? (float64)1.f : (float64)0.f);
}

inline float64 sc_distort(float64 x) { return x / ((float64)1. + std::abs(x)); }

inline float64 sc_distortneg(float64 x) {
    if (x < (float64)0.)
        return x / ((float64)1. - x);
    else
        return x;
}


inline float64 sc_softclip(float64 x) {
    float64 absx = std::abs(x);
    if (absx <= (float64)0.5)
        return x;
    else
        return (absx - (float64)0.25) / x;
}

// Taylor expansion out to x**9/9! factored  into multiply-adds
// from Phil Burk.
inline float64 taylorsin(float64 x) {
    x = pi2 - std::abs(pi2 - x);
    float64 x2 = x * x;
    return x * (x2 * (x2 * (x2 * (x2 * (1.0 / 362880.0) - (1.0 / 5040.0)) + (1.0 / 120.0)) - (1.0 / 6.0)) + 1.0);
}

inline float64 sc_trunc(float64 x) { return std::trunc(x); }

inline float64 sc_ceil(float64 x) {
#ifdef __SSE4_1__
    __m128d a = _mm_set_sd(x);
    const int cntrl = _MM_FROUND_TO_POS_INF;
    __m128d b = _mm_round_sd(a, a, cntrl);
    return _mm_cvtsd_f64(b);
#else
    return std::ceil(x);
#endif
}

inline float64 sc_floor(float64 x) {
#ifdef __SSE4_1__
    __m128d a = _mm_set_sd(x);
    const int cntrl = _MM_FROUND_TO_NEG_INF;
    __m128d b = _mm_round_sd(a, a, cntrl);
    return _mm_cvtsd_f64(b);
#else
    return std::floor(x);
#endif
}

inline float64 sc_reciprocal(float64 x) { return 1. / x; }


inline float64 sc_frac(float64 x) { return x - sc_floor(x); }

inline float64 sc_wrap1(float64 x) {
    if (x >= (float64)1.)
        return x + (float64)-2.;
    if (x < (float64)-1.)
        return x + (float64)2.;
    return x;
}

inline float64 sc_fold1(float64 x) {
    if (x >= (float64)1.)
        return (float64)2. - x;
    if (x < (float64)-1.)
        return (float64)-2. - x;
    return x;
}

inline int32 sc_grayCode(int32 x) { return x ^ (x >> 1); }
