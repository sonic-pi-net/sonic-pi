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

#include "SC_BoundsMacros.h"
#include "SC_InlineUnaryOp.h"
#include <cmath>

inline float sc_mod(float in, float hi) {
    // avoid the divide if possible
    const float lo = (float)0.;
    if (in >= hi) {
        in -= hi;
        if (in < hi)
            return in;
    } else if (in < lo) {
        in += hi;
        if (in >= lo)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    return in - hi * sc_floor(in / hi);
}

inline double sc_mod(double in, double hi) {
    // avoid the divide if possible
    const double lo = (double)0.;
    if (in >= hi) {
        in -= hi;
        if (in < hi)
            return in;
    } else if (in < lo) {
        in += hi;
        if (in >= lo)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    return in - hi * sc_floor(in / hi);
}

inline float sc_wrap(float in, float lo, float hi) {
    float range;
    // avoid the divide if possible
    if (in >= hi) {
        range = hi - lo;
        in -= range;
        if (in < hi)
            return in;
    } else if (in < lo) {
        range = hi - lo;
        in += range;
        if (in >= lo)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    return in - range * sc_floor((in - lo) / range);
}

inline double sc_wrap(double in, double lo, double hi) {
    double range;
    // avoid the divide if possible
    if (in >= hi) {
        range = hi - lo;
        in -= range;
        if (in < hi)
            return in;
    } else if (in < lo) {
        range = hi - lo;
        in += range;
        if (in >= lo)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    return in - range * sc_floor((in - lo) / range);
}

inline double sc_wrap(double in, double lo, double hi, double range) {
    // avoid the divide if possible
    if (in >= hi) {
        in -= range;
        if (in < hi)
            return in;
    } else if (in < lo) {
        in += range;
        if (in >= lo)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    return in - range * sc_floor((in - lo) / range);
}

inline double sc_wrap(float in, float lo, float hi, float range) {
    // avoid the divide if possible
    if (in >= hi) {
        in -= range;
        if (in < hi)
            return in;
    } else if (in < lo) {
        in += range;
        if (in >= lo)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    return in - range * sc_floor((in - lo) / range);
}

inline float sc_fold(float in, float lo, float hi) {
    float x, c, range, range2;
    x = in - lo;

    // avoid the divide if possible
    if (in >= hi) {
        in = hi + hi - in;
        if (in >= lo)
            return in;
    } else if (in < lo) {
        in = lo + lo - in;
        if (in < hi)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    // ok do the divide
    range = hi - lo;
    range2 = range + range;
    c = x - range2 * sc_floor(x / range2);
    if (c >= range)
        c = range2 - c;
    return c + lo;
}

inline double sc_fold(double in, double lo, double hi) {
    double x, c, range, range2;
    x = in - lo;

    // avoid the divide if possible
    if (in >= hi) {
        in = hi + hi - in;
        if (in >= lo)
            return in;
    } else if (in < lo) {
        in = lo + lo - in;
        if (in < hi)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    // ok do the divide
    range = hi - lo;
    range2 = range + range;
    c = x - range2 * sc_floor(x / range2);
    if (c >= range)
        c = range2 - c;
    return c + lo;
}

inline double sc_fold(float in, float lo, float hi, float range, float range2) {
    float x, c;
    x = in - lo;

    // avoid the divide if possible
    if (in >= hi) {
        in = hi + hi - in;
        if (in >= lo)
            return in;
    } else if (in < lo) {
        in = lo + lo - in;
        if (in < hi)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    // ok do the divide
    c = x - range2 * sc_floor(x / range2);
    if (c >= range)
        c = range2 - c;
    return c + lo;
}

inline double sc_fold(double in, double lo, double hi, double range, double range2) {
    double x, c;
    x = in - lo;

    // avoid the divide if possible
    if (in >= hi) {
        in = hi + hi - in;
        if (in >= lo)
            return in;
    } else if (in < lo) {
        in = lo + lo - in;
        if (in < hi)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;
    // ok do the divide
    c = x - range2 * sc_floor(x / range2);
    if (c >= range)
        c = range2 - c;
    return c + lo;
}

inline float sc_pow(float a, float b) { return a >= 0.f ? std::pow(a, b) : -std::pow(-a, b); }

inline double sc_pow(double a, double b) { return a >= 0.f ? std::pow(a, b) : -std::pow(-a, b); }

inline float sc_round(float x, float quant) { return quant == 0. ? x : sc_floor(x / quant + .5f) * quant; }

inline double sc_round(double x, double quant) { return quant == 0. ? x : sc_floor(x / quant + .5) * quant; }

inline float sc_roundUp(float x, float quant) { return quant == 0. ? x : sc_ceil(x / quant) * quant; }

inline double sc_roundUp(double x, double quant) { return quant == 0. ? x : sc_ceil(x / quant) * quant; }

inline float sc_trunc(float x, float quant) { return quant == 0. ? x : sc_floor(x / quant) * quant; }

inline double sc_trunc(double x, double quant) { return quant == 0. ? x : sc_floor(x / quant) * quant; }

inline float sc_atan2(float a, float b) { return std::atan2(a, b); }

const float kFSQRT2M1 = static_cast<float32>(sqrt(2.) - 1.);
const double kDSQRT2M1 = sqrt(2.) - 1.;

inline float sc_hypotx(float x, float y) {
    float minxy;

    x = std::abs(x);
    y = std::abs(y);

    minxy = sc_min(x, y);

    return x + y - kFSQRT2M1 * minxy;
}

inline double sc_hypotx(double x, double y) {
    double minxy;

    x = std::abs(x);
    y = std::abs(y);

    minxy = sc_min(x, y);

    return x + y - kDSQRT2M1 * minxy;
}

inline int sc_div(int a, int b) {
    int c;
    if (b) {
        if (a < 0)
            c = (a + 1) / b - 1;
        else
            c = a / b;
    } else
        c = a;
    return c;
}

/*
inline int sc_mod(int a, int b)
{
    long c;
    c = a % b;
    if (c<0) c += b;
    return c;
}
*/

inline int sc_mod(int in, int hi) {
    // avoid the divide if possible
    const int lo = 0;
    if (in >= hi) {
        in -= hi;
        if (in < hi)
            return in;
    } else if (in < lo) {
        in += hi;
        if (in >= lo)
            return in;
    } else
        return in;

    if (hi == lo)
        return lo;

    int c;
    c = in % hi;
    if (c < 0)
        c += hi;
    return c;
}

inline int sc_wrap(int in, int lo, int hi) { return sc_mod(in - lo, hi - lo + 1) + lo; }

inline int sc_fold(int in, int lo, int hi) {
    int b = hi - lo;
    int b2 = b + b;
    int c = sc_mod(in - lo, b2);
    if (c > b)
        c = b2 - c;
    return c + lo;
}


inline int sc_gcd(int a, int b) {
    if (a == 0)
        return b;

    if (b == 0)
        return a;

    const bool negative = (a <= 0 && b <= 0);

    a = sc_abs(a);
    b = sc_abs(b);

    if (a == 1 || b == 1) {
        if (negative) {
            return -1;
        } else {
            return 1;
        }
    }

    if (a < b) {
        int t = a;
        a = b;
        b = t;
    }

    while (b > 0) {
        int t = a % b;
        a = b;
        b = t;
    }

    if (negative) {
        a = 0 - a;
    }

    return a;
}


inline int sc_lcm(int a, int b) {
    if (a == 0 || b == 0)
        return 0;
    else
        return (a * b) / sc_gcd(a, b);
}


inline long sc_gcd(long a, long b) {
    if (a == 0)
        return b;

    if (b == 0)
        return a;

    const bool negative = (a <= 0 && b <= 0);

    a = sc_abs(a);
    b = sc_abs(b);

    if (a == 1 || b == 1) {
        if (negative) {
            return (long)-1;
        } else {
            return (long)1;
        }
    }

    if (a < b) {
        long t = a;
        a = b;
        b = t;
    }

    while (b > 0) {
        long t = a % b;
        a = b;
        b = t;
    }

    if (negative) {
        a = 0 - a;
    }

    return a;
}


inline long sc_lcm(long a, long b) {
    if (a == 0 || b == 0)
        return (long)0;
    else
        return (a * b) / sc_gcd(a, b);
}


inline float sc_gcd(float u, float v) { return (float)sc_gcd((long)std::trunc(u), (long)std::trunc(v)); }


inline float sc_lcm(float u, float v) { return (float)sc_lcm((long)std::trunc(u), (long)std::trunc(v)); }


inline int sc_bitAnd(int a, int b) { return a & b; }

inline int sc_bitOr(int a, int b) { return a | b; }

inline int sc_leftShift(int a, int b) { return a << b; }

inline int sc_rightShift(int a, int b) { return a >> b; }

inline int sc_unsignedRightShift(int a, int b) { return (int)((uint32)a >> b); }

inline int sc_round(int x, int quant) { return quant == 0 ? x : sc_div(x + quant / 2, quant) * quant; }


inline int sc_roundUp(int x, int quant) { return quant == 0 ? x : sc_div(x + quant - 1, quant) * quant; }

inline int sc_trunc(int x, int quant) { return quant == 0 ? x : sc_div(x, quant) * quant; }

template <typename F> inline F sc_powi(F x, unsigned int n) {
    F z = 1;
    while (n != 0) {
        if ((n & 1) != 0) {
            z *= x;
        }
        n >>= 1;
        x *= x;
    }

    return z;
}

template <typename T, typename U> inline T sc_thresh(T a, U b) { return a < b ? (T)0 : a; }

template <typename T> inline T sc_clip2(T a, T b) { return sc_clip(a, -b, b); }

template <typename T> inline T sc_wrap2(T a, T b) { return sc_wrap(a, -b, b); }

template <typename T> inline T sc_fold2(T a, T b) { return sc_fold(a, -b, b); }

template <typename T> inline T sc_excess(T a, T b) { return a - sc_clip(a, -b, b); }

template <typename T> inline T sc_scaleneg(T a, T b) {
    if (a < 0)
        return a * b;
    else
        return a;
}

template <> inline float sc_scaleneg<float>(float a, float b) {
    b = 0.5f * b + 0.5f;
    return (std::abs(a) - a) * b + a;
}

template <> inline double sc_scaleneg<double>(double a, double b) {
    b = 0.5 * b + 0.5;
    return (std::abs(a) - a) * b + a;
}

template <typename T> inline T sc_amclip(T a, T b) {
    if (b < 0)
        return 0;
    else
        return a * b;
}

template <> inline float sc_amclip<float>(float a, float b) { return a * 0.5f * (b + std::abs(b)); }

template <> inline double sc_amclip<double>(double a, double b) { return a * 0.5 * (b + std::abs(b)); }

template <typename T> inline T sc_ring1(T a, T b) { return a * b + a; }

template <typename T> inline T sc_ring2(T a, T b) { return a * b + a + b; }

template <typename T> inline T sc_ring3(T a, T b) { return a * a * b; }

template <typename T> inline T sc_ring4(T a, T b) { return a * a * b - a * b * b; }

template <typename T> inline T sc_difsqr(T a, T b) { return a * a - b * b; }

template <typename T> inline T sc_sumsqr(T a, T b) { return a * a + b * b; }

template <typename T> inline T sc_sqrsum(T a, T b) {
    T z = a + b;
    return z * z;
}

template <typename T> inline T sc_sqrdif(T a, T b) {
    T z = a - b;
    return z * z;
}

#if 0

inline long sc_div(long a, long b)
{
	int c;
	if (b) {
		if (a<0) c = (a+1)/b - 1;
		else c = a/b;
	} else c = a;
	return c;
}


inline long sc_wrap(long in, long lo, long hi)
{
	return sc_mod(in - lo, hi - lo + 1) + lo;
}

inline long sc_fold(long in, long lo, long hi)
{
	long b = hi - lo;
	int b2 = b+b;
	long c = sc_mod(in - lo, b2);
	if (c>b) c = b2-c;
	return c + lo;
}

inline long sc_bitAnd(long a, long b)
{
	return a & b;
}

inline long sc_bitOr(long a, long b)
{
	return a | b;
}

inline long sc_leftShift(long a, long b)
{
	return a << b;
}

inline long sc_rightShift(long a, long b)
{
	return a >> b;
}

inline long sc_unsignedRightShift(long a, long b)
{
	return (unsigned long)a >> b;
}

inline long sc_round(long x, long quant)
{
	return quant==0 ? x : sc_div(x + quant/2, quant) * quant;
}

#endif
