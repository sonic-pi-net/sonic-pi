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

/*

These macros allow one to write code which can be compiled optimally depending on
what loop constructs the compiler can best generate code.

*/

#pragma once

#include <string.h>
#include <cassert>

#if 1

// loop type
#    define FOR_IS_FASTER 1
#    define WHILE_IS_FASTER 0
// indexing type
#    define PREINCREMENT_IS_FASTER 1
#    define POSTINCREMENT_IS_FASTER 0

#else

// loop type
#    define FOR_IS_FASTER 1
#    define WHILE_IS_FASTER 0
// indexing type
#    define PREINCREMENT_IS_FASTER 0
#    define POSTINCREMENT_IS_FASTER 1

#endif


// LOOPING MACROS :

#if FOR_IS_FASTER

#    define LOOP(length, stmt)                                                                                         \
        for (int xxi = 0; xxi < (length); ++xxi) {                                                                     \
            stmt;                                                                                                      \
        }

#elif WHILE_IS_FASTER

#    define LOOP(length, stmt)                                                                                         \
        {                                                                                                              \
            int xxn = (length);                                                                                        \
            while (--xxn) {                                                                                            \
                stmt;                                                                                                  \
            }                                                                                                          \
        }

#endif


// above macros are not friendly to the debugger
#if FOR_IS_FASTER

#    define LooP(length) for (int xxi = 0; xxi < (length); ++xxi)

#elif WHILE_IS_FASTER

#    define LooP(length) for (int xxi = (length); --xxi;)

#endif

/* faster loop macro, length is required to be larger than 0 */
#define LOOP1(length, stmt)                                                                                            \
    {                                                                                                                  \
        int xxn = (length);                                                                                            \
        assert(length);                                                                                                \
        do {                                                                                                           \
            stmt;                                                                                                      \
        } while (--xxn);                                                                                               \
    }


// LOOP INDEXING :

/*
meanings of the indexing macros:
    ZXP = dereference and pre or post increment
    ZX = dereference
    PZ = preincrement (if applicable)
    ZP = postincrement (if applicable)
    ZOFF = offset from the pointer of the first element of the array
        (preincrement requires a ZOFF of 1 which is pre-subtracted from the
        base pointer. For other indexing types ZOFF is zero)
*/

#if PREINCREMENT_IS_FASTER
#    define ZXP(z) (*++(z))
#    define ZX(z) (*(z))
#    define PZ(z) (++(z))
#    define ZP(z) (z)
#    define ZOFF (1)
#elif POSTINCREMENT_IS_FASTER
#    define ZXP(z) (*(z)++)
#    define ZX(z) (*(z))
#    define PZ(z) (z)
#    define ZP(z) ((z)++)
#    define ZOFF (0)
#endif

// ACCESSING INLETS AND OUTLETS :

// unit inputs
#define ZIN(i) (IN(i) - ZOFF) // get buffer pointer offset for iteration
#define ZIN0(i) (IN(i)[0]) // get first sample

// unit outputs
#define ZOUT(i) (OUT(i) - ZOFF) // get buffer pointer offset for iteration
#define ZOUT0(i) (OUT(i)[0]) // get first sample

#include "SC_BoundsMacros.h"

#include <assert.h>

// Efficiency notes: Clear and Copy was benchmarked in October 2008.
// See http://www.mcld.co.uk/blog/blog.php?217 and http://www.mcld.co.uk/blog/blog.php?218

// Set floating-point data to all zeros
inline void Clear(int numSamples, float* out) {
    // The memset approach is valid on any system using IEEE floating-point. On other systems, please check...
    memset(out, 0, (unsigned int)numSamples * sizeof(float));
}

inline void Clear(int numSamples, double* out) {
    // The memset approach is valid on any system using IEEE floating-point. On other systems, please check...
    memset(out, 0, (unsigned int)numSamples * sizeof(double));
}


inline void Copy(int numSamples, float* out, float* in) { memcpy(out, in, (unsigned int)numSamples * sizeof(float)); }

inline void Fill(int numSamples, float* out, float level) {
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) = level;);
}

inline void Fill(int numSamples, float* out, float level, float slope) {
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) = level; level += slope;);
}

inline void Accum(int numSamples, float* out, float* in) {
    in -= ZOFF;
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) += ZXP(in););
}

inline void Scale(int numSamples, float* out, float level) {
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) *= level;);
}

inline float Scale(int numSamples, float* out, float level, float slope) {
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) *= level; level += slope;);
    return level;
}

inline float Scale(int numSamples, float* out, float* in, float level, float slope) {
    in -= ZOFF;
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) = ZXP(in) * level; level += slope;);
    return level;
}

inline float ScaleMix(int numSamples, float* out, float* in, float level, float slope) {
    in -= ZOFF;
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) += ZXP(in) * level; level += slope;);
    return level;
}

inline void Scale(int numSamples, float* out, float* in, float level) {
    in -= ZOFF;
    out -= ZOFF;
    LOOP(numSamples, ZXP(out) = ZXP(in) * level;);
}

// in these the pointers are assumed to already have been pre-offset.
inline void ZCopy(int numSamples, float* out, const float* in) {
    // pointers must be 8 byte aligned
    // assert((((long)(out+ZOFF) & 7) == 0) && (((long)(in+ZOFF) & 7) == 0));
    if (in == out)
        return;
    if ((numSamples & 1) == 0) {
        // copying doubles is faster on powerpc.
        double* outd = (double*)(out + ZOFF) - ZOFF;
        const double* ind = (const double*)(in + ZOFF) - ZOFF;
        LOOP(numSamples >> 1, ZXP(outd) = ZXP(ind););
    } else {
        LOOP(numSamples, ZXP(out) = ZXP(in););
    }
}

inline void ZClear(int numSamples, float* out) {
    // pointers must be 8 byte aligned
    // assert((((long)(out+ZOFF) & 7) == 0) && (((long)(in+ZOFF) & 7) == 0));
    if ((numSamples & 1) == 0) {
        // copying doubles is faster on powerpc.
        double* outd = (double*)(out + ZOFF) - ZOFF;
        LOOP(numSamples >> 1, ZXP(outd) = 0.;);
    } else {
        LOOP(numSamples, ZXP(out) = 0.f;);
    }
}

inline void ZAccum(int numSamples, float* out, float* in) { LOOP(numSamples, ZXP(out) += ZXP(in);); }

template <typename Functor> inline void loop(int length, Functor const& f) {
    for (int i = 0; i < length; ++i)
        f();
}

template <typename Functor> inline void loop1(int length, Functor const& f) {
    assert(length > 0);
    int i = length;
    do {
        f();
    } while (--i);
}
