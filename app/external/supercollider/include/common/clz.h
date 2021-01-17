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

count leading zeroes function and those that can be derived from it

*/


#pragma once

#include "SC_Types.h"

#ifdef __MWERKS__

#    define __PPC__ 1
#    define __X86__ 0

// powerpc native count leading zeroes instruction:
#    define CLZ(x) ((int)__cntlzw((unsigned int)x))

#elif defined(__GNUC__)

/* use gcc's builtins */
static __inline__ int32 CLZ(int32 arg) {
    if (arg)
        return __builtin_clz(arg);
    else
        return 32;
}

#elif defined(_MSC_VER)

#    include <intrin.h>
#    pragma intrinsic(_BitScanReverse)

__forceinline static int32 CLZ(int32 arg) {
    unsigned long idx;
    if (_BitScanReverse(&idx, (unsigned long)arg)) {
        return (int32)(31 - idx);
    }
    return 32;
}

#elif defined(__ppc__) || defined(__powerpc__) || defined(__PPC__)

static __inline__ int32 CLZ(int32 arg) {
    __asm__ volatile("cntlzw %0, %1" : "=r"(arg) : "r"(arg));
    return arg;
}

#elif defined(__i386__) || defined(__x86_64__)
static __inline__ int32 CLZ(int32 arg) {
    if (arg) {
        __asm__ volatile("bsrl %0, %0\nxorl $31, %0\n" : "=r"(arg) : "0"(arg));
    } else {
        arg = 32;
    }
    return arg;
}

#elif defined(SC_IPHONE)
static __inline__ int32 CLZ(int32 arg) { return __builtin_clz(arg); }

#else
#    error "clz.h: Unsupported architecture"
#endif

// count trailing zeroes
inline int32 CTZ(int32 x) { return 32 - CLZ(~x & (x - 1)); }

// count leading ones
inline int32 CLO(int32 x) { return CLZ(~x); }

// count trailing ones
inline int32 CTO(int32 x) { return 32 - CLZ(x & (~x - 1)); }

// number of bits required to represent x.
inline int32 NUMBITS(int32 x) { return 32 - CLZ(x); }

// log2 of the next power of two greater than or equal to x.
inline int32 LOG2CEIL(int32 x) { return 32 - CLZ(x - 1); }

// is x a power of two
inline bool ISPOWEROFTWO(int32 x) { return (x & (x - 1)) == 0; }

// next power of two greater than or equal to x
inline int32 NEXTPOWEROFTWO(int32 x) { return (int32)1L << LOG2CEIL(x); }

// previous power of two less than or equal to x
inline int32 PREVIOUSPOWEROFTWO(int32 x) {
    if (ISPOWEROFTWO(x))
        return x;
    return (int32)1L << (LOG2CEIL(x) - 1);
}

// input a series of counting integers, outputs a series of gray codes .
inline int32 GRAYCODE(int32 x) { return x ^ (x >> 1); }

// find least significant bit
inline int32 LSBit(int32 x) { return x & -x; }

// find least significant bit position
inline int32 LSBitPos(int32 x) { return CTZ(x & -x); }

// find most significant bit position
inline int32 MSBitPos(int32 x) { return 31 - CLZ(x); }

// find most significant bit
inline int32 MSBit(int32 x) { return (int32)1L << MSBitPos(x); }

// count number of one bits
inline uint32 ONES(uint32 x) {
    uint32 t;
    x = x - ((x >> 1) & 0x55555555);
    t = ((x >> 2) & 0x33333333);
    x = (x & 0x33333333) + t;
    x = (x + (x >> 4)) & 0x0F0F0F0F;
    x = x + (x << 8);
    x = x + (x << 16);
    return x >> 24;
}

// count number of zero bits
inline uint32 ZEROES(uint32 x) { return ONES(~x); }


// reverse bits in a word
inline uint32 BitReverse(uint32 x) {
    x = ((x & 0xAAAAAAAA) >> 1) | ((x & 0x55555555) << 1);
    x = ((x & 0xCCCCCCCC) >> 2) | ((x & 0x33333333) << 2);
    x = ((x & 0xF0F0F0F0) >> 4) | ((x & 0x0F0F0F0F) << 4);
    x = ((x & 0xFF00FF00) >> 8) | ((x & 0x00FF00FF) << 8);
    return (x >> 16) | (x << 16);
}

// barrel shifts
inline uint32 RotateRight(uint32 x, uint32 s) {
    s = s & 31;
    return (x << (32 - s)) | (x >> s);
}

inline uint32 RotateLeft(uint32 x, uint32 s) {
    s = s & 31;
    return (x >> (32 - s)) | (x << s);
}
