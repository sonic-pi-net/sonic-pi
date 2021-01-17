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
#include "SC_Endian.h"

// These hash functions are among the best there are in terms of both speed and quality.
// A good hash function makes a lot of difference.
// I have not used Bob Jenkins own hash function because the keys I use are relatively short.


// hash function for a string
inline int32 Hash(const char* inKey) {
    // the one-at-a-time hash.
    // a very good hash function. ref: a web page by Bob Jenkins.
    // http://www.burtleburtle.net/bob/hash/doobs.html
    int32 hash = 0;
    while (*inKey) {
        hash += *inKey++;
        hash += hash << 10;
        hash ^= hash >> 6;
    }
    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;
    return hash;
}

// hash function for a string that also returns the length
inline int32 Hash(const char* inKey, size_t* outLength) {
    // the one-at-a-time hash.
    // a very good hash function. ref: a web page by Bob Jenkins.
    const char* origKey = inKey;
    int32 hash = 0;
    while (*inKey) {
        hash += *inKey++;
        hash += hash << 10;
        hash ^= hash >> 6;
    }
    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;
    *outLength = (size_t)(inKey - origKey);
    return hash;
}

// hash function for an array of char
inline int32 Hash(const char* inKey, int32 inLength) {
    // the one-at-a-time hash.
    // a very good hash function. ref: a web page by Bob Jenkins.
    int32 hash = 0;
    for (int i = 0; i < inLength; ++i) {
        hash += *inKey++;
        hash += hash << 10;
        hash ^= hash >> 6;
    }
    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;
    return hash;
}

// hash function for integers
inline int32 Hash(int32 inKey) {
    // Thomas Wang's integer hash.
    // http://www.concentric.net/~Ttwang/tech/inthash.htm
    // a faster hash for integers. also very good.
    uint32 hash = (uint32)inKey;
    hash += ~(hash << 15);
    hash ^= hash >> 10;
    hash += hash << 3;
    hash ^= hash >> 6;
    hash += ~(hash << 11);
    hash ^= hash >> 16;
    return (int32)hash;
}

inline int64 Hash64(int64 inKey) {
    // Thomas Wang's 64 bit integer hash.
    uint64 hash = (uint64)inKey;
    hash += ~(hash << 32);
    hash ^= (hash >> 22);
    hash += ~(hash << 13);
    hash ^= (hash >> 8);
    hash += (hash << 3);
    hash ^= (hash >> 15);
    hash += ~(hash << 27);
    hash ^= (hash >> 31);
    return (int64)hash;
}

inline int32 Hash(const int32* inKey, int32 inLength) {
    // one-at-a-time hashing of a string of int32's.
    // uses Thomas Wang's integer hash for the combining step.
    int32 hash = 0;
    for (int i = 0; i < inLength; ++i) {
        hash = Hash(hash + *inKey++);
    }
    return hash;
}

#if BYTE_ORDER == LITTLE_ENDIAN
const int32 kLASTCHAR = (int32)0xFF000000;
#else
const int32 kLASTCHAR = (int32)0x000000FF;
#endif

inline int32 Hash(const int32* inKey) {
    // hashing of a string of int32's.
    // uses Thomas Wang's integer hash for the combining step.
    int32 hash = 0;
    int32 c;
    do {
        c = *inKey++;
        hash = Hash(hash + c);
    } while (c & kLASTCHAR);
    return hash;
}
