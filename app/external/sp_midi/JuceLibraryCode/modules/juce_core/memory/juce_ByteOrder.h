/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

#if ! DOXYGEN && (JUCE_MAC || JUCE_IOS)
 #include <libkern/OSByteOrder.h>
#endif

namespace juce
{

//==============================================================================
/** Contains static methods for converting the byte order between different
    endiannesses.

    @tags{Core}
*/
class JUCE_API  ByteOrder
{
public:
    //==============================================================================
    /** Swaps the upper and lower bytes of a 16-bit integer. */
    constexpr static uint16 swap (uint16 value) noexcept;

    /** Swaps the upper and lower bytes of a 16-bit integer. */
    constexpr static int16 swap (int16 value) noexcept;

    /** Reverses the order of the 4 bytes in a 32-bit integer. */
    static uint32 swap (uint32 value) noexcept;

    /** Reverses the order of the 4 bytes in a 32-bit integer. */
    static int32 swap (int32 value) noexcept;

    /** Reverses the order of the 8 bytes in a 64-bit integer. */
    static uint64 swap (uint64 value) noexcept;

    /** Reverses the order of the 8 bytes in a 64-bit integer. */
    static int64 swap (int64 value) noexcept;

    /** Returns a garbled float which has the reverse byte-order of the original. */
    static float swap (float value) noexcept;

    /** Returns a garbled double which has the reverse byte-order of the original. */
    static double swap (double value) noexcept;

    //==============================================================================
    /** Swaps the byte order of a signed or unsigned integer if the CPU is big-endian */
    template <typename Type>
    static Type swapIfBigEndian (Type value) noexcept
    {
       #if JUCE_LITTLE_ENDIAN
        return value;
       #else
        return swap (value);
       #endif
    }

    /** Swaps the byte order of a signed or unsigned integer if the CPU is little-endian */
    template <typename Type>
    static Type swapIfLittleEndian (Type value) noexcept
    {
       #if JUCE_LITTLE_ENDIAN
        return swap (value);
       #else
        return value;
       #endif
    }

    //==============================================================================
    /** Turns 4 bytes into a little-endian integer. */
    constexpr static uint32 littleEndianInt (const void* bytes) noexcept;

    /** Turns 8 bytes into a little-endian integer. */
    constexpr static uint64 littleEndianInt64 (const void* bytes) noexcept;

    /** Turns 2 bytes into a little-endian integer. */
    constexpr static uint16 littleEndianShort (const void* bytes) noexcept;

    /** Converts 3 little-endian bytes into a signed 24-bit value (which is sign-extended to 32 bits). */
    constexpr static int littleEndian24Bit (const void* bytes) noexcept;

    /** Copies a 24-bit number to 3 little-endian bytes. */
    static void littleEndian24BitToChars (int32 value, void* destBytes) noexcept;

    //==============================================================================
    /** Turns 4 bytes into a big-endian integer. */
    constexpr static uint32 bigEndianInt (const void* bytes) noexcept;

    /** Turns 8 bytes into a big-endian integer. */
    constexpr static uint64 bigEndianInt64 (const void* bytes) noexcept;

    /** Turns 2 bytes into a big-endian integer. */
    constexpr static uint16 bigEndianShort (const void* bytes) noexcept;

    /** Converts 3 big-endian bytes into a signed 24-bit value (which is sign-extended to 32 bits). */
    constexpr static int bigEndian24Bit (const void* bytes) noexcept;

    /** Copies a 24-bit number to 3 big-endian bytes. */
    static void bigEndian24BitToChars (int32 value, void* destBytes) noexcept;

    //==============================================================================
    /** Constructs a 16-bit integer from its constituent bytes, in order of significance. */
    constexpr static uint16 makeInt (uint8 leastSig, uint8 mostSig) noexcept;

    /** Constructs a 32-bit integer from its constituent bytes, in order of significance. */
    constexpr static uint32 makeInt (uint8 leastSig, uint8 byte1, uint8 byte2, uint8 mostSig) noexcept;

    /** Constructs a 64-bit integer from its constituent bytes, in order of significance. */
    constexpr static uint64 makeInt (uint8 leastSig, uint8 byte1, uint8 byte2, uint8 byte3,
                                     uint8 byte4, uint8 byte5, uint8 byte6, uint8 mostSig) noexcept;

    //==============================================================================
    /** Returns true if the current CPU is big-endian. */
    constexpr static bool isBigEndian() noexcept
    {
       #if JUCE_LITTLE_ENDIAN
        return false;
       #else
        return true;
       #endif
    }

private:
    ByteOrder() = delete;
};


//==============================================================================
constexpr inline uint16 ByteOrder::swap (uint16 v) noexcept         { return static_cast<uint16> ((v << 8) | (v >> 8)); }
constexpr inline int16  ByteOrder::swap (int16  v) noexcept         { return static_cast<int16> (swap (static_cast<uint16> (v))); }
inline int32  ByteOrder::swap (int32 v) noexcept                    { return static_cast<int32> (swap (static_cast<uint32> (v))); }
inline int64  ByteOrder::swap (int64 v) noexcept                    { return static_cast<int64> (swap (static_cast<uint64> (v))); }
inline float  ByteOrder::swap (float v) noexcept                    { union { uint32 asUInt; float asFloat;  } n; n.asFloat = v; n.asUInt = swap (n.asUInt); return n.asFloat; }
inline double ByteOrder::swap (double v) noexcept                   { union { uint64 asUInt; double asFloat; } n; n.asFloat = v; n.asUInt = swap (n.asUInt); return n.asFloat; }

#if JUCE_MSVC && ! defined (__INTEL_COMPILER)
 #pragma intrinsic (_byteswap_ulong)
#endif

inline uint32 ByteOrder::swap (uint32 n) noexcept
{
   #if JUCE_MAC || JUCE_IOS
    return OSSwapInt32 (n);
   #elif (JUCE_GCC  || JUCE_CLANG) && JUCE_INTEL && ! JUCE_NO_INLINE_ASM
    asm("bswap %%eax" : "=a"(n) : "a"(n));
    return n;
   #elif JUCE_MSVC
    return _byteswap_ulong (n);
   #elif JUCE_ANDROID
    return bswap_32 (n);
   #else
    return (n << 24) | (n >> 24) | ((n & 0xff00) << 8) | ((n & 0xff0000) >> 8);
   #endif
}

inline uint64 ByteOrder::swap (uint64 value) noexcept
{
   #if JUCE_MAC || JUCE_IOS
    return OSSwapInt64 (value);
   #elif JUCE_MSVC
    return _byteswap_uint64 (value);
   #else
    return (((uint64) swap ((uint32) value)) << 32) | swap ((uint32) (value >> 32));
   #endif
}

constexpr inline uint16 ByteOrder::makeInt (uint8 b0, uint8 b1) noexcept
{
    return static_cast<uint16> (static_cast<uint16> (b0) | (static_cast<uint16> (b1) << 8));
}

constexpr inline uint32 ByteOrder::makeInt (uint8 b0, uint8 b1, uint8 b2, uint8 b3) noexcept
{
    return static_cast<uint32> (b0)        | (static_cast<uint32> (b1) << 8)
        | (static_cast<uint32> (b2) << 16) | (static_cast<uint32> (b3) << 24);
}

constexpr inline uint64 ByteOrder::makeInt (uint8 b0, uint8 b1, uint8 b2, uint8 b3, uint8 b4, uint8 b5, uint8 b6, uint8 b7) noexcept
{
    return static_cast<uint64> (b0)        | (static_cast<uint64> (b1) << 8)  | (static_cast<uint64> (b2) << 16) | (static_cast<uint64> (b3) << 24)
        | (static_cast<uint64> (b4) << 32) | (static_cast<uint64> (b5) << 40) | (static_cast<uint64> (b6) << 48) | (static_cast<uint64> (b7) << 56);
}

constexpr inline uint16 ByteOrder::littleEndianShort (const void* bytes) noexcept         { return makeInt (static_cast<const uint8*> (bytes)[0], static_cast<const uint8*> (bytes)[1]); }
constexpr inline uint32 ByteOrder::littleEndianInt   (const void* bytes) noexcept         { return makeInt (static_cast<const uint8*> (bytes)[0], static_cast<const uint8*> (bytes)[1],
                                                                                                            static_cast<const uint8*> (bytes)[2], static_cast<const uint8*> (bytes)[3]); }
constexpr inline uint64 ByteOrder::littleEndianInt64 (const void* bytes) noexcept         { return makeInt (static_cast<const uint8*> (bytes)[0], static_cast<const uint8*> (bytes)[1],
                                                                                                            static_cast<const uint8*> (bytes)[2], static_cast<const uint8*> (bytes)[3],
                                                                                                            static_cast<const uint8*> (bytes)[4], static_cast<const uint8*> (bytes)[5],
                                                                                                            static_cast<const uint8*> (bytes)[6], static_cast<const uint8*> (bytes)[7]); }

constexpr inline uint16 ByteOrder::bigEndianShort    (const void* bytes) noexcept         { return makeInt (static_cast<const uint8*> (bytes)[1], static_cast<const uint8*> (bytes)[0]); }
constexpr inline uint32 ByteOrder::bigEndianInt      (const void* bytes) noexcept         { return makeInt (static_cast<const uint8*> (bytes)[3], static_cast<const uint8*> (bytes)[2],
                                                                                                            static_cast<const uint8*> (bytes)[1], static_cast<const uint8*> (bytes)[0]); }
constexpr inline uint64 ByteOrder::bigEndianInt64    (const void* bytes) noexcept         { return makeInt (static_cast<const uint8*> (bytes)[7], static_cast<const uint8*> (bytes)[6],
                                                                                                            static_cast<const uint8*> (bytes)[5], static_cast<const uint8*> (bytes)[4],
                                                                                                            static_cast<const uint8*> (bytes)[3], static_cast<const uint8*> (bytes)[2],
                                                                                                            static_cast<const uint8*> (bytes)[1], static_cast<const uint8*> (bytes)[0]); }

constexpr inline int32 ByteOrder::littleEndian24Bit  (const void* bytes) noexcept         { return (int32) ((((uint32) static_cast<const int8*> (bytes)[2]) << 16) | (((uint32) static_cast<const uint8*> (bytes)[1]) << 8) | ((uint32) static_cast<const uint8*> (bytes)[0])); }
constexpr inline int32 ByteOrder::bigEndian24Bit     (const void* bytes) noexcept         { return (int32) ((((uint32) static_cast<const int8*> (bytes)[0]) << 16) | (((uint32) static_cast<const uint8*> (bytes)[1]) << 8) | ((uint32) static_cast<const uint8*> (bytes)[2])); }

inline void ByteOrder::littleEndian24BitToChars (int32 value, void* destBytes) noexcept   { static_cast<uint8*> (destBytes)[0] = (uint8) value;         static_cast<uint8*> (destBytes)[1] = (uint8) (value >> 8); static_cast<uint8*> (destBytes)[2] = (uint8) (value >> 16); }
inline void ByteOrder::bigEndian24BitToChars    (int32 value, void* destBytes) noexcept   { static_cast<uint8*> (destBytes)[0] = (uint8) (value >> 16); static_cast<uint8*> (destBytes)[1] = (uint8) (value >> 8); static_cast<uint8*> (destBytes)[2] = (uint8) value; }

} // namespace juce
