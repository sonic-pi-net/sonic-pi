/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

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

namespace juce
{

//==============================================================================
/** Contains static methods for converting the byte order between different
    endiannesses.
*/
class JUCE_API  ByteOrder
{
public:
    //==============================================================================
    /** Swaps the upper and lower bytes of a 16-bit integer. */
    JUCE_CONSTEXPR static uint16 swap (uint16 value) noexcept;

    /** Reverses the order of the 4 bytes in a 32-bit integer. */
    static uint32 swap (uint32 value) noexcept;

    /** Reverses the order of the 8 bytes in a 64-bit integer. */
    static uint64 swap (uint64 value) noexcept;

    //==============================================================================
    /** Swaps the byte order of a 16-bit unsigned int if the CPU is big-endian */
    JUCE_CONSTEXPR static uint16 swapIfBigEndian (uint16 value) noexcept;

    /** Swaps the byte order of a 32-bit unsigned int if the CPU is big-endian */
    static uint32 swapIfBigEndian (uint32 value) noexcept;

    /** Swaps the byte order of a 64-bit unsigned int if the CPU is big-endian */
    static uint64 swapIfBigEndian (uint64 value) noexcept;

    /** Swaps the byte order of a 16-bit signed int if the CPU is big-endian */
    JUCE_CONSTEXPR static int16 swapIfBigEndian (int16 value) noexcept;

    /** Swaps the byte order of a 32-bit signed int if the CPU is big-endian */
    static int32 swapIfBigEndian (int32 value) noexcept;

    /** Swaps the byte order of a 64-bit signed int if the CPU is big-endian */
    static int64 swapIfBigEndian (int64 value) noexcept;

    /** Swaps the byte order of a 32-bit float if the CPU is big-endian */
    static float swapIfBigEndian (float value) noexcept;

    /** Swaps the byte order of a 64-bit float if the CPU is big-endian */
    static double swapIfBigEndian (double value) noexcept;

    /** Swaps the byte order of a 16-bit unsigned int if the CPU is little-endian */
    JUCE_CONSTEXPR static uint16 swapIfLittleEndian (uint16 value) noexcept;

    /** Swaps the byte order of a 32-bit unsigned int if the CPU is little-endian */
    static uint32 swapIfLittleEndian (uint32 value) noexcept;

    /** Swaps the byte order of a 64-bit unsigned int if the CPU is little-endian */
    static uint64 swapIfLittleEndian (uint64 value) noexcept;

    /** Swaps the byte order of a 16-bit signed int if the CPU is little-endian */
    JUCE_CONSTEXPR static int16 swapIfLittleEndian (int16 value) noexcept;

    /** Swaps the byte order of a 32-bit signed int if the CPU is little-endian */
    static int32 swapIfLittleEndian (int32 value) noexcept;

    /** Swaps the byte order of a 64-bit signed int if the CPU is little-endian */
    static int64 swapIfLittleEndian (int64 value) noexcept;

    /** Swaps the byte order of a 32-bit float if the CPU is little-endian */
    static float swapIfLittleEndian (float value) noexcept;

    /** Swaps the byte order of a 64-bit float if the CPU is little-endian */
    static double swapIfLittleEndian (double value) noexcept;

    //==============================================================================
    /** Turns 4 bytes into a little-endian integer. */
    static uint32 littleEndianInt (const void* bytes) noexcept;

    /** Turns 4 characters into a little-endian integer. */
    JUCE_CONSTEXPR static uint32 littleEndianInt (char c1, char c2, char c3, char c4) noexcept;

    /** Turns 8 bytes into a little-endian integer. */
    static uint64 littleEndianInt64 (const void* bytes) noexcept;

    /** Turns 2 bytes into a little-endian integer. */
    static uint16 littleEndianShort (const void* bytes) noexcept;

    /** Turns 4 bytes into a big-endian integer. */
    static uint32 bigEndianInt (const void* bytes) noexcept;

    /** Turns 8 bytes into a big-endian integer. */
    static uint64 bigEndianInt64 (const void* bytes) noexcept;

    /** Turns 2 bytes into a big-endian integer. */
    static uint16 bigEndianShort (const void* bytes) noexcept;

    //==============================================================================
    /** Converts 3 little-endian bytes into a signed 24-bit value (which is sign-extended to 32 bits). */
    static int littleEndian24Bit (const void* bytes) noexcept;

    /** Converts 3 big-endian bytes into a signed 24-bit value (which is sign-extended to 32 bits). */
    static int bigEndian24Bit (const void* bytes) noexcept;

    /** Copies a 24-bit number to 3 little-endian bytes. */
    static void littleEndian24BitToChars (int value, void* destBytes) noexcept;

    /** Copies a 24-bit number to 3 big-endian bytes. */
    static void bigEndian24BitToChars (int value, void* destBytes) noexcept;

    //==============================================================================
    /** Returns true if the current CPU is big-endian. */
    JUCE_CONSTEXPR static bool isBigEndian() noexcept;

private:
    ByteOrder() JUCE_DELETED_FUNCTION;

    JUCE_DECLARE_NON_COPYABLE (ByteOrder)
};


//==============================================================================
#if JUCE_MSVC && ! defined (__INTEL_COMPILER)
 #pragma intrinsic (_byteswap_ulong)
#endif

JUCE_CONSTEXPR inline uint16 ByteOrder::swap (uint16 n) noexcept
{
    return static_cast<uint16> ((n << 8) | (n >> 8));
}

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

#if JUCE_LITTLE_ENDIAN
 JUCE_CONSTEXPR inline uint16 ByteOrder::swapIfBigEndian (const uint16 v) noexcept                                  { return v; }
 inline uint32 ByteOrder::swapIfBigEndian (const uint32 v) noexcept                                                 { return v; }
 inline uint64 ByteOrder::swapIfBigEndian (const uint64 v) noexcept                                                 { return v; }
 JUCE_CONSTEXPR inline int16 ByteOrder::swapIfBigEndian (const int16 v) noexcept                                    { return v; }
 inline int32 ByteOrder::swapIfBigEndian (const int32 v) noexcept                                                   { return v; }
 inline int64 ByteOrder::swapIfBigEndian (const int64 v) noexcept                                                   { return v; }
 inline float ByteOrder::swapIfBigEndian (const float v) noexcept                                                   { return v; }
 inline double ByteOrder::swapIfBigEndian (const double v) noexcept                                                 { return v; }

 JUCE_CONSTEXPR inline uint16 ByteOrder::swapIfLittleEndian (const uint16 v) noexcept                               { return swap (v); }
 inline uint32 ByteOrder::swapIfLittleEndian (const uint32 v) noexcept                                              { return swap (v); }
 inline uint64 ByteOrder::swapIfLittleEndian (const uint64 v) noexcept                                              { return swap (v); }
 JUCE_CONSTEXPR inline int16 ByteOrder::swapIfLittleEndian (const int16 v) noexcept                                 { return static_cast<int16> (swap (static_cast<uint16> (v))); }
 inline int32 ByteOrder::swapIfLittleEndian (const int32 v) noexcept                                                { return static_cast<int32> (swap (static_cast<uint32> (v))); }
 inline int64 ByteOrder::swapIfLittleEndian (const int64 v) noexcept                                                { return static_cast<int64> (swap (static_cast<uint64> (v))); }
 inline float ByteOrder::swapIfLittleEndian (const float v) noexcept                                                { union { uint32 asUInt; float asFloat;  } n; n.asFloat = v; n.asUInt = ByteOrder::swap (n.asUInt); return n.asFloat; }
 inline double ByteOrder::swapIfLittleEndian (const double v) noexcept                                              { union { uint64 asUInt; double asFloat; } n; n.asFloat = v; n.asUInt = ByteOrder::swap (n.asUInt); return n.asFloat; }

 inline uint32 ByteOrder::littleEndianInt (const void* const bytes) noexcept                                        { return *static_cast<const uint32*> (bytes); }
 JUCE_CONSTEXPR inline uint32 ByteOrder::littleEndianInt (char c1, char c2, char c3, char c4) noexcept              { return (((uint32) c4) << 24) + (((uint32) c3) << 16) + (((uint32) c2) << 8) + (uint32) c1; }

 inline uint64 ByteOrder::littleEndianInt64 (const void* const bytes) noexcept                                      { return *static_cast<const uint64*> (bytes); }
 inline uint16 ByteOrder::littleEndianShort (const void* const bytes) noexcept                                      { return *static_cast<const uint16*> (bytes); }
 inline uint32 ByteOrder::bigEndianInt (const void* const bytes) noexcept                                           { return swap (*static_cast<const uint32*> (bytes)); }
 inline uint64 ByteOrder::bigEndianInt64 (const void* const bytes) noexcept                                         { return swap (*static_cast<const uint64*> (bytes)); }
 inline uint16 ByteOrder::bigEndianShort (const void* const bytes) noexcept                                         { return swap (*static_cast<const uint16*> (bytes)); }
 JUCE_CONSTEXPR inline bool ByteOrder::isBigEndian() noexcept                                                       { return false; }
#else
 JUCE_CONSTEXPR inline uint16 ByteOrder::swapIfBigEndian (const uint16 v) noexcept                                  { return swap (v); }
 inline uint32 ByteOrder::swapIfBigEndian (const uint32 v) noexcept                                                 { return swap (v); }
 inline uint64 ByteOrder::swapIfBigEndian (const uint64 v) noexcept                                                 { return swap (v); }
 JUCE_CONSTEXPR inline int16 ByteOrder::swapIfBigEndian (const int16 v) noexcept                                    { return static_cast<int16> (swap (static_cast<uint16> (v))); }
 inline int32 ByteOrder::swapIfBigEndian (const int32 v) noexcept                                                   { return static_cast<int16> (swap (static_cast<uint16> (v))); }
 inline int64 ByteOrder::swapIfBigEndian (const int64 v) noexcept                                                   { return static_cast<int16> (swap (static_cast<uint16> (v))); }
 inline float ByteOrder::swapIfBigEndian (const float v) noexcept                                                   { union { uint32 asUInt; float asFloat;  } n; n.asFloat = v; n.asUInt = ByteOrder::swap (n.asUInt); return n.asFloat; }
 inline double ByteOrder::swapIfBigEndian (const double v) noexcept                                                 { union { uint64 asUInt; double asFloat; } n; n.asFloat = v; n.asUInt = ByteOrder::swap (n.asUInt); return n.asFloat; }

 JUCE_CONSTEXPR inline uint16 ByteOrder::swapIfLittleEndian (const uint16 v) noexcept                               { return v; }
 inline uint32 ByteOrder::swapIfLittleEndian (const uint32 v) noexcept                                              { return v; }
 inline uint64 ByteOrder::swapIfLittleEndian (const uint64 v) noexcept                                              { return v; }
 JUCE_CONSTEXPR inline int16 ByteOrder::swapIfLittleEndian (const int16 v) noexcept                                 { return v; }
 inline int32 ByteOrder::swapIfLittleEndian (const int32 v) noexcept                                                { return v; }
 inline int64 ByteOrder::swapIfLittleEndian (const int64 v) noexcept                                                { return v; }
 inline float ByteOrder::swapIfLittleEndian (const float v) noexcept                                                { return v; }
 inline double ByteOrder::swapIfLittleEndian (const double v) noexcept                                              { return v; }

 inline uint32 ByteOrder::littleEndianInt (const void* const bytes) noexcept                                        { return swap (*static_cast<const uint32*> (bytes)); }
 JUCE_CONSTEXPR inline uint32 ByteOrder::littleEndianInt (char c1, char c2, char c3, char c4) noexcept              { return (((uint32) c1) << 24) + (((uint32) c2) << 16) + (((uint32) c3) << 8) + (uint32) c4; }
 inline uint64 ByteOrder::littleEndianInt64 (const void* const bytes) noexcept                                      { return swap (*static_cast<const uint64*> (bytes)); }
 inline uint16 ByteOrder::littleEndianShort (const void* const bytes) noexcept                                      { return swap (*static_cast<const uint16*> (bytes)); }
 inline uint32 ByteOrder::bigEndianInt (const void* const bytes) noexcept                                           { return *static_cast<const uint32*> (bytes); }
 inline uint64 ByteOrder::bigEndianInt64 (const void* const bytes) noexcept                                         { return *static_cast<const uint64*> (bytes); }
 inline uint16 ByteOrder::bigEndianShort (const void* const bytes) noexcept                                         { return *static_cast<const uint16*> (bytes); }
 JUCE_CONSTEXPR inline bool ByteOrder::isBigEndian() noexcept                                                       { return true; }
#endif

inline int  ByteOrder::littleEndian24Bit (const void* const bytes) noexcept                                         { return (int) ((((unsigned int) static_cast<const int8*> (bytes)[2]) << 16) | (((unsigned int) static_cast<const uint8*> (bytes)[1]) << 8) | ((unsigned int) static_cast<const uint8*> (bytes)[0])); }
inline int  ByteOrder::bigEndian24Bit (const void* const bytes) noexcept                                            { return (int) ((((unsigned int) static_cast<const int8*> (bytes)[0]) << 16) | (((unsigned int) static_cast<const uint8*> (bytes)[1]) << 8) | ((unsigned int) static_cast<const uint8*> (bytes)[2])); }
inline void ByteOrder::littleEndian24BitToChars (const int value, void* const destBytes) noexcept                   { static_cast<uint8*> (destBytes)[0] = (uint8) value;         static_cast<uint8*> (destBytes)[1] = (uint8) (value >> 8); static_cast<uint8*> (destBytes)[2] = (uint8) (value >> 16); }
inline void ByteOrder::bigEndian24BitToChars (const int value, void* const destBytes) noexcept                      { static_cast<uint8*> (destBytes)[0] = (uint8) (value >> 16); static_cast<uint8*> (destBytes)[1] = (uint8) (value >> 8); static_cast<uint8*> (destBytes)[2] = (uint8) value; }

} // namespace juce
