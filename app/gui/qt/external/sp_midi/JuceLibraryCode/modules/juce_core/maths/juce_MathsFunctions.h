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
/*
    This file sets up some handy mathematical typdefs and functions.
*/

//==============================================================================
// Definitions for the int8, int16, int32, int64 and pointer_sized_int types.

/** A platform-independent 8-bit signed integer type. */
using int8      = signed char;
/** A platform-independent 8-bit unsigned integer type. */
using uint8     = unsigned char;
/** A platform-independent 16-bit signed integer type. */
using int16     = signed short;
/** A platform-independent 16-bit unsigned integer type. */
using uint16    = unsigned short;
/** A platform-independent 32-bit signed integer type. */
using int32     = signed int;
/** A platform-independent 32-bit unsigned integer type. */
using uint32    = unsigned int;

#if JUCE_MSVC
  /** A platform-independent 64-bit integer type. */
  using int64  = __int64;
  /** A platform-independent 64-bit unsigned integer type. */
  using uint64 = unsigned __int64;
#else
  /** A platform-independent 64-bit integer type. */
  using int64  = long long;
  /** A platform-independent 64-bit unsigned integer type. */
  using uint64 = unsigned long long;
#endif

#ifndef DOXYGEN
 /** A macro for creating 64-bit literals.
     Historically, this was needed to support portability with MSVC6, and is kept here
     so that old code will still compile, but nowadays every compiler will support the
     LL and ULL suffixes, so you should use those in preference to this macro.
 */
 #define literal64bit(longLiteral)     (longLiteral##LL)
#endif

#if JUCE_64BIT
  /** A signed integer type that's guaranteed to be large enough to hold a pointer without truncating it. */
  using pointer_sized_int  = int64;
  /** An unsigned integer type that's guaranteed to be large enough to hold a pointer without truncating it. */
  using pointer_sized_uint = uint64;
#elif JUCE_MSVC
  /** A signed integer type that's guaranteed to be large enough to hold a pointer without truncating it. */
  using pointer_sized_int  = _W64 int;
  /** An unsigned integer type that's guaranteed to be large enough to hold a pointer without truncating it. */
  using pointer_sized_uint = _W64 unsigned int;
#else
  /** A signed integer type that's guaranteed to be large enough to hold a pointer without truncating it. */
  using pointer_sized_int  = int;
  /** An unsigned integer type that's guaranteed to be large enough to hold a pointer without truncating it. */
  using pointer_sized_uint = unsigned int;
#endif

#if JUCE_WINDOWS && ! JUCE_MINGW
  using ssize_t = pointer_sized_int;
#endif

//==============================================================================
// Some indispensable min/max functions

/** Returns the larger of two values. */
template <typename Type>
JUCE_CONSTEXPR Type jmax (Type a, Type b)                                   { return a < b ? b : a; }

/** Returns the larger of three values. */
template <typename Type>
JUCE_CONSTEXPR Type jmax (Type a, Type b, Type c)                           { return a < b ? (b < c ? c : b) : (a < c ? c : a); }

/** Returns the larger of four values. */
template <typename Type>
JUCE_CONSTEXPR Type jmax (Type a, Type b, Type c, Type d)                   { return jmax (a, jmax (b, c, d)); }

/** Returns the smaller of two values. */
template <typename Type>
JUCE_CONSTEXPR Type jmin (Type a, Type b)                                   { return b < a ? b : a; }

/** Returns the smaller of three values. */
template <typename Type>
JUCE_CONSTEXPR Type jmin (Type a, Type b, Type c)                           { return b < a ? (c < b ? c : b) : (c < a ? c : a); }

/** Returns the smaller of four values. */
template <typename Type>
JUCE_CONSTEXPR Type jmin (Type a, Type b, Type c, Type d)                   { return jmin (a, jmin (b, c, d)); }

/** Remaps a normalised value (between 0 and 1) to a target range.
    This effectively returns (targetRangeMin + value0To1 * (targetRangeMax - targetRangeMin)).
*/
template <typename Type>
JUCE_CONSTEXPR Type jmap (Type value0To1, Type targetRangeMin, Type targetRangeMax)
{
    return targetRangeMin + value0To1 * (targetRangeMax - targetRangeMin);
}

/** Remaps a value from a source range to a target range. */
template <typename Type>
Type jmap (Type sourceValue, Type sourceRangeMin, Type sourceRangeMax, Type targetRangeMin, Type targetRangeMax)
{
    jassert (sourceRangeMax != sourceRangeMin); // mapping from a range of zero will produce NaN!
    return targetRangeMin + ((targetRangeMax - targetRangeMin) * (sourceValue - sourceRangeMin)) / (sourceRangeMax - sourceRangeMin);
}

/** Scans an array of values, returning the minimum value that it contains. */
template <typename Type>
Type findMinimum (const Type* data, int numValues)
{
    if (numValues <= 0)
        return Type (0);

    auto result = *data++;

    while (--numValues > 0) // (> 0 rather than >= 0 because we've already taken the first sample)
    {
        auto v = *data++;

        if (v < result)
            result = v;
    }

    return result;
}

/** Scans an array of values, returning the maximum value that it contains. */
template <typename Type>
Type findMaximum (const Type* values, int numValues)
{
    if (numValues <= 0)
        return Type (0);

    auto result = *values++;

    while (--numValues > 0) // (> 0 rather than >= 0 because we've already taken the first sample)
    {
        auto v = *values++;

        if (result < v)
            result = v;
    }

    return result;
}

/** Scans an array of values, returning the minimum and maximum values that it contains. */
template <typename Type>
void findMinAndMax (const Type* values, int numValues, Type& lowest, Type& highest)
{
    if (numValues <= 0)
    {
        lowest = Type (0);
        highest = Type (0);
    }
    else
    {
        auto mn = *values++;
        auto mx = mn;

        while (--numValues > 0) // (> 0 rather than >= 0 because we've already taken the first sample)
        {
            auto v = *values++;

            if (mx < v)  mx = v;
            if (v < mn)  mn = v;
        }

        lowest = mn;
        highest = mx;
    }
}

//==============================================================================
/** Constrains a value to keep it within a given range.

    This will check that the specified value lies between the lower and upper bounds
    specified, and if not, will return the nearest value that would be in-range. Effectively,
    it's like calling jmax (lowerLimit, jmin (upperLimit, value)).

    Note that it expects that lowerLimit <= upperLimit. If this isn't true,
    the results will be unpredictable.

    @param lowerLimit           the minimum value to return
    @param upperLimit           the maximum value to return
    @param valueToConstrain     the value to try to return
    @returns    the closest value to valueToConstrain which lies between lowerLimit
                and upperLimit (inclusive)
    @see jmin, jmax, jmap
*/
template <typename Type>
Type jlimit (Type lowerLimit,
             Type upperLimit,
             Type valueToConstrain) noexcept
{
    jassert (lowerLimit <= upperLimit); // if these are in the wrong order, results are unpredictable..

    return valueToConstrain < lowerLimit ? lowerLimit
                                         : (upperLimit < valueToConstrain ? upperLimit
                                                                          : valueToConstrain);
}

/** Returns true if a value is at least zero, and also below a specified upper limit.
    This is basically a quicker way to write:
    @code valueToTest >= 0 && valueToTest < upperLimit
    @endcode
*/
template <typename Type1, typename Type2>
bool isPositiveAndBelow (Type1 valueToTest, Type2 upperLimit) noexcept
{
    jassert (Type1() <= static_cast<Type1> (upperLimit)); // makes no sense to call this if the upper limit is itself below zero..
    return Type1() <= valueToTest && valueToTest < static_cast<Type1> (upperLimit);
}

template <typename Type>
bool isPositiveAndBelow (int valueToTest, Type upperLimit) noexcept
{
    jassert (upperLimit >= 0); // makes no sense to call this if the upper limit is itself below zero..
    return static_cast<unsigned int> (valueToTest) < static_cast<unsigned int> (upperLimit);
}

/** Returns true if a value is at least zero, and also less than or equal to a specified upper limit.
    This is basically a quicker way to write:
    @code valueToTest >= 0 && valueToTest <= upperLimit
    @endcode
*/
template <typename Type1, typename Type2>
bool isPositiveAndNotGreaterThan (Type1 valueToTest, Type2 upperLimit) noexcept
{
    jassert (Type1() <= static_cast<Type1> (upperLimit)); // makes no sense to call this if the upper limit is itself below zero..
    return Type1() <= valueToTest && valueToTest <= static_cast<Type1> (upperLimit);
}

template <typename Type>
bool isPositiveAndNotGreaterThan (int valueToTest, Type upperLimit) noexcept
{
    jassert (upperLimit >= 0); // makes no sense to call this if the upper limit is itself below zero..
    return static_cast<unsigned int> (valueToTest) <= static_cast<unsigned int> (upperLimit);
}

/** Computes the absolute difference between two values and returns true if it is less than or equal
    to a given tolerance, otherwise it returns false.
*/
template <typename Type>
bool isWithin (Type a, Type b, Type tolerance) noexcept
{
    return std::abs (a - b) <= tolerance;
}

/** Returns true if the two numbers are approximately equal. This is useful for floating-point
    and double comparisons.
*/
template <typename Type>
bool approximatelyEqual (Type a, Type b) noexcept
{
    return std::abs (a - b) <= (std::numeric_limits<Type>::epsilon() * std::max (a, b))
            || std::abs (a - b) < std::numeric_limits<Type>::min();
}

//==============================================================================
/** Handy function for avoiding unused variables warning. */
template <typename... Types>
void ignoreUnused (Types&&...) noexcept {}

/** Handy function for getting the number of elements in a simple const C array.
    E.g.
    @code
    static int myArray[] = { 1, 2, 3 };

    int numElements = numElementsInArray (myArray) // returns 3
    @endcode
*/
template <typename Type, size_t N>
JUCE_CONSTEXPR int numElementsInArray (Type (&)[N]) noexcept     { return N; }

//==============================================================================
// Some useful maths functions that aren't always present with all compilers and build settings.

/** Using juce_hypot is easier than dealing with the different types of hypot function
    that are provided by the various platforms and compilers. */
template <typename Type>
Type juce_hypot (Type a, Type b) noexcept
{
   #if JUCE_MSVC
    return static_cast<Type> (_hypot (a, b));
   #else
    return static_cast<Type> (hypot (a, b));
   #endif
}

#ifndef DOXYGEN
template <>
inline float juce_hypot (float a, float b) noexcept
{
   #if JUCE_MSVC
    return _hypotf (a, b);
   #else
    return hypotf (a, b);
   #endif
}
#endif

//==============================================================================
#if JUCE_HAS_CONSTEXPR

/** Commonly used mathematical constants

    @tags{Core}
*/
template <typename FloatType>
struct MathConstants
{
    /** A predefined value for Pi */
    static constexpr FloatType pi = static_cast<FloatType> (3.141592653589793238L);

    /** A predefined value for 2 * Pi */
    static constexpr FloatType twoPi = static_cast<FloatType> (2 * 3.141592653589793238L);

    /** A predefined value for Pi / 2 */
    static constexpr FloatType halfPi = static_cast<FloatType> (3.141592653589793238L / 2);

    /** A predefined value for Euler's number */
    static constexpr FloatType euler = static_cast<FloatType> (2.71828182845904523536L);

    /** A predefined value for sqrt(2) */
    static constexpr FloatType sqrt2 = static_cast<FloatType> (1.4142135623730950488L);
};

#else

/** Commonly used mathematical constants

    @tags{Core}
*/
template <typename FloatType>
struct MathConstants
{
    /** A predefined value for Pi */
    static const FloatType pi;

    /** A predefined value for 2 * Pi */
    static const FloatType twoPi;

    /** A predefined value for Pi / 2 */
    static const FloatType halfPi;

    /** A predefined value for Euler's number */
    static const FloatType euler;

    /** A predefined value for sqrt(2) */
    static const FloatType sqrt2;
};

template <typename FloatType>
const FloatType MathConstants<FloatType>::pi = static_cast<FloatType> (3.141592653589793238L);

template <typename FloatType>
const FloatType MathConstants<FloatType>::twoPi = static_cast<FloatType> (2 * 3.141592653589793238L);

template <typename FloatType>
const FloatType MathConstants<FloatType>::halfPi = static_cast<FloatType> (3.141592653589793238L / 2);

template <typename FloatType>
const FloatType MathConstants<FloatType>::euler = static_cast<FloatType> (2.71828182845904523536L);

template <typename FloatType>
const FloatType MathConstants<FloatType>::sqrt2 = static_cast<FloatType> (1.4142135623730950488L);

#endif

#ifndef DOXYGEN
/** A double-precision constant for pi.
    @deprecated This is deprecated in favour of MathConstants<double>::pi.
    The reason is that "double_Pi" was a confusing name, and many people misused it,
    wrongly thinking it meant 2 * pi !
*/
const JUCE_CONSTEXPR double  double_Pi  = MathConstants<double>::pi;

/** A single-precision constant for pi.
    @deprecated This is deprecated in favour of MathConstants<float>::pi.
    The reason is that "double_Pi" was a confusing name, and many people misused it,
    wrongly thinking it meant 2 * pi !
*/
const JUCE_CONSTEXPR float   float_Pi   = MathConstants<float>::pi;
#endif

/** Converts an angle in degrees to radians. */
template <typename FloatType>
JUCE_CONSTEXPR FloatType degreesToRadians (FloatType degrees) noexcept     { return degrees * (MathConstants<FloatType>::pi / FloatType (180)); }

/** Converts an angle in radians to degrees. */
template <typename FloatType>
JUCE_CONSTEXPR FloatType radiansToDegrees (FloatType radians) noexcept     { return radians * (FloatType (180) / MathConstants<FloatType>::pi); }


//==============================================================================
/** The isfinite() method seems to vary between platforms, so this is a
    platform-independent function for it.
*/
template <typename NumericType>
bool juce_isfinite (NumericType) noexcept
{
    return true; // Integer types are always finite
}

template <>
inline bool juce_isfinite (float value) noexcept
{
   #if JUCE_WINDOWS && ! JUCE_MINGW
    return _finite (value) != 0;
   #else
    return std::isfinite (value);
   #endif
}

template <>
inline bool juce_isfinite (double value) noexcept
{
   #if JUCE_WINDOWS && ! JUCE_MINGW
    return _finite (value) != 0;
   #else
    return std::isfinite (value);
   #endif
}

//==============================================================================
#if JUCE_MSVC
 #pragma optimize ("t", off)
 #ifndef __INTEL_COMPILER
  #pragma float_control (precise, on, push)
 #endif
#endif

/** Fast floating-point-to-integer conversion.

    This is faster than using the normal c++ cast to convert a float to an int, and
    it will round the value to the nearest integer, rather than rounding it down
    like the normal cast does.

    Note that this routine gets its speed at the expense of some accuracy, and when
    rounding values whose floating point component is exactly 0.5, odd numbers and
    even numbers will be rounded up or down differently.
*/
template <typename FloatType>
int roundToInt (const FloatType value) noexcept
{
  #ifdef __INTEL_COMPILER
   #pragma float_control (precise, on, push)
  #endif

    union { int asInt[2]; double asDouble; } n;
    n.asDouble = ((double) value) + 6755399441055744.0;

   #if JUCE_BIG_ENDIAN
    return n.asInt [1];
   #else
    return n.asInt [0];
   #endif
}

inline int roundToInt (int value) noexcept
{
    return value;
}

#if JUCE_MSVC
 #ifndef __INTEL_COMPILER
  #pragma float_control (pop)
 #endif
 #pragma optimize ("", on)  // resets optimisations to the project defaults
#endif

/** Fast floating-point-to-integer conversion.

    This is a slightly slower and slightly more accurate version of roundToInt(). It works
    fine for values above zero, but negative numbers are rounded the wrong way.
*/
inline int roundToIntAccurate (double value) noexcept
{
   #ifdef __INTEL_COMPILER
    #pragma float_control (pop)
   #endif

    return roundToInt (value + 1.5e-8);
}

//==============================================================================
/** Truncates a positive floating-point number to an unsigned int.

    This is generally faster than static_cast<unsigned int> (std::floor (x))
    but it only works for positive numbers small enough to be represented as an
    unsigned int.
*/
template <typename FloatType>
unsigned int truncatePositiveToUnsignedInt (FloatType value) noexcept
{
    jassert (value >= static_cast<FloatType> (0));
    jassert (static_cast<FloatType> (value) <= std::numeric_limits<unsigned int>::max());

    return static_cast<unsigned int> (value);
}

//==============================================================================
/** Returns true if the specified integer is a power-of-two. */
template <typename IntegerType>
JUCE_CONSTEXPR bool isPowerOfTwo (IntegerType value)
{
   return (value & (value - 1)) == 0;
}

/** Returns the smallest power-of-two which is equal to or greater than the given integer. */
inline int nextPowerOfTwo (int n) noexcept
{
    --n;
    n |= (n >> 1);
    n |= (n >> 2);
    n |= (n >> 4);
    n |= (n >> 8);
    n |= (n >> 16);
    return n + 1;
}

/** Returns the index of the highest set bit in a (non-zero) number.
    So for n=3 this would return 1, for n=7 it returns 2, etc.
    An input value of 0 is illegal!
*/
int findHighestSetBit (uint32 n) noexcept;

/** Returns the number of bits in a 32-bit integer. */
inline int countNumberOfBits (uint32 n) noexcept
{
    n -= ((n >> 1) & 0x55555555);
    n =  (((n >> 2) & 0x33333333) + (n & 0x33333333));
    n =  (((n >> 4) + n) & 0x0f0f0f0f);
    n += (n >> 8);
    n += (n >> 16);
    return (int) (n & 0x3f);
}

/** Returns the number of bits in a 64-bit integer. */
inline int countNumberOfBits (uint64 n) noexcept
{
    return countNumberOfBits ((uint32) n) + countNumberOfBits ((uint32) (n >> 32));
}

/** Performs a modulo operation, but can cope with the dividend being negative.
    The divisor must be greater than zero.
*/
template <typename IntegerType>
IntegerType negativeAwareModulo (IntegerType dividend, const IntegerType divisor) noexcept
{
    jassert (divisor > 0);
    dividend %= divisor;
    return (dividend < 0) ? (dividend + divisor) : dividend;
}

/** Returns the square of its argument. */
template <typename NumericType>
inline JUCE_CONSTEXPR NumericType square (NumericType n) noexcept
{
    return n * n;
}

//==============================================================================
/** Writes a number of bits into a memory buffer at a given bit index.
    The buffer is treated as a sequence of 8-bit bytes, and the value is encoded in little-endian order,
    so for example if startBit = 10, and numBits = 11 then the lower 6 bits of the value would be written
    into bits 2-8 of targetBuffer[1], and the upper 5 bits of value into bits 0-5 of targetBuffer[2].

    @see readLittleEndianBitsInBuffer
*/
void writeLittleEndianBitsInBuffer (void* targetBuffer, uint32 startBit, uint32 numBits, uint32 value) noexcept;

/** Reads a number of bits from a buffer at a given bit index.
    The buffer is treated as a sequence of 8-bit bytes, and the value is encoded in little-endian order,
    so for example if startBit = 10, and numBits = 11 then the lower 6 bits of the result would be read
    from bits 2-8 of sourceBuffer[1], and the upper 5 bits of the result from bits 0-5 of sourceBuffer[2].

    @see writeLittleEndianBitsInBuffer
*/
uint32 readLittleEndianBitsInBuffer (const void* sourceBuffer, uint32 startBit, uint32 numBits) noexcept;


//==============================================================================
#if JUCE_INTEL || defined (DOXYGEN)
 /** This macro can be applied to a float variable to check whether it contains a denormalised
     value, and to normalise it if necessary.
     On CPUs that aren't vulnerable to denormalisation problems, this will have no effect.
 */
 #define JUCE_UNDENORMALISE(x)   { (x) += 0.1f; (x) -= 0.1f; }
#else
 #define JUCE_UNDENORMALISE(x)
#endif

//==============================================================================
/** This namespace contains a few template classes for helping work out class type variations.
*/
namespace TypeHelpers
{
    /** The ParameterType struct is used to find the best type to use when passing some kind
        of object as a parameter.

        Of course, this is only likely to be useful in certain esoteric template situations.

        E.g. "myFunction (typename TypeHelpers::ParameterType<int>::type, typename TypeHelpers::ParameterType<MyObject>::type)"
        would evaluate to "myfunction (int, const MyObject&)", keeping any primitive types as
        pass-by-value, but passing objects as a const reference, to avoid copying.

        @tags{Core}
    */
    template <typename Type> struct ParameterType                   { using type = const Type&; };

   #if ! DOXYGEN
    template <typename Type> struct ParameterType <Type&>           { using type = Type&; };
    template <typename Type> struct ParameterType <Type*>           { using type = Type*; };
    template <>              struct ParameterType <char>            { using type = char; };
    template <>              struct ParameterType <unsigned char>   { using type = unsigned char; };
    template <>              struct ParameterType <short>           { using type = short; };
    template <>              struct ParameterType <unsigned short>  { using type = unsigned short; };
    template <>              struct ParameterType <int>             { using type = int; };
    template <>              struct ParameterType <unsigned int>    { using type = unsigned int; };
    template <>              struct ParameterType <long>            { using type = long; };
    template <>              struct ParameterType <unsigned long>   { using type = unsigned long; };
    template <>              struct ParameterType <int64>           { using type = int64; };
    template <>              struct ParameterType <uint64>          { using type = uint64; };
    template <>              struct ParameterType <bool>            { using type = bool; };
    template <>              struct ParameterType <float>           { using type = float; };
    template <>              struct ParameterType <double>          { using type = double; };
   #endif

    /** These templates are designed to take a type, and if it's a double, they return a double
        type; for anything else, they return a float type.

        @tags{Core}
    */
    template <typename Type> struct SmallestFloatType               { using type = float; };

   #if ! DOXYGEN
    template <>              struct SmallestFloatType <double>      { using type = double; };
   #endif

    /** These templates are designed to take an integer type, and return an unsigned int
        version with the same size.

        @tags{Core}
    */
    template <int bytes>     struct UnsignedTypeWithSize            {};

   #if ! DOXYGEN
    template <>              struct UnsignedTypeWithSize<1>         { using type = uint8; };
    template <>              struct UnsignedTypeWithSize<2>         { using type = uint16; };
    template <>              struct UnsignedTypeWithSize<4>         { using type = uint32; };
    template <>              struct UnsignedTypeWithSize<8>         { using type = uint64; };
   #endif
}

//==============================================================================
#if ! DOXYGEN
 // These old functions are deprecated: Just use roundToInt instead.
 JUCE_DEPRECATED_ATTRIBUTE inline int roundDoubleToInt (double value) noexcept  { return roundToInt (value); }
 JUCE_DEPRECATED_ATTRIBUTE inline int roundFloatToInt  (float  value) noexcept  { return roundToInt (value); }

 // This old function isn't needed - just use std::abs() instead
 JUCE_DEPRECATED_ATTRIBUTE inline int64 abs64 (int64 n) noexcept                { return std::abs (n); }
#endif

} // namespace juce
