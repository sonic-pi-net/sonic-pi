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

namespace juce
{

#ifndef JUCE_SNAP_TO_ZERO
 #if JUCE_INTEL
  #define JUCE_SNAP_TO_ZERO(n)    if (! (n < -1.0e-8f || n > 1.0e-8f)) n = 0;
 #else
  #define JUCE_SNAP_TO_ZERO(n)    ignoreUnused (n)
 #endif
#endif
class ScopedNoDenormals;

//==============================================================================
/**
    A collection of simple vector operations on arrays of floats, accelerated with
    SIMD instructions where possible.

    @tags{Audio}
*/
class JUCE_API  FloatVectorOperations
{
public:
    //==============================================================================
    /** Clears a vector of floats. */
    static void JUCE_CALLTYPE clear (float* dest, int numValues) noexcept;

    /** Clears a vector of doubles. */
    static void JUCE_CALLTYPE clear (double* dest, int numValues) noexcept;

    /** Copies a repeated value into a vector of floats. */
    static void JUCE_CALLTYPE fill (float* dest, float valueToFill, int numValues) noexcept;

    /** Copies a repeated value into a vector of doubles. */
    static void JUCE_CALLTYPE fill (double* dest, double valueToFill, int numValues) noexcept;

    /** Copies a vector of floats. */
    static void JUCE_CALLTYPE copy (float* dest, const float* src, int numValues) noexcept;

    /** Copies a vector of doubles. */
    static void JUCE_CALLTYPE copy (double* dest, const double* src, int numValues) noexcept;

    /** Copies a vector of floats, multiplying each value by a given multiplier */
    static void JUCE_CALLTYPE copyWithMultiply (float* dest, const float* src, float multiplier, int numValues) noexcept;

    /** Copies a vector of doubles, multiplying each value by a given multiplier */
    static void JUCE_CALLTYPE copyWithMultiply (double* dest, const double* src, double multiplier, int numValues) noexcept;

    /** Adds a fixed value to the destination values. */
    static void JUCE_CALLTYPE add (float* dest, float amountToAdd, int numValues) noexcept;

    /** Adds a fixed value to the destination values. */
    static void JUCE_CALLTYPE add (double* dest, double amountToAdd, int numValues) noexcept;

    /** Adds a fixed value to each source value and stores it in the destination array. */
    static void JUCE_CALLTYPE add (float* dest, const float* src, float amount, int numValues) noexcept;

    /** Adds a fixed value to each source value and stores it in the destination array. */
    static void JUCE_CALLTYPE add (double* dest, const double* src, double amount, int numValues) noexcept;

    /** Adds the source values to the destination values. */
    static void JUCE_CALLTYPE add (float* dest, const float* src, int numValues) noexcept;

    /** Adds the source values to the destination values. */
    static void JUCE_CALLTYPE add (double* dest, const double* src, int numValues) noexcept;

    /** Adds each source1 value to the corresponding source2 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE add (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Adds each source1 value to the corresponding source2 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE add (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Subtracts the source values from the destination values. */
    static void JUCE_CALLTYPE subtract (float* dest, const float* src, int numValues) noexcept;

    /** Subtracts the source values from the destination values. */
    static void JUCE_CALLTYPE subtract (double* dest, const double* src, int numValues) noexcept;

    /** Subtracts each source2 value from the corresponding source1 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE subtract (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Subtracts each source2 value from the corresponding source1 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE subtract (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Multiplies each source value by the given multiplier, then adds it to the destination value. */
    static void JUCE_CALLTYPE addWithMultiply (float* dest, const float* src, float multiplier, int numValues) noexcept;

    /** Multiplies each source value by the given multiplier, then adds it to the destination value. */
    static void JUCE_CALLTYPE addWithMultiply (double* dest, const double* src, double multiplier, int numValues) noexcept;

    /** Multiplies each source1 value by the corresponding source2 value, then adds it to the destination value. */
    static void JUCE_CALLTYPE addWithMultiply (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Multiplies each source1 value by the corresponding source2 value, then adds it to the destination value. */
    static void JUCE_CALLTYPE addWithMultiply (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Multiplies each source value by the given multiplier, then subtracts it to the destination value. */
    static void JUCE_CALLTYPE subtractWithMultiply (float* dest, const float* src, float multiplier, int numValues) noexcept;

    /** Multiplies each source value by the given multiplier, then subtracts it to the destination value. */
    static void JUCE_CALLTYPE subtractWithMultiply (double* dest, const double* src, double multiplier, int numValues) noexcept;

    /** Multiplies each source1 value by the corresponding source2 value, then subtracts it to the destination value. */
    static void JUCE_CALLTYPE subtractWithMultiply (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Multiplies each source1 value by the corresponding source2 value, then subtracts it to the destination value. */
    static void JUCE_CALLTYPE subtractWithMultiply (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Multiplies the destination values by the source values. */
    static void JUCE_CALLTYPE multiply (float* dest, const float* src, int numValues) noexcept;

    /** Multiplies the destination values by the source values. */
    static void JUCE_CALLTYPE multiply (double* dest, const double* src, int numValues) noexcept;

    /** Multiplies each source1 value by the correspinding source2 value, then stores it in the destination array. */
    static void JUCE_CALLTYPE multiply (float* dest, const float* src1, const float* src2, int numValues) noexcept;

    /** Multiplies each source1 value by the correspinding source2 value, then stores it in the destination array. */
    static void JUCE_CALLTYPE multiply (double* dest, const double* src1, const double* src2, int numValues) noexcept;

    /** Multiplies each of the destination values by a fixed multiplier. */
    static void JUCE_CALLTYPE multiply (float* dest, float multiplier, int numValues) noexcept;

    /** Multiplies each of the destination values by a fixed multiplier. */
    static void JUCE_CALLTYPE multiply (double* dest, double multiplier, int numValues) noexcept;

    /** Multiplies each of the source values by a fixed multiplier and stores the result in the destination array. */
    static void JUCE_CALLTYPE multiply (float* dest, const float* src, float multiplier, int num) noexcept;

    /** Multiplies each of the source values by a fixed multiplier and stores the result in the destination array. */
    static void JUCE_CALLTYPE multiply (double* dest, const double* src, double multiplier, int num) noexcept;

    /** Copies a source vector to a destination, negating each value. */
    static void JUCE_CALLTYPE negate (float* dest, const float* src, int numValues) noexcept;

    /** Copies a source vector to a destination, negating each value. */
    static void JUCE_CALLTYPE negate (double* dest, const double* src, int numValues) noexcept;

    /** Copies a source vector to a destination, taking the absolute of each value. */
    static void JUCE_CALLTYPE abs (float* dest, const float* src, int numValues) noexcept;

    /** Copies a source vector to a destination, taking the absolute of each value. */
    static void JUCE_CALLTYPE abs (double* dest, const double* src, int numValues) noexcept;

    /** Converts a stream of integers to floats, multiplying each one by the given multiplier. */
    static void JUCE_CALLTYPE convertFixedToFloat (float* dest, const int* src, float multiplier, int numValues) noexcept;

    /** Each element of dest will be the minimum of the corresponding element of the source array and the given comp value. */
    static void JUCE_CALLTYPE min (float* dest, const float* src, float comp, int num) noexcept;

    /** Each element of dest will be the minimum of the corresponding element of the source array and the given comp value. */
    static void JUCE_CALLTYPE min (double* dest, const double* src, double comp, int num) noexcept;

    /** Each element of dest will be the minimum of the corresponding source1 and source2 values. */
    static void JUCE_CALLTYPE min (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Each element of dest will be the minimum of the corresponding source1 and source2 values. */
    static void JUCE_CALLTYPE min (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Each element of dest will be the maximum of the corresponding element of the source array and the given comp value. */
    static void JUCE_CALLTYPE max (float* dest, const float* src, float comp, int num) noexcept;

    /** Each element of dest will be the maximum of the corresponding element of the source array and the given comp value. */
    static void JUCE_CALLTYPE max (double* dest, const double* src, double comp, int num) noexcept;

    /** Each element of dest will be the maximum of the corresponding source1 and source2 values. */
    static void JUCE_CALLTYPE max (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Each element of dest will be the maximum of the corresponding source1 and source2 values. */
    static void JUCE_CALLTYPE max (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Each element of dest is calculated by hard clipping the corresponding src element so that it is in the range specified by the arguments low and high. */
    static void JUCE_CALLTYPE clip (float* dest, const float* src, float low, float high, int num) noexcept;

    /** Each element of dest is calculated by hard clipping the corresponding src element so that it is in the range specified by the arguments low and high. */
    static void JUCE_CALLTYPE clip (double* dest, const double* src, double low, double high, int num) noexcept;

    /** Finds the minimum and maximum values in the given array. */
    static Range<float> JUCE_CALLTYPE findMinAndMax (const float* src, int numValues) noexcept;

    /** Finds the minimum and maximum values in the given array. */
    static Range<double> JUCE_CALLTYPE findMinAndMax (const double* src, int numValues) noexcept;

    /** Finds the minimum value in the given array. */
    static float JUCE_CALLTYPE findMinimum (const float* src, int numValues) noexcept;

    /** Finds the minimum value in the given array. */
    static double JUCE_CALLTYPE findMinimum (const double* src, int numValues) noexcept;

    /** Finds the maximum value in the given array. */
    static float JUCE_CALLTYPE findMaximum (const float* src, int numValues) noexcept;

    /** Finds the maximum value in the given array. */
    static double JUCE_CALLTYPE findMaximum (const double* src, int numValues) noexcept;

    /** This method enables or disables the SSE/NEON flush-to-zero mode. */
    static void JUCE_CALLTYPE enableFlushToZeroMode (bool shouldEnable) noexcept;

    /** On Intel CPUs, this method enables the SSE flush-to-zero and denormalised-are-zero modes.
        This effectively sets the DAZ and FZ bits of the MXCSR register. On arm CPUs this will
        enable flush to zero mode.
        It's a convenient thing to call before audio processing code where you really want to
        avoid denormalisation performance hits.
    */
    static void JUCE_CALLTYPE disableDenormalisedNumberSupport (bool shouldDisable = true) noexcept;

    /** This method returns true if denormals are currently disabled. */
    static bool JUCE_CALLTYPE areDenormalsDisabled() noexcept;

private:
    friend ScopedNoDenormals;

    static intptr_t JUCE_CALLTYPE getFpStatusRegister() noexcept;
    static void JUCE_CALLTYPE setFpStatusRegister (intptr_t) noexcept;
};

//==============================================================================
/**
     Helper class providing an RAII-based mechanism for temporarily disabling
     denormals on your CPU.

    @tags{Audio}
*/
class ScopedNoDenormals
{
public:
    ScopedNoDenormals() noexcept;
    ~ScopedNoDenormals() noexcept;

private:
  #if JUCE_USE_SSE_INTRINSICS || (JUCE_USE_ARM_NEON || defined (__arm64__) || defined (__aarch64__))
    intptr_t fpsr;
  #endif
};

} // namespace juce
