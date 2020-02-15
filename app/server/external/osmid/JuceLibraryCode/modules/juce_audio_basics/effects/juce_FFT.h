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

/**
    A very minimal FFT class.

    This is only a simple low-footprint implementation and isn't tuned for speed - it may
    be useful for simple applications where one of the more complex FFT libraries would be
    overkill. (But in the future it may end up becoming optimised of course...)

    The FFT class itself contains lookup tables, so there's some overhead in creating
    one, you should create and cache an FFT object for each size/direction of transform
    that you need, and re-use them to perform the actual operation.
*/
class JUCE_API  FFT
{
public:
    /** Initialises an object for performing either a forward or inverse FFT with the given size.
        The the number of points the FFT will operate on will be 2 ^ order.
    */
    FFT (int order, bool isInverse);

    /** Destructor. */
    ~FFT();

    /** A complex number, for the purposes of the FFT class. */
    struct Complex
    {
        float r;  /**< Real part. */
        float i;  /**< Imaginary part. */
    };

    /** Performs an out-of-place FFT, either forward or inverse depending on the mode
        that was passed to this object's constructor.

        The arrays must contain at least getSize() elements.
    */
    void perform (const Complex* input, Complex* output) const noexcept;

    /** Performs an in-place forward transform on a block of real data.

        The size of the array passed in must be 2 * getSize(), and the first half
        should contain your raw input sample data. On return, the array will contain
        complex frequency + phase data, and can be passed to performRealOnlyInverseTransform()
        in order to convert it back to reals.
    */
    void performRealOnlyForwardTransform (float* inputOutputData) const noexcept;

    /** Performs a reverse operation to data created in performRealOnlyForwardTransform().

        The size of the array passed in must be 2 * getSize(), containing complex
        frequency and phase data. On return, the first half of the array will contain
        the reconstituted samples.
    */
    void performRealOnlyInverseTransform (float* inputOutputData) const noexcept;

    /** Takes an array and simply transforms it to the frequency spectrum.
        This may be handy for things like frequency displays or analysis.
    */
    void performFrequencyOnlyForwardTransform (float* inputOutputData) const noexcept;

    /** Returns the number of data points that this FFT was created to work with. */
    int getSize() const noexcept            { return size; }

private:
    JUCE_PUBLIC_IN_DLL_BUILD (struct FFTConfig)
    ScopedPointer<FFTConfig> config;
    const int size;

    void performRealOnlyForwardTransform (Complex*, float*) const noexcept;
    void performRealOnlyInverseTransform (Complex*, float*) const noexcept;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FFT)
};
