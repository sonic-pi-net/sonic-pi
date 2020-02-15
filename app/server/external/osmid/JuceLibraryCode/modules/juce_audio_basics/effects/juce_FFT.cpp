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

// (For the moment, we'll implement a few local operators for this complex class - one
// day we'll probably either have a juce complex class, or use the C++11 one)
static FFT::Complex operator+ (FFT::Complex a, FFT::Complex b) noexcept     { FFT::Complex c = { a.r + b.r, a.i + b.i }; return c; }
static FFT::Complex operator- (FFT::Complex a, FFT::Complex b) noexcept     { FFT::Complex c = { a.r - b.r, a.i - b.i }; return c; }
static FFT::Complex operator* (FFT::Complex a, FFT::Complex b) noexcept     { FFT::Complex c = { a.r * b.r - a.i * b.i, a.r * b.i + a.i * b.r }; return c; }
static FFT::Complex& operator+= (FFT::Complex& a, FFT::Complex b) noexcept  { a.r += b.r; a.i += b.i; return a; }

//==============================================================================
struct FFT::FFTConfig
{
    FFTConfig (int sizeOfFFT, bool isInverse)
        : fftSize (sizeOfFFT), inverse (isInverse), twiddleTable ((size_t) sizeOfFFT)
    {
        for (int i = 0; i < fftSize; ++i)
        {
            const double phase = (isInverse ? 2.0 : -2.0) * double_Pi * i / fftSize;
            twiddleTable[i].r = (float) cos (phase);
            twiddleTable[i].i = (float) sin (phase);
        }

        const int root = (int) std::sqrt ((double) fftSize);
        int divisor = 4, n = fftSize;

        for (int i = 0; i < numElementsInArray (factors); ++i)
        {
            while ((n % divisor) != 0)
            {
                if (divisor == 2)       divisor = 3;
                else if (divisor == 4)  divisor = 2;
                else                    divisor += 2;

                if (divisor > root)
                    divisor = n;
            }

            n /= divisor;

            jassert (divisor == 1 || divisor == 2 || divisor == 4);
            factors[i].radix = divisor;
            factors[i].length = n;
        }
    }

    void perform (const Complex* input, Complex* output) const noexcept
    {
        perform (input, output, 1, 1, factors);
    }

    const int fftSize;
    const bool inverse;

    struct Factor { int radix, length; };
    Factor factors[32];
    HeapBlock<Complex> twiddleTable;

    void perform (const Complex* input, Complex* output, const int stride, const int strideIn, const Factor* facs) const noexcept
    {
        const Factor factor (*facs++);
        Complex* const originalOutput = output;
        const Complex* const outputEnd = output + factor.radix * factor.length;

        if (stride == 1 && factor.radix <= 5)
        {
            for (int i = 0; i < factor.radix; ++i)
                perform (input + stride * strideIn * i, output + i * factor.length, stride * factor.radix, strideIn, facs);

            butterfly (factor, output, stride);
            return;
        }

        if (factor.length == 1)
        {
            do
            {
                *output++ = *input;
                input += stride * strideIn;
            }
            while (output < outputEnd);
        }
        else
        {
            do
            {
                perform (input, output, stride * factor.radix, strideIn, facs);
                input += stride * strideIn;
                output += factor.length;
            }
            while (output < outputEnd);
        }

        butterfly (factor, originalOutput, stride);
    }

    void butterfly (const Factor factor, Complex* data, const int stride) const noexcept
    {
        switch (factor.radix)
        {
            case 1:   break;
            case 2:   butterfly2 (data, stride, factor.length); return;
            case 4:   butterfly4 (data, stride, factor.length); return;
            default:  jassertfalse; break;
        }

        Complex* scratch = static_cast<Complex*> (alloca (sizeof (Complex) * (size_t) factor.radix));

        for (int i = 0; i < factor.length; ++i)
        {
            for (int k = i, q1 = 0; q1 < factor.radix; ++q1)
            {
                scratch[q1] = data[k];
                k += factor.length;
            }

            for (int k = i, q1 = 0; q1 < factor.radix; ++q1)
            {
                int twiddleIndex = 0;
                data[k] = scratch[0];

                for (int q = 1; q < factor.radix; ++q)
                {
                    twiddleIndex += stride * k;

                    if (twiddleIndex >= fftSize)
                        twiddleIndex -= fftSize;

                    data[k] += scratch[q] * twiddleTable[twiddleIndex];
                }

                k += factor.length;
            }
        }
    }

    void butterfly2 (Complex* data, const int stride, const int length) const noexcept
    {
        Complex* dataEnd = data + length;
        const Complex* tw = twiddleTable;

        for (int i = length; --i >= 0;)
        {
            const Complex s (*dataEnd * *tw);
            tw += stride;
            *dataEnd++ = *data - s;
            *data++ += s;
        }
    }

    void butterfly4 (Complex* data, const int stride, const int length) const noexcept
    {
        const int lengthX2 = length * 2;
        const int lengthX3 = length * 3;

        const Complex* twiddle1 = twiddleTable;
        const Complex* twiddle2 = twiddle1;
        const Complex* twiddle3 = twiddle1;

        for (int i = length; --i >= 0;)
        {
            const Complex s0 = data[length]   * *twiddle1;
            const Complex s1 = data[lengthX2] * *twiddle2;
            const Complex s2 = data[lengthX3] * *twiddle3;
            const Complex s3 = s0 + s2;
            const Complex s4 = s0 - s2;
            const Complex s5 = *data - s1;
            *data += s1;
            data[lengthX2] = *data - s3;
            twiddle1 += stride;
            twiddle2 += stride * 2;
            twiddle3 += stride * 3;
            *data += s3;

            if (inverse)
            {
                data[length].r   = s5.r - s4.i;
                data[length].i   = s5.i + s4.r;
                data[lengthX3].r = s5.r + s4.i;
                data[lengthX3].i = s5.i - s4.r;
            }
            else
            {
                data[length].r   = s5.r + s4.i;
                data[length].i   = s5.i - s4.r;
                data[lengthX3].r = s5.r - s4.i;
                data[lengthX3].i = s5.i + s4.r;
            }

            ++data;
        }
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FFTConfig)
};


//==============================================================================
FFT::FFT (int order, bool inverse)  : config (new FFTConfig (1 << order, inverse)), size (1 << order) {}
FFT::~FFT() {}

void FFT::perform (const Complex* const input, Complex* const output) const noexcept
{
    config->perform (input, output);
}

const size_t maxFFTScratchSpaceToAlloca = 256 * 1024;

void FFT::performRealOnlyForwardTransform (float* d) const noexcept
{
    const size_t scratchSize = 16 + sizeof (FFT::Complex) * (size_t) size;

    if (scratchSize < maxFFTScratchSpaceToAlloca)
    {
        performRealOnlyForwardTransform (static_cast<Complex*> (alloca (scratchSize)), d);
    }
    else
    {
        HeapBlock<char> heapSpace (scratchSize);
        performRealOnlyForwardTransform (reinterpret_cast<Complex*> (heapSpace.getData()), d);
    }
}

void FFT::performRealOnlyInverseTransform (float* d) const noexcept
{
    const size_t scratchSize = 16 + sizeof (FFT::Complex) * (size_t) size;

    if (scratchSize < maxFFTScratchSpaceToAlloca)
    {
        performRealOnlyInverseTransform (static_cast<Complex*> (alloca (scratchSize)), d);
    }
    else
    {
        HeapBlock<char> heapSpace (scratchSize);
        performRealOnlyInverseTransform (reinterpret_cast<Complex*> (heapSpace.getData()), d);
    }
}

void FFT::performRealOnlyForwardTransform (Complex* scratch, float* d) const noexcept
{
    // This can only be called on an FFT object that was created to do forward transforms.
    jassert (! config->inverse);

    for (int i = 0; i < size; ++i)
    {
        scratch[i].r = d[i];
        scratch[i].i = 0;
    }

    perform (scratch, reinterpret_cast<Complex*> (d));
}

void FFT::performRealOnlyInverseTransform (Complex* scratch, float* d) const noexcept
{
    // This can only be called on an FFT object that was created to do inverse transforms.
    jassert (config->inverse);

    perform (reinterpret_cast<const Complex*> (d), scratch);

    const float scaleFactor = 1.0f / size;

    for (int i = 0; i < size; ++i)
    {
        d[i]        = scratch[i].r * scaleFactor;
        d[i + size] = scratch[i].i * scaleFactor;
    }
}

void FFT::performFrequencyOnlyForwardTransform (float* d) const noexcept
{
    performRealOnlyForwardTransform (d);
    const int twiceSize = size * 2;

    for (int i = 0; i < twiceSize; i += 2)
    {
        d[i / 2] = juce_hypot (d[i], d[i + 1]);

        if (i >= size)
        {
            d[i] = 0;
            d[i + 1] = 0;
        }
    }
}
