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

class IIRFilter;

//==============================================================================
/**
    A set of coefficients for use in an IIRFilter object.

    @see IIRFilter

    @tags{Audio}
*/
class JUCE_API  IIRCoefficients
{
public:
    //==============================================================================
    /** Creates a null set of coefficients (which will produce silence). */
    IIRCoefficients() noexcept;

    /** Directly constructs an object from the raw coefficients.
        Most people will want to use the static methods instead of this, but
        the constructor is public to allow tinkerers to create their own custom
        filters!
    */
    IIRCoefficients (double c1, double c2, double c3,
                     double c4, double c5, double c6) noexcept;

    /** Creates a copy of another filter. */
    IIRCoefficients (const IIRCoefficients&) noexcept;
    /** Creates a copy of another filter. */
    IIRCoefficients& operator= (const IIRCoefficients&) noexcept;
    /** Destructor. */
    ~IIRCoefficients() noexcept;

    //==============================================================================
    /** Returns the coefficients for a low-pass filter. */
    static IIRCoefficients makeLowPass (double sampleRate,
                                        double frequency) noexcept;

    /** Returns the coefficients for a low-pass filter with variable Q. */
    static IIRCoefficients makeLowPass (double sampleRate,
                                        double frequency,
                                        double Q) noexcept;

    //==============================================================================
    /** Returns the coefficients for a high-pass filter. */
    static IIRCoefficients makeHighPass (double sampleRate,
                                         double frequency) noexcept;

    /** Returns the coefficients for a high-pass filter with variable Q. */
    static IIRCoefficients makeHighPass (double sampleRate,
                                         double frequency,
                                         double Q) noexcept;

    //==============================================================================
    /** Returns the coefficients for a band-pass filter. */
    static IIRCoefficients makeBandPass (double sampleRate, double frequency) noexcept;

    /** Returns the coefficients for a band-pass filter with variable Q. */
    static IIRCoefficients makeBandPass (double sampleRate,
                                         double frequency,
                                         double Q) noexcept;

    //==============================================================================
    /** Returns the coefficients for a notch filter. */
    static IIRCoefficients makeNotchFilter (double sampleRate, double frequency) noexcept;

    /** Returns the coefficients for a notch filter with variable Q. */
    static IIRCoefficients makeNotchFilter (double sampleRate,
                                            double frequency,
                                            double Q) noexcept;

    //==============================================================================
    /** Returns the coefficients for an all-pass filter. */
    static IIRCoefficients makeAllPass (double sampleRate, double frequency) noexcept;

    /** Returns the coefficients for an all-pass filter with variable Q. */
    static IIRCoefficients makeAllPass (double sampleRate,
                                        double frequency,
                                        double Q) noexcept;

    //==============================================================================
    /** Returns the coefficients for a low-pass shelf filter with variable Q and gain.

        The gain is a scale factor that the low frequencies are multiplied by, so values
        greater than 1.0 will boost the low frequencies, values less than 1.0 will
        attenuate them.
    */
    static IIRCoefficients makeLowShelf (double sampleRate,
                                         double cutOffFrequency,
                                         double Q,
                                         float gainFactor) noexcept;

    /** Returns the coefficients for a high-pass shelf filter with variable Q and gain.

        The gain is a scale factor that the high frequencies are multiplied by, so values
        greater than 1.0 will boost the high frequencies, values less than 1.0 will
        attenuate them.
    */
    static IIRCoefficients makeHighShelf (double sampleRate,
                                          double cutOffFrequency,
                                          double Q,
                                          float gainFactor) noexcept;

    /** Returns the coefficients for a peak filter centred around a
        given frequency, with a variable Q and gain.

        The gain is a scale factor that the centre frequencies are multiplied by, so
        values greater than 1.0 will boost the centre frequencies, values less than
        1.0 will attenuate them.
    */
    static IIRCoefficients makePeakFilter (double sampleRate,
                                           double centreFrequency,
                                           double Q,
                                           float gainFactor) noexcept;

    //==============================================================================
    /** The raw coefficients.
        You should leave these numbers alone unless you really know what you're doing.
    */
    float coefficients[5];
};

//==============================================================================
/**
    An IIR filter that can perform low, high, or band-pass filtering on an
    audio signal.

    @see IIRCoefficient, IIRFilterAudioSource

    @tags{Audio}
*/
class JUCE_API  IIRFilter
{
public:
    //==============================================================================
    /** Creates a filter.

        Initially the filter is inactive, so will have no effect on samples that
        you process with it. Use the setCoefficients() method to turn it into the
        type of filter needed.
    */
    IIRFilter() noexcept;

    /** Creates a copy of another filter. */
    IIRFilter (const IIRFilter&) noexcept;

    /** Destructor. */
    ~IIRFilter() noexcept;

    //==============================================================================
    /** Clears the filter so that any incoming data passes through unchanged. */
    void makeInactive() noexcept;

    /** Applies a set of coefficients to this filter. */
    void setCoefficients (const IIRCoefficients& newCoefficients) noexcept;

    /** Returns the coefficients that this filter is using. */
    IIRCoefficients getCoefficients() const noexcept    { return coefficients; }

    //==============================================================================
    /** Resets the filter's processing pipeline, ready to start a new stream of data.

        Note that this clears the processing state, but the type of filter and
        its coefficients aren't changed. To put a filter into an inactive state, use
        the makeInactive() method.
    */
    void reset() noexcept;

    /** Performs the filter operation on the given set of samples. */
    void processSamples (float* samples, int numSamples) noexcept;

    /** Processes a single sample, without any locking or checking.

        Use this if you need fast processing of a single value, but be aware that
        this isn't thread-safe in the way that processSamples() is.
    */
    float processSingleSampleRaw (float sample) noexcept;

protected:
    //==============================================================================
    SpinLock processLock;
    IIRCoefficients coefficients;
    float v1 = 0, v2 = 0;
    bool active = false;

    // The exact meaning of an assignment operator would be ambiguous since the filters are
    // stateful. If you want to copy the coefficients, then just use setCoefficients().
    IIRFilter& operator= (const IIRFilter&) = delete;

    JUCE_LEAK_DETECTOR (IIRFilter)
};

} // namespace juce
