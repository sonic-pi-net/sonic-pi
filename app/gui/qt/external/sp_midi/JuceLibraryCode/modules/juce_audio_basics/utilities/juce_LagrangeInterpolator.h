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

/**
    Interpolator for resampling a stream of floats using 4-point lagrange interpolation.

    Note that the resampler is stateful, so when there's a break in the continuity
    of the input stream you're feeding it, you should call reset() before feeding
    it any new data. And like with any other stateful filter, if you're resampling
    multiple channels, make sure each one uses its own LagrangeInterpolator
    object.

    @see CatmullRomInterpolator

    @tags{Audio}
*/
class JUCE_API  LagrangeInterpolator
{
public:
    LagrangeInterpolator() noexcept;
    ~LagrangeInterpolator() noexcept;

    LagrangeInterpolator (LagrangeInterpolator&&) noexcept = default;
    LagrangeInterpolator& operator= (LagrangeInterpolator&&) noexcept = default;

    /** Resets the state of the interpolator.
        Call this when there's a break in the continuity of the input data stream.
    */
    void reset() noexcept;

    /** Resamples a stream of samples.

        @param speedRatio       the number of input samples to use for each output sample
        @param inputSamples     the source data to read from. This must contain at
                                least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples    the buffer to write the results into
        @param numOutputSamplesToProduce    the number of output samples that should be created

        @returns the actual number of input samples that were used
    */
    int process (double speedRatio,
                 const float* inputSamples,
                 float* outputSamples,
                 int numOutputSamplesToProduce) noexcept;

    /** Resamples a stream of samples.

        @param speedRatio       the number of input samples to use for each output sample
        @param inputSamples     the source data to read from. This must contain at
                                least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples    the buffer to write the results into
        @param numOutputSamplesToProduce    the number of output samples that should be created
        @param available        the number of available input samples. If it needs more samples
                                than available, it either wraps back for wrapAround samples, or
                                it feeds zeroes
        @param wrapAround       if the stream exceeds available samples, it wraps back for
                                wrapAround samples. If wrapAround is set to 0, it will feed zeroes.

        @returns the actual number of input samples that were used
    */
    int process (double speedRatio,
                 const float* inputSamples,
                 float* outputSamples,
                 int numOutputSamplesToProduce,
                 int available,
                 int wrapAround) noexcept;

    /** Resamples a stream of samples, adding the results to the output data
        with a gain.

        @param speedRatio       the number of input samples to use for each output sample
        @param inputSamples     the source data to read from. This must contain at
                                least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples    the buffer to write the results to - the result values will be added
                                to any pre-existing data in this buffer after being multiplied by
                                the gain factor
        @param numOutputSamplesToProduce    the number of output samples that should be created
        @param gain             a gain factor to multiply the resulting samples by before
                                adding them to the destination buffer

        @returns the actual number of input samples that were used
    */
    int processAdding (double speedRatio,
                       const float* inputSamples,
                       float* outputSamples,
                       int numOutputSamplesToProduce,
                       float gain) noexcept;

    /** Resamples a stream of samples, adding the results to the output data
        with a gain.

        @param speedRatio       the number of input samples to use for each output sample
        @param inputSamples     the source data to read from. This must contain at
                                least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples    the buffer to write the results to - the result values will be added
                                to any pre-existing data in this buffer after being multiplied by
                                the gain factor
        @param numOutputSamplesToProduce    the number of output samples that should be created
        @param available        the number of available input samples. If it needs more samples
                                than available, it either wraps back for wrapAround samples, or
                                it feeds zeroes
        @param wrapAround       if the stream exceeds available samples, it wraps back for
                                wrapAround samples. If wrapAround is set to 0, it will feed zeroes.
        @param gain             a gain factor to multiply the resulting samples by before
                                adding them to the destination buffer

        @returns the actual number of input samples that were used
    */
    int processAdding (double speedRatio,
                       const float* inputSamples,
                       float* outputSamples,
                       int numOutputSamplesToProduce,
                       int available,
                       int wrapAround,
                       float gain) noexcept;

private:
    float lastInputSamples[5];
    double subSamplePos;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (LagrangeInterpolator)
};

} // namespace juce
