/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 6 End-User License
   Agreement and JUCE Privacy Policy (both effective as of the 16th June 2020).

   End User License Agreement: www.juce.com/juce-6-licence
   Privacy Policy: www.juce.com/juce-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

/**
    An interpolator base class for resampling streams of floats.

    Note that the resamplers are stateful, so when there's a break in the continuity
    of the input stream you're feeding it, you should call reset() before feeding
    it any new data. And like with any other stateful filter, if you're resampling
    multiple channels, make sure each one uses its own interpolator object.

    @see LagrangeInterpolator, CatmullRomInterpolator, WindowedSincInterpolator,
         LinearInterpolator, ZeroOrderHoldInterpolator

    @tags{Audio}
*/
template <class InterpolatorTraits, int memorySize>
class JUCE_API  GenericInterpolator
{
public:
    GenericInterpolator() noexcept                        { reset(); }

    GenericInterpolator (GenericInterpolator&&) noexcept = default;
    GenericInterpolator& operator= (GenericInterpolator&&) noexcept = default;

    /** Returns the latency of the interpolation algorithm in isolation.

        In the context of resampling the total latency of a process using
        the interpolator is the base latency divided by the speed ratio.
    */
    static constexpr float getBaseLatency() noexcept
    {
        return InterpolatorTraits::algorithmicLatency;
    }

    /** Resets the state of the interpolator.

        Call this when there's a break in the continuity of the input data stream.
    */
    void reset() noexcept
    {
        indexBuffer = 0;
        subSamplePos = 1.0;
        std::fill (std::begin (lastInputSamples), std::end (lastInputSamples), 0.0f);
    }

    /** Resamples a stream of samples.

        @param speedRatio                   the number of input samples to use for each output sample
        @param inputSamples                 the source data to read from. This must contain at
                                            least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples                the buffer to write the results into
        @param numOutputSamplesToProduce    the number of output samples that should be created

        @returns the actual number of input samples that were used
    */
    int process (double speedRatio,
                 const float* inputSamples,
                 float* outputSamples,
                 int numOutputSamplesToProduce) noexcept
    {
        return interpolate (speedRatio, inputSamples, outputSamples, numOutputSamplesToProduce);
    }

    /** Resamples a stream of samples.

        @param speedRatio                   the number of input samples to use for each output sample
        @param inputSamples                 the source data to read from. This must contain at
                                            least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples                the buffer to write the results into
        @param numOutputSamplesToProduce    the number of output samples that should be created
        @param numInputSamplesAvailable     the number of available input samples. If it needs more samples
                                            than available, it either wraps back for wrapAround samples, or
                                            it feeds zeroes
        @param wrapAround                   if the stream exceeds available samples, it wraps back for
                                            wrapAround samples. If wrapAround is set to 0, it will feed zeroes.

        @returns the actual number of input samples that were used
    */
    int process (double speedRatio,
                 const float* inputSamples,
                 float* outputSamples,
                 int numOutputSamplesToProduce,
                 int numInputSamplesAvailable,
                 int wrapAround) noexcept
    {
        return interpolate (speedRatio, inputSamples, outputSamples,
                            numOutputSamplesToProduce, numInputSamplesAvailable, wrapAround);
    }

    /** Resamples a stream of samples, adding the results to the output data
        with a gain.

        @param speedRatio                   the number of input samples to use for each output sample
        @param inputSamples                 the source data to read from. This must contain at
                                            least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples                the buffer to write the results to - the result values will be added
                                            to any pre-existing data in this buffer after being multiplied by
                                            the gain factor
        @param numOutputSamplesToProduce    the number of output samples that should be created
        @param gain                         a gain factor to multiply the resulting samples by before
                                            adding them to the destination buffer

        @returns the actual number of input samples that were used
    */
    int processAdding (double speedRatio,
                       const float* inputSamples,
                       float* outputSamples,
                       int numOutputSamplesToProduce,
                       float gain) noexcept
    {
        return interpolateAdding (speedRatio, inputSamples, outputSamples, numOutputSamplesToProduce, gain);
    }

    /** Resamples a stream of samples, adding the results to the output data
        with a gain.

        @param speedRatio                   the number of input samples to use for each output sample
        @param inputSamples                 the source data to read from. This must contain at
                                            least (speedRatio * numOutputSamplesToProduce) samples.
        @param outputSamples                the buffer to write the results to - the result values will be added
                                            to any pre-existing data in this buffer after being multiplied by
                                            the gain factor
        @param numOutputSamplesToProduce    the number of output samples that should be created
        @param numInputSamplesAvailable     the number of available input samples. If it needs more samples
                                            than available, it either wraps back for wrapAround samples, or
                                            it feeds zeroes
        @param wrapAround                   if the stream exceeds available samples, it wraps back for
                                            wrapAround samples. If wrapAround is set to 0, it will feed zeroes.
        @param gain                         a gain factor to multiply the resulting samples by before
                                            adding them to the destination buffer

        @returns the actual number of input samples that were used
    */
    int processAdding (double speedRatio,
                       const float* inputSamples,
                       float* outputSamples,
                       int numOutputSamplesToProduce,
                       int numInputSamplesAvailable,
                       int wrapAround,
                       float gain) noexcept
    {
        return interpolateAdding (speedRatio, inputSamples, outputSamples,
                                  numOutputSamplesToProduce, numInputSamplesAvailable, wrapAround, gain);
    }

private:
    //==============================================================================
    forcedinline void pushInterpolationSample (float newValue) noexcept
    {
        lastInputSamples[indexBuffer] = newValue;

        if (++indexBuffer == memorySize)
            indexBuffer = 0;
    }

    forcedinline void pushInterpolationSamples (const float* input,
                                                int numOutputSamplesToProduce) noexcept
    {
        if (numOutputSamplesToProduce >= memorySize)
        {
            const auto* const offsetInput = input + (numOutputSamplesToProduce - memorySize);

            for (int i = 0; i < memorySize; ++i)
                pushInterpolationSample (offsetInput[i]);
        }
        else
        {
            for (int i = 0; i < numOutputSamplesToProduce; ++i)
                pushInterpolationSample (input[i]);
        }
    }

    forcedinline void pushInterpolationSamples (const float* input,
                                                int numOutputSamplesToProduce,
                                                int numInputSamplesAvailable,
                                                int wrapAround) noexcept
    {
        if (numOutputSamplesToProduce >= memorySize)
        {
            if (numInputSamplesAvailable >= memorySize)
            {
                pushInterpolationSamples (input,
                                          numOutputSamplesToProduce);
            }
            else
            {
                pushInterpolationSamples (input + ((numOutputSamplesToProduce - numInputSamplesAvailable) - 1),
                                          numInputSamplesAvailable);

                if (wrapAround > 0)
                {
                    numOutputSamplesToProduce -= wrapAround;

                    pushInterpolationSamples (input + ((numOutputSamplesToProduce - (memorySize - numInputSamplesAvailable)) - 1),
                                              memorySize - numInputSamplesAvailable);
                }
                else
                {
                    for (int i = numInputSamplesAvailable; i < memorySize; ++i)
                        pushInterpolationSample (0.0f);
                }
            }
        }
        else
        {
            if (numOutputSamplesToProduce > numInputSamplesAvailable)
            {
                for (int i = 0; i < numInputSamplesAvailable; ++i)
                    pushInterpolationSample (input[i]);

                const auto extraSamples = numOutputSamplesToProduce - numInputSamplesAvailable;

                if (wrapAround > 0)
                {
                    const auto* const offsetInput = input + (numInputSamplesAvailable - wrapAround);

                    for (int i = 0; i < extraSamples; ++i)
                        pushInterpolationSample (offsetInput[i]);
                }
                else
                {
                    for (int i = 0; i < extraSamples; ++i)
                        pushInterpolationSample (0.0f);
                }
            }
            else
            {
                for (int i = 0; i < numOutputSamplesToProduce; ++i)
                    pushInterpolationSample (input[i]);
            }
        }
    }

    //==============================================================================
    int interpolate (double speedRatio,
                     const float* input,
                     float* output,
                     int numOutputSamplesToProduce) noexcept
    {
        auto pos = subSamplePos;
        int numUsed = 0;

        while (numOutputSamplesToProduce > 0)
        {
            while (pos >= 1.0)
            {
                pushInterpolationSample (input[numUsed++]);
                pos -= 1.0;
            }

            *output++ = InterpolatorTraits::valueAtOffset (lastInputSamples, (float) pos, indexBuffer);
            pos += speedRatio;
            --numOutputSamplesToProduce;
        }

        subSamplePos = pos;
        return numUsed;
    }

    int interpolate (double speedRatio,
                     const float* input, float* output,
                     int numOutputSamplesToProduce,
                     int numInputSamplesAvailable,
                     int wrap) noexcept
    {
        auto originalIn = input;
        auto pos = subSamplePos;
        bool exceeded = false;

        if (speedRatio < 1.0)
        {
            for (int i = numOutputSamplesToProduce; --i >= 0;)
            {
                if (pos >= 1.0)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (0.0f);
                    }
                    else
                    {
                        pushInterpolationSample (*input++);

                        if (--numInputSamplesAvailable <= 0)
                        {
                            if (wrap > 0)
                            {
                                input -= wrap;
                                numInputSamplesAvailable += wrap;
                            }
                            else
                            {
                                exceeded = true;
                            }
                        }
                    }

                    pos -= 1.0;
                }

                *output++ = InterpolatorTraits::valueAtOffset (lastInputSamples, (float) pos, indexBuffer);
                pos += speedRatio;
            }
        }
        else
        {
            for (int i = numOutputSamplesToProduce; --i >= 0;)
            {
                while (pos < speedRatio)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (0);
                    }
                    else
                    {
                        pushInterpolationSample (*input++);

                        if (--numInputSamplesAvailable <= 0)
                        {
                            if (wrap > 0)
                            {
                                input -= wrap;
                                numInputSamplesAvailable += wrap;
                            }
                            else
                            {
                                exceeded = true;
                            }
                        }
                    }

                    pos += 1.0;
                }

                pos -= speedRatio;
                *output++ = InterpolatorTraits::valueAtOffset (lastInputSamples, jmax (0.0f, 1.0f - (float) pos), indexBuffer);
            }
        }

        subSamplePos = pos;

        if (wrap == 0)
            return (int) (input - originalIn);

        return ((int) (input - originalIn) + wrap) % wrap;
    }

    int interpolateAdding (double speedRatio,
                           const float* input,
                           float* output,
                           int numOutputSamplesToProduce,
                           int numInputSamplesAvailable,
                           int wrap,
                           float gain) noexcept
    {
        auto originalIn = input;
        auto pos = subSamplePos;
        bool exceeded = false;

        if (speedRatio < 1.0)
        {
            for (int i = numOutputSamplesToProduce; --i >= 0;)
            {
                if (pos >= 1.0)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (0.0);
                    }
                    else
                    {
                        pushInterpolationSample (*input++);

                        if (--numInputSamplesAvailable <= 0)
                        {
                            if (wrap > 0)
                            {
                                input -= wrap;
                                numInputSamplesAvailable += wrap;
                            }
                            else
                            {
                                numInputSamplesAvailable = true;
                            }
                        }
                    }

                    pos -= 1.0;
                }

                *output++ += gain * InterpolatorTraits::valueAtOffset ((float) pos);
                pos += speedRatio;
            }
        }
        else
        {
            for (int i = numOutputSamplesToProduce; --i >= 0;)
            {
                while (pos < speedRatio)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (0.0);
                    }
                    else
                    {
                        pushInterpolationSample (*input++);

                        if (--numInputSamplesAvailable <= 0)
                        {
                            if (wrap > 0)
                            {
                                input -= wrap;
                                numInputSamplesAvailable += wrap;
                            }
                            else
                            {
                                exceeded = true;
                            }
                        }
                    }

                    pos += 1.0;
                }

                pos -= speedRatio;
                *output++ += gain * InterpolatorTraits::valueAtOffset (lastInputSamples, jmax (0.0f, 1.0f - (float) pos), indexBuffer);
            }
        }

        subSamplePos = pos;

        if (wrap == 0)
            return (int) (input - originalIn);

        return ((int) (input - originalIn) + wrap) % wrap;
    }

    int interpolateAdding (double speedRatio,
                           const float* input,
                           float* output,
                           int numOutputSamplesToProduce,
                           float gain) noexcept
    {
        auto pos = subSamplePos;
        int numUsed = 0;

        while (numOutputSamplesToProduce > 0)
        {
            while (pos >= 1.0)
            {
                pushInterpolationSample (input[numUsed++]);
                pos -= 1.0;
            }

            *output++ += gain * InterpolatorTraits::valueAtOffset (lastInputSamples, (float) pos, indexBuffer);
            pos += speedRatio;
            --numOutputSamplesToProduce;
        }

        subSamplePos = pos;
        return numUsed;
    }

    //==============================================================================
    float lastInputSamples[(size_t) memorySize];
    double subSamplePos = 1.0;
    int indexBuffer = 0;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (GenericInterpolator)
};

} // namespace juce
