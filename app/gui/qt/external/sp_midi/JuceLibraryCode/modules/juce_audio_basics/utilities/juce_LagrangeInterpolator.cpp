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

namespace
{
    static forcedinline void pushInterpolationSample (float* lastInputSamples, float newValue) noexcept
    {
        lastInputSamples[4] = lastInputSamples[3];
        lastInputSamples[3] = lastInputSamples[2];
        lastInputSamples[2] = lastInputSamples[1];
        lastInputSamples[1] = lastInputSamples[0];
        lastInputSamples[0] = newValue;
    }

    static forcedinline void pushInterpolationSamples (float* lastInputSamples, const float* input, int numOut) noexcept
    {
        if (numOut >= 5)
        {
            for (int i = 0; i < 5; ++i)
                lastInputSamples[i] = input[--numOut];
        }
        else
        {
            for (int i = 0; i < numOut; ++i)
                pushInterpolationSample (lastInputSamples, input[i]);
        }
    }

    static forcedinline void pushInterpolationSamples (float* lastInputSamples, const float* input,
                                                       int numOut, int available, int wrapAround) noexcept
    {
        if (numOut >= 5)
        {
            if (available >= 5)
            {
                for (int i = 0; i < 5; ++i)
                    lastInputSamples[i] = input[--numOut];
            }
            else
            {
                for (int i = 0; i < available; ++i)
                    lastInputSamples[i] = input[--numOut];

                if (wrapAround > 0)
                {
                    numOut -= wrapAround;

                    for (int i = available; i < 5; ++i)
                        lastInputSamples[i] = input[--numOut];
                }
                else
                {
                    for (int i = available; i < 5; ++i)
                        lastInputSamples[i] = 0.0f;
                }
            }
        }
        else
        {
            if (numOut > available)
            {
                for (int i = 0; i < available; ++i)
                    pushInterpolationSample (lastInputSamples, input[i]);

                if (wrapAround > 0)
                {
                    for (int i = 0; i < numOut - available; ++i)
                        pushInterpolationSample (lastInputSamples, input[i + available - wrapAround]);
                }
                else
                {
                    for (int i = 0; i < numOut - available; ++i)
                        pushInterpolationSample (lastInputSamples, 0);
                }
            }
            else
            {
                for (int i = 0; i < numOut; ++i)
                    pushInterpolationSample (lastInputSamples, input[i]);
            }
        }
    }

    template <typename InterpolatorType>
    static int interpolate (float* lastInputSamples, double& subSamplePos, double actualRatio,
                            const float* in, float* out, int numOut) noexcept
    {
        auto pos = subSamplePos;

        if (actualRatio == 1.0 && pos == 1.0)
        {
            memcpy (out, in, (size_t) numOut * sizeof (float));
            pushInterpolationSamples (lastInputSamples, in, numOut);
            return numOut;
        }

        int numUsed = 0;

        while (numOut > 0)
        {
            while (pos >= 1.0)
            {
                pushInterpolationSample (lastInputSamples, in[numUsed++]);
                pos -= 1.0;
            }

            *out++ = InterpolatorType::valueAtOffset (lastInputSamples, (float) pos);
            pos += actualRatio;
            --numOut;
        }

        subSamplePos = pos;
        return numUsed;
    }

    template <typename InterpolatorType>
    static int interpolate (float* lastInputSamples, double& subSamplePos, double actualRatio,
                            const float* in, float* out, int numOut, int available, int wrap) noexcept
    {
        if (actualRatio == 1.0)
        {
            if (available >= numOut)
            {
                memcpy (out, in, (size_t) numOut * sizeof (float));
                pushInterpolationSamples (lastInputSamples, in, numOut, available, wrap);
            }
            else
            {
                memcpy (out, in, (size_t) available * sizeof (float));
                pushInterpolationSamples (lastInputSamples, in, numOut, available, wrap);

                if (wrap > 0)
                {
                    memcpy (out + available, in + available - wrap, (size_t) (numOut - available) * sizeof (float));
                    pushInterpolationSamples (lastInputSamples, in, numOut, available, wrap);
                }
                else
                {
                    for (int i = 0; i < numOut - available; ++i)
                        pushInterpolationSample (lastInputSamples, 0);
                }
            }

            return numOut;
        }

        auto originalIn = in;
        auto pos = subSamplePos;
        bool exceeded = false;

        if (actualRatio < 1.0)
        {
            for (int i = numOut; --i >= 0;)
            {
                if (pos >= 1.0)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (lastInputSamples, 0);
                    }
                    else
                    {
                        pushInterpolationSample (lastInputSamples, *in++);

                        if (--available <= 0)
                        {
                            if (wrap > 0)
                            {
                                in -= wrap;
                                available += wrap;
                            }
                            else
                            {
                                exceeded = true;
                            }
                        }
                    }

                    pos -= 1.0;
                }

                *out++ = InterpolatorType::valueAtOffset (lastInputSamples, (float) pos);
                pos += actualRatio;
            }
        }
        else
        {
            for (int i = numOut; --i >= 0;)
            {
                while (pos < actualRatio)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (lastInputSamples, 0);
                    }
                    else
                    {
                        pushInterpolationSample (lastInputSamples, *in++);

                        if (--available <= 0)
                        {
                            if (wrap > 0)
                            {
                                in -= wrap;
                                available += wrap;
                            }
                            else
                            {
                                exceeded = true;
                            }
                        }
                    }

                    pos += 1.0;
                }

                pos -= actualRatio;
                *out++ = InterpolatorType::valueAtOffset (lastInputSamples, jmax (0.0f, 1.0f - (float) pos));
            }
        }

        subSamplePos = pos;

        if (wrap == 0)
            return (int) (in - originalIn);

        return ((int) (in - originalIn) + wrap) % wrap;
    }

    template <typename InterpolatorType>
    static int interpolateAdding (float* lastInputSamples, double& subSamplePos, double actualRatio,
                                  const float* in, float* out, int numOut,
                                  int available, int wrap, float gain) noexcept
    {
        if (actualRatio == 1.0)
        {
            if (available >= numOut)
            {
                FloatVectorOperations::addWithMultiply (out, in, gain, numOut);
                pushInterpolationSamples (lastInputSamples, in, numOut, available, wrap);
            }
            else
            {
                FloatVectorOperations::addWithMultiply (out, in, gain, available);
                pushInterpolationSamples (lastInputSamples, in, available, available, wrap);

                if (wrap > 0)
                {
                    FloatVectorOperations::addWithMultiply (out, in - wrap, gain, numOut - available);
                    pushInterpolationSamples (lastInputSamples, in - wrap, numOut - available, available, wrap);
                }
                else
                {
                    for (int i = 0; i < numOut-available; ++i)
                        pushInterpolationSample (lastInputSamples, 0.0);
                }
            }

            return numOut;
        }

        auto originalIn = in;
        auto pos = subSamplePos;
        bool exceeded = false;

        if (actualRatio < 1.0)
        {
            for (int i = numOut; --i >= 0;)
            {
                if (pos >= 1.0)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (lastInputSamples, 0.0);
                    }
                    else
                    {
                        pushInterpolationSample (lastInputSamples, *in++);

                        if (--available <= 0)
                        {
                            if (wrap > 0)
                            {
                                in -= wrap;
                                available += wrap;
                            }
                            else
                            {
                                exceeded = true;
                            }
                        }
                    }

                    pos -= 1.0;
                }

                *out++ += gain * InterpolatorType::valueAtOffset (lastInputSamples, (float) pos);
                pos += actualRatio;
            }
        }
        else
        {
            for (int i = numOut; --i >= 0;)
            {
                while (pos < actualRatio)
                {
                    if (exceeded)
                    {
                        pushInterpolationSample (lastInputSamples, 0.0);
                    }
                    else
                    {
                        pushInterpolationSample (lastInputSamples, *in++);

                        if (--available <= 0)
                        {
                            if (wrap > 0)
                            {
                                in -= wrap;
                                available += wrap;
                            }
                            else
                            {
                                exceeded = true;
                            }
                        }
                    }

                    pos += 1.0;
                }

                pos -= actualRatio;
                *out++ += gain * InterpolatorType::valueAtOffset (lastInputSamples, jmax (0.0f, 1.0f - (float) pos));
            }
        }

        subSamplePos = pos;

        if (wrap == 0)
            return (int) (in - originalIn);

        return ((int) (in - originalIn) + wrap) % wrap;
    }

    template <typename InterpolatorType>
    static int interpolateAdding (float* lastInputSamples, double& subSamplePos, double actualRatio,
                                  const float* in, float* out, int numOut, float gain) noexcept
    {
        auto pos = subSamplePos;

        if (actualRatio == 1.0 && pos == 1.0)
        {
            FloatVectorOperations::addWithMultiply (out, in, gain, numOut);
            pushInterpolationSamples (lastInputSamples, in, numOut);
            return numOut;
        }

        int numUsed = 0;

        while (numOut > 0)
        {
            while (pos >= 1.0)
            {
                pushInterpolationSample (lastInputSamples, in[numUsed++]);
                pos -= 1.0;
            }

            *out++ += gain * InterpolatorType::valueAtOffset (lastInputSamples, (float) pos);
            pos += actualRatio;
            --numOut;
        }

        subSamplePos = pos;
        return numUsed;
    }
}

//==============================================================================
template <int k>
struct LagrangeResampleHelper
{
    static forcedinline void calc (float& a, float b) noexcept   { a *= b * (1.0f / k); }
};

template<>
struct LagrangeResampleHelper<0>
{
    static forcedinline void calc (float&, float) noexcept {}
};

struct LagrangeAlgorithm
{
    static forcedinline float valueAtOffset (const float* inputs, float offset) noexcept
    {
        return calcCoefficient<0> (inputs[4], offset)
             + calcCoefficient<1> (inputs[3], offset)
             + calcCoefficient<2> (inputs[2], offset)
             + calcCoefficient<3> (inputs[1], offset)
             + calcCoefficient<4> (inputs[0], offset);
    }

    template <int k>
    static forcedinline float calcCoefficient (float input, float offset) noexcept
    {
        LagrangeResampleHelper<0 - k>::calc (input, -2.0f - offset);
        LagrangeResampleHelper<1 - k>::calc (input, -1.0f - offset);
        LagrangeResampleHelper<2 - k>::calc (input,  0.0f - offset);
        LagrangeResampleHelper<3 - k>::calc (input,  1.0f - offset);
        LagrangeResampleHelper<4 - k>::calc (input,  2.0f - offset);
        return input;
    }
};

LagrangeInterpolator::LagrangeInterpolator() noexcept  { reset(); }
LagrangeInterpolator::~LagrangeInterpolator() noexcept {}

void LagrangeInterpolator::reset() noexcept
{
    subSamplePos = 1.0;

    for (auto& s : lastInputSamples)
        s = 0;
}

int LagrangeInterpolator::process (double actualRatio, const float* in, float* out, int numOut, int available, int wrap) noexcept
{
    return interpolate<LagrangeAlgorithm> (lastInputSamples, subSamplePos, actualRatio, in, out, numOut, available, wrap);
}

int LagrangeInterpolator::process (double actualRatio, const float* in, float* out, int numOut) noexcept
{
    return interpolate<LagrangeAlgorithm> (lastInputSamples, subSamplePos, actualRatio, in, out, numOut);
}

int LagrangeInterpolator::processAdding (double actualRatio, const float* in, float* out, int numOut, int available, int wrap, float gain) noexcept
{
    return interpolateAdding<LagrangeAlgorithm> (lastInputSamples, subSamplePos, actualRatio, in, out, numOut, available, wrap, gain);
}

int LagrangeInterpolator::processAdding (double actualRatio, const float* in, float* out, int numOut, float gain) noexcept
{
    return interpolateAdding<LagrangeAlgorithm> (lastInputSamples, subSamplePos, actualRatio, in, out, numOut, gain);
}

} // namespace juce
