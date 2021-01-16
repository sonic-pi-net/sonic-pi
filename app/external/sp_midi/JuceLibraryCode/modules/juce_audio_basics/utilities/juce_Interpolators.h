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
    A collection of different interpolators for resampling streams of floats.

    @see GenericInterpolator, WindowedSincInterpolator, LagrangeInterpolator,
         CatmullRomInterpolator, LinearInterpolator, ZeroOrderHoldInterpolator

    @tags{Audio}
*/
class Interpolators
{
private:
    struct WindowedSincTraits
    {
        static constexpr float algorithmicLatency = 100.0f;

        static forcedinline float windowedSinc (float firstFrac, int index) noexcept
        {
            auto index2 = index + 1;
            auto frac = firstFrac;

            auto value1 = lookupTable[index];
            auto value2 = lookupTable[index2];

            return value1 + (frac * (value2 - value1));
        }

        static forcedinline float valueAtOffset (const float* const inputs, const float offset, int indexBuffer) noexcept
        {
            const int numCrossings = 100;
            const float floatCrossings = (float) numCrossings;
            float result = 0.0f;

            auto samplePosition = indexBuffer;
            float firstFrac = 0.0f;
            float lastSincPosition = -1.0f;
            int index = 0, sign = -1;

            for (int i = -numCrossings; i <= numCrossings; ++i)
            {
                auto sincPosition = (1.0f - offset) + (float) i;

                if (i == -numCrossings || (sincPosition >= 0 && lastSincPosition < 0))
                {
                    auto indexFloat = (sincPosition >= 0.f ? sincPosition : -sincPosition) * 100.0f;
                    auto indexFloored = std::floor (indexFloat);
                    index = (int) indexFloored;
                    firstFrac = indexFloat - indexFloored;
                    sign = (sincPosition < 0 ? -1 : 1);
                }

                if (sincPosition == 0.0f)
                    result += inputs[samplePosition];
                else if (sincPosition < floatCrossings && sincPosition > -floatCrossings)
                    result += inputs[samplePosition] * windowedSinc (firstFrac, index);

                if (++samplePosition == numCrossings * 2)
                    samplePosition = 0;

                lastSincPosition = sincPosition;
                index += 100 * sign;
            }

            return result;
        }

        static const float lookupTable[10001];
    };

    struct LagrangeTraits
    {
        static constexpr float algorithmicLatency = 2.0f;

        static float valueAtOffset (const float*, float, int) noexcept;
    };

    struct CatmullRomTraits
    {
        //==============================================================================
        static constexpr float algorithmicLatency = 2.0f;

        static forcedinline float valueAtOffset (const float* const inputs, const float offset, int index) noexcept
        {
            auto y0 = inputs[index]; if (++index == 4) index = 0;
            auto y1 = inputs[index]; if (++index == 4) index = 0;
            auto y2 = inputs[index]; if (++index == 4) index = 0;
            auto y3 = inputs[index];

            auto halfY0 = 0.5f * y0;
            auto halfY3 = 0.5f * y3;

            return y1 + offset * ((0.5f * y2 - halfY0)
                      + (offset * (((y0 + 2.0f * y2) - (halfY3 + 2.5f * y1))
                      + (offset * ((halfY3 + 1.5f * y1) - (halfY0 + 1.5f * y2))))));
        }
    };

    struct LinearTraits
    {
        static constexpr float algorithmicLatency = 1.0f;

        static forcedinline float valueAtOffset (const float* const inputs, const float offset, int index) noexcept
        {
            auto y0 = inputs[index];
            auto y1 = inputs[index == 0 ? 1 : 0];

            return y1 * offset + y0 * (1.0f - offset);
        }
    };

    struct ZeroOrderHoldTraits
    {
        static constexpr float algorithmicLatency = 0.0f;

        static forcedinline float valueAtOffset (const float* const inputs, const float, int) noexcept
        {
            return inputs[0];
        }
    };

public:
    using WindowedSinc  = GenericInterpolator<WindowedSincTraits,  200>;
    using Lagrange      = GenericInterpolator<LagrangeTraits,      5>;
    using CatmullRom    = GenericInterpolator<CatmullRomTraits,    4>;
    using Linear        = GenericInterpolator<LinearTraits,        2>;
    using ZeroOrderHold = GenericInterpolator<ZeroOrderHoldTraits, 1>;
};

//==============================================================================
/**
    An interpolator for resampling a stream of floats using high order windowed
    (hann) sinc interpolation, recommended for high quality resampling.

    Note that the resampler is stateful, so when there's a break in the continuity
    of the input stream you're feeding it, you should call reset() before feeding
    it any new data. And like with any other stateful filter, if you're resampling
    multiple channels, make sure each one uses its own LinearInterpolator object.

    @see GenericInterpolator

    @see LagrangeInterpolator, CatmullRomInterpolator, LinearInterpolator,
         ZeroOrderHoldInterpolator

    @tags{Audio}
*/
using WindowedSincInterpolator = Interpolators::WindowedSinc;

/**
    An interpolator for resampling a stream of floats using 4-point lagrange interpolation.

    Note that the resampler is stateful, so when there's a break in the continuity
    of the input stream you're feeding it, you should call reset() before feeding
    it any new data. And like with any other stateful filter, if you're resampling
    multiple channels, make sure each one uses its own LagrangeInterpolator object.

    @see GenericInterpolator

    @see CatmullRomInterpolator, WindowedSincInterpolator, LinearInterpolator,
         ZeroOrderHoldInterpolator

    @tags{Audio}
*/
using LagrangeInterpolator = Interpolators::Lagrange;

/**
    An interpolator for resampling a stream of floats using Catmull-Rom interpolation.

    Note that the resampler is stateful, so when there's a break in the continuity
    of the input stream you're feeding it, you should call reset() before feeding
    it any new data. And like with any other stateful filter, if you're resampling
    multiple channels, make sure each one uses its own CatmullRomInterpolator object.

    @see GenericInterpolator

    @see LagrangeInterpolator, WindowedSincInterpolator, LinearInterpolator,
         ZeroOrderHoldInterpolator

    @tags{Audio}
*/
using CatmullRomInterpolator = Interpolators::CatmullRom;

/**
    An interpolator for resampling a stream of floats using linear interpolation.

    Note that the resampler is stateful, so when there's a break in the continuity
    of the input stream you're feeding it, you should call reset() before feeding
    it any new data. And like with any other stateful filter, if you're resampling
    multiple channels, make sure each one uses its own LinearInterpolator object.

    @see GenericInterpolator

    @see LagrangeInterpolator, CatmullRomInterpolator, WindowedSincInterpolator,
         ZeroOrderHoldInterpolator

    @tags{Audio}
*/
using LinearInterpolator = Interpolators::Linear;

/**
    An interpolator for resampling a stream of floats using zero order hold
    interpolation.

    Note that the resampler is stateful, so when there's a break in the continuity
    of the input stream you're feeding it, you should call reset() before feeding
    it any new data. And like with any other stateful filter, if you're resampling
    multiple channels, make sure each one uses its own ZeroOrderHoldInterpolator
    object.

    @see GenericInterpolator

    @see LagrangeInterpolator, CatmullRomInterpolator, WindowedSincInterpolator,
         LinearInterpolator

    @tags{Audio}
*/
using ZeroOrderHoldInterpolator = Interpolators::ZeroOrderHold;

} // namespace juce
