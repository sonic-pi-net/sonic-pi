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

//==============================================================================
/**
    A base class for the smoothed value classes.

    This class is used to provide common functionality to the SmoothedValue and
    dsp::LogRampedValue classes.

    @tags{Audio}
*/
template <typename SmoothedValueType>
class SmoothedValueBase
{
private:
    //==============================================================================
    template <typename T> struct FloatTypeHelper;

    template <template <typename> class SmoothedValueClass, typename FloatType>
    struct FloatTypeHelper <SmoothedValueClass <FloatType>>
    {
        using Type = FloatType;
    };

    template <template <typename, typename> class SmoothedValueClass, typename FloatType, typename SmoothingType>
    struct FloatTypeHelper <SmoothedValueClass <FloatType, SmoothingType>>
    {
        using Type = FloatType;
    };

public:
    using FloatType = typename FloatTypeHelper<SmoothedValueType>::Type;

    //==============================================================================
    /** Constructor. */
    SmoothedValueBase() = default;

    virtual ~SmoothedValueBase() {}

    //==============================================================================
    /** Returns true if the current value is currently being interpolated. */
    bool isSmoothing() const noexcept                    { return countdown > 0; }

    /** Returns the current value of the ramp. */
    FloatType getCurrentValue() const noexcept           { return currentValue; }

    //==============================================================================
    /** Returns the target value towards which the smoothed value is currently moving. */
    FloatType getTargetValue() const noexcept            { return target; }

    /** Sets the current value and the target value.
        @param newValue    the new value to take
    */
    void setCurrentAndTargetValue (FloatType newValue)
    {
        target = currentValue = newValue;
        countdown = 0;
    }

    //==============================================================================
    /** Applies a smoothed gain to a stream of samples
        S[i] *= gain
        @param samples Pointer to a raw array of samples
        @param numSamples Length of array of samples
    */
    void applyGain (FloatType* samples, int numSamples) noexcept
    {
        jassert (numSamples >= 0);

        if (isSmoothing())
        {
            for (int i = 0; i < numSamples; ++i)
                samples[i] *= getNextSmoothedValue();
        }
        else
        {
            FloatVectorOperations::multiply (samples, target, numSamples);
        }
    }

    /** Computes output as a smoothed gain applied to a stream of samples.
        Sout[i] = Sin[i] * gain
        @param samplesOut A pointer to a raw array of output samples
        @param samplesIn  A pointer to a raw array of input samples
        @param numSamples The length of the array of samples
    */
    void applyGain (FloatType* samplesOut, const FloatType* samplesIn, int numSamples) noexcept
    {
        jassert (numSamples >= 0);

        if (isSmoothing())
        {
            for (int i = 0; i < numSamples; ++i)
                samplesOut[i] = samplesIn[i] * getNextSmoothedValue();
        }
        else
        {
            FloatVectorOperations::multiply (samplesOut, samplesIn, target, numSamples);
        }
    }

    /** Applies a smoothed gain to a buffer */
    void applyGain (AudioBuffer<FloatType>& buffer, int numSamples) noexcept
    {
        jassert (numSamples >= 0);

        if (isSmoothing())
        {
            if (buffer.getNumChannels() == 1)
            {
                auto* samples = buffer.getWritePointer (0);

                for (int i = 0; i < numSamples; ++i)
                    samples[i] *= getNextSmoothedValue();
            }
            else
            {
                for (auto i = 0; i < numSamples; ++i)
                {
                    auto gain = getNextSmoothedValue();

                    for (int channel = 0; channel < buffer.getNumChannels(); channel++)
                        buffer.setSample (channel, i, buffer.getSample (channel, i) * gain);
                }
            }
        }
        else
        {
            buffer.applyGain (0, numSamples, target);
        }
    }

private:
    //==============================================================================
    FloatType getNextSmoothedValue() noexcept
    {
        return static_cast <SmoothedValueType*> (this)->getNextValue();
    }

protected:
    //==============================================================================
    FloatType currentValue = 0;
    FloatType target = currentValue;
    int countdown = 0;
};

//==============================================================================
/**
    A namespace containing a set of types used for specifying the smoothing
    behaviour of the SmoothedValue class.

    For example:
    @code
    SmoothedValue<float, ValueSmoothingTypes::Multiplicative> frequency (1.0f);
    @endcode
*/
namespace ValueSmoothingTypes
{
    /**
        Used to indicate a linear smoothing between values.

        @tags{Audio}
    */
    struct Linear {};

    /**
        Used to indicate a smoothing between multiplicative values.

        @tags{Audio}
    */
    struct Multiplicative {};
}

//==============================================================================
/**
    A utility class for values that need smoothing to avoid audio glitches.

    A ValueSmoothingTypes::Linear template parameter selects linear smoothing,
    which increments the SmoothedValue linearly towards its target value.

    @code
    SmoothedValue<float, ValueSmoothingTypes::Linear> yourSmoothedValue;
    @endcode

    A ValueSmoothingTypes::Multiplicative template parameter selects
    multiplicative smoothing increments towards the target value.

    @code
    SmoothedValue<float, ValueSmoothingTypes::Multiplicative> yourSmoothedValue;
    @endcode

    Multiplicative smoothing is useful when you are dealing with
    exponential/logarithmic values like volume in dB or frequency in Hz. For
    example a 12 step ramp from 440.0 Hz (A4) to 880.0 Hz (A5) will increase the
    frequency with an equal temperament tuning across the octave. A 10 step
    smoothing from 1.0 (0 dB) to 3.16228 (10 dB) will increase the value in
    increments of 1 dB.

    Note that when you are using multiplicative smoothing you cannot ever reach a
    target value of zero!

    @tags{Audio}
*/
template <typename FloatType, typename SmoothingType = ValueSmoothingTypes::Linear>
class SmoothedValue   : public SmoothedValueBase <SmoothedValue <FloatType, SmoothingType>>
{
public:
    //==============================================================================
    /** Constructor. */
    SmoothedValue() noexcept
        : SmoothedValue ((FloatType) (std::is_same<SmoothingType, ValueSmoothingTypes::Linear>::value ? 0 : 1))
    {
    }

    /** Constructor. */
    SmoothedValue (FloatType initialValue) noexcept
    {
        // Multiplicative smoothed values cannot ever reach 0!
        jassert (! (std::is_same<SmoothingType, ValueSmoothingTypes::Multiplicative>::value && initialValue == 0));

        // Visual Studio can't handle base class initialisation with CRTP
        this->currentValue = initialValue;
        this->target = this->currentValue;
    }

    //==============================================================================
    /** Reset to a new sample rate and ramp length.
        @param sampleRate           The sample rate
        @param rampLengthInSeconds  The duration of the ramp in seconds
    */
    void reset (double sampleRate, double rampLengthInSeconds) noexcept
    {
        jassert (sampleRate > 0 && rampLengthInSeconds >= 0);
        reset ((int) std::floor (rampLengthInSeconds * sampleRate));
    }

    /** Set a new ramp length directly in samples.
        @param numSteps     The number of samples over which the ramp should be active
    */
    void reset (int numSteps) noexcept
    {
        stepsToTarget = numSteps;
        this->setCurrentAndTargetValue (this->target);
    }

    //==============================================================================
    /** Set the next value to ramp towards.
        @param newValue     The new target value
    */
    void setTargetValue (FloatType newValue) noexcept
    {
        if (newValue == this->target)
            return;

        if (stepsToTarget <= 0)
        {
            this->setCurrentAndTargetValue (newValue);
            return;
        }

        // Multiplicative smoothed values cannot ever reach 0!
        jassert (! (std::is_same<SmoothingType, ValueSmoothingTypes::Multiplicative>::value && newValue == 0));

        this->target = newValue;
        this->countdown = stepsToTarget;

        setStepSize();
    }

    //==============================================================================
    /** Compute the next value.
        @returns Smoothed value
    */
    FloatType getNextValue() noexcept
    {
        if (! this->isSmoothing())
            return this->target;

        --(this->countdown);

        if (this->isSmoothing())
            setNextValue();
        else
            this->currentValue = this->target;

        return this->currentValue;
    }

    //==============================================================================
    /** Skip the next numSamples samples.
        This is identical to calling getNextValue numSamples times. It returns
        the new current value.
        @see getNextValue
    */
    FloatType skip (int numSamples) noexcept
    {
        if (numSamples >= this->countdown)
        {
            this->setCurrentAndTargetValue (this->target);
            return this->target;
        }

        skipCurrentValue (numSamples);

        this->countdown -= numSamples;
        return this->currentValue;
    }

    //==============================================================================
    /** THIS FUNCTION IS DEPRECATED.

        Use `setTargetValue (float)` and `setCurrentAndTargetValue()` instead:

        lsv.setValue (x, false); -> lsv.setTargetValue (x);
        lsv.setValue (x, true);  -> lsv.setCurrentAndTargetValue (x);

        @param newValue     The new target value
        @param force        If true, the value will be set immediately, bypassing the ramp
    */
    JUCE_DEPRECATED_WITH_BODY (void setValue (FloatType newValue, bool force = false) noexcept,
    {
        if (force)
        {
            this->setCurrentAndTargetValue (newValue);
            return;
        }

        setTargetValue (newValue);
    })

private:
    //==============================================================================
    template <typename T>
    using LinearVoid = typename std::enable_if <std::is_same <T, ValueSmoothingTypes::Linear>::value, void>::type;

    template <typename T>
    using MultiplicativeVoid = typename std::enable_if <std::is_same <T, ValueSmoothingTypes::Multiplicative>::value, void>::type;

    //==============================================================================
    template <typename T = SmoothingType>
    LinearVoid<T> setStepSize() noexcept
    {
        step = (this->target - this->currentValue) / (FloatType) this->countdown;
    }

    template <typename T = SmoothingType>
    MultiplicativeVoid<T> setStepSize()
    {
        step = std::exp ((std::log (std::abs (this->target)) - std::log (std::abs (this->currentValue))) / (FloatType) this->countdown);
    }

    //==============================================================================
    template <typename T = SmoothingType>
    LinearVoid<T> setNextValue() noexcept
    {
        this->currentValue += step;
    }

    template <typename T = SmoothingType>
    MultiplicativeVoid<T> setNextValue() noexcept
    {
        this->currentValue *= step;
    }

    //==============================================================================
    template <typename T = SmoothingType>
    LinearVoid<T> skipCurrentValue (int numSamples) noexcept
    {
        this->currentValue += step * (FloatType) numSamples;
    }

    template <typename T = SmoothingType>
    MultiplicativeVoid<T> skipCurrentValue (int numSamples)
    {
        this->currentValue *= (FloatType) std::pow (step, numSamples);
    }

    //==============================================================================
    FloatType step = FloatType();
    int stepsToTarget = 0;
};

template <typename FloatType>
using LinearSmoothedValue = SmoothedValue <FloatType, ValueSmoothingTypes::Linear>;


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

template <class SmoothedValueType>
class CommonSmoothedValueTests  : public UnitTest
{
public:
    CommonSmoothedValueTests()
        : UnitTest ("CommonSmoothedValueTests", UnitTestCategories::smoothedValues)
    {}

    void runTest() override
    {
        beginTest ("Initial state");
        {
            SmoothedValueType sv;

            auto value = sv.getCurrentValue();
            expectEquals (sv.getTargetValue(), value);

            sv.getNextValue();
            expectEquals (sv.getCurrentValue(), value);
            expect (! sv.isSmoothing());
        }

        beginTest ("Resetting");
        {
            auto initialValue = 15.0f;

            SmoothedValueType sv (initialValue);
            sv.reset (3);
            expectEquals (sv.getCurrentValue(), initialValue);

            auto targetValue = initialValue + 1.0f;
            sv.setTargetValue (targetValue);
            expectEquals (sv.getTargetValue(), targetValue);
            expectEquals (sv.getCurrentValue(), initialValue);
            expect (sv.isSmoothing());

            auto currentValue = sv.getNextValue();
            expect (currentValue > initialValue);
            expectEquals (sv.getCurrentValue(), currentValue);
            expectEquals (sv.getTargetValue(), targetValue);
            expect (sv.isSmoothing());

            sv.reset (5);

            expectEquals (sv.getCurrentValue(), targetValue);
            expectEquals (sv.getTargetValue(),  targetValue);
            expect (! sv.isSmoothing());

            sv.getNextValue();
            expectEquals (sv.getCurrentValue(), targetValue);

            sv.setTargetValue (1.5f);
            sv.getNextValue();

            float newStart = 0.2f;
            sv.setCurrentAndTargetValue (newStart);
            expectEquals (sv.getNextValue(), newStart);
            expectEquals (sv.getTargetValue(), newStart);
            expectEquals (sv.getCurrentValue(), newStart);
            expect (! sv.isSmoothing());
        }

        beginTest ("Sample rate");
        {
            SmoothedValueType svSamples { 3.0f };
            auto svTime = svSamples;

            auto numSamples = 12;

            svSamples.reset (numSamples);
            svTime.reset (numSamples * 2, 1.0);

            for (int i = 0; i < numSamples; ++i)
            {
                svTime.skip (1);
                expectWithinAbsoluteError (svSamples.getNextValue(),
                                           svTime.getNextValue(),
                                           1.0e-7f);
            }
        }

        beginTest ("Block processing");
        {
            SmoothedValueType sv (1.0f);

            sv.reset (12);
            sv.setTargetValue (2.0f);

            const auto numSamples = 15;

            AudioBuffer<float> referenceData (1, numSamples);

            for (int i = 0; i < numSamples; ++i)
                referenceData.setSample (0, i, sv.getNextValue());

            expect (referenceData.getSample (0, 0) > 0);
            expect (referenceData.getSample (0, 10) < sv.getTargetValue());
            expectWithinAbsoluteError (referenceData.getSample (0, 11),
                                       sv.getTargetValue(),
                                       1.0e-7f);

            auto getUnitData = [] (int numSamplesToGenerate)
            {
                AudioBuffer<float> result (1, numSamplesToGenerate);

                for (int i = 0; i < numSamplesToGenerate; ++i)
                    result.setSample (0, i, 1.0f);

                return result;
            };

            auto compareData = [this] (const AudioBuffer<float>& test,
                                       const AudioBuffer<float>& reference)
            {
                for (int i = 0; i < test.getNumSamples(); ++i)
                    expectWithinAbsoluteError (test.getSample (0, i),
                                               reference.getSample (0, i),
                                               1.0e-7f);
            };

            auto testData = getUnitData (numSamples);
            sv.setCurrentAndTargetValue (1.0f);
            sv.setTargetValue (2.0f);
            sv.applyGain (testData.getWritePointer (0), numSamples);
            compareData (testData, referenceData);

            testData = getUnitData (numSamples);
            AudioBuffer<float> destData (1, numSamples);
            sv.setCurrentAndTargetValue (1.0f);
            sv.setTargetValue (2.0f);
            sv.applyGain (destData.getWritePointer (0),
                           testData.getReadPointer (0),
                           numSamples);
            compareData (destData, referenceData);
            compareData (testData, getUnitData (numSamples));

            testData = getUnitData (numSamples);
            sv.setCurrentAndTargetValue (1.0f);
            sv.setTargetValue (2.0f);
            sv.applyGain (testData, numSamples);
            compareData (testData, referenceData);
        }

        beginTest ("Skip");
        {
            SmoothedValueType sv;

            sv.reset (12);
            sv.setCurrentAndTargetValue (1.0f);
            sv.setTargetValue (2.0f);

            Array<float> reference;

            for (int i = 0; i < 15; ++i)
                reference.add (sv.getNextValue());

            sv.setCurrentAndTargetValue (1.0f);
            sv.setTargetValue (2.0f);

            expectWithinAbsoluteError (sv.skip (1), reference[0], 1.0e-6f);
            expectWithinAbsoluteError (sv.skip (1), reference[1], 1.0e-6f);
            expectWithinAbsoluteError (sv.skip (2), reference[3], 1.0e-6f);
            sv.skip (3);
            expectWithinAbsoluteError (sv.getCurrentValue(), reference[6], 1.0e-6f);
            expectEquals (sv.skip (300), sv.getTargetValue());
            expectEquals (sv.getCurrentValue(), sv.getTargetValue());
        }

        beginTest ("Negative");
        {
            SmoothedValueType sv;

            auto numValues = 12;
            sv.reset (numValues);

            std::vector<std::pair<float, float>> ranges = { { -1.0f, -2.0f },
                                                            { -100.0f, -3.0f } };

            for (auto range : ranges)
            {
                auto start = range.first, end = range.second;

                sv.setCurrentAndTargetValue (start);
                sv.setTargetValue (end);

                auto val = sv.skip (numValues / 2);

                if (end > start)
                    expect (val > start && val < end);
                else
                    expect (val < start && val > end);

                auto nextVal = sv.getNextValue();
                expect (end > start ? (nextVal > val) : (nextVal < val));

                auto endVal = sv.skip (500);
                expectEquals (endVal, end);
                expectEquals (sv.getNextValue(), end);
                expectEquals (sv.getCurrentValue(), end);

                sv.setCurrentAndTargetValue (start);
                sv.setTargetValue (end);

                SmoothedValueType positiveSv { -start };
                positiveSv.reset (numValues);
                positiveSv.setTargetValue (-end);

                for (int i = 0; i < numValues + 2; ++i)
                    expectEquals (sv.getNextValue(), -positiveSv.getNextValue());
            }
        }
    }
};

#endif

} // namespace juce
