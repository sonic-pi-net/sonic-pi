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

IIRCoefficients::IIRCoefficients() noexcept
{
    zeromem (coefficients, sizeof (coefficients));
}

IIRCoefficients::~IIRCoefficients() noexcept {}

IIRCoefficients::IIRCoefficients (const IIRCoefficients& other) noexcept
{
    memcpy (coefficients, other.coefficients, sizeof (coefficients));
}

IIRCoefficients& IIRCoefficients::operator= (const IIRCoefficients& other) noexcept
{
    memcpy (coefficients, other.coefficients, sizeof (coefficients));
    return *this;
}

IIRCoefficients::IIRCoefficients (double c1, double c2, double c3,
                                  double c4, double c5, double c6) noexcept
{
    const double a = 1.0 / c4;

    coefficients[0] = (float) (c1 * a);
    coefficients[1] = (float) (c2 * a);
    coefficients[2] = (float) (c3 * a);
    coefficients[3] = (float) (c5 * a);
    coefficients[4] = (float) (c6 * a);
}

IIRCoefficients IIRCoefficients::makeLowPass (const double sampleRate,
                                              const double frequency) noexcept
{
    return makeLowPass (sampleRate, frequency, 1.0 / std::sqrt (2.0));
}

IIRCoefficients IIRCoefficients::makeLowPass (const double sampleRate,
                                              const double frequency,
                                              const double Q) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (frequency > 0.0 && frequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double n = 1.0 / std::tan (double_Pi * frequency / sampleRate);
    const double nSquared = n * n;
    const double c1 = 1.0 / (1.0 + 1.0 / Q * n + nSquared);

    return IIRCoefficients (c1,
                            c1 * 2.0,
                            c1,
                            1.0,
                            c1 * 2.0 * (1.0 - nSquared),
                            c1 * (1.0 - 1.0 / Q * n + nSquared));
}

IIRCoefficients IIRCoefficients::makeHighPass (const double sampleRate,
                                               const double frequency) noexcept
{
    return makeHighPass (sampleRate, frequency, 1.0 / std::sqrt(2.0));
}

IIRCoefficients IIRCoefficients::makeHighPass (const double sampleRate,
                                               const double frequency,
                                               const double Q) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (frequency > 0.0 && frequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double n = std::tan (double_Pi * frequency / sampleRate);
    const double nSquared = n * n;
    const double c1 = 1.0 / (1.0 + 1.0 / Q * n + nSquared);

    return IIRCoefficients (c1,
                            c1 * -2.0,
                            c1,
                            1.0,
                            c1 * 2.0 * (nSquared - 1.0),
                            c1 * (1.0 - 1.0 / Q * n + nSquared));
}

IIRCoefficients IIRCoefficients::makeBandPass (const double sampleRate,
                                               const double frequency) noexcept
{
    return makeBandPass (sampleRate, frequency, 1.0 / std::sqrt (2.0));
}

IIRCoefficients IIRCoefficients::makeBandPass (const double sampleRate,
                                               const double frequency,
                                               const double Q) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (frequency > 0.0 && frequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double n = 1.0 / std::tan (double_Pi * frequency / sampleRate);
    const double nSquared = n * n;
    const double c1 = 1.0 / (1.0 + 1.0 / Q * n + nSquared);

    return IIRCoefficients (c1 * n / Q,
                            0.0,
                            -c1 * n / Q,
                            1.0,
                            c1 * 2.0 * (1.0 - nSquared),
                            c1 * (1.0 - 1.0 / Q * n + nSquared));
}

IIRCoefficients IIRCoefficients::makeNotchFilter (const double sampleRate,
                                                  const double frequency) noexcept
{
    return makeNotchFilter (sampleRate, frequency, 1.0 / std::sqrt (2.0));
}

IIRCoefficients IIRCoefficients::makeNotchFilter (const double sampleRate,
                                                  const double frequency,
                                                  const double Q) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (frequency > 0.0 && frequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double n = 1.0 / std::tan (double_Pi * frequency / sampleRate);
    const double nSquared = n * n;
    const double c1 = 1.0 / (1.0 + n / Q + nSquared);

    return IIRCoefficients (c1 * (1.0 + nSquared),
                            2.0 * c1 * (1.0 - nSquared),
                            c1 * (1.0 + nSquared),
                            1.0,
                            c1 * 2.0 * (1.0 - nSquared),
                            c1 * (1.0 - n / Q + nSquared));
}

IIRCoefficients IIRCoefficients::makeAllPass (const double sampleRate,
                                              const double frequency) noexcept
{
    return makeAllPass (sampleRate, frequency, 1.0 / std::sqrt (2.0));
}

IIRCoefficients IIRCoefficients::makeAllPass (const double sampleRate,
                                              const double frequency,
                                              const double Q) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (frequency > 0.0 && frequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double n = 1.0 / std::tan (double_Pi * frequency / sampleRate);
    const double nSquared = n * n;
    const double c1 = 1.0 / (1.0 + 1.0 / Q * n + nSquared);

    return IIRCoefficients (c1 * (1.0 - n / Q + nSquared),
                            c1 * 2.0 * (1.0 - nSquared),
                            1.0,
                            1.0,
                            c1 * 2.0 * (1.0 - nSquared),
                            c1 * (1.0 - n / Q + nSquared));
}

IIRCoefficients IIRCoefficients::makeLowShelf (const double sampleRate,
                                               const double cutOffFrequency,
                                               const double Q,
                                               const float gainFactor) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (cutOffFrequency > 0.0 && cutOffFrequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double A = jmax (0.0f, std::sqrt (gainFactor));
    const double aminus1 = A - 1.0;
    const double aplus1 = A + 1.0;
    const double omega = (double_Pi * 2.0 * jmax (cutOffFrequency, 2.0)) / sampleRate;
    const double coso = std::cos (omega);
    const double beta = std::sin (omega) * std::sqrt (A) / Q;
    const double aminus1TimesCoso = aminus1 * coso;

    return IIRCoefficients (A * (aplus1 - aminus1TimesCoso + beta),
                            A * 2.0 * (aminus1 - aplus1 * coso),
                            A * (aplus1 - aminus1TimesCoso - beta),
                            aplus1 + aminus1TimesCoso + beta,
                            -2.0 * (aminus1 + aplus1 * coso),
                            aplus1 + aminus1TimesCoso - beta);
}

IIRCoefficients IIRCoefficients::makeHighShelf (const double sampleRate,
                                                const double cutOffFrequency,
                                                const double Q,
                                                const float gainFactor) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (cutOffFrequency > 0.0 && cutOffFrequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double A = jmax (0.0f, std::sqrt (gainFactor));
    const double aminus1 = A - 1.0;
    const double aplus1 = A + 1.0;
    const double omega = (double_Pi * 2.0 * jmax (cutOffFrequency, 2.0)) / sampleRate;
    const double coso = std::cos (omega);
    const double beta = std::sin (omega) * std::sqrt (A) / Q;
    const double aminus1TimesCoso = aminus1 * coso;

    return IIRCoefficients (A * (aplus1 + aminus1TimesCoso + beta),
                            A * -2.0 * (aminus1 + aplus1 * coso),
                            A * (aplus1 + aminus1TimesCoso - beta),
                            aplus1 - aminus1TimesCoso + beta,
                            2.0 * (aminus1 - aplus1 * coso),
                            aplus1 - aminus1TimesCoso - beta);
}

IIRCoefficients IIRCoefficients::makePeakFilter (const double sampleRate,
                                                 const double frequency,
                                                 const double Q,
                                                 const float gainFactor) noexcept
{
    jassert (sampleRate > 0.0);
    jassert (frequency > 0.0 && frequency <= sampleRate * 0.5);
    jassert (Q > 0.0);

    const double A = jmax (0.0f, std::sqrt (gainFactor));
    const double omega = (double_Pi * 2.0 * jmax (frequency, 2.0)) / sampleRate;
    const double alpha = 0.5 * std::sin (omega) / Q;
    const double c2 = -2.0 * std::cos (omega);
    const double alphaTimesA = alpha * A;
    const double alphaOverA = alpha / A;

    return IIRCoefficients (1.0 + alphaTimesA,
                            c2,
                            1.0 - alphaTimesA,
                            1.0 + alphaOverA,
                            c2,
                            1.0 - alphaOverA);
}

//==============================================================================
IIRFilter::IIRFilter() noexcept
    : v1 (0.0), v2 (0.0), active (false)
{
}

IIRFilter::IIRFilter (const IIRFilter& other) noexcept
    : v1 (0.0), v2 (0.0), active (other.active)
{
    const SpinLock::ScopedLockType sl (other.processLock);
    coefficients = other.coefficients;
}

IIRFilter::~IIRFilter() noexcept
{
}

//==============================================================================
void IIRFilter::makeInactive() noexcept
{
    const SpinLock::ScopedLockType sl (processLock);
    active = false;
}

void IIRFilter::setCoefficients (const IIRCoefficients& newCoefficients) noexcept
{
    const SpinLock::ScopedLockType sl (processLock);

    coefficients = newCoefficients;
    active = true;
}

//==============================================================================
void IIRFilter::reset() noexcept
{
    const SpinLock::ScopedLockType sl (processLock);
    v1 = v2 = 0.0;
}

float IIRFilter::processSingleSampleRaw (const float in) noexcept
{
    float out = coefficients.coefficients[0] * in + v1;

    JUCE_SNAP_TO_ZERO (out);

    v1 = coefficients.coefficients[1] * in - coefficients.coefficients[3] * out + v2;
    v2 = coefficients.coefficients[2] * in - coefficients.coefficients[4] * out;

    return out;
}

void IIRFilter::processSamples (float* const samples, const int numSamples) noexcept
{
    const SpinLock::ScopedLockType sl (processLock);

    if (active)
    {
        const float c0 = coefficients.coefficients[0];
        const float c1 = coefficients.coefficients[1];
        const float c2 = coefficients.coefficients[2];
        const float c3 = coefficients.coefficients[3];
        const float c4 = coefficients.coefficients[4];
        float lv1 = v1, lv2 = v2;

        for (int i = 0; i < numSamples; ++i)
        {
            const float in = samples[i];
            const float out = c0 * in + lv1;
            samples[i] = out;

            lv1 = c1 * in - c3 * out + lv2;
            lv2 = c2 * in - c4 * out;
        }

        JUCE_SNAP_TO_ZERO (lv1);  v1 = lv1;
        JUCE_SNAP_TO_ZERO (lv2);  v2 = lv2;
    }
}

} // namespace juce
