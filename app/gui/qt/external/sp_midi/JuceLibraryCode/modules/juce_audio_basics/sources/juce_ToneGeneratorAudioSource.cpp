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

ToneGeneratorAudioSource::ToneGeneratorAudioSource()
    : frequency (1000.0),
      sampleRate (44100.0),
      currentPhase (0.0),
      phasePerSample (0.0),
      amplitude (0.5f)
{
}

ToneGeneratorAudioSource::~ToneGeneratorAudioSource()
{
}

//==============================================================================
void ToneGeneratorAudioSource::setAmplitude (const float newAmplitude)
{
    amplitude = newAmplitude;
}

void ToneGeneratorAudioSource::setFrequency (const double newFrequencyHz)
{
    frequency = newFrequencyHz;
    phasePerSample = 0.0;
}

//==============================================================================
void ToneGeneratorAudioSource::prepareToPlay (int /*samplesPerBlockExpected*/, double rate)
{
    currentPhase = 0.0;
    phasePerSample = 0.0;
    sampleRate = rate;
}

void ToneGeneratorAudioSource::releaseResources()
{
}

void ToneGeneratorAudioSource::getNextAudioBlock (const AudioSourceChannelInfo& info)
{
    if (phasePerSample == 0.0)
        phasePerSample = MathConstants<double>::twoPi / (sampleRate / frequency);

    for (int i = 0; i < info.numSamples; ++i)
    {
        const float sample = amplitude * (float) std::sin (currentPhase);
        currentPhase += phasePerSample;

        for (int j = info.buffer->getNumChannels(); --j >= 0;)
            info.buffer->setSample (j, info.startSample + i, sample);
    }
}

} // namespace juce
