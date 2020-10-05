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

IIRFilterAudioSource::IIRFilterAudioSource (AudioSource* const inputSource,
                                            const bool deleteInputWhenDeleted)
    : input (inputSource, deleteInputWhenDeleted)
{
    jassert (inputSource != nullptr);

    for (int i = 2; --i >= 0;)
        iirFilters.add (new IIRFilter());
}

IIRFilterAudioSource::~IIRFilterAudioSource()  {}

//==============================================================================
void IIRFilterAudioSource::setCoefficients (const IIRCoefficients& newCoefficients)
{
    for (int i = iirFilters.size(); --i >= 0;)
        iirFilters.getUnchecked(i)->setCoefficients (newCoefficients);
}

void IIRFilterAudioSource::makeInactive()
{
    for (int i = iirFilters.size(); --i >= 0;)
        iirFilters.getUnchecked(i)->makeInactive();
}

//==============================================================================
void IIRFilterAudioSource::prepareToPlay (int samplesPerBlockExpected, double sampleRate)
{
    input->prepareToPlay (samplesPerBlockExpected, sampleRate);

    for (int i = iirFilters.size(); --i >= 0;)
        iirFilters.getUnchecked(i)->reset();
}

void IIRFilterAudioSource::releaseResources()
{
    input->releaseResources();
}

void IIRFilterAudioSource::getNextAudioBlock (const AudioSourceChannelInfo& bufferToFill)
{
    input->getNextAudioBlock (bufferToFill);

    const int numChannels = bufferToFill.buffer->getNumChannels();

    while (numChannels > iirFilters.size())
        iirFilters.add (new IIRFilter (*iirFilters.getUnchecked (0)));

    for (int i = 0; i < numChannels; ++i)
        iirFilters.getUnchecked(i)
            ->processSamples (bufferToFill.buffer->getWritePointer (i, bufferToFill.startSample),
                              bufferToFill.numSamples);
}

} // namespace juce
