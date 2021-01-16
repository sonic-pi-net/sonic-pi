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

ChannelRemappingAudioSource::ChannelRemappingAudioSource (AudioSource* const source_,
                                                          const bool deleteSourceWhenDeleted)
   : source (source_, deleteSourceWhenDeleted),
     requiredNumberOfChannels (2)
{
    remappedInfo.buffer = &buffer;
    remappedInfo.startSample = 0;
}

ChannelRemappingAudioSource::~ChannelRemappingAudioSource() {}

//==============================================================================
void ChannelRemappingAudioSource::setNumberOfChannelsToProduce (const int requiredNumberOfChannels_)
{
    const ScopedLock sl (lock);
    requiredNumberOfChannels = requiredNumberOfChannels_;
}

void ChannelRemappingAudioSource::clearAllMappings()
{
    const ScopedLock sl (lock);

    remappedInputs.clear();
    remappedOutputs.clear();
}

void ChannelRemappingAudioSource::setInputChannelMapping (const int destIndex, const int sourceIndex)
{
    const ScopedLock sl (lock);

    while (remappedInputs.size() < destIndex)
        remappedInputs.add (-1);

    remappedInputs.set (destIndex, sourceIndex);
}

void ChannelRemappingAudioSource::setOutputChannelMapping (const int sourceIndex, const int destIndex)
{
    const ScopedLock sl (lock);

    while (remappedOutputs.size() < sourceIndex)
        remappedOutputs.add (-1);

    remappedOutputs.set (sourceIndex, destIndex);
}

int ChannelRemappingAudioSource::getRemappedInputChannel (const int inputChannelIndex) const
{
    const ScopedLock sl (lock);

    if (inputChannelIndex >= 0 && inputChannelIndex < remappedInputs.size())
        return remappedInputs.getUnchecked (inputChannelIndex);

    return -1;
}

int ChannelRemappingAudioSource::getRemappedOutputChannel (const int outputChannelIndex) const
{
    const ScopedLock sl (lock);

    if (outputChannelIndex >= 0 && outputChannelIndex < remappedOutputs.size())
        return remappedOutputs .getUnchecked (outputChannelIndex);

    return -1;
}

//==============================================================================
void ChannelRemappingAudioSource::prepareToPlay (int samplesPerBlockExpected, double sampleRate)
{
    source->prepareToPlay (samplesPerBlockExpected, sampleRate);
}

void ChannelRemappingAudioSource::releaseResources()
{
    source->releaseResources();
}

void ChannelRemappingAudioSource::getNextAudioBlock (const AudioSourceChannelInfo& bufferToFill)
{
    const ScopedLock sl (lock);

    buffer.setSize (requiredNumberOfChannels, bufferToFill.numSamples, false, false, true);

    const int numChans = bufferToFill.buffer->getNumChannels();

    for (int i = 0; i < buffer.getNumChannels(); ++i)
    {
        const int remappedChan = getRemappedInputChannel (i);

        if (remappedChan >= 0 && remappedChan < numChans)
        {
            buffer.copyFrom (i, 0, *bufferToFill.buffer,
                             remappedChan,
                             bufferToFill.startSample,
                             bufferToFill.numSamples);
        }
        else
        {
            buffer.clear (i, 0, bufferToFill.numSamples);
        }
    }

    remappedInfo.numSamples = bufferToFill.numSamples;

    source->getNextAudioBlock (remappedInfo);

    bufferToFill.clearActiveBufferRegion();

    for (int i = 0; i < requiredNumberOfChannels; ++i)
    {
        const int remappedChan = getRemappedOutputChannel (i);

        if (remappedChan >= 0 && remappedChan < numChans)
        {
            bufferToFill.buffer->addFrom (remappedChan, bufferToFill.startSample,
                                          buffer, i, 0, bufferToFill.numSamples);

        }
    }
}

//==============================================================================
std::unique_ptr<XmlElement> ChannelRemappingAudioSource::createXml() const
{
    auto e = std::make_unique<XmlElement> ("MAPPINGS");
    String ins, outs;

    const ScopedLock sl (lock);

    for (int i = 0; i < remappedInputs.size(); ++i)
        ins << remappedInputs.getUnchecked(i) << ' ';

    for (int i = 0; i < remappedOutputs.size(); ++i)
        outs << remappedOutputs.getUnchecked(i) << ' ';

    e->setAttribute ("inputs", ins.trimEnd());
    e->setAttribute ("outputs", outs.trimEnd());

    return e;
}

void ChannelRemappingAudioSource::restoreFromXml (const XmlElement& e)
{
    if (e.hasTagName ("MAPPINGS"))
    {
        const ScopedLock sl (lock);

        clearAllMappings();

        StringArray ins, outs;
        ins.addTokens (e.getStringAttribute ("inputs"), false);
        outs.addTokens (e.getStringAttribute ("outputs"), false);

        for (int i = 0; i < ins.size(); ++i)
            remappedInputs.add (ins[i].getIntValue());

        for (int i = 0; i < outs.size(); ++i)
            remappedOutputs.add (outs[i].getIntValue());
    }
}

} // namespace juce
