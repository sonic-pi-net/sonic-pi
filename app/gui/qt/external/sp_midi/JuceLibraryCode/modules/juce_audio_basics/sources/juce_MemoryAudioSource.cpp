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

MemoryAudioSource::MemoryAudioSource (AudioBuffer<float>& bufferToUse, bool copyMemory, bool shouldLoop)
    : isCurrentlyLooping (shouldLoop)
{
    if (copyMemory)
        buffer.makeCopyOf (bufferToUse);
    else
        buffer.setDataToReferTo (bufferToUse.getArrayOfWritePointers(),
                                 bufferToUse.getNumChannels(),
                                 bufferToUse.getNumSamples());
}

//==============================================================================
void MemoryAudioSource::prepareToPlay (int /*samplesPerBlockExpected*/, double /*sampleRate*/)
{
    position = 0;
}

void MemoryAudioSource::releaseResources()   {}

void MemoryAudioSource::getNextAudioBlock (const AudioSourceChannelInfo& bufferToFill)
{
    auto& dst = *bufferToFill.buffer;
    auto channels = jmin (dst.getNumChannels(), buffer.getNumChannels());
    auto max = 0, pos = 0;
    auto n = buffer.getNumSamples(), m = bufferToFill.numSamples;

    int i;
    for (i = position; (i < n || isCurrentlyLooping) && (pos < m); i += max)
    {
        max = jmin (m - pos, n - (i % n));

        int ch = 0;
        for (; ch < channels; ++ch)
            dst.copyFrom (ch, bufferToFill.startSample + pos, buffer, ch, i % n, max);

        for (; ch < dst.getNumChannels(); ++ch)
            dst.clear (ch, bufferToFill.startSample + pos, max);

        pos += max;
    }

    if (pos < m)
        dst.clear (bufferToFill.startSample + pos, m - pos);

    position = (i % n);
}

//==============================================================================
void MemoryAudioSource::setNextReadPosition (int64 newPosition)
{
    position = (int) newPosition;
}

int64 MemoryAudioSource::getNextReadPosition() const
{
    return position;
}

int64 MemoryAudioSource::getTotalLength() const
{
    return buffer.getNumSamples();
}

//==============================================================================
bool MemoryAudioSource::isLooping() const
{
    return isCurrentlyLooping;
}

void MemoryAudioSource::setLooping (bool shouldLoop)
{
    isCurrentlyLooping = shouldLoop;
}

} // namespace juce
