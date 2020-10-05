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

BufferingAudioSource::BufferingAudioSource (PositionableAudioSource* s,
                                            TimeSliceThread& thread,
                                            bool deleteSourceWhenDeleted,
                                            int bufferSizeSamples,
                                            int numChannels,
                                            bool prefillBufferOnPrepareToPlay)
    : source (s, deleteSourceWhenDeleted),
      backgroundThread (thread),
      numberOfSamplesToBuffer (jmax (1024, bufferSizeSamples)),
      numberOfChannels (numChannels),
      prefillBuffer (prefillBufferOnPrepareToPlay)
{
    jassert (source != nullptr);

    jassert (numberOfSamplesToBuffer > 1024); // not much point using this class if you're
                                              //  not using a larger buffer..
}

BufferingAudioSource::~BufferingAudioSource()
{
    releaseResources();
}

//==============================================================================
void BufferingAudioSource::prepareToPlay (int samplesPerBlockExpected, double newSampleRate)
{
    auto bufferSizeNeeded = jmax (samplesPerBlockExpected * 2, numberOfSamplesToBuffer);

    if (newSampleRate != sampleRate
         || bufferSizeNeeded != buffer.getNumSamples()
         || ! isPrepared)
    {
        backgroundThread.removeTimeSliceClient (this);

        isPrepared = true;
        sampleRate = newSampleRate;

        source->prepareToPlay (samplesPerBlockExpected, newSampleRate);

        buffer.setSize (numberOfChannels, bufferSizeNeeded);
        buffer.clear();

        bufferValidStart = 0;
        bufferValidEnd = 0;

        backgroundThread.addTimeSliceClient (this);

        do
        {
            backgroundThread.moveToFrontOfQueue (this);
            Thread::sleep (5);
        }
        while (prefillBuffer
         && (bufferValidEnd - bufferValidStart < jmin (((int) newSampleRate) / 4, buffer.getNumSamples() / 2)));
    }
}

void BufferingAudioSource::releaseResources()
{
    isPrepared = false;
    backgroundThread.removeTimeSliceClient (this);

    buffer.setSize (numberOfChannels, 0);

    // MSVC2015 seems to need this if statement to not generate a warning during linking.
    // As source is set in the constructor, there is no way that source could
    // ever equal this, but it seems to make MSVC2015 happy.
    if (source != this)
        source->releaseResources();
}

void BufferingAudioSource::getNextAudioBlock (const AudioSourceChannelInfo& info)
{
    const ScopedLock sl (bufferStartPosLock);

    auto start = bufferValidStart.load();
    auto end   = bufferValidEnd.load();
    auto pos   = nextPlayPos.load();

    auto validStart = (int) (jlimit (start, end, pos) - pos);
    auto validEnd   = (int) (jlimit (start, end, pos + info.numSamples) - pos);

    if (validStart == validEnd)
    {
        // total cache miss
        info.clearActiveBufferRegion();
    }
    else
    {
        if (validStart > 0)
            info.buffer->clear (info.startSample, validStart);  // partial cache miss at start

        if (validEnd < info.numSamples)
            info.buffer->clear (info.startSample + validEnd,
                                info.numSamples - validEnd);    // partial cache miss at end

        if (validStart < validEnd)
        {
            for (int chan = jmin (numberOfChannels, info.buffer->getNumChannels()); --chan >= 0;)
            {
                jassert (buffer.getNumSamples() > 0);
                auto startBufferIndex = (int) ((validStart + nextPlayPos) % buffer.getNumSamples());
                auto endBufferIndex   = (int) ((validEnd + nextPlayPos)   % buffer.getNumSamples());

                if (startBufferIndex < endBufferIndex)
                {
                    info.buffer->copyFrom (chan, info.startSample + validStart,
                                           buffer,
                                           chan, startBufferIndex,
                                           validEnd - validStart);
                }
                else
                {
                    auto initialSize = buffer.getNumSamples() - startBufferIndex;

                    info.buffer->copyFrom (chan, info.startSample + validStart,
                                           buffer,
                                           chan, startBufferIndex,
                                           initialSize);

                    info.buffer->copyFrom (chan, info.startSample + validStart + initialSize,
                                           buffer,
                                           chan, 0,
                                           (validEnd - validStart) - initialSize);
                }
            }
        }

        nextPlayPos += info.numSamples;
    }
}

bool BufferingAudioSource::waitForNextAudioBlockReady (const AudioSourceChannelInfo& info, uint32 timeout)
{
    if (!source || source->getTotalLength() <= 0)
        return false;

    if (nextPlayPos + info.numSamples < 0)
        return true;

    if (! isLooping() && nextPlayPos > getTotalLength())
        return true;

    auto now = Time::getMillisecondCounter();
    auto startTime = now;

    auto elapsed = (now >= startTime ? now - startTime
                                     : (std::numeric_limits<uint32>::max() - startTime) + now);

    while (elapsed <= timeout)
    {
        {
            const ScopedLock sl (bufferStartPosLock);

            auto start = bufferValidStart.load();
            auto end   = bufferValidEnd.load();
            auto pos   = nextPlayPos.load();

            auto validStart = static_cast<int> (jlimit (start, end, pos) - pos);
            auto validEnd   = static_cast<int> (jlimit (start, end, pos + info.numSamples) - pos);

            if (validStart <= 0 && validStart < validEnd && validEnd >= info.numSamples)
                return true;
        }

        if (elapsed < timeout  && (! bufferReadyEvent.wait (static_cast<int> (timeout - elapsed))))
            return false;

        now = Time::getMillisecondCounter();
        elapsed = (now >= startTime ? now - startTime
                                    : (std::numeric_limits<uint32>::max() - startTime) + now);
    }

    return false;
}

int64 BufferingAudioSource::getNextReadPosition() const
{
    jassert (source->getTotalLength() > 0);
    auto pos = nextPlayPos.load();

    return (source->isLooping() && nextPlayPos > 0)
                    ? pos % source->getTotalLength()
                    : pos;
}

void BufferingAudioSource::setNextReadPosition (int64 newPosition)
{
    const ScopedLock sl (bufferStartPosLock);

    nextPlayPos = newPosition;
    backgroundThread.moveToFrontOfQueue (this);
}

bool BufferingAudioSource::readNextBufferChunk()
{
    int64 newBVS, newBVE, sectionToReadStart, sectionToReadEnd;

    {
        const ScopedLock sl (bufferStartPosLock);

        if (wasSourceLooping != isLooping())
        {
            wasSourceLooping = isLooping();
            bufferValidStart = 0;
            bufferValidEnd = 0;
        }

        newBVS = jmax ((int64) 0, nextPlayPos.load());
        newBVE = newBVS + buffer.getNumSamples() - 4;
        sectionToReadStart = 0;
        sectionToReadEnd = 0;

        const int maxChunkSize = 2048;

        if (newBVS < bufferValidStart || newBVS >= bufferValidEnd)
        {
            newBVE = jmin (newBVE, newBVS + maxChunkSize);

            sectionToReadStart = newBVS;
            sectionToReadEnd = newBVE;

            bufferValidStart = 0;
            bufferValidEnd = 0;
        }
        else if (std::abs ((int) (newBVS - bufferValidStart)) > 512
                  || std::abs ((int) (newBVE - bufferValidEnd)) > 512)
        {
            newBVE = jmin (newBVE, bufferValidEnd + maxChunkSize);

            sectionToReadStart = bufferValidEnd;
            sectionToReadEnd = newBVE;

            bufferValidStart = newBVS;
            bufferValidEnd = jmin (bufferValidEnd.load(), newBVE);
        }
    }

    if (sectionToReadStart == sectionToReadEnd)
        return false;

    jassert (buffer.getNumSamples() > 0);
    auto bufferIndexStart = (int) (sectionToReadStart % buffer.getNumSamples());
    auto bufferIndexEnd   = (int) (sectionToReadEnd   % buffer.getNumSamples());

    if (bufferIndexStart < bufferIndexEnd)
    {
        readBufferSection (sectionToReadStart,
                           (int) (sectionToReadEnd - sectionToReadStart),
                           bufferIndexStart);
    }
    else
    {
        auto initialSize = buffer.getNumSamples() - bufferIndexStart;

        readBufferSection (sectionToReadStart,
                           initialSize,
                           bufferIndexStart);

        readBufferSection (sectionToReadStart + initialSize,
                           (int) (sectionToReadEnd - sectionToReadStart) - initialSize,
                           0);
    }

    {
        const ScopedLock sl2 (bufferStartPosLock);

        bufferValidStart = newBVS;
        bufferValidEnd = newBVE;
    }

    bufferReadyEvent.signal();
    return true;
}

void BufferingAudioSource::readBufferSection (int64 start, int length, int bufferOffset)
{
    if (source->getNextReadPosition() != start)
        source->setNextReadPosition (start);

    AudioSourceChannelInfo info (&buffer, bufferOffset, length);
    source->getNextAudioBlock (info);
}

int BufferingAudioSource::useTimeSlice()
{
    return readNextBufferChunk() ? 1 : 100;
}

} // namespace juce
