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

MidiMessageCollector::MidiMessageCollector()
{
}

MidiMessageCollector::~MidiMessageCollector()
{
}

//==============================================================================
void MidiMessageCollector::reset (const double newSampleRate)
{
    const ScopedLock sl (midiCallbackLock);

    jassert (newSampleRate > 0);

   #if JUCE_DEBUG
    hasCalledReset = true;
   #endif
    sampleRate = newSampleRate;
    incomingMessages.clear();
    lastCallbackTime = Time::getMillisecondCounterHiRes();
}

void MidiMessageCollector::addMessageToQueue (const MidiMessage& message)
{
    const ScopedLock sl (midiCallbackLock);

   #if JUCE_DEBUG
    jassert (hasCalledReset); // you need to call reset() to set the correct sample rate before using this object
   #endif

    // the messages that come in here need to be time-stamped correctly - see MidiInput
    // for details of what the number should be.
    jassert (message.getTimeStamp() != 0);

    auto sampleNumber = (int) ((message.getTimeStamp() - 0.001 * lastCallbackTime) * sampleRate);

    incomingMessages.addEvent (message, sampleNumber);

    // if the messages don't get used for over a second, we'd better
    // get rid of any old ones to avoid the queue getting too big
    if (sampleNumber > sampleRate)
        incomingMessages.clear (0, sampleNumber - (int) sampleRate);
}

void MidiMessageCollector::removeNextBlockOfMessages (MidiBuffer& destBuffer,
                                                      const int numSamples)
{
    const ScopedLock sl (midiCallbackLock);

   #if JUCE_DEBUG
    jassert (hasCalledReset); // you need to call reset() to set the correct sample rate before using this object
   #endif

    jassert (numSamples > 0);

    auto timeNow = Time::getMillisecondCounterHiRes();
    auto msElapsed = timeNow - lastCallbackTime;

    lastCallbackTime = timeNow;

    if (! incomingMessages.isEmpty())
    {
        int numSourceSamples = jmax (1, roundToInt (msElapsed * 0.001 * sampleRate));
        int startSample = 0;
        int scale = 1 << 16;

        if (numSourceSamples > numSamples)
        {
            // if our list of events is longer than the buffer we're being
            // asked for, scale them down to squeeze them all in..
            const int maxBlockLengthToUse = numSamples << 5;

            auto iter = incomingMessages.cbegin();

            if (numSourceSamples > maxBlockLengthToUse)
            {
                startSample = numSourceSamples - maxBlockLengthToUse;
                numSourceSamples = maxBlockLengthToUse;
                iter = incomingMessages.findNextSamplePosition (startSample);
            }

            scale = (numSamples << 10) / numSourceSamples;

            std::for_each (iter, incomingMessages.cend(), [&] (const MidiMessageMetadata& meta)
            {
                const auto pos = ((meta.samplePosition - startSample) * scale) >> 10;
                destBuffer.addEvent (meta.data, meta.numBytes, jlimit (0, numSamples - 1, pos));
            });
        }
        else
        {
            // if our event list is shorter than the number we need, put them
            // towards the end of the buffer
            startSample = numSamples - numSourceSamples;

            for (const auto metadata : incomingMessages)
                destBuffer.addEvent (metadata.data, metadata.numBytes,
                                     jlimit (0, numSamples - 1, metadata.samplePosition + startSample));
        }

        incomingMessages.clear();
    }
}

void MidiMessageCollector::ensureStorageAllocated (size_t bytes)
{
    incomingMessages.ensureSize (bytes);
}

//==============================================================================
void MidiMessageCollector::handleNoteOn (MidiKeyboardState*, int midiChannel, int midiNoteNumber, float velocity)
{
    MidiMessage m (MidiMessage::noteOn (midiChannel, midiNoteNumber, velocity));
    m.setTimeStamp (Time::getMillisecondCounterHiRes() * 0.001);

    addMessageToQueue (m);
}

void MidiMessageCollector::handleNoteOff (MidiKeyboardState*, int midiChannel, int midiNoteNumber, float velocity)
{
    MidiMessage m (MidiMessage::noteOff (midiChannel, midiNoteNumber, velocity));
    m.setTimeStamp (Time::getMillisecondCounterHiRes() * 0.001);

    addMessageToQueue (m);
}

void MidiMessageCollector::handleIncomingMidiMessage (MidiInput*, const MidiMessage& message)
{
    addMessageToQueue (message);
}

} // namespace juce
