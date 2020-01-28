/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

AudioFormatReaderSource::AudioFormatReaderSource (AudioFormatReader* const r,
                                                  const bool deleteReaderWhenThisIsDeleted)
    : reader (r, deleteReaderWhenThisIsDeleted),
      nextPlayPos (0),
      looping (false)
{
    jassert (reader != nullptr);
}

AudioFormatReaderSource::~AudioFormatReaderSource() {}

int64 AudioFormatReaderSource::getTotalLength() const                   { return reader->lengthInSamples; }
void AudioFormatReaderSource::setNextReadPosition (int64 newPosition)   { nextPlayPos = newPosition; }
void AudioFormatReaderSource::setLooping (bool shouldLoop)              { looping = shouldLoop; }

int64 AudioFormatReaderSource::getNextReadPosition() const
{
    return looping ? nextPlayPos % reader->lengthInSamples
                   : nextPlayPos;
}

void AudioFormatReaderSource::prepareToPlay (int /*samplesPerBlockExpected*/, double /*sampleRate*/) {}
void AudioFormatReaderSource::releaseResources() {}

void AudioFormatReaderSource::getNextAudioBlock (const AudioSourceChannelInfo& info)
{
    if (info.numSamples > 0)
    {
        const int64 start = nextPlayPos;

        if (looping)
        {
            const int64 newStart = start % reader->lengthInSamples;
            const int64 newEnd = (start + info.numSamples) % reader->lengthInSamples;

            if (newEnd > newStart)
            {
                reader->read (info.buffer, info.startSample,
                              (int) (newEnd - newStart), newStart, true, true);
            }
            else
            {
                const int endSamps = (int) (reader->lengthInSamples - newStart);

                reader->read (info.buffer, info.startSample,
                              endSamps, newStart, true, true);

                reader->read (info.buffer, info.startSample + endSamps,
                              (int) newEnd, 0, true, true);
            }

            nextPlayPos = newEnd;
        }
        else
        {
            reader->read (info.buffer, info.startSample,
                          info.numSamples, start, true, true);
            nextPlayPos += info.numSamples;
        }
    }
}

} // namespace juce
