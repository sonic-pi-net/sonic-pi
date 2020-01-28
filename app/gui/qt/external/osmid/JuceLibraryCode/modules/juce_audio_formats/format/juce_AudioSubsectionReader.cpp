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

AudioSubsectionReader::AudioSubsectionReader (AudioFormatReader* const source_,
                                              const int64 startSample_,
                                              const int64 length_,
                                              const bool deleteSourceWhenDeleted_)
   : AudioFormatReader (0, source_->getFormatName()),
     source (source_),
     startSample (startSample_),
     deleteSourceWhenDeleted (deleteSourceWhenDeleted_)
{
    length = jmin (jmax ((int64) 0, source->lengthInSamples - startSample), length_);

    sampleRate = source->sampleRate;
    bitsPerSample = source->bitsPerSample;
    lengthInSamples = length;
    numChannels = source->numChannels;
    usesFloatingPointData = source->usesFloatingPointData;
}

AudioSubsectionReader::~AudioSubsectionReader()
{
    if (deleteSourceWhenDeleted)
        delete source;
}

//==============================================================================
bool AudioSubsectionReader::readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                                         int64 startSampleInFile, int numSamples)
{
    clearSamplesBeyondAvailableLength (destSamples, numDestChannels, startOffsetInDestBuffer,
                                       startSampleInFile, numSamples, length);

    return source->readSamples (destSamples, numDestChannels, startOffsetInDestBuffer,
                                startSampleInFile + startSample, numSamples);
}

void AudioSubsectionReader::readMaxLevels (int64 startSampleInFile, int64 numSamples, Range<float>* results, int numChannelsToRead)
{
    startSampleInFile = jmax ((int64) 0, startSampleInFile);
    numSamples = jmax ((int64) 0, jmin (numSamples, length - startSampleInFile));

    source->readMaxLevels (startSampleInFile + startSample, numSamples, results, numChannelsToRead);
}

} // namespace juce
