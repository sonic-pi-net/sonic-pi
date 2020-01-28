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

#if JUCE_QUICKTIME

//==============================================================================
/**
    Uses QuickTime to read the audio track a movie or media file.

    As well as QuickTime movies, this should also manage to open other audio
    files that quicktime can understand, like mp3, m4a, etc.

    @see AudioFormat
*/
class JUCE_API  QuickTimeAudioFormat  : public AudioFormat
{
public:
    //==============================================================================
    /** Creates a format object. */
    QuickTimeAudioFormat();

    /** Destructor. */
    ~QuickTimeAudioFormat();

    //==============================================================================
    Array<int> getPossibleSampleRates();
    Array<int> getPossibleBitDepths();
    bool canDoStereo();
    bool canDoMono();

    //==============================================================================
    AudioFormatReader* createReaderFor (InputStream* sourceStream,
                                        bool deleteStreamIfOpeningFails);

    AudioFormatWriter* createWriterFor (OutputStream* streamToWriteTo,
                                        double sampleRateToUse,
                                        unsigned int numberOfChannels,
                                        int bitsPerSample,
                                        const StringPairArray& metadataValues,
                                        int qualityOptionIndex);


private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (QuickTimeAudioFormat)
};


#endif
