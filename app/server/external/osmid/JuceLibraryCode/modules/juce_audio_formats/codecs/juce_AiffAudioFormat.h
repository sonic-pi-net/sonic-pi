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

//==============================================================================
/**
    Reads and Writes AIFF format audio files.

    @see AudioFormat
*/
class JUCE_API  AiffAudioFormat  : public AudioFormat
{
public:
    //==============================================================================
    /** Creates an format object. */
    AiffAudioFormat();

    /** Destructor. */
    ~AiffAudioFormat();

    //==============================================================================
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleOneShot;
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleRootSet;
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleRootNote;
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleBeats;
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleDenominator;
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleNumerator;
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleTag;
    /** Metadata property name used when reading a aiff file with a basc chunk. */
    static const char* const appleKey;

    //==============================================================================
    Array<int> getPossibleSampleRates() override;
    Array<int> getPossibleBitDepths() override;
    bool canDoStereo() override;
    bool canDoMono() override;

   #if JUCE_MAC
    bool canHandleFile (const File& fileToTest) override;
   #endif

    //==============================================================================
    AudioFormatReader* createReaderFor (InputStream* sourceStream,
                                        bool deleteStreamIfOpeningFails) override;

    MemoryMappedAudioFormatReader* createMemoryMappedReader (const File&)      override;
    MemoryMappedAudioFormatReader* createMemoryMappedReader (FileInputStream*) override;

    AudioFormatWriter* createWriterFor (OutputStream* streamToWriteTo,
                                        double sampleRateToUse,
                                        unsigned int numberOfChannels,
                                        int bitsPerSample,
                                        const StringPairArray& metadataValues,
                                        int qualityOptionIndex) override;

private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(AiffAudioFormat)
};

} // namespace juce
