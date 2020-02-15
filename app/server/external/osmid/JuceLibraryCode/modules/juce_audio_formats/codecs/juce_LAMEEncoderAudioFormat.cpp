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

#if JUCE_USE_LAME_AUDIO_FORMAT

class LAMEEncoderAudioFormat::Writer   : public AudioFormatWriter
{
public:
    Writer (OutputStream* destStream, const String& formatName,
            const File& appFile, int vbr, int cbr,
            double sampleRate, unsigned int numberOfChannels,
            int bitsPerSample, const StringPairArray& metadata)
        : AudioFormatWriter (destStream, formatName, sampleRate,
                             numberOfChannels, (unsigned int) bitsPerSample),
          vbrLevel (vbr), cbrBitrate (cbr)
    {
        WavAudioFormat wavFormat;

        if (auto* out = tempWav.getFile().createOutputStream())
        {
            writer = wavFormat.createWriterFor (out, sampleRate, numChannels,
                                                bitsPerSample, metadata, 0);

            args.add (appFile.getFullPathName());

            args.add ("--quiet");

            if (cbrBitrate == 0)
            {
                args.add ("--vbr-new");
                args.add ("-V");
                args.add (String (vbrLevel));
            }
            else
            {
                args.add ("--cbr");
                args.add ("-b");
                args.add (String (cbrBitrate));
            }

            addMetadataArg (metadata, "id3title",       "--tt");
            addMetadataArg (metadata, "id3artist",      "--ta");
            addMetadataArg (metadata, "id3album",       "--tl");
            addMetadataArg (metadata, "id3comment",     "--tc");
            addMetadataArg (metadata, "id3date",        "--ty");
            addMetadataArg (metadata, "id3genre",       "--tg");
            addMetadataArg (metadata, "id3trackNumber", "--tn");
        }
    }

    void addMetadataArg (const StringPairArray& metadata, const char* key, const char* lameFlag)
    {
        auto value = metadata.getValue (key, {});

        if (value.isNotEmpty())
        {
            args.add (lameFlag);
            args.add (value);
        }
    }

    ~Writer()
    {
        if (writer != nullptr)
        {
            writer = nullptr;

            if (! convertToMP3())
                convertToMP3(); // try again
        }
    }

    bool write (const int** samplesToWrite, int numSamples)
    {
        return writer != nullptr && writer->write (samplesToWrite, numSamples);
    }

private:
    int vbrLevel, cbrBitrate;
    TemporaryFile tempWav { ".wav" };
    ScopedPointer<AudioFormatWriter> writer;
    StringArray args;

    bool runLameChildProcess (const TemporaryFile& tempMP3, const StringArray& processArgs) const
    {
        ChildProcess cp;

        if (cp.start (processArgs))
        {
            auto childOutput = cp.readAllProcessOutput();
            DBG (childOutput); ignoreUnused (childOutput);

            cp.waitForProcessToFinish (10000);
            return tempMP3.getFile().getSize() > 0;
        }

        return false;
    }

    bool convertToMP3() const
    {
        TemporaryFile tempMP3 (".mp3");

        StringArray args2 (args);
        args2.add (tempWav.getFile().getFullPathName());
        args2.add (tempMP3.getFile().getFullPathName());

        DBG (args2.joinIntoString (" "));

        if (runLameChildProcess (tempMP3, args2))
        {
            FileInputStream fis (tempMP3.getFile());

            if (fis.openedOk() && output->writeFromInputStream (fis, -1) > 0)
            {
                output->flush();
                return true;
            }
        }

        return false;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Writer)
};

//==============================================================================
LAMEEncoderAudioFormat::LAMEEncoderAudioFormat (const File& lameApplication)
    : AudioFormat ("MP3 file", ".mp3"),
      lameApp (lameApplication)
{
}

LAMEEncoderAudioFormat::~LAMEEncoderAudioFormat()
{
}

bool LAMEEncoderAudioFormat::canHandleFile (const File&)
{
    return false;
}

Array<int> LAMEEncoderAudioFormat::getPossibleSampleRates()
{
    return { 32000, 44100, 48000 };
}

Array<int> LAMEEncoderAudioFormat::getPossibleBitDepths()
{
    return { 16 };
}

bool LAMEEncoderAudioFormat::canDoStereo()      { return true; }
bool LAMEEncoderAudioFormat::canDoMono()        { return true; }
bool LAMEEncoderAudioFormat::isCompressed()     { return true; }

StringArray LAMEEncoderAudioFormat::getQualityOptions()
{
    static const char* vbrOptions[] = { "VBR quality 0 (best)", "VBR quality 1", "VBR quality 2", "VBR quality 3",
                                        "VBR quality 4 (normal)", "VBR quality 5", "VBR quality 6", "VBR quality 7",
                                        "VBR quality 8", "VBR quality 9 (smallest)", nullptr };
    StringArray opts (vbrOptions);

    const int cbrRates[] = { 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320 };

    for (int i = 0; i < numElementsInArray (cbrRates); ++i)
        opts.add (String (cbrRates[i]) + " Kb/s CBR");

    return opts;
}

AudioFormatReader* LAMEEncoderAudioFormat::createReaderFor (InputStream*, const bool)
{
    return nullptr;
}

AudioFormatWriter* LAMEEncoderAudioFormat::createWriterFor (OutputStream* streamToWriteTo,
                                                            double sampleRateToUse,
                                                            unsigned int numberOfChannels,
                                                            int bitsPerSample,
                                                            const StringPairArray& metadataValues,
                                                            int qualityOptionIndex)
{
    if (streamToWriteTo == nullptr)
        return nullptr;

    int vbr = 4;
    int cbr = 0;

    const String qual (getQualityOptions() [qualityOptionIndex]);

    if (qual.contains ("VBR"))
        vbr = qual.retainCharacters ("0123456789").getIntValue();
    else
        cbr = qual.getIntValue();

    return new Writer (streamToWriteTo, getFormatName(), lameApp, vbr, cbr,
                       sampleRateToUse, numberOfChannels, bitsPerSample, metadataValues);
}

#endif

} // namespace juce
