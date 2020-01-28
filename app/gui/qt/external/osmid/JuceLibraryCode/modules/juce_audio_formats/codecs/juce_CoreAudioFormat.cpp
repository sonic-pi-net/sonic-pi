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

#if JUCE_MAC || JUCE_IOS

#include "../../juce_audio_basics/native/juce_mac_CoreAudioLayouts.h"

namespace juce
{

//==============================================================================
namespace
{
    const char* const coreAudioFormatName = "CoreAudio supported file";

    StringArray findFileExtensionsForCoreAudioCodecs()
    {
        StringArray extensionsArray;
        CFArrayRef extensions = nullptr;
        UInt32 sizeOfArray = sizeof (extensions);

        if (AudioFileGetGlobalInfo (kAudioFileGlobalInfo_AllExtensions, 0, 0, &sizeOfArray, &extensions) == noErr)
        {
            const CFIndex numValues = CFArrayGetCount (extensions);

            for (CFIndex i = 0; i < numValues; ++i)
                extensionsArray.add ("." + String::fromCFString ((CFStringRef) CFArrayGetValueAtIndex (extensions, i)));

            CFRelease (extensions);
        }

        return extensionsArray;
    }
}

//==============================================================================
const char* const CoreAudioFormat::midiDataBase64   = "midiDataBase64";
const char* const CoreAudioFormat::tempo            = "tempo";
const char* const CoreAudioFormat::timeSig          = "time signature";
const char* const CoreAudioFormat::keySig           = "key signature";

//==============================================================================
struct CoreAudioFormatMetatdata
{
    static uint32 chunkName (const char* const name) noexcept   { return ByteOrder::bigEndianInt (name); }

    //==============================================================================
    struct FileHeader
    {
        FileHeader (InputStream& input)
        {
            fileType    = (uint32) input.readIntBigEndian();
            fileVersion = (uint16) input.readShortBigEndian();
            fileFlags   = (uint16) input.readShortBigEndian();
        }

        uint32 fileType;
        uint16 fileVersion;
        uint16 fileFlags;
    };

    //==============================================================================
    struct ChunkHeader
    {
        ChunkHeader (InputStream& input)
        {
            chunkType = (uint32) input.readIntBigEndian();
            chunkSize = (int64)  input.readInt64BigEndian();
        }

        uint32 chunkType;
        int64 chunkSize;
    };

    //==============================================================================
    struct AudioDescriptionChunk
    {
        AudioDescriptionChunk (InputStream& input)
        {
            sampleRate          = input.readDoubleBigEndian();
            formatID            = (uint32) input.readIntBigEndian();
            formatFlags         = (uint32) input.readIntBigEndian();
            bytesPerPacket      = (uint32) input.readIntBigEndian();
            framesPerPacket     = (uint32) input.readIntBigEndian();
            channelsPerFrame    = (uint32) input.readIntBigEndian();
            bitsPerChannel      = (uint32) input.readIntBigEndian();
        }

        double sampleRate;
        uint32 formatID;
        uint32 formatFlags;
        uint32 bytesPerPacket;
        uint32 framesPerPacket;
        uint32 channelsPerFrame;
        uint32 bitsPerChannel;
    };

    //==============================================================================
    static StringPairArray parseUserDefinedChunk (InputStream& input, int64 size)
    {
        StringPairArray infoStrings;
        const int64 originalPosition = input.getPosition();

        uint8 uuid[16];
        input.read (uuid, sizeof (uuid));

        if (memcmp (uuid, "\x29\x81\x92\x73\xB5\xBF\x4A\xEF\xB7\x8D\x62\xD1\xEF\x90\xBB\x2C", 16) == 0)
        {
            const uint32 numEntries = (uint32) input.readIntBigEndian();

            for (uint32 i = 0; i < numEntries && input.getPosition() < originalPosition + size; ++i)
            {
                String keyName = input.readString();
                infoStrings.set (keyName, input.readString());
            }
        }

        input.setPosition (originalPosition + size);
        return infoStrings;
    }

    //==============================================================================
    static StringPairArray parseMidiChunk (InputStream& input, int64 size)
    {
        auto originalPosition = input.getPosition();

        MemoryBlock midiBlock;
        input.readIntoMemoryBlock (midiBlock, (ssize_t) size);
        MemoryInputStream midiInputStream (midiBlock, false);

        StringPairArray midiMetadata;
        MidiFile midiFile;

        if (midiFile.readFrom (midiInputStream))
        {
            midiMetadata.set (CoreAudioFormat::midiDataBase64, midiBlock.toBase64Encoding());

            findTempoEvents (midiFile, midiMetadata);
            findTimeSigEvents (midiFile, midiMetadata);
            findKeySigEvents (midiFile, midiMetadata);
        }

        input.setPosition (originalPosition + size);
        return midiMetadata;
    }

    static void findTempoEvents (MidiFile& midiFile, StringPairArray& midiMetadata)
    {
        MidiMessageSequence tempoEvents;
        midiFile.findAllTempoEvents (tempoEvents);

        auto numTempoEvents = tempoEvents.getNumEvents();
        MemoryOutputStream tempoSequence;

        for (int i = 0; i < numTempoEvents; ++i)
        {
            auto tempo = getTempoFromTempoMetaEvent (tempoEvents.getEventPointer (i));

            if (tempo > 0.0)
            {
                if (i == 0)
                    midiMetadata.set (CoreAudioFormat::tempo, String (tempo));

                if (numTempoEvents > 1)
                    tempoSequence << String (tempo) << ',' << tempoEvents.getEventTime (i) << ';';
            }
        }

        if (tempoSequence.getDataSize() > 0)
            midiMetadata.set ("tempo sequence", tempoSequence.toUTF8());
    }

    static double getTempoFromTempoMetaEvent (MidiMessageSequence::MidiEventHolder* holder)
    {
        if (holder != nullptr)
        {
            auto& midiMessage = holder->message;

            if (midiMessage.isTempoMetaEvent())
            {
                auto tempoSecondsPerQuarterNote = midiMessage.getTempoSecondsPerQuarterNote();

                if (tempoSecondsPerQuarterNote > 0.0)
                    return 60.0 / tempoSecondsPerQuarterNote;
            }
        }

        return 0.0;
    }

    static void findTimeSigEvents (MidiFile& midiFile, StringPairArray& midiMetadata)
    {
        MidiMessageSequence timeSigEvents;
        midiFile.findAllTimeSigEvents (timeSigEvents);
        auto numTimeSigEvents = timeSigEvents.getNumEvents();

        MemoryOutputStream timeSigSequence;

        for (int i = 0; i < numTimeSigEvents; ++i)
        {
            int numerator, denominator;
            timeSigEvents.getEventPointer(i)->message.getTimeSignatureInfo (numerator, denominator);

            String timeSigString;
            timeSigString << numerator << '/' << denominator;

            if (i == 0)
                midiMetadata.set (CoreAudioFormat::timeSig, timeSigString);

            if (numTimeSigEvents > 1)
                timeSigSequence << timeSigString << ',' << timeSigEvents.getEventTime (i) << ';';
        }

        if (timeSigSequence.getDataSize() > 0)
            midiMetadata.set ("time signature sequence", timeSigSequence.toUTF8());
    }

    static void findKeySigEvents (MidiFile& midiFile, StringPairArray& midiMetadata)
    {
        MidiMessageSequence keySigEvents;
        midiFile.findAllKeySigEvents (keySigEvents);
        auto numKeySigEvents = keySigEvents.getNumEvents();

        MemoryOutputStream keySigSequence;

        for (int i = 0; i < numKeySigEvents; ++i)
        {
            auto& message (keySigEvents.getEventPointer (i)->message);
            auto key = jlimit (0, 14, message.getKeySignatureNumberOfSharpsOrFlats() + 7);
            bool isMajor = message.isKeySignatureMajorKey();

            static const char* majorKeys[] = { "Cb", "Gb", "Db", "Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B", "F#", "C#" };
            static const char* minorKeys[] = { "Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B", "F#", "C#", "G#", "D#", "A#" };

            String keySigString (isMajor ? majorKeys[key]
                                         : minorKeys[key]);

            if (! isMajor)
                keySigString << 'm';

            if (i == 0)
                midiMetadata.set (CoreAudioFormat::keySig, keySigString);

            if (numKeySigEvents > 1)
                keySigSequence << keySigString << ',' << keySigEvents.getEventTime (i) << ';';
        }

        if (keySigSequence.getDataSize() > 0)
            midiMetadata.set ("key signature sequence", keySigSequence.toUTF8());
    }

    //==============================================================================
    static StringPairArray parseInformationChunk (InputStream& input)
    {
        StringPairArray infoStrings;
        auto numEntries = (uint32) input.readIntBigEndian();

        for (uint32 i = 0; i < numEntries; ++i)
            infoStrings.set (input.readString(), input.readString());

        return infoStrings;
    }

    //==============================================================================
    static bool read (InputStream& input, StringPairArray& metadataValues)
    {
        auto originalPos = input.getPosition();

        const FileHeader cafFileHeader (input);
        const bool isCafFile = cafFileHeader.fileType == chunkName ("caff");

        if (isCafFile)
        {
            while (! input.isExhausted())
            {
                const ChunkHeader chunkHeader (input);

                if (chunkHeader.chunkType == chunkName ("desc"))
                {
                    AudioDescriptionChunk audioDescriptionChunk (input);
                }
                else if (chunkHeader.chunkType == chunkName ("uuid"))
                {
                    metadataValues.addArray (parseUserDefinedChunk (input, chunkHeader.chunkSize));
                }
                else if (chunkHeader.chunkType == chunkName ("data"))
                {
                    // -1 signifies an unknown data size so the data has to be at the
                    // end of the file so we must have finished the header

                    if (chunkHeader.chunkSize == -1)
                        break;

                    input.skipNextBytes (chunkHeader.chunkSize);
                }
                else if (chunkHeader.chunkType == chunkName ("midi"))
                {
                    metadataValues.addArray (parseMidiChunk (input, chunkHeader.chunkSize));
                }
                else if (chunkHeader.chunkType == chunkName ("info"))
                {
                    metadataValues.addArray (parseInformationChunk (input));
                }
                else
                {
                    // we aren't decoding this chunk yet so just skip over it
                    input.skipNextBytes (chunkHeader.chunkSize);
                }
            }
        }

        input.setPosition (originalPos);

        return isCafFile;
    }
};

//==============================================================================
class CoreAudioReader : public AudioFormatReader
{
public:
    CoreAudioReader (InputStream* inp)  : AudioFormatReader (inp, coreAudioFormatName)
    {
        usesFloatingPointData = true;
        bitsPerSample = 32;

        if (input != nullptr)
            CoreAudioFormatMetatdata::read (*input, metadataValues);

        OSStatus status = AudioFileOpenWithCallbacks (this,
                                                      &readCallback,
                                                      nullptr,  // write needs to be null to avoid permisisions errors
                                                      &getSizeCallback,
                                                      nullptr,  // setSize needs to be null to avoid permisisions errors
                                                      0,        // AudioFileTypeID inFileTypeHint
                                                      &audioFileID);
        if (status == noErr)
        {
            status = ExtAudioFileWrapAudioFileID (audioFileID, false, &audioFileRef);

            if (status == noErr)
            {
                AudioStreamBasicDescription sourceAudioFormat;
                UInt32 audioStreamBasicDescriptionSize = sizeof (AudioStreamBasicDescription);
                ExtAudioFileGetProperty (audioFileRef,
                                         kExtAudioFileProperty_FileDataFormat,
                                         &audioStreamBasicDescriptionSize,
                                         &sourceAudioFormat);

                numChannels = sourceAudioFormat.mChannelsPerFrame;
                sampleRate  = sourceAudioFormat.mSampleRate;

                UInt32 sizeOfLengthProperty = sizeof (int64);
                ExtAudioFileGetProperty (audioFileRef,
                                         kExtAudioFileProperty_FileLengthFrames,
                                         &sizeOfLengthProperty,
                                         &lengthInSamples);

                HeapBlock<AudioChannelLayout> caLayout;
                bool hasLayout = false;
                UInt32 sizeOfLayout = 0, isWritable = 0;

                status = AudioFileGetPropertyInfo (audioFileID, kAudioFilePropertyChannelLayout, &sizeOfLayout, &isWritable);

                if (status == noErr)
                {
                    caLayout.malloc (1, static_cast<size_t> (sizeOfLayout));

                    status = AudioFileGetProperty (audioFileID, kAudioFilePropertyChannelLayout,
                                                   &sizeOfLayout, caLayout.get());

                    if (status == noErr)
                    {
                        auto fileLayout = CoreAudioLayouts::fromCoreAudio (*caLayout.get());

                        if (fileLayout.size() == static_cast<int> (numChannels))
                        {
                            hasLayout = true;
                            channelSet = fileLayout;
                        }
                    }
                }

                destinationAudioFormat.mSampleRate       = sampleRate;
                destinationAudioFormat.mFormatID         = kAudioFormatLinearPCM;
                destinationAudioFormat.mFormatFlags      = kLinearPCMFormatFlagIsFloat | kLinearPCMFormatFlagIsNonInterleaved | kAudioFormatFlagsNativeEndian;
                destinationAudioFormat.mBitsPerChannel   = sizeof (float) * 8;
                destinationAudioFormat.mChannelsPerFrame = numChannels;
                destinationAudioFormat.mBytesPerFrame    = sizeof (float);
                destinationAudioFormat.mFramesPerPacket  = 1;
                destinationAudioFormat.mBytesPerPacket   = destinationAudioFormat.mFramesPerPacket * destinationAudioFormat.mBytesPerFrame;

                status = ExtAudioFileSetProperty (audioFileRef,
                                                  kExtAudioFileProperty_ClientDataFormat,
                                                  sizeof (AudioStreamBasicDescription),
                                                  &destinationAudioFormat);
                if (status == noErr)
                {
                    bufferList.malloc (1, sizeof (AudioBufferList) + numChannels * sizeof (::AudioBuffer));
                    bufferList->mNumberBuffers = numChannels;
                    channelMap.malloc (numChannels);

                    if (hasLayout && caLayout != nullptr)
                    {
                        auto caOrder = CoreAudioLayouts::getCoreAudioLayoutChannels (*caLayout);

                        for (int i = 0; i < static_cast<int> (numChannels); ++i)
                        {
                            auto idx = channelSet.getChannelIndexForType (caOrder.getReference (i));
                            jassert (isPositiveAndBelow (idx, static_cast<int> (numChannels)));

                            channelMap[i] = idx;
                        }
                    }
                    else
                    {
                        for (int i = 0; i < static_cast<int> (numChannels); ++i)
                            channelMap[i] = i;
                    }

                    ok = true;
                }
            }
        }
    }

    ~CoreAudioReader()
    {
        ExtAudioFileDispose (audioFileRef);
        AudioFileClose (audioFileID);
    }

    //==============================================================================
    bool readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                      int64 startSampleInFile, int numSamples) override
    {
        clearSamplesBeyondAvailableLength (destSamples, numDestChannels, startOffsetInDestBuffer,
                                           startSampleInFile, numSamples, lengthInSamples);

        if (numSamples <= 0)
            return true;

        if (lastReadPosition != startSampleInFile)
        {
            OSStatus status = ExtAudioFileSeek (audioFileRef, startSampleInFile);
            if (status != noErr)
                return false;

            lastReadPosition = startSampleInFile;
        }

        while (numSamples > 0)
        {
            auto numThisTime = jmin (8192, numSamples);
            auto numBytes = sizeof (float) * (size_t) numThisTime;

            audioDataBlock.ensureSize (numBytes * numChannels, false);
            auto* data = static_cast<float*> (audioDataBlock.getData());

            for (int j = (int) numChannels; --j >= 0;)
            {
                bufferList->mBuffers[j].mNumberChannels = 1;
                bufferList->mBuffers[j].mDataByteSize = (UInt32) numBytes;
                bufferList->mBuffers[j].mData = data;
                data += numThisTime;
            }

            auto numFramesToRead = (UInt32) numThisTime;
            auto status = ExtAudioFileRead (audioFileRef, &numFramesToRead, bufferList);

            if (status != noErr)
                return false;

            for (int i = numDestChannels; --i >= 0;)
            {
                auto* dest = destSamples[(i < (int) numChannels ? channelMap[i] : i)];

                if (dest != nullptr)
                {
                    if (i < (int) numChannels)
                        memcpy (dest + startOffsetInDestBuffer, bufferList->mBuffers[i].mData, numBytes);
                    else
                        zeromem (dest + startOffsetInDestBuffer, numBytes);
                }
            }

            startOffsetInDestBuffer += numThisTime;
            numSamples -= numThisTime;
            lastReadPosition += numThisTime;
        }

        return true;
    }

    AudioChannelSet getChannelLayout() override
    {
        if (channelSet.size() == static_cast<int> (numChannels))
            return channelSet;

        return AudioFormatReader::getChannelLayout();
    }

    bool ok = false;

private:
    AudioFileID audioFileID;
    ExtAudioFileRef audioFileRef;
    AudioChannelSet channelSet;
    AudioStreamBasicDescription destinationAudioFormat;
    MemoryBlock audioDataBlock;
    HeapBlock<AudioBufferList> bufferList;
    int64 lastReadPosition = 0;
    HeapBlock<int> channelMap;

    static SInt64 getSizeCallback (void* inClientData)
    {
        return static_cast<CoreAudioReader*> (inClientData)->input->getTotalLength();
    }

    static OSStatus readCallback (void* inClientData, SInt64 inPosition, UInt32 requestCount,
                                  void* buffer, UInt32* actualCount)
    {
        auto* reader = static_cast<CoreAudioReader*> (inClientData);
        reader->input->setPosition (inPosition);
        *actualCount = (UInt32) reader->input->read (buffer, (int) requestCount);
        return noErr;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CoreAudioReader)
};

//==============================================================================
CoreAudioFormat::CoreAudioFormat()
    : AudioFormat (coreAudioFormatName, findFileExtensionsForCoreAudioCodecs())
{
}

CoreAudioFormat::~CoreAudioFormat() {}

Array<int> CoreAudioFormat::getPossibleSampleRates()    { return {}; }
Array<int> CoreAudioFormat::getPossibleBitDepths()      { return {}; }

bool CoreAudioFormat::canDoStereo()     { return true; }
bool CoreAudioFormat::canDoMono()       { return true; }

//==============================================================================
AudioFormatReader* CoreAudioFormat::createReaderFor (InputStream* sourceStream,
                                                     bool deleteStreamIfOpeningFails)
{
    ScopedPointer<CoreAudioReader> r (new CoreAudioReader (sourceStream));

    if (r->ok)
        return r.release();

    if (! deleteStreamIfOpeningFails)
        r->input = nullptr;

    return nullptr;
}

AudioFormatWriter* CoreAudioFormat::createWriterFor (OutputStream*,
                                                     double /*sampleRateToUse*/,
                                                     unsigned int /*numberOfChannels*/,
                                                     int /*bitsPerSample*/,
                                                     const StringPairArray& /*metadataValues*/,
                                                     int /*qualityOptionIndex*/)
{
    jassertfalse; // not yet implemented!
    return nullptr;
}

//==============================================================================
// Unit tests for Core Audio layout conversions
//==============================================================================
#if JUCE_UNIT_TESTS

#define DEFINE_CHANNEL_LAYOUT_DFL_ENTRY(x) CoreAudioChannelLayoutTag { x, #x, AudioChannelSet() }
#define DEFINE_CHANNEL_LAYOUT_TAG_ENTRY(x, y) CoreAudioChannelLayoutTag { x, #x, y }

class CoreAudioLayoutsUnitTest  : public UnitTest
{
public:
    CoreAudioLayoutsUnitTest() : UnitTest ("Core Audio Layout <-> JUCE channel layout conversion", "Audio") {}

    void runTest() override
    {
        auto& knownTags = getAllKnownLayoutTags();

        {
            // Check that all known tags defined in CoreAudio SDK version 10.12.4 are known to JUCE
            // Include all defined tags even if there are duplicates as Apple will sometimes change
            // definitions
            beginTest ("All CA tags handled");

            for (auto tagEntry : knownTags)
            {
                auto labels = CoreAudioLayouts::fromCoreAudio (tagEntry.tag);

                expect (! labels.isDiscreteLayout(), String ("Tag \"") + String (tagEntry.name) + "\" is not handled by JUCE");
            }
        }

        {
            beginTest ("Number of speakers");

            for (auto tagEntry : knownTags)
            {
                auto labels = CoreAudioLayouts::getSpeakerLayoutForCoreAudioTag (tagEntry.tag);

                expect (labels.size() == (tagEntry.tag & 0xffff), String ("Tag \"") + String (tagEntry.name) + "\" has incorrect channel count");
            }
        }

        {
            beginTest ("No duplicate speaker");

            for (auto tagEntry : knownTags)
            {
                auto labels = CoreAudioLayouts::getSpeakerLayoutForCoreAudioTag (tagEntry.tag);
                labels.sort();

                for (int i = 0; i < (labels.size() - 1); ++i)
                    expect (labels.getReference (i) != labels.getReference (i + 1),
                            String ("Tag \"") + String (tagEntry.name) + "\" has the same speaker twice");
            }
        }

        {
            beginTest ("CA speaker list and juce layouts are consistent");

            for (auto tagEntry : knownTags)
                expect (AudioChannelSet::channelSetWithChannels (CoreAudioLayouts::getSpeakerLayoutForCoreAudioTag (tagEntry.tag))
                            == CoreAudioLayouts::fromCoreAudio (tagEntry.tag),
                        String ("Tag \"") + String (tagEntry.name) + "\" is not converted consistantly by JUCE");
        }

        {
            beginTest ("AudioChannelSet documentation is correct");

            for (auto tagEntry : knownTags)
            {
                if (tagEntry.equivalentChannelSet.isDisabled())
                    continue;

                expect (CoreAudioLayouts::fromCoreAudio (tagEntry.tag) == tagEntry.equivalentChannelSet,
                        String ("Documentation for tag \"") + String (tagEntry.name) + "\" is incorrect");
            }
        }

        {
            beginTest ("CA tag reverse conversion");

            for (auto tagEntry : knownTags)
            {
                if (tagEntry.equivalentChannelSet.isDisabled())
                    continue;

                expect (CoreAudioLayouts::toCoreAudio (tagEntry.equivalentChannelSet) == tagEntry.tag,
                        String ("Incorrect reverse conversion for tag \"") + String (tagEntry.name) + "\"");
            }
        }
    }

private:
    struct CoreAudioChannelLayoutTag
    {
        AudioChannelLayoutTag tag;
        const char* name;
        AudioChannelSet equivalentChannelSet; /* referred to this in the AudioChannelSet documentation */
    };

    //==============================================================================
    const Array<CoreAudioChannelLayoutTag>& getAllKnownLayoutTags() const
    {
        static CoreAudioChannelLayoutTag tags[] = {
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_Mono,   AudioChannelSet::mono()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_Stereo, AudioChannelSet::stereo()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_StereoHeadphones),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MatrixStereo),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MidSide),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_XY),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_Binaural),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_Ambisonic_B_Format, AudioChannelSet::ambisonic()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_Quadraphonic, AudioChannelSet::quadraphonic()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_Pentagonal, AudioChannelSet::pentagonal()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_Hexagonal, AudioChannelSet::hexagonal()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_Octagonal, AudioChannelSet::octagonal()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_Cube),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_1_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_2_0),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_MPEG_3_0_A, AudioChannelSet::createLCR()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_3_0_B),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_MPEG_4_0_A, AudioChannelSet::createLCRS()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_4_0_B),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_MPEG_5_0_A, AudioChannelSet::create5point0()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_5_0_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_5_0_C),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_5_0_D),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_MPEG_5_1_A, AudioChannelSet::create5point1()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_5_1_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_5_1_C),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_5_1_D),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_MPEG_6_1_A, AudioChannelSet::create6point1()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_MPEG_7_1_A, AudioChannelSet::create7point1SDDS()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_MPEG_7_1_B),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_MPEG_7_1_C, AudioChannelSet::create7point1()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_Emagic_Default_7_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_SMPTE_DTV),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_1_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_2_0),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_ITU_2_1, AudioChannelSet::createLRS()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_2_2),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_3_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_3_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_3_2),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_3_2_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_ITU_3_4_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_2),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_3),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_4),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_5),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_6),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_7),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_8),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_9),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_10),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_11),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_12),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_13),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_14),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_15),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_16),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_17),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_18),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_19),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DVD_20),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_4),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_5),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_6),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_8),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_5_0),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_AudioUnit_6_0, AudioChannelSet::create6point0()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_AudioUnit_7_0, AudioChannelSet::create7point0()),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_AudioUnit_7_0_Front, AudioChannelSet::create7point0SDDS()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_5_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_6_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_7_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AudioUnit_7_1_Front),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_3_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_Quadraphonic),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_4_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_5_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_5_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_6_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_6_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_7_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_7_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_7_1_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_7_1_C),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AAC_Octagonal),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_TMH_10_2_std),
            // DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_TMH_10_2_full), no indication on how to handle this tag
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AC3_1_0_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AC3_3_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AC3_3_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AC3_3_0_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AC3_2_1_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_AC3_3_1_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC_6_0_A),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC_7_0_A),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_6_1_A),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_6_1_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_6_1_C),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_A),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_C),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_D),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_E),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_F),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_G),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_EAC3_7_1_H),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_3_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_4_1),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_DTS_6_0_A, AudioChannelSet::create6point0Music()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_6_0_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_6_0_C),
            DEFINE_CHANNEL_LAYOUT_TAG_ENTRY (kAudioChannelLayoutTag_DTS_6_1_A, AudioChannelSet::create6point1Music()),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_6_1_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_6_1_C),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_7_0),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_7_1),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_8_0_A),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_8_0_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_8_1_A),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_8_1_B),
            DEFINE_CHANNEL_LAYOUT_DFL_ENTRY (kAudioChannelLayoutTag_DTS_6_1_D)
        };
        static Array<CoreAudioChannelLayoutTag> knownTags (tags, sizeof (tags) / sizeof (CoreAudioChannelLayoutTag));

        return knownTags;
    }
};

static CoreAudioLayoutsUnitTest coreAudioLayoutsUnitTest;

#endif

} // namespace juce

#endif
