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

static const char* const aiffFormatName = "AIFF file";

//==============================================================================
const char* const AiffAudioFormat::appleOneShot         = "apple one shot";
const char* const AiffAudioFormat::appleRootSet         = "apple root set";
const char* const AiffAudioFormat::appleRootNote        = "apple root note";
const char* const AiffAudioFormat::appleBeats           = "apple beats";
const char* const AiffAudioFormat::appleDenominator     = "apple denominator";
const char* const AiffAudioFormat::appleNumerator       = "apple numerator";
const char* const AiffAudioFormat::appleTag             = "apple tag";
const char* const AiffAudioFormat::appleKey             = "apple key";

//==============================================================================
namespace AiffFileHelpers
{
    inline int chunkName (const char* name) noexcept    { return (int) ByteOrder::littleEndianInt (name); }

   #if JUCE_MSVC
    #pragma pack (push, 1)
   #endif

    //==============================================================================
    struct InstChunk
    {
        struct Loop
        {
            uint16 type; // these are different in AIFF and WAV
            uint16 startIdentifier;
            uint16 endIdentifier;
        } JUCE_PACKED;

        int8 baseNote;
        int8 detune;
        int8 lowNote;
        int8 highNote;
        int8 lowVelocity;
        int8 highVelocity;
        int16 gain;
        Loop sustainLoop;
        Loop releaseLoop;

        void copyTo (StringPairArray& values) const
        {
            values.set ("MidiUnityNote",        String (baseNote));
            values.set ("Detune",               String (detune));

            values.set ("LowNote",              String (lowNote));
            values.set ("HighNote",             String (highNote));
            values.set ("LowVelocity",          String (lowVelocity));
            values.set ("HighVelocity",         String (highVelocity));

            values.set ("Gain",                 String ((int16) ByteOrder::swapIfLittleEndian ((uint16) gain)));

            values.set ("NumSampleLoops",       String (2));        // always 2 with AIFF, WAV can have more
            values.set ("Loop0Type",            String (ByteOrder::swapIfLittleEndian (sustainLoop.type)));
            values.set ("Loop0StartIdentifier", String (ByteOrder::swapIfLittleEndian (sustainLoop.startIdentifier)));
            values.set ("Loop0EndIdentifier",   String (ByteOrder::swapIfLittleEndian (sustainLoop.endIdentifier)));
            values.set ("Loop1Type",            String (ByteOrder::swapIfLittleEndian (releaseLoop.type)));
            values.set ("Loop1StartIdentifier", String (ByteOrder::swapIfLittleEndian (releaseLoop.startIdentifier)));
            values.set ("Loop1EndIdentifier",   String (ByteOrder::swapIfLittleEndian (releaseLoop.endIdentifier)));
        }

        static uint16 getValue16 (const StringPairArray& values, const char* name, const char* def)
        {
            return ByteOrder::swapIfLittleEndian ((uint16) values.getValue (name, def).getIntValue());
        }

        static int8 getValue8 (const StringPairArray& values, const char* name, const char* def)
        {
            return (int8) values.getValue (name, def).getIntValue();
        }

        static void create (MemoryBlock& block, const StringPairArray& values)
        {
            if (values.getAllKeys().contains ("MidiUnityNote", true))
            {
                block.setSize ((sizeof (InstChunk) + 3) & ~(size_t) 3, true);
                auto& inst = *static_cast<InstChunk*> (block.getData());

                inst.baseNote      = getValue8 (values, "MidiUnityNote", "60");
                inst.detune        = getValue8 (values, "Detune", "0");
                inst.lowNote       = getValue8 (values, "LowNote", "0");
                inst.highNote      = getValue8 (values, "HighNote", "127");
                inst.lowVelocity   = getValue8 (values, "LowVelocity", "1");
                inst.highVelocity  = getValue8 (values, "HighVelocity", "127");
                inst.gain          = (int16) getValue16 (values, "Gain", "0");

                inst.sustainLoop.type              = getValue16 (values, "Loop0Type", "0");
                inst.sustainLoop.startIdentifier   = getValue16 (values, "Loop0StartIdentifier", "0");
                inst.sustainLoop.endIdentifier     = getValue16 (values, "Loop0EndIdentifier", "0");
                inst.releaseLoop.type              = getValue16 (values, "Loop1Type", "0");
                inst.releaseLoop.startIdentifier   = getValue16 (values, "Loop1StartIdentifier", "0");
                inst.releaseLoop.endIdentifier     = getValue16 (values, "Loop1EndIdentifier", "0");
            }
        }

    } JUCE_PACKED;

    //==============================================================================
    struct BASCChunk
    {
        enum Key
        {
            minor = 1,
            major = 2,
            neither = 3,
            both = 4
        };

        BASCChunk (InputStream& input)
        {
            zerostruct (*this);

            flags       = (uint32) input.readIntBigEndian();
            numBeats    = (uint32) input.readIntBigEndian();
            rootNote    = (uint16) input.readShortBigEndian();
            key         = (uint16) input.readShortBigEndian();
            timeSigNum  = (uint16) input.readShortBigEndian();
            timeSigDen  = (uint16) input.readShortBigEndian();
            oneShot     = (uint16) input.readShortBigEndian();
            input.read (unknown, sizeof (unknown));
        }

        void addToMetadata (StringPairArray& metadata) const
        {
            const bool rootNoteSet = rootNote != 0;

            setBoolFlag (metadata, AiffAudioFormat::appleOneShot, oneShot == 2);
            setBoolFlag (metadata, AiffAudioFormat::appleRootSet, rootNoteSet);

            if (rootNoteSet)
                metadata.set (AiffAudioFormat::appleRootNote,   String (rootNote));

            metadata.set (AiffAudioFormat::appleBeats,          String (numBeats));
            metadata.set (AiffAudioFormat::appleDenominator,    String (timeSigDen));
            metadata.set (AiffAudioFormat::appleNumerator,      String (timeSigNum));

            const char* keyString = nullptr;

            switch (key)
            {
                case minor:     keyString = "major";        break;
                case major:     keyString = "major";        break;
                case neither:   keyString = "neither";      break;
                case both:      keyString = "both";         break;
            }

            if (keyString != nullptr)
                metadata.set (AiffAudioFormat::appleKey, keyString);
        }

        void setBoolFlag (StringPairArray& values, const char* name, bool shouldBeSet) const
        {
            values.set (name, shouldBeSet ? "1" : "0");
        }

        uint32 flags;
        uint32 numBeats;
        uint16 rootNote;
        uint16 key;
        uint16 timeSigNum;
        uint16 timeSigDen;
        uint16 oneShot;
        uint8 unknown[66];
    } JUCE_PACKED;

   #if JUCE_MSVC
    #pragma pack (pop)
   #endif

    //==============================================================================
    namespace CATEChunk
    {
        static bool isValidTag (const char* d) noexcept
        {
            return CharacterFunctions::isLetterOrDigit (d[0]) && CharacterFunctions::isUpperCase (static_cast<juce_wchar> (d[0]))
                && CharacterFunctions::isLetterOrDigit (d[1]) && CharacterFunctions::isLowerCase (static_cast<juce_wchar> (d[1]))
                && CharacterFunctions::isLetterOrDigit (d[2]) && CharacterFunctions::isLowerCase (static_cast<juce_wchar> (d[2]));
        }

        static bool isAppleGenre (const String& tag) noexcept
        {
            static const char* appleGenres[] =
            {
                "Rock/Blues",
                "Electronic/Dance",
                "Jazz",
                "Urban",
                "World/Ethnic",
                "Cinematic/New Age",
                "Orchestral",
                "Country/Folk",
                "Experimental",
                "Other Genre"
            };

            for (int i = 0; i < numElementsInArray (appleGenres); ++i)
                if (tag == appleGenres[i])
                    return true;

            return false;
        }

        static String read (InputStream& input, const uint32 length)
        {
            MemoryBlock mb;
            input.skipNextBytes (4);
            input.readIntoMemoryBlock (mb, (ssize_t) length - 4);

            StringArray tagsArray;

            auto* data = static_cast<const char*> (mb.getData());
            auto* dataEnd = data + mb.getSize();

            while (data < dataEnd)
            {
                bool isGenre = false;

                if (isValidTag (data))
                {
                    auto tag = String (CharPointer_UTF8 (data), CharPointer_UTF8 (dataEnd));
                    isGenre = isAppleGenre (tag);
                    tagsArray.add (tag);
                }

                data += isGenre ? 118 : 50;

                if (data < dataEnd && data[0] == 0)
                {
                    if      (data + 52  < dataEnd && isValidTag (data + 50))   data += 50;
                    else if (data + 120 < dataEnd && isValidTag (data + 118))  data += 118;
                    else if (data + 170 < dataEnd && isValidTag (data + 168))  data += 168;
                }
            }

            return tagsArray.joinIntoString (";");
        }
    }

    //==============================================================================
    namespace MarkChunk
    {
        static bool metaDataContainsZeroIdentifiers (const StringPairArray& values)
        {
            // (zero cue identifiers are valid for WAV but not for AIFF)
            const String cueString ("Cue");
            const String noteString ("CueNote");
            const String identifierString ("Identifier");

            for (auto& key : values.getAllKeys())
            {
                if (key.startsWith (noteString))
                    continue; // zero identifier IS valid in a COMT chunk

                if (key.startsWith (cueString) && key.contains (identifierString))
                    if (values.getValue (key, "-1").getIntValue() == 0)
                        return true;
            }

            return false;
        }

        static void create (MemoryBlock& block, const StringPairArray& values)
        {
            auto numCues = values.getValue ("NumCuePoints", "0").getIntValue();

            if (numCues > 0)
            {
                MemoryOutputStream out (block, false);
                out.writeShortBigEndian ((short) numCues);

                auto numCueLabels = values.getValue ("NumCueLabels", "0").getIntValue();
                auto idOffset = metaDataContainsZeroIdentifiers (values) ? 1 : 0; // can't have zero IDs in AIFF

               #if JUCE_DEBUG
                Array<int> identifiers;
               #endif

                for (int i = 0; i < numCues; ++i)
                {
                    auto prefixCue = "Cue" + String (i);
                    auto identifier = idOffset + values.getValue (prefixCue + "Identifier", "1").getIntValue();

                   #if JUCE_DEBUG
                    jassert (! identifiers.contains (identifier));
                    identifiers.add (identifier);
                   #endif

                    auto offset = values.getValue (prefixCue + "Offset", "0").getIntValue();
                    auto label = "CueLabel" + String (i);

                    for (int labelIndex = 0; labelIndex < numCueLabels; ++labelIndex)
                    {
                        auto prefixLabel = "CueLabel" + String (labelIndex);
                        auto labelIdentifier = idOffset + values.getValue (prefixLabel + "Identifier", "1").getIntValue();

                        if (labelIdentifier == identifier)
                        {
                            label = values.getValue (prefixLabel + "Text", label);
                            break;
                        }
                    }

                    out.writeShortBigEndian ((short) identifier);
                    out.writeIntBigEndian (offset);

                    auto labelLength = jmin ((size_t) 254, label.getNumBytesAsUTF8()); // seems to need null terminator even though it's a pstring
                    out.writeByte ((char) labelLength + 1);
                    out.write (label.toUTF8(), labelLength);
                    out.writeByte (0);

                    if ((out.getDataSize() & 1) != 0)
                        out.writeByte (0);
                }
            }
        }
    }

    //==============================================================================
    namespace COMTChunk
    {
        static void create (MemoryBlock& block, const StringPairArray& values)
        {
            auto numNotes = values.getValue ("NumCueNotes", "0").getIntValue();

            if (numNotes > 0)
            {
                MemoryOutputStream out (block, false);
                out.writeShortBigEndian ((short) numNotes);

                for (int i = 0; i < numNotes; ++i)
                {
                    auto prefix = "CueNote" + String (i);

                    out.writeIntBigEndian (values.getValue (prefix + "TimeStamp", "0").getIntValue());
                    out.writeShortBigEndian ((short) values.getValue (prefix + "Identifier", "0").getIntValue());

                    auto comment = values.getValue (prefix + "Text", String());
                    auto commentLength = jmin (comment.getNumBytesAsUTF8(), (size_t) 65534);

                    out.writeShortBigEndian ((short) commentLength + 1);
                    out.write (comment.toUTF8(), commentLength);
                    out.writeByte (0);

                    if ((out.getDataSize() & 1) != 0)
                        out.writeByte (0);
                }
            }
        }
    }
}

//==============================================================================
class AiffAudioFormatReader  : public AudioFormatReader
{
public:
    AiffAudioFormatReader (InputStream* in)
        : AudioFormatReader (in, aiffFormatName)
    {
        using namespace AiffFileHelpers;

        if (input->readInt() == chunkName ("FORM"))
        {
            auto len = input->readIntBigEndian();
            auto end = input->getPosition() + len;
            auto nextType = input->readInt();

            if (nextType == chunkName ("AIFF") || nextType == chunkName ("AIFC"))
            {
                bool hasGotVer = false;
                bool hasGotData = false;
                bool hasGotType = false;

                while (input->getPosition() < end)
                {
                    auto type = input->readInt();
                    auto length = (uint32) input->readIntBigEndian();
                    auto chunkEnd = input->getPosition() + length;

                    if (type == chunkName ("FVER"))
                    {
                        hasGotVer = true;
                        auto ver = input->readIntBigEndian();

                        if (ver != 0 && ver != (int) 0xa2805140)
                            break;
                    }
                    else if (type == chunkName ("COMM"))
                    {
                        hasGotType = true;

                        numChannels = (unsigned int) input->readShortBigEndian();
                        lengthInSamples = input->readIntBigEndian();
                        bitsPerSample = (unsigned int) input->readShortBigEndian();
                        bytesPerFrame = (int) ((numChannels * bitsPerSample) >> 3);

                        unsigned char sampleRateBytes[10];
                        input->read (sampleRateBytes, 10);
                        const int byte0 = sampleRateBytes[0];

                        if ((byte0 & 0x80) != 0
                             || byte0 <= 0x3F || byte0 > 0x40
                             || (byte0 == 0x40 && sampleRateBytes[1] > 0x1C))
                            break;

                        auto sampRate = ByteOrder::bigEndianInt (sampleRateBytes + 2);
                        sampRate >>= (16414 - ByteOrder::bigEndianShort (sampleRateBytes));
                        sampleRate = (int) sampRate;

                        if (length <= 18)
                        {
                            // some types don't have a chunk large enough to include a compression
                            // type, so assume it's just big-endian pcm
                            littleEndian = false;
                        }
                        else
                        {
                            auto compType = input->readInt();

                            if (compType == chunkName ("NONE") || compType == chunkName ("twos"))
                            {
                                littleEndian = false;
                            }
                            else if (compType == chunkName ("sowt"))
                            {
                                littleEndian = true;
                            }
                            else if (compType == chunkName ("fl32") || compType == chunkName ("FL32"))
                            {
                                littleEndian = false;
                                usesFloatingPointData = true;
                            }
                            else
                            {
                                sampleRate = 0;
                                break;
                            }
                        }
                    }
                    else if (type == chunkName ("SSND"))
                    {
                        hasGotData = true;

                        auto offset = input->readIntBigEndian();
                        dataChunkStart = input->getPosition() + 4 + offset;
                        lengthInSamples = (bytesPerFrame > 0) ? jmin (lengthInSamples, ((int64) length) / (int64) bytesPerFrame) : 0;
                    }
                    else if (type == chunkName ("MARK"))
                    {
                        auto numCues = (uint16) input->readShortBigEndian();

                        // these two are always the same for AIFF-read files
                        metadataValues.set ("NumCuePoints", String (numCues));
                        metadataValues.set ("NumCueLabels", String (numCues));

                        for (uint16 i = 0; i < numCues; ++i)
                        {
                            auto identifier = (uint16) input->readShortBigEndian();
                            auto offset = (uint32) input->readIntBigEndian();
                            auto stringLength = (uint8) input->readByte();
                            MemoryBlock textBlock;
                            input->readIntoMemoryBlock (textBlock, stringLength);

                            // if the stringLength is even then read one more byte as the
                            // string needs to be an even number of bytes INCLUDING the
                            // leading length character in the pascal string
                            if ((stringLength & 1) == 0)
                                input->readByte();

                            auto prefixCue = "Cue" + String (i);
                            metadataValues.set (prefixCue + "Identifier", String (identifier));
                            metadataValues.set (prefixCue + "Offset", String (offset));

                            auto prefixLabel = "CueLabel" + String (i);
                            metadataValues.set (prefixLabel + "Identifier", String (identifier));
                            metadataValues.set (prefixLabel + "Text", textBlock.toString());
                        }
                    }
                    else if (type == chunkName ("COMT"))
                    {
                        auto numNotes = (uint16) input->readShortBigEndian();
                        metadataValues.set ("NumCueNotes", String (numNotes));

                        for (uint16 i = 0; i < numNotes; ++i)
                        {
                            auto timestamp = (uint32) input->readIntBigEndian();
                            auto identifier = (uint16) input->readShortBigEndian(); // may be zero in this case
                            auto stringLength = (uint16) input->readShortBigEndian();

                            MemoryBlock textBlock;
                            input->readIntoMemoryBlock (textBlock, stringLength + (stringLength & 1));

                            auto prefix = "CueNote" + String (i);
                            metadataValues.set (prefix + "TimeStamp", String (timestamp));
                            metadataValues.set (prefix + "Identifier", String (identifier));
                            metadataValues.set (prefix + "Text", textBlock.toString());
                        }
                    }
                    else if (type == chunkName ("INST"))
                    {
                        HeapBlock<InstChunk> inst;
                        inst.calloc (jmax ((size_t) length + 1, sizeof (InstChunk)), 1);
                        input->read (inst, (int) length);
                        inst->copyTo (metadataValues);
                    }
                    else if (type == chunkName ("basc"))
                    {
                        AiffFileHelpers::BASCChunk (*input).addToMetadata (metadataValues);
                    }
                    else if (type == chunkName ("cate"))
                    {
                        metadataValues.set (AiffAudioFormat::appleTag,
                                            AiffFileHelpers::CATEChunk::read (*input, length));
                    }
                    else if ((hasGotVer && hasGotData && hasGotType)
                              || chunkEnd < input->getPosition()
                              || input->isExhausted())
                    {
                        break;
                    }

                    input->setPosition (chunkEnd + (chunkEnd & 1)); // (chunks should be aligned to an even byte address)
                }
            }
        }

        if (metadataValues.size() > 0)
            metadataValues.set ("MetaDataSource", "AIFF");
    }

    //==============================================================================
    bool readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                      int64 startSampleInFile, int numSamples) override
    {
        clearSamplesBeyondAvailableLength (destSamples, numDestChannels, startOffsetInDestBuffer,
                                           startSampleInFile, numSamples, lengthInSamples);

        if (numSamples <= 0)
            return true;

        input->setPosition (dataChunkStart + startSampleInFile * bytesPerFrame);

        while (numSamples > 0)
        {
            const int tempBufSize = 480 * 3 * 4; // (keep this a multiple of 3)
            char tempBuffer [tempBufSize];

            const int numThisTime = jmin (tempBufSize / bytesPerFrame, numSamples);
            const int bytesRead = input->read (tempBuffer, numThisTime * bytesPerFrame);

            if (bytesRead < numThisTime * bytesPerFrame)
            {
                jassert (bytesRead >= 0);
                zeromem (tempBuffer + bytesRead, (size_t) (numThisTime * bytesPerFrame - bytesRead));
            }

            if (littleEndian)
                copySampleData<AudioData::LittleEndian> (bitsPerSample, usesFloatingPointData,
                                                         destSamples, startOffsetInDestBuffer, numDestChannels,
                                                         tempBuffer, (int) numChannels, numThisTime);
            else
                copySampleData<AudioData::BigEndian> (bitsPerSample, usesFloatingPointData,
                                                      destSamples, startOffsetInDestBuffer, numDestChannels,
                                                      tempBuffer, (int) numChannels, numThisTime);

            startOffsetInDestBuffer += numThisTime;
            numSamples -= numThisTime;
        }

        return true;
    }

    template <typename Endianness>
    static void copySampleData (unsigned int bitsPerSample, bool usesFloatingPointData,
                                int* const* destSamples, int startOffsetInDestBuffer, int numDestChannels,
                                const void* sourceData, int numChannels, int numSamples) noexcept
    {
        switch (bitsPerSample)
        {
            case 8:     ReadHelper<AudioData::Int32, AudioData::Int8,  Endianness>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples); break;
            case 16:    ReadHelper<AudioData::Int32, AudioData::Int16, Endianness>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples); break;
            case 24:    ReadHelper<AudioData::Int32, AudioData::Int24, Endianness>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples); break;
            case 32:    if (usesFloatingPointData) ReadHelper<AudioData::Float32, AudioData::Float32, Endianness>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples);
                        else                       ReadHelper<AudioData::Int32,   AudioData::Int32,   Endianness>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples);
                        break;
            default:    jassertfalse; break;
        }
    }

    int bytesPerFrame;
    int64 dataChunkStart;
    bool littleEndian;

private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (AiffAudioFormatReader)
};

//==============================================================================
class AiffAudioFormatWriter  : public AudioFormatWriter
{
public:
    AiffAudioFormatWriter (OutputStream* out, double rate,
                           unsigned int numChans, unsigned int bits,
                           const StringPairArray& metadataValues)
        : AudioFormatWriter (out, aiffFormatName, rate, numChans, bits)
    {
        using namespace AiffFileHelpers;

        if (metadataValues.size() > 0)
        {
            // The meta data should have been sanitised for the AIFF format.
            // If it was originally sourced from a WAV file the MetaDataSource
            // key should be removed (or set to "AIFF") once this has been done
            jassert (metadataValues.getValue ("MetaDataSource", "None") != "WAV");

            MarkChunk::create (markChunk, metadataValues);
            COMTChunk::create (comtChunk, metadataValues);
            InstChunk::create (instChunk, metadataValues);
        }

        headerPosition = out->getPosition();
        writeHeader();
    }

    ~AiffAudioFormatWriter()
    {
        if ((bytesWritten & 1) != 0)
            output->writeByte (0);

        writeHeader();
    }

    //==============================================================================
    bool write (const int** data, int numSamples) override
    {
        jassert (numSamples >= 0);
        jassert (data != nullptr && *data != nullptr); // the input must contain at least one channel!

        if (writeFailed)
            return false;

        auto bytes = numChannels * (size_t) numSamples * bitsPerSample / 8;
        tempBlock.ensureSize (bytes, false);

        switch (bitsPerSample)
        {
            case 8:     WriteHelper<AudioData::Int8,  AudioData::Int32, AudioData::BigEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            case 16:    WriteHelper<AudioData::Int16, AudioData::Int32, AudioData::BigEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            case 24:    WriteHelper<AudioData::Int24, AudioData::Int32, AudioData::BigEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            case 32:    WriteHelper<AudioData::Int32, AudioData::Int32, AudioData::BigEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            default:    jassertfalse; break;
        }

        if (bytesWritten + bytes >= (size_t) 0xfff00000
             || ! output->write (tempBlock.getData(), bytes))
        {
            // failed to write to disk, so let's try writing the header.
            // If it's just run out of disk space, then if it does manage
            // to write the header, we'll still have a useable file..
            writeHeader();
            writeFailed = true;
            return false;
        }

        bytesWritten += bytes;
        lengthInSamples += (uint64) numSamples;
        return true;
    }

private:
    MemoryBlock tempBlock, markChunk, comtChunk, instChunk;
    uint64 lengthInSamples = 0, bytesWritten = 0;
    int64 headerPosition = 0;
    bool writeFailed = false;

    void writeHeader()
    {
        using namespace AiffFileHelpers;

        const bool couldSeekOk = output->setPosition (headerPosition);
        ignoreUnused (couldSeekOk);

        // if this fails, you've given it an output stream that can't seek! It needs
        // to be able to seek back to write the header
        jassert (couldSeekOk);

        auto headerLen = (int) (54 + (markChunk.getSize() > 0 ? markChunk.getSize() + 8 : 0)
                                   + (comtChunk.getSize() > 0 ? comtChunk.getSize() + 8 : 0)
                                   + (instChunk.getSize() > 0 ? instChunk.getSize() + 8 : 0));
        auto audioBytes = (int) (lengthInSamples * ((bitsPerSample * numChannels) / 8));
        audioBytes += (audioBytes & 1);

        output->writeInt (chunkName ("FORM"));
        output->writeIntBigEndian (headerLen + audioBytes - 8);
        output->writeInt (chunkName ("AIFF"));
        output->writeInt (chunkName ("COMM"));
        output->writeIntBigEndian (18);
        output->writeShortBigEndian ((short) numChannels);
        output->writeIntBigEndian ((int) lengthInSamples);
        output->writeShortBigEndian ((short) bitsPerSample);

        uint8 sampleRateBytes[10] = {};

        if (sampleRate <= 1)
        {
            sampleRateBytes[0] = 0x3f;
            sampleRateBytes[1] = 0xff;
            sampleRateBytes[2] = 0x80;
        }
        else
        {
            int mask = 0x40000000;
            sampleRateBytes[0] = 0x40;

            if (sampleRate >= mask)
            {
                jassertfalse;
                sampleRateBytes[1] = 0x1d;
            }
            else
            {
                int n = (int) sampleRate;
                int i;

                for (i = 0; i <= 32 ; ++i)
                {
                    if ((n & mask) != 0)
                        break;

                    mask >>= 1;
                }

                n = n << (i + 1);

                sampleRateBytes[1] = (uint8) (29 - i);
                sampleRateBytes[2] = (uint8) ((n >> 24) & 0xff);
                sampleRateBytes[3] = (uint8) ((n >> 16) & 0xff);
                sampleRateBytes[4] = (uint8) ((n >>  8) & 0xff);
                sampleRateBytes[5] = (uint8) (n & 0xff);
            }
        }

        output->write (sampleRateBytes, 10);

        if (markChunk.getSize() > 0)
        {
            output->writeInt (chunkName ("MARK"));
            output->writeIntBigEndian ((int) markChunk.getSize());
            *output << markChunk;
        }

        if (comtChunk.getSize() > 0)
        {
            output->writeInt (chunkName ("COMT"));
            output->writeIntBigEndian ((int) comtChunk.getSize());
            *output << comtChunk;
        }

        if (instChunk.getSize() > 0)
        {
            output->writeInt (chunkName ("INST"));
            output->writeIntBigEndian ((int) instChunk.getSize());
            *output << instChunk;
        }

        output->writeInt (chunkName ("SSND"));
        output->writeIntBigEndian (audioBytes + 8);
        output->writeInt (0);
        output->writeInt (0);

        jassert (output->getPosition() == headerLen);
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (AiffAudioFormatWriter)
};

//==============================================================================
class MemoryMappedAiffReader   : public MemoryMappedAudioFormatReader
{
public:
    MemoryMappedAiffReader (const File& f, const AiffAudioFormatReader& reader)
        : MemoryMappedAudioFormatReader (f, reader, reader.dataChunkStart,
                                         reader.bytesPerFrame * reader.lengthInSamples, reader.bytesPerFrame),
          littleEndian (reader.littleEndian)
    {
    }

    bool readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                      int64 startSampleInFile, int numSamples) override
    {
        clearSamplesBeyondAvailableLength (destSamples, numDestChannels, startOffsetInDestBuffer,
                                           startSampleInFile, numSamples, lengthInSamples);

        if (map == nullptr || ! mappedSection.contains (Range<int64> (startSampleInFile, startSampleInFile + numSamples)))
        {
            jassertfalse; // you must make sure that the window contains all the samples you're going to attempt to read.
            return false;
        }

        if (littleEndian)
            AiffAudioFormatReader::copySampleData<AudioData::LittleEndian>
                    (bitsPerSample, usesFloatingPointData, destSamples, startOffsetInDestBuffer,
                     numDestChannels, sampleToPointer (startSampleInFile), (int) numChannels, numSamples);
        else
            AiffAudioFormatReader::copySampleData<AudioData::BigEndian>
                    (bitsPerSample, usesFloatingPointData, destSamples, startOffsetInDestBuffer,
                     numDestChannels, sampleToPointer (startSampleInFile), (int) numChannels, numSamples);

        return true;
    }

    void getSample (int64 sample, float* result) const noexcept override
    {
        auto num = (int) numChannels;

        if (map == nullptr || ! mappedSection.contains (sample))
        {
            jassertfalse; // you must make sure that the window contains all the samples you're going to attempt to read.

            zeromem (result, sizeof (float) * (size_t) num);
            return;
        }

        float** dest = &result;
        const void* source = sampleToPointer (sample);

        if (littleEndian)
        {
            switch (bitsPerSample)
            {
                case 8:     ReadHelper<AudioData::Float32, AudioData::UInt8, AudioData::LittleEndian>::read (dest, 0, 1, source, 1, num); break;
                case 16:    ReadHelper<AudioData::Float32, AudioData::Int16, AudioData::LittleEndian>::read (dest, 0, 1, source, 1, num); break;
                case 24:    ReadHelper<AudioData::Float32, AudioData::Int24, AudioData::LittleEndian>::read (dest, 0, 1, source, 1, num); break;
                case 32:    if (usesFloatingPointData) ReadHelper<AudioData::Float32, AudioData::Float32, AudioData::LittleEndian>::read (dest, 0, 1, source, 1, num);
                            else                       ReadHelper<AudioData::Float32, AudioData::Int32,   AudioData::LittleEndian>::read (dest, 0, 1, source, 1, num);
                            break;
                default:    jassertfalse; break;
            }
        }
        else
        {
            switch (bitsPerSample)
            {
                case 8:     ReadHelper<AudioData::Float32, AudioData::UInt8, AudioData::BigEndian>::read (dest, 0, 1, source, 1, num); break;
                case 16:    ReadHelper<AudioData::Float32, AudioData::Int16, AudioData::BigEndian>::read (dest, 0, 1, source, 1, num); break;
                case 24:    ReadHelper<AudioData::Float32, AudioData::Int24, AudioData::BigEndian>::read (dest, 0, 1, source, 1, num); break;
                case 32:    if (usesFloatingPointData) ReadHelper<AudioData::Float32, AudioData::Float32, AudioData::BigEndian>::read (dest, 0, 1, source, 1, num);
                            else                       ReadHelper<AudioData::Float32, AudioData::Int32,   AudioData::BigEndian>::read (dest, 0, 1, source, 1, num);
                            break;
                default:    jassertfalse; break;
            }
        }
    }

    void readMaxLevels (int64 startSampleInFile, int64 numSamples, Range<float>* results, int numChannelsToRead) override
    {
        numSamples = jmin (numSamples, lengthInSamples - startSampleInFile);

        if (map == nullptr || numSamples <= 0 || ! mappedSection.contains (Range<int64> (startSampleInFile, startSampleInFile + numSamples)))
        {
            jassert (numSamples <= 0); // you must make sure that the window contains all the samples you're going to attempt to read.

            for (int i = 0; i < numChannelsToRead; ++i)
                results[i] = Range<float>();

            return;
        }

        switch (bitsPerSample)
        {
            case 8:     scanMinAndMax<AudioData::UInt8> (startSampleInFile, numSamples, results, numChannelsToRead); break;
            case 16:    scanMinAndMax<AudioData::Int16> (startSampleInFile, numSamples, results, numChannelsToRead); break;
            case 24:    scanMinAndMax<AudioData::Int24> (startSampleInFile, numSamples, results, numChannelsToRead); break;
            case 32:    if (usesFloatingPointData) scanMinAndMax<AudioData::Float32> (startSampleInFile, numSamples, results, numChannelsToRead);
                        else                       scanMinAndMax<AudioData::Int32>   (startSampleInFile, numSamples, results, numChannelsToRead);
                        break;
            default:    jassertfalse; break;
        }
    }

private:
    const bool littleEndian;

    template <typename SampleType>
    void scanMinAndMax (int64 startSampleInFile, int64 numSamples, Range<float>* results, int numChannelsToRead) const noexcept
    {
        for (int i = 0; i < numChannelsToRead; ++i)
            results[i] = scanMinAndMaxForChannel<SampleType> (i, startSampleInFile, numSamples);
    }

    template <typename SampleType>
    Range<float> scanMinAndMaxForChannel (int channel, int64 startSampleInFile, int64 numSamples) const noexcept
    {
        return littleEndian ? scanMinAndMaxInterleaved<SampleType, AudioData::LittleEndian> (channel, startSampleInFile, numSamples)
                            : scanMinAndMaxInterleaved<SampleType, AudioData::BigEndian>    (channel, startSampleInFile, numSamples);
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MemoryMappedAiffReader)
};

//==============================================================================
AiffAudioFormat::AiffAudioFormat()  : AudioFormat (aiffFormatName, ".aiff .aif") {}
AiffAudioFormat::~AiffAudioFormat() {}

Array<int> AiffAudioFormat::getPossibleSampleRates()
{
    return { 22050, 32000, 44100, 48000, 88200, 96000, 176400, 192000 };
}

Array<int> AiffAudioFormat::getPossibleBitDepths()
{
     return { 8, 16, 24 };
}

bool AiffAudioFormat::canDoStereo() { return true; }
bool AiffAudioFormat::canDoMono()   { return true; }

#if JUCE_MAC
bool AiffAudioFormat::canHandleFile (const File& f)
{
    if (AudioFormat::canHandleFile (f))
        return true;

    auto type = f.getMacOSType();

    // (NB: written as hex to avoid four-char-constant warnings)
    return type == 0x41494646 /* AIFF */ || type == 0x41494643 /* AIFC */
        || type == 0x61696666 /* aiff */ || type == 0x61696663 /* aifc */;
}
#endif

AudioFormatReader* AiffAudioFormat::createReaderFor (InputStream* sourceStream, bool deleteStreamIfOpeningFails)
{
    ScopedPointer<AiffAudioFormatReader> w (new AiffAudioFormatReader (sourceStream));

    if (w->sampleRate > 0 && w->numChannels > 0)
        return w.release();

    if (! deleteStreamIfOpeningFails)
        w->input = nullptr;

    return nullptr;
}

MemoryMappedAudioFormatReader* AiffAudioFormat::createMemoryMappedReader (const File& file)
{
    return createMemoryMappedReader (file.createInputStream());
}

MemoryMappedAudioFormatReader* AiffAudioFormat::createMemoryMappedReader (FileInputStream* fin)
{
    if (fin != nullptr)
    {
        AiffAudioFormatReader reader (fin);

        if (reader.lengthInSamples > 0)
            return new MemoryMappedAiffReader (fin->getFile(), reader);
    }

    return nullptr;
}

AudioFormatWriter* AiffAudioFormat::createWriterFor (OutputStream* out,
                                                     double sampleRate,
                                                     unsigned int numberOfChannels,
                                                     int bitsPerSample,
                                                     const StringPairArray& metadataValues,
                                                     int /*qualityOptionIndex*/)
{
    if (out != nullptr && getPossibleBitDepths().contains (bitsPerSample))
        return new AiffAudioFormatWriter (out, sampleRate, numberOfChannels,
                                          (unsigned int) bitsPerSample, metadataValues);

    return nullptr;
}

} // namespace juce
