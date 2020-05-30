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

static const char* const wavFormatName = "WAV file";

//==============================================================================
const char* const WavAudioFormat::bwavDescription      = "bwav description";
const char* const WavAudioFormat::bwavOriginator       = "bwav originator";
const char* const WavAudioFormat::bwavOriginatorRef    = "bwav originator ref";
const char* const WavAudioFormat::bwavOriginationDate  = "bwav origination date";
const char* const WavAudioFormat::bwavOriginationTime  = "bwav origination time";
const char* const WavAudioFormat::bwavTimeReference    = "bwav time reference";
const char* const WavAudioFormat::bwavCodingHistory    = "bwav coding history";

StringPairArray WavAudioFormat::createBWAVMetadata (const String& description,
                                                    const String& originator,
                                                    const String& originatorRef,
                                                    Time date,
                                                    int64 timeReferenceSamples,
                                                    const String& codingHistory)
{
    StringPairArray m;

    m.set (bwavDescription, description);
    m.set (bwavOriginator, originator);
    m.set (bwavOriginatorRef, originatorRef);
    m.set (bwavOriginationDate, date.formatted ("%Y-%m-%d"));
    m.set (bwavOriginationTime, date.formatted ("%H:%M:%S"));
    m.set (bwavTimeReference, String (timeReferenceSamples));
    m.set (bwavCodingHistory, codingHistory);

    return m;
}

const char* const WavAudioFormat::acidOneShot          = "acid one shot";
const char* const WavAudioFormat::acidRootSet          = "acid root set";
const char* const WavAudioFormat::acidStretch          = "acid stretch";
const char* const WavAudioFormat::acidDiskBased        = "acid disk based";
const char* const WavAudioFormat::acidizerFlag         = "acidizer flag";
const char* const WavAudioFormat::acidRootNote         = "acid root note";
const char* const WavAudioFormat::acidBeats            = "acid beats";
const char* const WavAudioFormat::acidDenominator      = "acid denominator";
const char* const WavAudioFormat::acidNumerator        = "acid numerator";
const char* const WavAudioFormat::acidTempo            = "acid tempo";

const char* const WavAudioFormat::riffInfoArchivalLocation      = "IARL";
const char* const WavAudioFormat::riffInfoArtist                = "IART";
const char* const WavAudioFormat::riffInfoBaseURL               = "IBSU";
const char* const WavAudioFormat::riffInfoCinematographer       = "ICNM";
const char* const WavAudioFormat::riffInfoComment               = "CMNT";
const char* const WavAudioFormat::riffInfoComment2              = "ICMT";
const char* const WavAudioFormat::riffInfoComments              = "COMM";
const char* const WavAudioFormat::riffInfoCommissioned          = "ICMS";
const char* const WavAudioFormat::riffInfoCopyright             = "ICOP";
const char* const WavAudioFormat::riffInfoCostumeDesigner       = "ICDS";
const char* const WavAudioFormat::riffInfoCountry               = "ICNT";
const char* const WavAudioFormat::riffInfoCropped               = "ICRP";
const char* const WavAudioFormat::riffInfoDateCreated           = "ICRD";
const char* const WavAudioFormat::riffInfoDateTimeOriginal      = "IDIT";
const char* const WavAudioFormat::riffInfoDefaultAudioStream    = "ICAS";
const char* const WavAudioFormat::riffInfoDimension             = "IDIM";
const char* const WavAudioFormat::riffInfoDirectory             = "DIRC";
const char* const WavAudioFormat::riffInfoDistributedBy         = "IDST";
const char* const WavAudioFormat::riffInfoDotsPerInch           = "IDPI";
const char* const WavAudioFormat::riffInfoEditedBy              = "IEDT";
const char* const WavAudioFormat::riffInfoEighthLanguage        = "IAS8";
const char* const WavAudioFormat::riffInfoEncodedBy             = "CODE";
const char* const WavAudioFormat::riffInfoEndTimecode           = "TCDO";
const char* const WavAudioFormat::riffInfoEngineer              = "IENG";
const char* const WavAudioFormat::riffInfoFifthLanguage         = "IAS5";
const char* const WavAudioFormat::riffInfoFirstLanguage         = "IAS1";
const char* const WavAudioFormat::riffInfoFourthLanguage        = "IAS4";
const char* const WavAudioFormat::riffInfoGenre                 = "GENR";
const char* const WavAudioFormat::riffInfoKeywords              = "IKEY";
const char* const WavAudioFormat::riffInfoLanguage              = "LANG";
const char* const WavAudioFormat::riffInfoLength                = "TLEN";
const char* const WavAudioFormat::riffInfoLightness             = "ILGT";
const char* const WavAudioFormat::riffInfoLocation              = "LOCA";
const char* const WavAudioFormat::riffInfoLogoIconURL           = "ILIU";
const char* const WavAudioFormat::riffInfoLogoURL               = "ILGU";
const char* const WavAudioFormat::riffInfoMedium                = "IMED";
const char* const WavAudioFormat::riffInfoMoreInfoBannerImage   = "IMBI";
const char* const WavAudioFormat::riffInfoMoreInfoBannerURL     = "IMBU";
const char* const WavAudioFormat::riffInfoMoreInfoText          = "IMIT";
const char* const WavAudioFormat::riffInfoMoreInfoURL           = "IMIU";
const char* const WavAudioFormat::riffInfoMusicBy               = "IMUS";
const char* const WavAudioFormat::riffInfoNinthLanguage         = "IAS9";
const char* const WavAudioFormat::riffInfoNumberOfParts         = "PRT2";
const char* const WavAudioFormat::riffInfoOrganisation          = "TORG";
const char* const WavAudioFormat::riffInfoPart                  = "PRT1";
const char* const WavAudioFormat::riffInfoProducedBy            = "IPRO";
const char* const WavAudioFormat::riffInfoProductName           = "IPRD";
const char* const WavAudioFormat::riffInfoProductionDesigner    = "IPDS";
const char* const WavAudioFormat::riffInfoProductionStudio      = "ISDT";
const char* const WavAudioFormat::riffInfoRate                  = "RATE";
const char* const WavAudioFormat::riffInfoRated                 = "AGES";
const char* const WavAudioFormat::riffInfoRating                = "IRTD";
const char* const WavAudioFormat::riffInfoRippedBy              = "IRIP";
const char* const WavAudioFormat::riffInfoSecondaryGenre        = "ISGN";
const char* const WavAudioFormat::riffInfoSecondLanguage        = "IAS2";
const char* const WavAudioFormat::riffInfoSeventhLanguage       = "IAS7";
const char* const WavAudioFormat::riffInfoSharpness             = "ISHP";
const char* const WavAudioFormat::riffInfoSixthLanguage         = "IAS6";
const char* const WavAudioFormat::riffInfoSoftware              = "ISFT";
const char* const WavAudioFormat::riffInfoSoundSchemeTitle      = "DISP";
const char* const WavAudioFormat::riffInfoSource                = "ISRC";
const char* const WavAudioFormat::riffInfoSourceFrom            = "ISRF";
const char* const WavAudioFormat::riffInfoStarring_ISTR         = "ISTR";
const char* const WavAudioFormat::riffInfoStarring_STAR         = "STAR";
const char* const WavAudioFormat::riffInfoStartTimecode         = "TCOD";
const char* const WavAudioFormat::riffInfoStatistics            = "STAT";
const char* const WavAudioFormat::riffInfoSubject               = "ISBJ";
const char* const WavAudioFormat::riffInfoTapeName              = "TAPE";
const char* const WavAudioFormat::riffInfoTechnician            = "ITCH";
const char* const WavAudioFormat::riffInfoThirdLanguage         = "IAS3";
const char* const WavAudioFormat::riffInfoTimeCode              = "ISMP";
const char* const WavAudioFormat::riffInfoTitle                 = "INAM";
const char* const WavAudioFormat::riffInfoTrackNo               = "IPRT";
const char* const WavAudioFormat::riffInfoTrackNumber           = "TRCK";
const char* const WavAudioFormat::riffInfoURL                   = "TURL";
const char* const WavAudioFormat::riffInfoVegasVersionMajor     = "VMAJ";
const char* const WavAudioFormat::riffInfoVegasVersionMinor     = "VMIN";
const char* const WavAudioFormat::riffInfoVersion               = "TVER";
const char* const WavAudioFormat::riffInfoWatermarkURL          = "IWMU";
const char* const WavAudioFormat::riffInfoWrittenBy             = "IWRI";
const char* const WavAudioFormat::riffInfoYear                  = "YEAR";

const char* const WavAudioFormat::ISRC                 = "ISRC";
const char* const WavAudioFormat::tracktionLoopInfo    = "tracktion loop info";

//==============================================================================
namespace WavFileHelpers
{
    inline int chunkName (const char* const name) noexcept   { return (int) ByteOrder::littleEndianInt (name[0], name[1], name[2], name[3]); }
    inline size_t roundUpSize (size_t sz) noexcept           { return (sz + 3) & ~3u; }

    #if JUCE_MSVC
     #pragma pack (push, 1)
    #endif

    struct BWAVChunk
    {
        char description[256];
        char originator[32];
        char originatorRef[32];
        char originationDate[10];
        char originationTime[8];
        uint32 timeRefLow;
        uint32 timeRefHigh;
        uint16 version;
        uint8 umid[64];
        uint8 reserved[190];
        char codingHistory[1];

        void copyTo (StringPairArray& values, const int totalSize) const
        {
            values.set (WavAudioFormat::bwavDescription,     String::fromUTF8 (description,     sizeof (description)));
            values.set (WavAudioFormat::bwavOriginator,      String::fromUTF8 (originator,      sizeof (originator)));
            values.set (WavAudioFormat::bwavOriginatorRef,   String::fromUTF8 (originatorRef,   sizeof (originatorRef)));
            values.set (WavAudioFormat::bwavOriginationDate, String::fromUTF8 (originationDate, sizeof (originationDate)));
            values.set (WavAudioFormat::bwavOriginationTime, String::fromUTF8 (originationTime, sizeof (originationTime)));

            auto timeLow  = ByteOrder::swapIfBigEndian (timeRefLow);
            auto timeHigh = ByteOrder::swapIfBigEndian (timeRefHigh);
            auto time = (((int64) timeHigh) << 32) + timeLow;

            values.set (WavAudioFormat::bwavTimeReference, String (time));
            values.set (WavAudioFormat::bwavCodingHistory,
                        String::fromUTF8 (codingHistory, totalSize - (int) offsetof (BWAVChunk, codingHistory)));
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            MemoryBlock data (roundUpSize (sizeof (BWAVChunk) + values[WavAudioFormat::bwavCodingHistory].getNumBytesAsUTF8()));
            data.fillWith (0);

            auto* b = (BWAVChunk*) data.getData();

            // Allow these calls to overwrite an extra byte at the end, which is fine as long
            // as they get called in the right order..
            values[WavAudioFormat::bwavDescription]    .copyToUTF8 (b->description, 257);
            values[WavAudioFormat::bwavOriginator]     .copyToUTF8 (b->originator, 33);
            values[WavAudioFormat::bwavOriginatorRef]  .copyToUTF8 (b->originatorRef, 33);
            values[WavAudioFormat::bwavOriginationDate].copyToUTF8 (b->originationDate, 11);
            values[WavAudioFormat::bwavOriginationTime].copyToUTF8 (b->originationTime, 9);

            auto time = values[WavAudioFormat::bwavTimeReference].getLargeIntValue();
            b->timeRefLow = ByteOrder::swapIfBigEndian ((uint32) (time & 0xffffffff));
            b->timeRefHigh = ByteOrder::swapIfBigEndian ((uint32) (time >> 32));

            values[WavAudioFormat::bwavCodingHistory].copyToUTF8 (b->codingHistory, 0x7fffffff);

            if (b->description[0] != 0
                || b->originator[0] != 0
                || b->originationDate[0] != 0
                || b->originationTime[0] != 0
                || b->codingHistory[0] != 0
                || time != 0)
            {
                return data;
            }

            return {};
        }

    } JUCE_PACKED;

    //==============================================================================
    inline AudioChannelSet canonicalWavChannelSet (int numChannels)
    {
        if (numChannels == 1)  return AudioChannelSet::mono();
        if (numChannels == 2)  return AudioChannelSet::stereo();
        if (numChannels == 3)  return AudioChannelSet::createLCR();
        if (numChannels == 4)  return AudioChannelSet::quadraphonic();
        if (numChannels == 5)  return AudioChannelSet::create5point0();
        if (numChannels == 6)  return AudioChannelSet::create5point1();
        if (numChannels == 7)  return AudioChannelSet::create7point0SDDS();
        if (numChannels == 8)  return AudioChannelSet::create7point1SDDS();

        return AudioChannelSet::discreteChannels (numChannels);
    }

    //==============================================================================
    struct SMPLChunk
    {
        struct SampleLoop
        {
            uint32 identifier;
            uint32 type; // these are different in AIFF and WAV
            uint32 start;
            uint32 end;
            uint32 fraction;
            uint32 playCount;
        } JUCE_PACKED;

        uint32 manufacturer;
        uint32 product;
        uint32 samplePeriod;
        uint32 midiUnityNote;
        uint32 midiPitchFraction;
        uint32 smpteFormat;
        uint32 smpteOffset;
        uint32 numSampleLoops;
        uint32 samplerData;
        SampleLoop loops[1];

        template <typename NameType>
        static void setValue (StringPairArray& values, NameType name, uint32 val)
        {
            values.set (name, String (ByteOrder::swapIfBigEndian (val)));
        }

        static void setValue (StringPairArray& values, int prefix, const char* name, uint32 val)
        {
            setValue (values, "Loop" + String (prefix) + name, val);
        }

        void copyTo (StringPairArray& values, const int totalSize) const
        {
            setValue (values, "Manufacturer",      manufacturer);
            setValue (values, "Product",           product);
            setValue (values, "SamplePeriod",      samplePeriod);
            setValue (values, "MidiUnityNote",     midiUnityNote);
            setValue (values, "MidiPitchFraction", midiPitchFraction);
            setValue (values, "SmpteFormat",       smpteFormat);
            setValue (values, "SmpteOffset",       smpteOffset);
            setValue (values, "NumSampleLoops",    numSampleLoops);
            setValue (values, "SamplerData",       samplerData);

            for (int i = 0; i < (int) numSampleLoops; ++i)
            {
                if ((uint8*) (loops + (i + 1)) > ((uint8*) this) + totalSize)
                    break;

                setValue (values, i, "Identifier", loops[i].identifier);
                setValue (values, i, "Type",       loops[i].type);
                setValue (values, i, "Start",      loops[i].start);
                setValue (values, i, "End",        loops[i].end);
                setValue (values, i, "Fraction",   loops[i].fraction);
                setValue (values, i, "PlayCount",  loops[i].playCount);
            }
        }

        template <typename NameType>
        static uint32 getValue (const StringPairArray& values, NameType name, const char* def)
        {
            return ByteOrder::swapIfBigEndian ((uint32) values.getValue (name, def).getIntValue());
        }

        static uint32 getValue (const StringPairArray& values, int prefix, const char* name, const char* def)
        {
            return getValue (values, "Loop" + String (prefix) + name, def);
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            MemoryBlock data;
            auto numLoops = jmin (64, values.getValue ("NumSampleLoops", "0").getIntValue());

            data.setSize (roundUpSize (sizeof (SMPLChunk) + (size_t) (jmax (0, numLoops - 1)) * sizeof (SampleLoop)), true);

            auto s = static_cast<SMPLChunk*> (data.getData());

            s->manufacturer      = getValue (values, "Manufacturer", "0");
            s->product           = getValue (values, "Product", "0");
            s->samplePeriod      = getValue (values, "SamplePeriod", "0");
            s->midiUnityNote     = getValue (values, "MidiUnityNote", "60");
            s->midiPitchFraction = getValue (values, "MidiPitchFraction", "0");
            s->smpteFormat       = getValue (values, "SmpteFormat", "0");
            s->smpteOffset       = getValue (values, "SmpteOffset", "0");
            s->numSampleLoops    = ByteOrder::swapIfBigEndian ((uint32) numLoops);
            s->samplerData       = getValue (values, "SamplerData", "0");

            for (int i = 0; i < numLoops; ++i)
            {
                auto& loop = s->loops[i];
                loop.identifier = getValue (values, i, "Identifier", "0");
                loop.type       = getValue (values, i, "Type", "0");
                loop.start      = getValue (values, i, "Start", "0");
                loop.end        = getValue (values, i, "End", "0");
                loop.fraction   = getValue (values, i, "Fraction", "0");
                loop.playCount  = getValue (values, i, "PlayCount", "0");
            }

            return data;
        }
    } JUCE_PACKED;

    //==============================================================================
    struct InstChunk
    {
        int8 baseNote;
        int8 detune;
        int8 gain;
        int8 lowNote;
        int8 highNote;
        int8 lowVelocity;
        int8 highVelocity;

        static void setValue (StringPairArray& values, const char* name, int val)
        {
            values.set (name, String (val));
        }

        void copyTo (StringPairArray& values) const
        {
            setValue (values, "MidiUnityNote",  baseNote);
            setValue (values, "Detune",         detune);
            setValue (values, "Gain",           gain);
            setValue (values, "LowNote",        lowNote);
            setValue (values, "HighNote",       highNote);
            setValue (values, "LowVelocity",    lowVelocity);
            setValue (values, "HighVelocity",   highVelocity);
        }

        static int8 getValue (const StringPairArray& values, const char* name, const char* def)
        {
            return (int8) values.getValue (name, def).getIntValue();
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            MemoryBlock data;
            auto& keys = values.getAllKeys();

            if (keys.contains ("LowNote", true) && keys.contains ("HighNote", true))
            {
                data.setSize (8, true);
                auto* inst = static_cast<InstChunk*> (data.getData());

                inst->baseNote      = getValue (values, "MidiUnityNote", "60");
                inst->detune        = getValue (values, "Detune", "0");
                inst->gain          = getValue (values, "Gain", "0");
                inst->lowNote       = getValue (values, "LowNote", "0");
                inst->highNote      = getValue (values, "HighNote", "127");
                inst->lowVelocity   = getValue (values, "LowVelocity", "1");
                inst->highVelocity  = getValue (values, "HighVelocity", "127");
            }

            return data;
        }
    } JUCE_PACKED;

    //==============================================================================
    struct CueChunk
    {
        struct Cue
        {
            uint32 identifier;
            uint32 order;
            uint32 chunkID;
            uint32 chunkStart;
            uint32 blockStart;
            uint32 offset;
        } JUCE_PACKED;

        uint32 numCues;
        Cue cues[1];

        static void setValue (StringPairArray& values, int prefix, const char* name, uint32 val)
        {
            values.set ("Cue" + String (prefix) + name, String (ByteOrder::swapIfBigEndian (val)));
        }

        void copyTo (StringPairArray& values, const int totalSize) const
        {
            values.set ("NumCuePoints", String (ByteOrder::swapIfBigEndian (numCues)));

            for (int i = 0; i < (int) numCues; ++i)
            {
                if ((uint8*) (cues + (i + 1)) > ((uint8*) this) + totalSize)
                    break;

                setValue (values, i, "Identifier",  cues[i].identifier);
                setValue (values, i, "Order",       cues[i].order);
                setValue (values, i, "ChunkID",     cues[i].chunkID);
                setValue (values, i, "ChunkStart",  cues[i].chunkStart);
                setValue (values, i, "BlockStart",  cues[i].blockStart);
                setValue (values, i, "Offset",      cues[i].offset);
            }
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            MemoryBlock data;
            const int numCues = values.getValue ("NumCuePoints", "0").getIntValue();

            if (numCues > 0)
            {
                data.setSize (roundUpSize (sizeof (CueChunk) + (size_t) (numCues - 1) * sizeof (Cue)), true);

                auto c = static_cast<CueChunk*> (data.getData());

                c->numCues = ByteOrder::swapIfBigEndian ((uint32) numCues);

                const String dataChunkID (chunkName ("data"));
                int nextOrder = 0;

               #if JUCE_DEBUG
                Array<uint32> identifiers;
               #endif

                for (int i = 0; i < numCues; ++i)
                {
                    auto prefix = "Cue" + String (i);
                    auto identifier = (uint32) values.getValue (prefix + "Identifier", "0").getIntValue();

                   #if JUCE_DEBUG
                    jassert (! identifiers.contains (identifier));
                    identifiers.add (identifier);
                   #endif

                    auto order = values.getValue (prefix + "Order", String (nextOrder)).getIntValue();
                    nextOrder = jmax (nextOrder, order) + 1;

                    auto& cue = c->cues[i];
                    cue.identifier   = ByteOrder::swapIfBigEndian ((uint32) identifier);
                    cue.order        = ByteOrder::swapIfBigEndian ((uint32) order);
                    cue.chunkID      = ByteOrder::swapIfBigEndian ((uint32) values.getValue (prefix + "ChunkID", dataChunkID).getIntValue());
                    cue.chunkStart   = ByteOrder::swapIfBigEndian ((uint32) values.getValue (prefix + "ChunkStart", "0").getIntValue());
                    cue.blockStart   = ByteOrder::swapIfBigEndian ((uint32) values.getValue (prefix + "BlockStart", "0").getIntValue());
                    cue.offset       = ByteOrder::swapIfBigEndian ((uint32) values.getValue (prefix + "Offset", "0").getIntValue());
                }
            }

            return data;
        }

    } JUCE_PACKED;

    //==============================================================================
    namespace ListChunk
    {
        static int getValue (const StringPairArray& values, const String& name)
        {
            return values.getValue (name, "0").getIntValue();
        }

        static int getValue (const StringPairArray& values, const String& prefix, const char* name)
        {
            return getValue (values, prefix + name);
        }

        static void appendLabelOrNoteChunk (const StringPairArray& values, const String& prefix,
                                            const int chunkType, MemoryOutputStream& out)
        {
            auto label = values.getValue (prefix + "Text", prefix);
            auto labelLength = (int) label.getNumBytesAsUTF8() + 1;
            auto chunkLength = 4 + labelLength + (labelLength & 1);

            out.writeInt (chunkType);
            out.writeInt (chunkLength);
            out.writeInt (getValue (values, prefix, "Identifier"));
            out.write (label.toUTF8(), (size_t) labelLength);

            if ((out.getDataSize() & 1) != 0)
                out.writeByte (0);
        }

        static void appendExtraChunk (const StringPairArray& values, const String& prefix, MemoryOutputStream& out)
        {
            auto text = values.getValue (prefix + "Text", prefix);

            auto textLength = (int) text.getNumBytesAsUTF8() + 1; // include null terminator
            auto chunkLength = textLength + 20 + (textLength & 1);

            out.writeInt (chunkName ("ltxt"));
            out.writeInt (chunkLength);
            out.writeInt (getValue (values, prefix, "Identifier"));
            out.writeInt (getValue (values, prefix, "SampleLength"));
            out.writeInt (getValue (values, prefix, "Purpose"));
            out.writeShort ((short) getValue (values, prefix, "Country"));
            out.writeShort ((short) getValue (values, prefix, "Language"));
            out.writeShort ((short) getValue (values, prefix, "Dialect"));
            out.writeShort ((short) getValue (values, prefix, "CodePage"));
            out.write (text.toUTF8(), (size_t) textLength);

            if ((out.getDataSize() & 1) != 0)
                out.writeByte (0);
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            auto numCueLabels  = getValue (values, "NumCueLabels");
            auto numCueNotes   = getValue (values, "NumCueNotes");
            auto numCueRegions = getValue (values, "NumCueRegions");

            MemoryOutputStream out;

            if (numCueLabels + numCueNotes + numCueRegions > 0)
            {
                out.writeInt (chunkName ("adtl"));

                for (int i = 0; i < numCueLabels; ++i)
                    appendLabelOrNoteChunk (values, "CueLabel" + String (i), chunkName ("labl"), out);

                for (int i = 0; i < numCueNotes; ++i)
                    appendLabelOrNoteChunk (values, "CueNote" + String (i), chunkName ("note"), out);

                for (int i = 0; i < numCueRegions; ++i)
                    appendExtraChunk (values, "CueRegion" + String (i), out);
            }

            return out.getMemoryBlock();
        }
    }

    //==============================================================================
    /** Reads a RIFF List Info chunk from a stream positioned just after the size byte. */
    namespace ListInfoChunk
    {
        static const char* const types[] =
        {
            WavAudioFormat::riffInfoArchivalLocation,
            WavAudioFormat::riffInfoArtist,
            WavAudioFormat::riffInfoBaseURL,
            WavAudioFormat::riffInfoCinematographer,
            WavAudioFormat::riffInfoComment,
            WavAudioFormat::riffInfoComments,
            WavAudioFormat::riffInfoComment2,
            WavAudioFormat::riffInfoCommissioned,
            WavAudioFormat::riffInfoCopyright,
            WavAudioFormat::riffInfoCostumeDesigner,
            WavAudioFormat::riffInfoCountry,
            WavAudioFormat::riffInfoCropped,
            WavAudioFormat::riffInfoDateCreated,
            WavAudioFormat::riffInfoDateTimeOriginal,
            WavAudioFormat::riffInfoDefaultAudioStream,
            WavAudioFormat::riffInfoDimension,
            WavAudioFormat::riffInfoDirectory,
            WavAudioFormat::riffInfoDistributedBy,
            WavAudioFormat::riffInfoDotsPerInch,
            WavAudioFormat::riffInfoEditedBy,
            WavAudioFormat::riffInfoEighthLanguage,
            WavAudioFormat::riffInfoEncodedBy,
            WavAudioFormat::riffInfoEndTimecode,
            WavAudioFormat::riffInfoEngineer,
            WavAudioFormat::riffInfoFifthLanguage,
            WavAudioFormat::riffInfoFirstLanguage,
            WavAudioFormat::riffInfoFourthLanguage,
            WavAudioFormat::riffInfoGenre,
            WavAudioFormat::riffInfoKeywords,
            WavAudioFormat::riffInfoLanguage,
            WavAudioFormat::riffInfoLength,
            WavAudioFormat::riffInfoLightness,
            WavAudioFormat::riffInfoLocation,
            WavAudioFormat::riffInfoLogoIconURL,
            WavAudioFormat::riffInfoLogoURL,
            WavAudioFormat::riffInfoMedium,
            WavAudioFormat::riffInfoMoreInfoBannerImage,
            WavAudioFormat::riffInfoMoreInfoBannerURL,
            WavAudioFormat::riffInfoMoreInfoText,
            WavAudioFormat::riffInfoMoreInfoURL,
            WavAudioFormat::riffInfoMusicBy,
            WavAudioFormat::riffInfoNinthLanguage,
            WavAudioFormat::riffInfoNumberOfParts,
            WavAudioFormat::riffInfoOrganisation,
            WavAudioFormat::riffInfoPart,
            WavAudioFormat::riffInfoProducedBy,
            WavAudioFormat::riffInfoProductName,
            WavAudioFormat::riffInfoProductionDesigner,
            WavAudioFormat::riffInfoProductionStudio,
            WavAudioFormat::riffInfoRate,
            WavAudioFormat::riffInfoRated,
            WavAudioFormat::riffInfoRating,
            WavAudioFormat::riffInfoRippedBy,
            WavAudioFormat::riffInfoSecondaryGenre,
            WavAudioFormat::riffInfoSecondLanguage,
            WavAudioFormat::riffInfoSeventhLanguage,
            WavAudioFormat::riffInfoSharpness,
            WavAudioFormat::riffInfoSixthLanguage,
            WavAudioFormat::riffInfoSoftware,
            WavAudioFormat::riffInfoSoundSchemeTitle,
            WavAudioFormat::riffInfoSource,
            WavAudioFormat::riffInfoSourceFrom,
            WavAudioFormat::riffInfoStarring_ISTR,
            WavAudioFormat::riffInfoStarring_STAR,
            WavAudioFormat::riffInfoStartTimecode,
            WavAudioFormat::riffInfoStatistics,
            WavAudioFormat::riffInfoSubject,
            WavAudioFormat::riffInfoTapeName,
            WavAudioFormat::riffInfoTechnician,
            WavAudioFormat::riffInfoThirdLanguage,
            WavAudioFormat::riffInfoTimeCode,
            WavAudioFormat::riffInfoTitle,
            WavAudioFormat::riffInfoTrackNo,
            WavAudioFormat::riffInfoTrackNumber,
            WavAudioFormat::riffInfoURL,
            WavAudioFormat::riffInfoVegasVersionMajor,
            WavAudioFormat::riffInfoVegasVersionMinor,
            WavAudioFormat::riffInfoVersion,
            WavAudioFormat::riffInfoWatermarkURL,
            WavAudioFormat::riffInfoWrittenBy,
            WavAudioFormat::riffInfoYear
        };

        static bool isMatchingTypeIgnoringCase (const int value, const char* const name) noexcept
        {
            for (int i = 0; i < 4; ++i)
                if ((juce_wchar) name[i] != CharacterFunctions::toUpperCase ((juce_wchar) ((value >> (i * 8)) & 0xff)))
                    return false;

            return true;
        }

        static void addToMetadata (StringPairArray& values, InputStream& input, int64 chunkEnd)
        {
            while (input.getPosition() < chunkEnd)
            {
                auto infoType = input.readInt();
                auto infoLength = chunkEnd - input.getPosition();

                if (infoLength > 0)
                {
                    infoLength = jmin (infoLength, (int64) input.readInt());

                    if (infoLength <= 0)
                        return;

                    for (auto& type : types)
                    {
                        if (isMatchingTypeIgnoringCase (infoType, type))
                        {
                            MemoryBlock mb;
                            input.readIntoMemoryBlock (mb, (ssize_t) infoLength);
                            values.set (type, String::createStringFromData ((const char*) mb.getData(),
                                                                            (int) mb.getSize()));
                            break;
                        }
                    }
                }
            }
        }

        static bool writeValue (const StringPairArray& values, MemoryOutputStream& out, const char* paramName)
        {
            auto value = values.getValue (paramName, {});

            if (value.isEmpty())
                return false;

            auto valueLength = (int) value.getNumBytesAsUTF8() + 1;
            auto chunkLength = valueLength + (valueLength & 1);

            out.writeInt (chunkName (paramName));
            out.writeInt (chunkLength);
            out.write (value.toUTF8(), (size_t) valueLength);

            if ((out.getDataSize() & 1) != 0)
                out.writeByte (0);

            return true;
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            MemoryOutputStream out;
            out.writeInt (chunkName ("INFO"));
            bool anyParamsDefined = false;

            for (auto& type : types)
                if (writeValue (values, out, type))
                    anyParamsDefined = true;

            return anyParamsDefined ? out.getMemoryBlock() : MemoryBlock();
        }
    }

    //==============================================================================
    struct AcidChunk
    {
        /** Reads an acid RIFF chunk from a stream positioned just after the size byte. */
        AcidChunk (InputStream& input, size_t length)
        {
            zerostruct (*this);
            input.read (this, (int) jmin (sizeof (*this), length));
        }

        AcidChunk (const StringPairArray& values)
        {
            zerostruct (*this);

            flags = getFlagIfPresent (values, WavAudioFormat::acidOneShot,   0x01)
                  | getFlagIfPresent (values, WavAudioFormat::acidRootSet,   0x02)
                  | getFlagIfPresent (values, WavAudioFormat::acidStretch,   0x04)
                  | getFlagIfPresent (values, WavAudioFormat::acidDiskBased, 0x08)
                  | getFlagIfPresent (values, WavAudioFormat::acidizerFlag,  0x10);

            if (values[WavAudioFormat::acidRootSet].getIntValue() != 0)
                rootNote = ByteOrder::swapIfBigEndian ((uint16) values[WavAudioFormat::acidRootNote].getIntValue());

            numBeats          = ByteOrder::swapIfBigEndian ((uint32) values[WavAudioFormat::acidBeats].getIntValue());
            meterDenominator  = ByteOrder::swapIfBigEndian ((uint16) values[WavAudioFormat::acidDenominator].getIntValue());
            meterNumerator    = ByteOrder::swapIfBigEndian ((uint16) values[WavAudioFormat::acidNumerator].getIntValue());

            if (values.containsKey (WavAudioFormat::acidTempo))
                tempo = swapFloatByteOrder (values[WavAudioFormat::acidTempo].getFloatValue());
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            return AcidChunk (values).toMemoryBlock();
        }

        MemoryBlock toMemoryBlock() const
        {
            return (flags != 0 || rootNote != 0 || numBeats != 0 || meterDenominator != 0 || meterNumerator != 0)
                      ? MemoryBlock (this, sizeof (*this)) : MemoryBlock();
        }

        void addToMetadata (StringPairArray& values) const
        {
            setBoolFlag (values, WavAudioFormat::acidOneShot,   0x01);
            setBoolFlag (values, WavAudioFormat::acidRootSet,   0x02);
            setBoolFlag (values, WavAudioFormat::acidStretch,   0x04);
            setBoolFlag (values, WavAudioFormat::acidDiskBased, 0x08);
            setBoolFlag (values, WavAudioFormat::acidizerFlag,  0x10);

            if (flags & 0x02) // root note set
                values.set (WavAudioFormat::acidRootNote, String (ByteOrder::swapIfBigEndian (rootNote)));

            values.set (WavAudioFormat::acidBeats,       String (ByteOrder::swapIfBigEndian (numBeats)));
            values.set (WavAudioFormat::acidDenominator, String (ByteOrder::swapIfBigEndian (meterDenominator)));
            values.set (WavAudioFormat::acidNumerator,   String (ByteOrder::swapIfBigEndian (meterNumerator)));
            values.set (WavAudioFormat::acidTempo,       String (swapFloatByteOrder (tempo)));
        }

        void setBoolFlag (StringPairArray& values, const char* name, uint32 mask) const
        {
            values.set (name, (flags & ByteOrder::swapIfBigEndian (mask)) ? "1" : "0");
        }

        static uint32 getFlagIfPresent (const StringPairArray& values, const char* name, uint32 flag)
        {
            return values[name].getIntValue() != 0 ? ByteOrder::swapIfBigEndian (flag) : 0;
        }

        static float swapFloatByteOrder (const float x) noexcept
        {
           #ifdef JUCE_BIG_ENDIAN
            union { uint32 asInt; float asFloat; } n;
            n.asFloat = x;
            n.asInt = ByteOrder::swap (n.asInt);
            return n.asFloat;
           #else
            return x;
           #endif
        }

        uint32 flags;
        uint16 rootNote;
        uint16 reserved1;
        float reserved2;
        uint32 numBeats;
        uint16 meterDenominator;
        uint16 meterNumerator;
        float tempo;

    } JUCE_PACKED;

    //==============================================================================
    struct TracktionChunk
    {
        static MemoryBlock createFrom (const StringPairArray& values)
        {
            MemoryOutputStream out;
            auto s = values[WavAudioFormat::tracktionLoopInfo];

            if (s.isNotEmpty())
            {
                out.writeString (s);

                if ((out.getDataSize() & 1) != 0)
                    out.writeByte (0);
            }

            return out.getMemoryBlock();
        }
    };

    //==============================================================================
    namespace AXMLChunk
    {
        static void addToMetadata (StringPairArray& destValues, const String& source)
        {
            ScopedPointer<XmlElement> xml (XmlDocument::parse (source));

            if (xml != nullptr && xml->hasTagName ("ebucore:ebuCoreMain"))
            {
                if (auto* xml2 = xml->getChildByName ("ebucore:coreMetadata"))
                {
                    if (auto* xml3 = xml2->getChildByName ("ebucore:identifier"))
                    {
                        if (auto* xml4 = xml3->getChildByName ("dc:identifier"))
                        {
                            auto ISRCCode = xml4->getAllSubText().fromFirstOccurrenceOf ("ISRC:", false, true);

                            if (ISRCCode.isNotEmpty())
                                destValues.set (WavAudioFormat::ISRC, ISRCCode);
                        }
                    }
                }
            }
        }

        static MemoryBlock createFrom (const StringPairArray& values)
        {
            auto ISRC = values.getValue (WavAudioFormat::ISRC, {});
            MemoryOutputStream xml;

            if (ISRC.isNotEmpty())
            {
                xml << "<ebucore:ebuCoreMain xmlns:dc=\" http://purl.org/dc/elements/1.1/\" "
                                            "xmlns:ebucore=\"urn:ebu:metadata-schema:ebuCore_2012\">"
                         "<ebucore:coreMetadata>"
                           "<ebucore:identifier typeLabel=\"GUID\" "
                                               "typeDefinition=\"Globally Unique Identifier\" "
                                               "formatLabel=\"ISRC\" "
                                               "formatDefinition=\"International Standard Recording Code\" "
                                               "formatLink=\"http://www.ebu.ch/metadata/cs/ebu_IdentifierTypeCodeCS.xml#3.7\">"
                             "<dc:identifier>ISRC:" << ISRC << "</dc:identifier>"
                           "</ebucore:identifier>"
                         "</ebucore:coreMetadata>"
                       "</ebucore:ebuCoreMain>";

                xml.writeRepeatedByte (0, xml.getDataSize());  // ensures even size, null termination and room for future growing
            }

            return xml.getMemoryBlock();
        }
    }

    //==============================================================================
    struct ExtensibleWavSubFormat
    {
        uint32 data1;
        uint16 data2;
        uint16 data3;
        uint8  data4[8];

        bool operator== (const ExtensibleWavSubFormat& other) const noexcept   { return memcmp (this, &other, sizeof (*this)) == 0; }
        bool operator!= (const ExtensibleWavSubFormat& other) const noexcept   { return ! operator== (other); }

    } JUCE_PACKED;

    static const ExtensibleWavSubFormat pcmFormat       = { 0x00000001, 0x0000, 0x0010, { 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 } };
    static const ExtensibleWavSubFormat IEEEFloatFormat = { 0x00000003, 0x0000, 0x0010, { 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 } };
    static const ExtensibleWavSubFormat ambisonicFormat = { 0x00000001, 0x0721, 0x11d3, { 0x86, 0x44, 0xC8, 0xC1, 0xCA, 0x00, 0x00, 0x00 } };

    struct DataSize64Chunk   // chunk ID = 'ds64' if data size > 0xffffffff, 'JUNK' otherwise
    {
        uint32 riffSizeLow;     // low 4 byte size of RF64 block
        uint32 riffSizeHigh;    // high 4 byte size of RF64 block
        uint32 dataSizeLow;     // low 4 byte size of data chunk
        uint32 dataSizeHigh;    // high 4 byte size of data chunk
        uint32 sampleCountLow;  // low 4 byte sample count of fact chunk
        uint32 sampleCountHigh; // high 4 byte sample count of fact chunk
        uint32 tableLength;     // number of valid entries in array 'table'
    } JUCE_PACKED;

    #if JUCE_MSVC
     #pragma pack (pop)
    #endif
}

//==============================================================================
class WavAudioFormatReader  : public AudioFormatReader
{
public:
    WavAudioFormatReader (InputStream* in)  : AudioFormatReader (in, wavFormatName)
    {
        using namespace WavFileHelpers;
        uint64 len = 0, end = 0;
        int cueNoteIndex = 0;
        int cueLabelIndex = 0;
        int cueRegionIndex = 0;

        auto streamStartPos = input->getPosition();
        auto firstChunkType = input->readInt();

        if (firstChunkType == chunkName ("RF64"))
        {
            input->skipNextBytes (4); // size is -1 for RF64
            isRF64 = true;
        }
        else if (firstChunkType == chunkName ("RIFF"))
        {
            len = (uint64) (uint32) input->readInt();
            end = len + (uint64) input->getPosition();
        }
        else
        {
            return;
        }

        auto startOfRIFFChunk = input->getPosition();

        if (input->readInt() == chunkName ("WAVE"))
        {
            if (isRF64 && input->readInt() == chunkName ("ds64"))
            {
                auto length = (uint32) input->readInt();

                if (length < 28)
                    return;

                auto chunkEnd = input->getPosition() + length + (length & 1);
                len = (uint64) input->readInt64();
                end = len + (uint64) startOfRIFFChunk;
                dataLength = input->readInt64();
                input->setPosition (chunkEnd);
            }

            while ((uint64) input->getPosition() < end && ! input->isExhausted())
            {
                auto chunkType = input->readInt();
                auto length = (uint32) input->readInt();
                auto chunkEnd = input->getPosition() + length + (length & 1);

                if (chunkType == chunkName ("fmt "))
                {
                    // read the format chunk
                    auto format = (unsigned short) input->readShort();
                    numChannels = (unsigned int) input->readShort();
                    sampleRate = input->readInt();
                    auto bytesPerSec = input->readInt();
                    input->skipNextBytes (2);
                    bitsPerSample = (unsigned int) (int) input->readShort();

                    if (bitsPerSample > 64)
                    {
                        bytesPerFrame = bytesPerSec / (int) sampleRate;
                        bitsPerSample = 8 * (unsigned int) bytesPerFrame / numChannels;
                    }
                    else
                    {
                        bytesPerFrame = numChannels * bitsPerSample / 8;
                    }

                    if (format == 3)
                    {
                        usesFloatingPointData = true;
                    }
                    else if (format == 0xfffe) // WAVE_FORMAT_EXTENSIBLE
                    {
                        if (length < 40) // too short
                        {
                            bytesPerFrame = 0;
                        }
                        else
                        {
                            input->skipNextBytes (4); // skip over size and bitsPerSample
                            auto channelMask = input->readInt();
                            metadataValues.set ("ChannelMask", String (channelMask));
                            channelLayout = getChannelLayoutFromMask (channelMask, numChannels);

                            ExtensibleWavSubFormat subFormat;
                            subFormat.data1 = (uint32) input->readInt();
                            subFormat.data2 = (uint16) input->readShort();
                            subFormat.data3 = (uint16) input->readShort();
                            input->read (subFormat.data4, sizeof (subFormat.data4));

                            if (subFormat == IEEEFloatFormat)
                                usesFloatingPointData = true;
                            else if (subFormat != pcmFormat && subFormat != ambisonicFormat)
                                bytesPerFrame = 0;
                        }
                    }
                    else if (format == 0x674f  // WAVE_FORMAT_OGG_VORBIS_MODE_1
                          || format == 0x6750  // WAVE_FORMAT_OGG_VORBIS_MODE_2
                          || format == 0x6751  // WAVE_FORMAT_OGG_VORBIS_MODE_3
                          || format == 0x676f  // WAVE_FORMAT_OGG_VORBIS_MODE_1_PLUS
                          || format == 0x6770  // WAVE_FORMAT_OGG_VORBIS_MODE_2_PLUS
                          || format == 0x6771) // WAVE_FORMAT_OGG_VORBIS_MODE_3_PLUS
                    {
                        isSubformatOggVorbis = true;
                        sampleRate = 0; // to mark the wav reader as failed
                        input->setPosition (streamStartPos);
                        return;
                    }
                    else if (format != 1)
                    {
                        bytesPerFrame = 0;
                    }
                }
                else if (chunkType == chunkName ("data"))
                {
                    if (! isRF64) // data size is expected to be -1, actual data size is in ds64 chunk
                        dataLength = length;

                    dataChunkStart = input->getPosition();
                    lengthInSamples = (bytesPerFrame > 0) ? (dataLength / bytesPerFrame) : 0;
                }
                else if (chunkType == chunkName ("bext"))
                {
                    bwavChunkStart = input->getPosition();
                    bwavSize = length;

                    HeapBlock<BWAVChunk> bwav;
                    bwav.calloc (jmax ((size_t) length + 1, sizeof (BWAVChunk)), 1);
                    input->read (bwav, (int) length);
                    bwav->copyTo (metadataValues, (int) length);
                }
                else if (chunkType == chunkName ("smpl"))
                {
                    HeapBlock<SMPLChunk> smpl;
                    smpl.calloc (jmax ((size_t) length + 1, sizeof (SMPLChunk)), 1);
                    input->read (smpl, (int) length);
                    smpl->copyTo (metadataValues, (int) length);
                }
                else if (chunkType == chunkName ("inst") || chunkType == chunkName ("INST")) // need to check which...
                {
                    HeapBlock<InstChunk> inst;
                    inst.calloc (jmax ((size_t) length + 1, sizeof (InstChunk)), 1);
                    input->read (inst, (int) length);
                    inst->copyTo (metadataValues);
                }
                else if (chunkType == chunkName ("cue "))
                {
                    HeapBlock<CueChunk> cue;
                    cue.calloc (jmax ((size_t) length + 1, sizeof (CueChunk)), 1);
                    input->read (cue, (int) length);
                    cue->copyTo (metadataValues, (int) length);
                }
                else if (chunkType == chunkName ("axml"))
                {
                    MemoryBlock axml;
                    input->readIntoMemoryBlock (axml, (ssize_t) length);
                    AXMLChunk::addToMetadata (metadataValues, axml.toString());
                }
                else if (chunkType == chunkName ("LIST"))
                {
                    auto subChunkType = input->readInt();

                    if (subChunkType == chunkName ("info") || subChunkType == chunkName ("INFO"))
                    {
                        ListInfoChunk::addToMetadata (metadataValues, *input, chunkEnd);
                    }
                    else if (subChunkType == chunkName ("adtl"))
                    {
                        while (input->getPosition() < chunkEnd)
                        {
                            auto adtlChunkType = input->readInt();
                            auto adtlLength = (uint32) input->readInt();
                            auto adtlChunkEnd = input->getPosition() + (adtlLength + (adtlLength & 1));

                            if (adtlChunkType == chunkName ("labl") || adtlChunkType == chunkName ("note"))
                            {
                                String prefix;

                                if (adtlChunkType == chunkName ("labl"))
                                    prefix << "CueLabel" << cueLabelIndex++;
                                else if (adtlChunkType == chunkName ("note"))
                                    prefix << "CueNote" << cueNoteIndex++;

                                auto identifier = (uint32) input->readInt();
                                auto stringLength = (int) adtlLength - 4;

                                MemoryBlock textBlock;
                                input->readIntoMemoryBlock (textBlock, stringLength);

                                metadataValues.set (prefix + "Identifier", String (identifier));
                                metadataValues.set (prefix + "Text", textBlock.toString());
                            }
                            else if (adtlChunkType == chunkName ("ltxt"))
                            {
                                auto prefix = "CueRegion" + String (cueRegionIndex++);
                                auto identifier     = (uint32) input->readInt();
                                auto sampleLength   = (uint32) input->readInt();
                                auto purpose        = (uint32) input->readInt();
                                auto country        = (uint16) input->readInt();
                                auto language       = (uint16) input->readInt();
                                auto dialect        = (uint16) input->readInt();
                                auto codePage       = (uint16) input->readInt();
                                auto stringLength   = adtlLength - 20;

                                MemoryBlock textBlock;
                                input->readIntoMemoryBlock (textBlock, (int) stringLength);

                                metadataValues.set (prefix + "Identifier",   String (identifier));
                                metadataValues.set (prefix + "SampleLength", String (sampleLength));
                                metadataValues.set (prefix + "Purpose",      String (purpose));
                                metadataValues.set (prefix + "Country",      String (country));
                                metadataValues.set (prefix + "Language",     String (language));
                                metadataValues.set (prefix + "Dialect",      String (dialect));
                                metadataValues.set (prefix + "CodePage",     String (codePage));
                                metadataValues.set (prefix + "Text",         textBlock.toString());
                            }

                            input->setPosition (adtlChunkEnd);
                        }
                    }
                }
                else if (chunkType == chunkName ("acid"))
                {
                    AcidChunk (*input, length).addToMetadata (metadataValues);
                }
                else if (chunkType == chunkName ("Trkn"))
                {
                    MemoryBlock tracktion;
                    input->readIntoMemoryBlock (tracktion, (ssize_t) length);
                    metadataValues.set (WavAudioFormat::tracktionLoopInfo, tracktion.toString());
                }
                else if (chunkEnd <= input->getPosition())
                {
                    break;
                }

                input->setPosition (chunkEnd);
            }
        }

        if (cueLabelIndex > 0)          metadataValues.set ("NumCueLabels",    String (cueLabelIndex));
        if (cueNoteIndex > 0)           metadataValues.set ("NumCueNotes",     String (cueNoteIndex));
        if (cueRegionIndex > 0)         metadataValues.set ("NumCueRegions",   String (cueRegionIndex));
        if (metadataValues.size() > 0)  metadataValues.set ("MetaDataSource",  "WAV");
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
            char tempBuffer[tempBufSize];

            auto numThisTime = jmin (tempBufSize / bytesPerFrame, numSamples);
            auto bytesRead = input->read (tempBuffer, numThisTime * bytesPerFrame);

            if (bytesRead < numThisTime * bytesPerFrame)
            {
                jassert (bytesRead >= 0);
                zeromem (tempBuffer + bytesRead, (size_t) (numThisTime * bytesPerFrame - bytesRead));
            }

            copySampleData (bitsPerSample, usesFloatingPointData,
                            destSamples, startOffsetInDestBuffer, numDestChannels,
                            tempBuffer, (int) numChannels, numThisTime);

            startOffsetInDestBuffer += numThisTime;
            numSamples -= numThisTime;
        }

        return true;
    }

    static void copySampleData (unsigned int bitsPerSample, const bool usesFloatingPointData,
                                int* const* destSamples, int startOffsetInDestBuffer, int numDestChannels,
                                const void* sourceData, int numChannels, int numSamples) noexcept
    {
        switch (bitsPerSample)
        {
            case 8:     ReadHelper<AudioData::Int32, AudioData::UInt8, AudioData::LittleEndian>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples); break;
            case 16:    ReadHelper<AudioData::Int32, AudioData::Int16, AudioData::LittleEndian>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples); break;
            case 24:    ReadHelper<AudioData::Int32, AudioData::Int24, AudioData::LittleEndian>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples); break;
            case 32:    if (usesFloatingPointData) ReadHelper<AudioData::Float32, AudioData::Float32, AudioData::LittleEndian>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples);
                        else                       ReadHelper<AudioData::Int32,   AudioData::Int32,   AudioData::LittleEndian>::read (destSamples, startOffsetInDestBuffer, numDestChannels, sourceData, numChannels, numSamples);
                        break;
            default:    jassertfalse; break;
        }
    }

    //==============================================================================
    AudioChannelSet getChannelLayout() override
    {
        if (channelLayout.size() == static_cast<int> (numChannels))
            return channelLayout;

        return WavFileHelpers::canonicalWavChannelSet (static_cast<int> (numChannels));
    }

    static AudioChannelSet getChannelLayoutFromMask (int dwChannelMask, size_t totalNumChannels)
    {
        AudioChannelSet wavFileChannelLayout;

        // AudioChannelSet and wav's dwChannelMask are compatible
        BigInteger channelBits (dwChannelMask);

        for (auto bit = channelBits.findNextSetBit (0); bit >= 0; bit = channelBits.findNextSetBit (bit + 1))
            wavFileChannelLayout.addChannel (static_cast<AudioChannelSet::ChannelType> (bit + 1));

        // channel layout and number of channels do not match
        if (wavFileChannelLayout.size() != static_cast<int> (totalNumChannels))
        {
            // for backward compatibility with old wav files, assume 1 or 2
            // channel wav files are mono/stereo respectively
            if (totalNumChannels <= 2 && dwChannelMask == 0)
                wavFileChannelLayout = AudioChannelSet::canonicalChannelSet (static_cast<int> (totalNumChannels));
            else
            {
                auto discreteSpeaker = static_cast<int> (AudioChannelSet::discreteChannel0);

                while (wavFileChannelLayout.size() < static_cast<int> (totalNumChannels))
                    wavFileChannelLayout.addChannel (static_cast<AudioChannelSet::ChannelType> (discreteSpeaker++));
            }
        }

        return wavFileChannelLayout;
    }

    int64 bwavChunkStart = 0, bwavSize = 0;
    int64 dataChunkStart = 0, dataLength = 0;
    int bytesPerFrame = 0;
    bool isRF64 = false;
    bool isSubformatOggVorbis = false;

    AudioChannelSet channelLayout;

private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WavAudioFormatReader)
};

//==============================================================================
class WavAudioFormatWriter  : public AudioFormatWriter
{
public:
    WavAudioFormatWriter (OutputStream* const out, const double rate,
                          const AudioChannelSet& channelLayoutToUse, const unsigned int bits,
                          const StringPairArray& metadataValues)
        : AudioFormatWriter (out, wavFormatName, rate, channelLayoutToUse, bits)
    {
        using namespace WavFileHelpers;

        if (metadataValues.size() > 0)
        {
            // The meta data should have been sanitised for the WAV format.
            // If it was originally sourced from an AIFF file the MetaDataSource
            // key should be removed (or set to "WAV") once this has been done
            jassert (metadataValues.getValue ("MetaDataSource", "None") != "AIFF");

            bwavChunk     = BWAVChunk::createFrom (metadataValues);
            axmlChunk     = AXMLChunk::createFrom (metadataValues);
            smplChunk     = SMPLChunk::createFrom (metadataValues);
            instChunk     = InstChunk::createFrom (metadataValues);
            cueChunk      = CueChunk ::createFrom (metadataValues);
            listChunk     = ListChunk::createFrom (metadataValues);
            listInfoChunk = ListInfoChunk::createFrom (metadataValues);
            acidChunk     = AcidChunk::createFrom (metadataValues);
            trckChunk     = TracktionChunk::createFrom (metadataValues);
        }

        headerPosition = out->getPosition();
        writeHeader();
    }

    ~WavAudioFormatWriter()
    {
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
            case 8:     WriteHelper<AudioData::UInt8, AudioData::Int32, AudioData::LittleEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            case 16:    WriteHelper<AudioData::Int16, AudioData::Int32, AudioData::LittleEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            case 24:    WriteHelper<AudioData::Int24, AudioData::Int32, AudioData::LittleEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            case 32:    WriteHelper<AudioData::Int32, AudioData::Int32, AudioData::LittleEndian>::write (tempBlock.getData(), (int) numChannels, data, numSamples); break;
            default:    jassertfalse; break;
        }

        if (! output->write (tempBlock.getData(), bytes))
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

    bool flush() override
    {
        auto lastWritePos = output->getPosition();
        writeHeader();

        if (output->setPosition (lastWritePos))
            return true;

        // if this fails, you've given it an output stream that can't seek! It needs
        // to be able to seek back to write the header
        jassertfalse;
        return false;
    }

private:
    MemoryBlock tempBlock, bwavChunk, axmlChunk, smplChunk, instChunk, cueChunk, listChunk, listInfoChunk, acidChunk, trckChunk;
    uint64 lengthInSamples = 0, bytesWritten = 0;
    int64 headerPosition = 0;
    bool writeFailed = false;

    void writeHeader()
    {
        if ((bytesWritten & 1) != 0) // pad to an even length
            output->writeByte (0);

        using namespace WavFileHelpers;

        if (headerPosition != output->getPosition() && ! output->setPosition (headerPosition))
        {
            // if this fails, you've given it an output stream that can't seek! It needs to be
            // able to seek back to go back and write the header after the data has been written.
            jassertfalse;
            return;
        }

        const size_t bytesPerFrame = numChannels * bitsPerSample / 8;
        uint64 audioDataSize = bytesPerFrame * lengthInSamples;
        auto channelMask = getChannelMaskFromChannelLayout (channelLayout);

        const bool isRF64 = (bytesWritten >= 0x100000000LL);
        const bool isWaveFmtEx = isRF64 || (channelMask != 0);

        int64 riffChunkSize = (int64) (4 /* 'RIFF' */ + 8 + 40 /* WAVEFORMATEX */
                                       + 8 + audioDataSize + (audioDataSize & 1)
                                       + chunkSize (bwavChunk)
                                       + chunkSize (axmlChunk)
                                       + chunkSize (smplChunk)
                                       + chunkSize (instChunk)
                                       + chunkSize (cueChunk)
                                       + chunkSize (listChunk)
                                       + chunkSize (listInfoChunk)
                                       + chunkSize (acidChunk)
                                       + chunkSize (trckChunk)
                                       + (8 + 28)); // (ds64 chunk)

        riffChunkSize += (riffChunkSize & 1);

        if (isRF64)
            writeChunkHeader (chunkName ("RF64"), -1);
        else
            writeChunkHeader (chunkName ("RIFF"), (int) riffChunkSize);

        output->writeInt (chunkName ("WAVE"));

        if (! isRF64)
        {
           #if ! JUCE_WAV_DO_NOT_PAD_HEADER_SIZE
            /* NB: This junk chunk is added for padding, so that the header is a fixed size
               regardless of whether it's RF64 or not. That way, we can begin recording a file,
               and when it's finished, can go back and write either a RIFF or RF64 header,
               depending on whether more than 2^32 samples were written.

               The JUCE_WAV_DO_NOT_PAD_HEADER_SIZE macro allows you to disable this feature in case
               you need to create files for crappy WAV players with bugs that stop them skipping chunks
               which they don't recognise. But DO NOT USE THIS option unless you really have no choice,
               because it means that if you write more than 2^32 samples to the file, you'll corrupt it.
            */
            writeChunkHeader (chunkName ("JUNK"), 28 + (isWaveFmtEx? 0 : 24));
            output->writeRepeatedByte (0, 28 /* ds64 */ + (isWaveFmtEx? 0 : 24));
           #endif
        }
        else
        {
           #if JUCE_WAV_DO_NOT_PAD_HEADER_SIZE
            // If you disable padding, then you MUST NOT write more than 2^32 samples to a file.
            jassertfalse;
           #endif

            writeChunkHeader (chunkName ("ds64"), 28);  // chunk size for uncompressed data (no table)
            output->writeInt64 (riffChunkSize);
            output->writeInt64 ((int64) audioDataSize);
            output->writeRepeatedByte (0, 12);
        }

        if (isWaveFmtEx)
        {
            writeChunkHeader (chunkName ("fmt "), 40);
            output->writeShort ((short) (uint16) 0xfffe); // WAVE_FORMAT_EXTENSIBLE
        }
        else
        {
            writeChunkHeader (chunkName ("fmt "), 16);
            output->writeShort (bitsPerSample < 32 ? (short) 1 /*WAVE_FORMAT_PCM*/
                                                   : (short) 3 /*WAVE_FORMAT_IEEE_FLOAT*/);
        }

        output->writeShort ((short) numChannels);
        output->writeInt ((int) sampleRate);
        output->writeInt ((int) (bytesPerFrame * sampleRate)); // nAvgBytesPerSec
        output->writeShort ((short) bytesPerFrame); // nBlockAlign
        output->writeShort ((short) bitsPerSample); // wBitsPerSample

        if (isWaveFmtEx)
        {
            output->writeShort (22); // cbSize (size of the extension)
            output->writeShort ((short) bitsPerSample); // wValidBitsPerSample
            output->writeInt (channelMask);

            const ExtensibleWavSubFormat& subFormat = bitsPerSample < 32 ? pcmFormat : IEEEFloatFormat;

            output->writeInt ((int) subFormat.data1);
            output->writeShort ((short) subFormat.data2);
            output->writeShort ((short) subFormat.data3);
            output->write (subFormat.data4, sizeof (subFormat.data4));
        }

        writeChunk (bwavChunk,     chunkName ("bext"));
        writeChunk (axmlChunk,     chunkName ("axml"));
        writeChunk (smplChunk,     chunkName ("smpl"));
        writeChunk (instChunk,     chunkName ("inst"), 7);
        writeChunk (cueChunk,      chunkName ("cue "));
        writeChunk (listChunk,     chunkName ("LIST"));
        writeChunk (listInfoChunk, chunkName ("LIST"));
        writeChunk (acidChunk,     chunkName ("acid"));
        writeChunk (trckChunk,     chunkName ("Trkn"));

        writeChunkHeader (chunkName ("data"), isRF64 ? -1 : (int) (lengthInSamples * bytesPerFrame));

        usesFloatingPointData = (bitsPerSample == 32);
    }

    static size_t chunkSize (const MemoryBlock& data) noexcept     { return data.getSize() > 0 ? (8 + data.getSize()) : 0; }

    void writeChunkHeader (int chunkType, int size) const
    {
        output->writeInt (chunkType);
        output->writeInt (size);
    }

    void writeChunk (const MemoryBlock& data, int chunkType, int size = 0) const
    {
        if (data.getSize() > 0)
        {
            writeChunkHeader (chunkType, size != 0 ? size : (int) data.getSize());
            *output << data;
        }
    }

    static int getChannelMaskFromChannelLayout (const AudioChannelSet& channelLayout)
    {
        if (channelLayout.isDiscreteLayout())
            return 0;

        // Don't add an extended format chunk for mono and stereo. Basically, all wav players
        // interpret a wav file with only one or two channels to be mono or stereo anyway.
        if (channelLayout == AudioChannelSet::mono() || channelLayout == AudioChannelSet::stereo())
            return 0;

        auto channels = channelLayout.getChannelTypes();
        auto wavChannelMask = 0;

        for (auto channel : channels)
        {
            int wavChannelBit = static_cast<int> (channel) - 1;
            jassert (wavChannelBit >= 0 && wavChannelBit <= 31);

            wavChannelMask |= (1 << wavChannelBit);
        }

        return wavChannelMask;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WavAudioFormatWriter)
};

//==============================================================================
class MemoryMappedWavReader   : public MemoryMappedAudioFormatReader
{
public:
    MemoryMappedWavReader (const File& wavFile, const WavAudioFormatReader& reader)
        : MemoryMappedAudioFormatReader (wavFile, reader, reader.dataChunkStart,
                                         reader.dataLength, reader.bytesPerFrame)
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

        WavAudioFormatReader::copySampleData (bitsPerSample, usesFloatingPointData,
                                              destSamples, startOffsetInDestBuffer, numDestChannels,
                                              sampleToPointer (startSampleInFile), (int) numChannels, numSamples);
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

        auto dest = &result;
        auto source = sampleToPointer (sample);

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

    void readMaxLevels (int64 startSampleInFile, int64 numSamples, Range<float>* results, int numChannelsToRead) override
    {
        numSamples = jmin (numSamples, lengthInSamples - startSampleInFile);

        if (map == nullptr || numSamples <= 0 || ! mappedSection.contains (Range<int64> (startSampleInFile, startSampleInFile + numSamples)))
        {
            jassert (numSamples <= 0); // you must make sure that the window contains all the samples you're going to attempt to read.

            for (int i = 0; i < numChannelsToRead; ++i)
                results[i] = {};

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
    template <typename SampleType>
    void scanMinAndMax (int64 startSampleInFile, int64 numSamples, Range<float>* results, int numChannelsToRead) const noexcept
    {
        for (int i = 0; i < numChannelsToRead; ++i)
            results[i] = scanMinAndMaxInterleaved<SampleType, AudioData::LittleEndian> (i, startSampleInFile, numSamples);
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MemoryMappedWavReader)
};

//==============================================================================
WavAudioFormat::WavAudioFormat()  : AudioFormat (wavFormatName, ".wav .bwf") {}
WavAudioFormat::~WavAudioFormat() {}

Array<int> WavAudioFormat::getPossibleSampleRates()
{
    return { 8000,  11025, 12000, 16000,  22050,  32000,  44100,
             48000, 88200, 96000, 176400, 192000, 352800, 384000 };
}

Array<int> WavAudioFormat::getPossibleBitDepths()
{
    return { 8, 16, 24, 32 };
}

bool WavAudioFormat::canDoStereo()  { return true; }
bool WavAudioFormat::canDoMono()    { return true; }

bool WavAudioFormat::isChannelLayoutSupported (const AudioChannelSet& channelSet)
{
    auto channelTypes = channelSet.getChannelTypes();

    // When
    if (channelSet.isDiscreteLayout())
        return true;

    // WAV supports all channel types from left ... topRearRight
    for (auto channel : channelTypes)
        if (channel < AudioChannelSet::left || channel > AudioChannelSet::topRearRight)
            return false;

    return true;
}

AudioFormatReader* WavAudioFormat::createReaderFor (InputStream* sourceStream, bool deleteStreamIfOpeningFails)
{
    ScopedPointer<WavAudioFormatReader> r (new WavAudioFormatReader (sourceStream));

   #if JUCE_USE_OGGVORBIS
    if (r->isSubformatOggVorbis)
    {
        r->input = nullptr;
        return OggVorbisAudioFormat().createReaderFor (sourceStream, deleteStreamIfOpeningFails);
    }
   #endif

    if (r->sampleRate > 0 && r->numChannels > 0 && r->bytesPerFrame > 0)
        return r.release();

    if (! deleteStreamIfOpeningFails)
        r->input = nullptr;

    return nullptr;
}

MemoryMappedAudioFormatReader* WavAudioFormat::createMemoryMappedReader (const File& file)
{
    return createMemoryMappedReader (file.createInputStream());
}

MemoryMappedAudioFormatReader* WavAudioFormat::createMemoryMappedReader (FileInputStream* fin)
{
    if (fin != nullptr)
    {
        WavAudioFormatReader reader (fin);

        if (reader.lengthInSamples > 0)
            return new MemoryMappedWavReader (fin->getFile(), reader);
    }

    return nullptr;
}

AudioFormatWriter* WavAudioFormat::createWriterFor (OutputStream* out, double sampleRate,
                                                    unsigned int numChannels, int bitsPerSample,
                                                    const StringPairArray& metadataValues, int qualityOptionIndex)
{
    return createWriterFor (out, sampleRate, WavFileHelpers::canonicalWavChannelSet (static_cast<int> (numChannels)),
                            bitsPerSample, metadataValues, qualityOptionIndex);
}

AudioFormatWriter* WavAudioFormat::createWriterFor (OutputStream* out,
                                                    double sampleRate,
                                                    const AudioChannelSet& channelLayout,
                                                    int bitsPerSample,
                                                    const StringPairArray& metadataValues,
                                                    int /*qualityOptionIndex*/)
{
    if (out != nullptr && getPossibleBitDepths().contains (bitsPerSample) && isChannelLayoutSupported (channelLayout))
        return new WavAudioFormatWriter (out, sampleRate, channelLayout,
                                         (unsigned int) bitsPerSample, metadataValues);

    return nullptr;
}

namespace WavFileHelpers
{
    static bool slowCopyWavFileWithNewMetadata (const File& file, const StringPairArray& metadata)
    {
        TemporaryFile tempFile (file);
        WavAudioFormat wav;

        if (ScopedPointer<AudioFormatReader> reader = wav.createReaderFor (file.createInputStream(), true))
        {
            if (ScopedPointer<OutputStream> outStream = tempFile.getFile().createOutputStream())
            {
                if (ScopedPointer<AudioFormatWriter> writer = wav.createWriterFor (outStream, reader->sampleRate,
                                                                                   reader->numChannels, (int) reader->bitsPerSample,
                                                                                   metadata, 0))
                {
                    outStream.release();

                    bool ok = writer->writeFromAudioReader (*reader, 0, -1);
                    writer = nullptr;
                    reader = nullptr;

                    return ok && tempFile.overwriteTargetFileWithTemporary();
                }
            }
        }

        return false;
    }
}

bool WavAudioFormat::replaceMetadataInFile (const File& wavFile, const StringPairArray& newMetadata)
{
    using namespace WavFileHelpers;

    if (ScopedPointer<WavAudioFormatReader> reader = static_cast<WavAudioFormatReader*> (createReaderFor (wavFile.createInputStream(), true)))
    {
        auto bwavPos  = reader->bwavChunkStart;
        auto bwavSize = reader->bwavSize;
        reader = nullptr;

        if (bwavSize > 0)
        {
            auto chunk = BWAVChunk::createFrom (newMetadata);

            if (chunk.getSize() <= (size_t) bwavSize)
            {
                // the new one will fit in the space available, so write it directly..
                auto oldSize = wavFile.getSize();

                {
                    FileOutputStream out (wavFile);

                    if (out.openedOk())
                    {
                        out.setPosition (bwavPos);
                        out << chunk;
                        out.setPosition (oldSize);
                    }
                }

                jassert (wavFile.getSize() == oldSize);
                return true;
            }
        }
    }

    return slowCopyWavFileWithNewMetadata (wavFile, newMetadata);
}

//==============================================================================
#if JUCE_UNIT_TESTS

struct WaveAudioFormatTests : public UnitTest
{
    WaveAudioFormatTests() : UnitTest ("Wave audio format tests") {}

    void runTest() override
    {
        beginTest ("Setting up metadata");

        StringPairArray metadataValues = WavAudioFormat::createBWAVMetadata ("description",
                                                                             "originator",
                                                                             "originatorRef",
                                                                             Time::getCurrentTime(),
                                                                             numTestAudioBufferSamples,
                                                                             "codingHistory");

        for (int i = numElementsInArray (WavFileHelpers::ListInfoChunk::types); --i >= 0;)
            metadataValues.set (WavFileHelpers::ListInfoChunk::types[i],
                                WavFileHelpers::ListInfoChunk::types[i]);

        if (metadataValues.size() > 0)
            metadataValues.set ("MetaDataSource", "WAV");

        metadataValues.addArray (createDefaultSMPLMetadata());

        WavAudioFormat format;
        MemoryBlock memoryBlock;

        {
            beginTest ("Creating a basic wave writer");

            ScopedPointer<AudioFormatWriter> writer (format.createWriterFor (new MemoryOutputStream (memoryBlock, false),
                                                                             44100.0, numTestAudioBufferChannels,
                                                                             32, metadataValues, 0));
            expect (writer != nullptr);

            AudioSampleBuffer buffer (numTestAudioBufferChannels, numTestAudioBufferSamples);
            buffer.clear();

            beginTest ("Writing audio data to the basic wave writer");
            expect (writer->writeFromAudioSampleBuffer (buffer, 0, numTestAudioBufferSamples));
        }

        {
            beginTest ("Creating a basic wave reader");

            ScopedPointer<AudioFormatReader> reader (format.createReaderFor (new MemoryInputStream (memoryBlock, false), false));
            expect (reader != nullptr);
            expect (reader->metadataValues == metadataValues, "Somehow, the metadata is different!");
        }
    }

private:
    enum
    {
        numTestAudioBufferChannels = 2,
        numTestAudioBufferSamples = 256
    };

    StringPairArray createDefaultSMPLMetadata() const
    {
        StringPairArray m;

        m.set ("Manufacturer", "0");
        m.set ("Product", "0");
        m.set ("SamplePeriod", "0");
        m.set ("MidiUnityNote", "60");
        m.set ("MidiPitchFraction", "0");
        m.set ("SmpteFormat", "0");
        m.set ("SmpteOffset", "0");
        m.set ("NumSampleLoops", "0");
        m.set ("SamplerData", "0");

        return m;
    }

    JUCE_DECLARE_NON_COPYABLE (WaveAudioFormatTests)
};

static const WaveAudioFormatTests waveAudioFormatTests;

#endif

} // namespace juce
