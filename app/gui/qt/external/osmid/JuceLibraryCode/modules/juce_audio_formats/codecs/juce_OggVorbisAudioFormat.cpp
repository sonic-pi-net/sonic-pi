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

#if JUCE_USE_OGGVORBIS

#if JUCE_MAC && ! defined (__MACOSX__)
 #define __MACOSX__ 1
#endif

namespace OggVorbisNamespace
{
#if JUCE_INCLUDE_OGGVORBIS_CODE || ! defined (JUCE_INCLUDE_OGGVORBIS_CODE)
 #if JUCE_MSVC
  #pragma warning (push)
  #pragma warning (disable: 4267 4127 4244 4996 4100 4701 4702 4013 4133 4206 4305 4189 4706 4995 4365 4456 4457 4459)
 #elif JUCE_CLANG
  #pragma clang diagnostic push
  #pragma clang diagnostic ignored "-Wconversion"
  #pragma clang diagnostic ignored "-Wshadow"
  #pragma clang diagnostic ignored "-Wdeprecated-register"
 #elif JUCE_GCC
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wshadow"
 #endif

 #include "oggvorbis/vorbisenc.h"
 #include "oggvorbis/codec.h"
 #include "oggvorbis/vorbisfile.h"

 #include "oggvorbis/bitwise.c"
 #include "oggvorbis/framing.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/analysis.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/bitrate.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/block.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/codebook.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/envelope.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/floor0.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/floor1.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/info.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/lpc.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/lsp.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/mapping0.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/mdct.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/psy.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/registry.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/res0.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/sharedbook.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/smallft.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/synthesis.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/vorbisenc.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/vorbisfile.c"
 #include "oggvorbis/libvorbis-1.3.2/lib/window.c"

 #if JUCE_MSVC
  #pragma warning (pop)
 #elif JUCE_CLANG
  #pragma clang diagnostic pop
 #elif JUCE_GCC
  #pragma GCC diagnostic pop
 #endif
#else
 #include <vorbis/vorbisenc.h>
 #include <vorbis/codec.h>
 #include <vorbis/vorbisfile.h>
#endif
}

#undef max
#undef min

//==============================================================================
static const char* const oggFormatName = "Ogg-Vorbis file";

const char* const OggVorbisAudioFormat::encoderName = "encoder";
const char* const OggVorbisAudioFormat::id3title = "id3title";
const char* const OggVorbisAudioFormat::id3artist = "id3artist";
const char* const OggVorbisAudioFormat::id3album = "id3album";
const char* const OggVorbisAudioFormat::id3comment = "id3comment";
const char* const OggVorbisAudioFormat::id3date = "id3date";
const char* const OggVorbisAudioFormat::id3genre = "id3genre";
const char* const OggVorbisAudioFormat::id3trackNumber = "id3trackNumber";


//==============================================================================
class OggReader : public AudioFormatReader
{
public:
    OggReader (InputStream* inp)  : AudioFormatReader (inp, oggFormatName)
    {
        sampleRate = 0;
        usesFloatingPointData = true;

        callbacks.read_func  = &oggReadCallback;
        callbacks.seek_func  = &oggSeekCallback;
        callbacks.close_func = &oggCloseCallback;
        callbacks.tell_func  = &oggTellCallback;

        auto err = ov_open_callbacks (input, &ovFile, 0, 0, callbacks);

        if (err == 0)
        {
            auto* info = ov_info (&ovFile, -1);

            auto* comment = ov_comment (&ovFile, -1);
            addMetadataItem (comment, "ENCODER",     OggVorbisAudioFormat::encoderName);
            addMetadataItem (comment, "TITLE",       OggVorbisAudioFormat::id3title);
            addMetadataItem (comment, "ARTIST",      OggVorbisAudioFormat::id3artist);
            addMetadataItem (comment, "ALBUM",       OggVorbisAudioFormat::id3album);
            addMetadataItem (comment, "COMMENT",     OggVorbisAudioFormat::id3comment);
            addMetadataItem (comment, "DATE",        OggVorbisAudioFormat::id3date);
            addMetadataItem (comment, "GENRE",       OggVorbisAudioFormat::id3genre);
            addMetadataItem (comment, "TRACKNUMBER", OggVorbisAudioFormat::id3trackNumber);

            lengthInSamples = (uint32) ov_pcm_total (&ovFile, -1);
            numChannels = (unsigned int) info->channels;
            bitsPerSample = 16;
            sampleRate = info->rate;

            reservoir.setSize ((int) numChannels, (int) jmin (lengthInSamples, (int64) 4096));
        }
    }

    ~OggReader()
    {
        ov_clear (&ovFile);
    }

    void addMetadataItem (OggVorbisNamespace::vorbis_comment* comment, const char* name, const char* metadataName)
    {
        if (auto* value = vorbis_comment_query (comment, name, 0))
            metadataValues.set (metadataName, value);
    }

    //==============================================================================
    bool readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                      int64 startSampleInFile, int numSamples) override
    {
        while (numSamples > 0)
        {
            auto numAvailable = (int) (reservoirStart + samplesInReservoir - startSampleInFile);

            if (startSampleInFile >= reservoirStart && numAvailable > 0)
            {
                // got a few samples overlapping, so use them before seeking..

                auto numToUse = jmin (numSamples, numAvailable);

                for (int i = jmin (numDestChannels, reservoir.getNumChannels()); --i >= 0;)
                    if (destSamples[i] != nullptr)
                        memcpy (destSamples[i] + startOffsetInDestBuffer,
                                reservoir.getReadPointer (i, (int) (startSampleInFile - reservoirStart)),
                                sizeof (float) * (size_t) numToUse);

                startSampleInFile += numToUse;
                numSamples -= numToUse;
                startOffsetInDestBuffer += numToUse;

                if (numSamples == 0)
                    break;
            }

            if (startSampleInFile < reservoirStart
                || startSampleInFile + numSamples > reservoirStart + samplesInReservoir)
            {
                // buffer miss, so refill the reservoir
                reservoirStart = jmax (0, (int) startSampleInFile);
                samplesInReservoir = reservoir.getNumSamples();

                if (reservoirStart != (int) ov_pcm_tell (&ovFile))
                    ov_pcm_seek (&ovFile, reservoirStart);

                int bitStream = 0;
                int offset = 0;
                int numToRead = samplesInReservoir;

                while (numToRead > 0)
                {
                    float** dataIn = nullptr;
                    auto samps = ov_read_float (&ovFile, &dataIn, numToRead, &bitStream);

                    if (samps <= 0)
                        break;

                    jassert (samps <= numToRead);

                    for (int i = jmin ((int) numChannels, reservoir.getNumChannels()); --i >= 0;)
                        memcpy (reservoir.getWritePointer (i, offset), dataIn[i], sizeof (float) * (size_t) samps);

                    numToRead -= samps;
                    offset += samps;
                }

                if (numToRead > 0)
                    reservoir.clear (offset, numToRead);
            }
        }

        if (numSamples > 0)
        {
            for (int i = numDestChannels; --i >= 0;)
                if (destSamples[i] != nullptr)
                    zeromem (destSamples[i] + startOffsetInDestBuffer, sizeof (int) * (size_t) numSamples);
        }

        return true;
    }

    //==============================================================================
    static size_t oggReadCallback (void* ptr, size_t size, size_t nmemb, void* datasource)
    {
        return (size_t) (static_cast<InputStream*> (datasource)->read (ptr, (int) (size * nmemb))) / size;
    }

    static int oggSeekCallback (void* datasource, OggVorbisNamespace::ogg_int64_t offset, int whence)
    {
        auto* in = static_cast<InputStream*> (datasource);

        if (whence == SEEK_CUR)
            offset += in->getPosition();
        else if (whence == SEEK_END)
            offset += in->getTotalLength();

        in->setPosition (offset);
        return 0;
    }

    static int oggCloseCallback (void*)
    {
        return 0;
    }

    static long oggTellCallback (void* datasource)
    {
        return (long) static_cast<InputStream*> (datasource)->getPosition();
    }

private:
    OggVorbisNamespace::OggVorbis_File ovFile;
    OggVorbisNamespace::ov_callbacks callbacks;
    AudioSampleBuffer reservoir;
    int reservoirStart = 0, samplesInReservoir = 0;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OggReader)
};

//==============================================================================
class OggWriter  : public AudioFormatWriter
{
public:
    OggWriter (OutputStream* out, double rate,
               unsigned int numChans, unsigned int bitsPerSamp,
               int qualityIndex, const StringPairArray& metadata)
        : AudioFormatWriter (out, oggFormatName, rate, numChans, bitsPerSamp)
    {
        vorbis_info_init (&vi);

        if (vorbis_encode_init_vbr (&vi, (int) numChans, (int) rate,
                                    jlimit (0.0f, 1.0f, qualityIndex * 0.1f)) == 0)
        {
            vorbis_comment_init (&vc);

            addMetadata (metadata, OggVorbisAudioFormat::encoderName,    "ENCODER");
            addMetadata (metadata, OggVorbisAudioFormat::id3title,       "TITLE");
            addMetadata (metadata, OggVorbisAudioFormat::id3artist,      "ARTIST");
            addMetadata (metadata, OggVorbisAudioFormat::id3album,       "ALBUM");
            addMetadata (metadata, OggVorbisAudioFormat::id3comment,     "COMMENT");
            addMetadata (metadata, OggVorbisAudioFormat::id3date,        "DATE");
            addMetadata (metadata, OggVorbisAudioFormat::id3genre,       "GENRE");
            addMetadata (metadata, OggVorbisAudioFormat::id3trackNumber, "TRACKNUMBER");

            vorbis_analysis_init (&vd, &vi);
            vorbis_block_init (&vd, &vb);

            ogg_stream_init (&os, Random::getSystemRandom().nextInt());

            OggVorbisNamespace::ogg_packet header, header_comm, header_code;
            vorbis_analysis_headerout (&vd, &vc, &header, &header_comm, &header_code);

            ogg_stream_packetin (&os, &header);
            ogg_stream_packetin (&os, &header_comm);
            ogg_stream_packetin (&os, &header_code);

            for (;;)
            {
                if (ogg_stream_flush (&os, &og) == 0)
                    break;

                output->write (og.header, (size_t) og.header_len);
                output->write (og.body,   (size_t) og.body_len);
            }

            ok = true;
        }
    }

    ~OggWriter()
    {
        if (ok)
        {
            // write a zero-length packet to show ogg that we're finished..
            writeSamples (0);

            ogg_stream_clear (&os);
            vorbis_block_clear (&vb);
            vorbis_dsp_clear (&vd);
            vorbis_comment_clear (&vc);

            vorbis_info_clear (&vi);
            output->flush();
        }
        else
        {
            vorbis_info_clear (&vi);
            output = nullptr; // to stop the base class deleting this, as it needs to be returned
                              // to the caller of createWriter()
        }
    }

    //==============================================================================
    bool write (const int** samplesToWrite, int numSamples) override
    {
        if (ok)
        {
            if (numSamples > 0)
            {
                const double gain = 1.0 / 0x80000000u;
                float** const vorbisBuffer = vorbis_analysis_buffer (&vd, numSamples);

                for (int i = (int) numChannels; --i >= 0;)
                {
                    if (auto* dst = vorbisBuffer[i])
                    {
                        if (const int* src = samplesToWrite [i])
                        {
                            for (int j = 0; j < numSamples; ++j)
                                dst[j] = (float) (src[j] * gain);
                        }
                    }
                }
            }

            writeSamples (numSamples);
        }

        return ok;
    }

    void writeSamples (int numSamples)
    {
        vorbis_analysis_wrote (&vd, numSamples);

        while (vorbis_analysis_blockout (&vd, &vb) == 1)
        {
            vorbis_analysis (&vb, 0);
            vorbis_bitrate_addblock (&vb);

            while (vorbis_bitrate_flushpacket (&vd, &op))
            {
                ogg_stream_packetin (&os, &op);

                for (;;)
                {
                    if (ogg_stream_pageout (&os, &og) == 0)
                        break;

                    output->write (og.header, (size_t) og.header_len);
                    output->write (og.body,   (size_t) og.body_len);

                    if (ogg_page_eos (&og))
                        break;
                }
            }
        }
    }

    bool ok = false;

private:
    OggVorbisNamespace::ogg_stream_state os;
    OggVorbisNamespace::ogg_page og;
    OggVorbisNamespace::ogg_packet op;
    OggVorbisNamespace::vorbis_info vi;
    OggVorbisNamespace::vorbis_comment vc;
    OggVorbisNamespace::vorbis_dsp_state vd;
    OggVorbisNamespace::vorbis_block vb;

    void addMetadata (const StringPairArray& metadata, const char* name, const char* vorbisName)
    {
        auto s = metadata [name];

        if (s.isNotEmpty())
            vorbis_comment_add_tag (&vc, vorbisName, const_cast<char*> (s.toRawUTF8()));
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OggWriter)
};


//==============================================================================
OggVorbisAudioFormat::OggVorbisAudioFormat()  : AudioFormat (oggFormatName, ".ogg")
{
}

OggVorbisAudioFormat::~OggVorbisAudioFormat()
{
}

Array<int> OggVorbisAudioFormat::getPossibleSampleRates()
{
    return { 8000, 11025, 12000, 16000, 22050, 32000,
             44100, 48000, 88200, 96000, 176400, 192000 };
}

Array<int> OggVorbisAudioFormat::getPossibleBitDepths()
{
    return { 32 };
}

bool OggVorbisAudioFormat::canDoStereo()    { return true; }
bool OggVorbisAudioFormat::canDoMono()      { return true; }
bool OggVorbisAudioFormat::isCompressed()   { return true; }

AudioFormatReader* OggVorbisAudioFormat::createReaderFor (InputStream* in, bool deleteStreamIfOpeningFails)
{
    ScopedPointer<OggReader> r (new OggReader (in));

    if (r->sampleRate > 0)
        return r.release();

    if (! deleteStreamIfOpeningFails)
        r->input = nullptr;

    return nullptr;
}

AudioFormatWriter* OggVorbisAudioFormat::createWriterFor (OutputStream* out,
                                                          double sampleRate,
                                                          unsigned int numChannels,
                                                          int bitsPerSample,
                                                          const StringPairArray& metadataValues,
                                                          int qualityOptionIndex)
{
    if (out == nullptr)
        return nullptr;

    ScopedPointer<OggWriter> w (new OggWriter (out, sampleRate, numChannels,
                                               (unsigned int) bitsPerSample,
                                               qualityOptionIndex, metadataValues));

    return w->ok ? w.release() : nullptr;
}

StringArray OggVorbisAudioFormat::getQualityOptions()
{
    return { "64 kbps", "80 kbps", "96 kbps", "112 kbps", "128 kbps", "160 kbps",
             "192 kbps", "224 kbps", "256 kbps", "320 kbps", "500 kbps" };
}

int OggVorbisAudioFormat::estimateOggFileQuality (const File& source)
{
    if (auto* in = source.createInputStream())
    {
        if (ScopedPointer<AudioFormatReader> r = createReaderFor (in, true))
        {
            auto lengthSecs = r->lengthInSamples / r->sampleRate;
            auto approxBitsPerSecond = (int) (source.getSize() * 8 / lengthSecs);

            auto qualities = getQualityOptions();
            int bestIndex = 0;
            int bestDiff = 10000;

            for (int i = qualities.size(); --i >= 0;)
            {
                auto diff = std::abs (qualities[i].getIntValue() - approxBitsPerSecond);

                if (diff < bestDiff)
                {
                    bestDiff = diff;
                    bestIndex = i;
                }
            }

            return bestIndex;
        }
    }

    return 0;
}

#endif

} // namespace juce
