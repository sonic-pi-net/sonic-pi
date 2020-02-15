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

#if JUCE_USE_FLAC

}

#if defined _WIN32 && !defined __CYGWIN__
 #include <io.h>
#else
 #include <unistd.h>
#endif

#if defined _MSC_VER || defined __BORLANDC__ || defined __MINGW32__
 #include <sys/types.h> /* for off_t */
#endif

#if HAVE_INTTYPES_H
 #define __STDC_FORMAT_MACROS
 #include <inttypes.h>
#endif

#if defined _MSC_VER || defined __MINGW32__ || defined __CYGWIN__ || defined __EMX__
 #include <io.h> /* for _setmode(), chmod() */
 #include <fcntl.h> /* for _O_BINARY */
#else
 #include <unistd.h> /* for chown(), unlink() */
#endif

#if defined _MSC_VER || defined __BORLANDC__ || defined __MINGW32__
 #if defined __BORLANDC__
  #include <utime.h> /* for utime() */
 #else
  #include <sys/utime.h> /* for utime() */
 #endif
#else
 #include <sys/types.h> /* some flavors of BSD (like OS X) require this to get time_t */
 #include <utime.h> /* for utime() */
#endif

#if defined _MSC_VER
 #if _MSC_VER >= 1600
  #include <stdint.h>
 #else
  #include <limits.h>
 #endif
#endif

#ifdef _WIN32
 #include <stdio.h>
 #include <sys/stat.h>
 #include <stdarg.h>
 #include <windows.h>
#endif

#ifdef DEBUG
 #include <assert.h>
#endif

#include <stdlib.h>
#include <stdio.h>

namespace juce
{

namespace FlacNamespace
{
#if JUCE_INCLUDE_FLAC_CODE || ! defined (JUCE_INCLUDE_FLAC_CODE)

 #undef VERSION
 #define VERSION "1.3.1"

 #define FLAC__NO_DLL 1

 #if JUCE_MSVC
  #pragma warning (disable: 4267 4127 4244 4996 4100 4701 4702 4013 4133 4206 4312 4505 4365 4005 4334 181 111)
 #else
  #define HAVE_LROUND 1
 #endif

 #if JUCE_MAC
  #define FLAC__SYS_DARWIN 1
 #endif

 #ifndef SIZE_MAX
  #define SIZE_MAX 0xffffffff
 #endif

 #if JUCE_CLANG
  #pragma clang diagnostic push
  #pragma clang diagnostic ignored "-Wconversion"
  #pragma clang diagnostic ignored "-Wshadow"
  #pragma clang diagnostic ignored "-Wdeprecated-register"
 #endif

 #if JUCE_INTEL
  #if JUCE_32BIT
   #define FLAC__CPU_IA32 1
  #endif
  #if JUCE_64BIT
   #define FLAC__CPU_X86_64 1
  #endif
  #define FLAC__HAS_X86INTRIN 1
 #endif

 #undef __STDC_LIMIT_MACROS
 #define __STDC_LIMIT_MACROS 1
 #define flac_max jmax
 #define flac_min jmin
 #undef DEBUG // (some flac code dumps debug trace if the app defines this macro)
 #include "flac/all.h"
 #include "flac/libFLAC/bitmath.c"
 #include "flac/libFLAC/bitreader.c"
 #include "flac/libFLAC/bitwriter.c"
 #include "flac/libFLAC/cpu.c"
 #include "flac/libFLAC/crc.c"
 #include "flac/libFLAC/fixed.c"
 #include "flac/libFLAC/float.c"
 #include "flac/libFLAC/format.c"
 #include "flac/libFLAC/lpc_flac.c"
 #include "flac/libFLAC/md5.c"
 #include "flac/libFLAC/memory.c"
 #include "flac/libFLAC/stream_decoder.c"
 #include "flac/libFLAC/stream_encoder.c"
 #include "flac/libFLAC/stream_encoder_framing.c"
 #include "flac/libFLAC/window_flac.c"
 #undef VERSION
#else
 #include <FLAC/all.h>
#endif

 #if JUCE_CLANG
  #pragma clang diagnostic pop
 #endif
}

#undef max
#undef min

//==============================================================================
static const char* const flacFormatName = "FLAC file";


//==============================================================================
class FlacReader  : public AudioFormatReader
{
public:
    FlacReader (InputStream* in)  : AudioFormatReader (in, flacFormatName)
    {
        lengthInSamples = 0;
        decoder = FlacNamespace::FLAC__stream_decoder_new();

        ok = FLAC__stream_decoder_init_stream (decoder,
                                               readCallback_, seekCallback_, tellCallback_, lengthCallback_,
                                               eofCallback_, writeCallback_, metadataCallback_, errorCallback_,
                                               this) == FlacNamespace::FLAC__STREAM_DECODER_INIT_STATUS_OK;

        if (ok)
        {
            FLAC__stream_decoder_process_until_end_of_metadata (decoder);

            if (lengthInSamples == 0 && sampleRate > 0)
            {
                // the length hasn't been stored in the metadata, so we'll need to
                // work it out the length the hard way, by scanning the whole file..
                scanningForLength = true;
                FLAC__stream_decoder_process_until_end_of_stream (decoder);
                scanningForLength = false;
                auto tempLength = lengthInSamples;

                FLAC__stream_decoder_reset (decoder);
                FLAC__stream_decoder_process_until_end_of_metadata (decoder);
                lengthInSamples = tempLength;
            }
        }
    }

    ~FlacReader()
    {
        FlacNamespace::FLAC__stream_decoder_delete (decoder);
    }

    void useMetadata (const FlacNamespace::FLAC__StreamMetadata_StreamInfo& info)
    {
        sampleRate = info.sample_rate;
        bitsPerSample = info.bits_per_sample;
        lengthInSamples = (unsigned int) info.total_samples;
        numChannels = info.channels;

        reservoir.setSize ((int) numChannels, 2 * (int) info.max_blocksize, false, false, true);
    }

    // returns the number of samples read
    bool readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                      int64 startSampleInFile, int numSamples) override
    {
        if (! ok)
            return false;

        while (numSamples > 0)
        {
            if (startSampleInFile >= reservoirStart
                 && startSampleInFile < reservoirStart + samplesInReservoir)
            {
                auto num = (int) jmin ((int64) numSamples,
                                       reservoirStart + samplesInReservoir - startSampleInFile);

                jassert (num > 0);

                for (int i = jmin (numDestChannels, reservoir.getNumChannels()); --i >= 0;)
                    if (destSamples[i] != nullptr)
                        memcpy (destSamples[i] + startOffsetInDestBuffer,
                                reservoir.getReadPointer (i, (int) (startSampleInFile - reservoirStart)),
                                sizeof (int) * (size_t) num);

                startOffsetInDestBuffer += num;
                startSampleInFile += num;
                numSamples -= num;
            }
            else
            {
                if (startSampleInFile >= (int) lengthInSamples)
                {
                    samplesInReservoir = 0;
                }
                else if (startSampleInFile < reservoirStart
                          || startSampleInFile > reservoirStart + jmax (samplesInReservoir, 511))
                {
                    // had some problems with flac crashing if the read pos is aligned more
                    // accurately than this. Probably fixed in newer versions of the library, though.
                    reservoirStart = (int) (startSampleInFile & ~511);
                    samplesInReservoir = 0;
                    FLAC__stream_decoder_seek_absolute (decoder, (FlacNamespace::FLAC__uint64) reservoirStart);
                }
                else
                {
                    reservoirStart += samplesInReservoir;
                    samplesInReservoir = 0;
                    FLAC__stream_decoder_process_single (decoder);
                }

                if (samplesInReservoir == 0)
                    break;
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

    void useSamples (const FlacNamespace::FLAC__int32* const buffer[], int numSamples)
    {
        if (scanningForLength)
        {
            lengthInSamples += numSamples;
        }
        else
        {
            if (numSamples > reservoir.getNumSamples())
                reservoir.setSize ((int) numChannels, numSamples, false, false, true);

            auto bitsToShift = 32 - bitsPerSample;

            for (int i = 0; i < (int) numChannels; ++i)
            {
                auto* src = buffer[i];
                int n = i;

                while (src == 0 && n > 0)
                    src = buffer [--n];

                if (src != nullptr)
                {
                    auto* dest = reinterpret_cast<int*> (reservoir.getWritePointer(i));

                    for (int j = 0; j < numSamples; ++j)
                        dest[j] = src[j] << bitsToShift;
                }
            }

            samplesInReservoir = numSamples;
        }
    }

    //==============================================================================
    static FlacNamespace::FLAC__StreamDecoderReadStatus readCallback_ (const FlacNamespace::FLAC__StreamDecoder*, FlacNamespace::FLAC__byte buffer[], size_t* bytes, void* client_data)
    {
        *bytes = (size_t) static_cast<const FlacReader*> (client_data)->input->read (buffer, (int) *bytes);
        return FlacNamespace::FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
    }

    static FlacNamespace::FLAC__StreamDecoderSeekStatus seekCallback_ (const FlacNamespace::FLAC__StreamDecoder*, FlacNamespace::FLAC__uint64 absolute_byte_offset, void* client_data)
    {
        static_cast<const FlacReader*> (client_data)->input->setPosition ((int) absolute_byte_offset);
        return FlacNamespace::FLAC__STREAM_DECODER_SEEK_STATUS_OK;
    }

    static FlacNamespace::FLAC__StreamDecoderTellStatus tellCallback_ (const FlacNamespace::FLAC__StreamDecoder*, FlacNamespace::FLAC__uint64* absolute_byte_offset, void* client_data)
    {
        *absolute_byte_offset = (uint64) static_cast<const FlacReader*> (client_data)->input->getPosition();
        return FlacNamespace::FLAC__STREAM_DECODER_TELL_STATUS_OK;
    }

    static FlacNamespace::FLAC__StreamDecoderLengthStatus lengthCallback_ (const FlacNamespace::FLAC__StreamDecoder*, FlacNamespace::FLAC__uint64* stream_length, void* client_data)
    {
        *stream_length = (uint64) static_cast<const FlacReader*> (client_data)->input->getTotalLength();
        return FlacNamespace::FLAC__STREAM_DECODER_LENGTH_STATUS_OK;
    }

    static FlacNamespace::FLAC__bool eofCallback_ (const FlacNamespace::FLAC__StreamDecoder*, void* client_data)
    {
        return static_cast<const FlacReader*> (client_data)->input->isExhausted();
    }

    static FlacNamespace::FLAC__StreamDecoderWriteStatus writeCallback_ (const FlacNamespace::FLAC__StreamDecoder*,
                                                                         const FlacNamespace::FLAC__Frame* frame,
                                                                         const FlacNamespace::FLAC__int32* const buffer[],
                                                                         void* client_data)
    {
        static_cast<FlacReader*> (client_data)->useSamples (buffer, (int) frame->header.blocksize);
        return FlacNamespace::FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
    }

    static void metadataCallback_ (const FlacNamespace::FLAC__StreamDecoder*,
                                   const FlacNamespace::FLAC__StreamMetadata* metadata,
                                   void* client_data)
    {
        static_cast<FlacReader*> (client_data)->useMetadata (metadata->data.stream_info);
    }

    static void errorCallback_ (const FlacNamespace::FLAC__StreamDecoder*, FlacNamespace::FLAC__StreamDecoderErrorStatus, void*)
    {
    }

private:
    FlacNamespace::FLAC__StreamDecoder* decoder;
    AudioSampleBuffer reservoir;
    int reservoirStart = 0, samplesInReservoir = 0;
    bool ok = false, scanningForLength = false;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FlacReader)
};


//==============================================================================
class FlacWriter  : public AudioFormatWriter
{
public:
    FlacWriter (OutputStream* out, double rate, uint32 numChans, uint32 bits, int qualityOptionIndex)
        : AudioFormatWriter (out, flacFormatName, rate, numChans, bits),
          streamStartPos (output != nullptr ? jmax (output->getPosition(), 0ll) : 0ll)
    {
        encoder = FlacNamespace::FLAC__stream_encoder_new();

        if (qualityOptionIndex > 0)
            FLAC__stream_encoder_set_compression_level (encoder, (uint32) jmin (8, qualityOptionIndex));

        FLAC__stream_encoder_set_do_mid_side_stereo (encoder, numChannels == 2);
        FLAC__stream_encoder_set_loose_mid_side_stereo (encoder, numChannels == 2);
        FLAC__stream_encoder_set_channels (encoder, numChannels);
        FLAC__stream_encoder_set_bits_per_sample (encoder, jmin ((unsigned int) 24, bitsPerSample));
        FLAC__stream_encoder_set_sample_rate (encoder, (unsigned int) sampleRate);
        FLAC__stream_encoder_set_blocksize (encoder, 0);
        FLAC__stream_encoder_set_do_escape_coding (encoder, true);

        ok = FLAC__stream_encoder_init_stream (encoder,
                                               encodeWriteCallback, encodeSeekCallback,
                                               encodeTellCallback, encodeMetadataCallback,
                                               this) == FlacNamespace::FLAC__STREAM_ENCODER_INIT_STATUS_OK;
    }

    ~FlacWriter()
    {
        if (ok)
        {
            FlacNamespace::FLAC__stream_encoder_finish (encoder);
            output->flush();
        }
        else
        {
            output = nullptr; // to stop the base class deleting this, as it needs to be returned
                              // to the caller of createWriter()
        }

        FlacNamespace::FLAC__stream_encoder_delete (encoder);
    }

    //==============================================================================
    bool write (const int** samplesToWrite, int numSamples) override
    {
        if (! ok)
            return false;

        HeapBlock<int*> channels;
        HeapBlock<int> temp;
        auto bitsToShift = 32 - (int) bitsPerSample;

        if (bitsToShift > 0)
        {
            temp.malloc (numChannels * (size_t) numSamples);
            channels.calloc (numChannels + 1);

            for (unsigned int i = 0; i < numChannels; ++i)
            {
                if (samplesToWrite[i] == nullptr)
                    break;

                auto* destData = temp.get() + i * (size_t) numSamples;
                channels[i] = destData;

                for (int j = 0; j < numSamples; ++j)
                    destData[j] = (samplesToWrite[i][j] >> bitsToShift);
            }

            samplesToWrite = const_cast<const int**> (channels.get());
        }

        return FLAC__stream_encoder_process (encoder, (const FlacNamespace::FLAC__int32**) samplesToWrite, (unsigned) numSamples) != 0;
    }

    bool writeData (const void* const data, const int size) const
    {
        return output->write (data, (size_t) size);
    }

    static void packUint32 (FlacNamespace::FLAC__uint32 val, FlacNamespace::FLAC__byte* b, const int bytes)
    {
        b += bytes;

        for (int i = 0; i < bytes; ++i)
        {
            *(--b) = (FlacNamespace::FLAC__byte) (val & 0xff);
            val >>= 8;
        }
    }

    void writeMetaData (const FlacNamespace::FLAC__StreamMetadata* metadata)
    {
        using namespace FlacNamespace;
        auto& info = metadata->data.stream_info;

        unsigned char buffer[FLAC__STREAM_METADATA_STREAMINFO_LENGTH];
        const unsigned int channelsMinus1 = info.channels - 1;
        const unsigned int bitsMinus1 = info.bits_per_sample - 1;

        packUint32 (info.min_blocksize, buffer, 2);
        packUint32 (info.max_blocksize, buffer + 2, 2);
        packUint32 (info.min_framesize, buffer + 4, 3);
        packUint32 (info.max_framesize, buffer + 7, 3);
        buffer[10] = (uint8) ((info.sample_rate >> 12) & 0xff);
        buffer[11] = (uint8) ((info.sample_rate >> 4) & 0xff);
        buffer[12] = (uint8) (((info.sample_rate & 0x0f) << 4) | (channelsMinus1 << 1) | (bitsMinus1 >> 4));
        buffer[13] = (FLAC__byte) (((bitsMinus1 & 0x0f) << 4) | (unsigned int) ((info.total_samples >> 32) & 0x0f));
        packUint32 ((FLAC__uint32) info.total_samples, buffer + 14, 4);
        memcpy (buffer + 18, info.md5sum, 16);

        const bool seekOk = output->setPosition (streamStartPos + 4);
        ignoreUnused (seekOk);

        // if this fails, you've given it an output stream that can't seek! It needs
        // to be able to seek back to write the header
        jassert (seekOk);

        output->writeIntBigEndian (FLAC__STREAM_METADATA_STREAMINFO_LENGTH);
        output->write (buffer, FLAC__STREAM_METADATA_STREAMINFO_LENGTH);
    }

    //==============================================================================
    static FlacNamespace::FLAC__StreamEncoderWriteStatus encodeWriteCallback (const FlacNamespace::FLAC__StreamEncoder*,
                                                                              const FlacNamespace::FLAC__byte buffer[],
                                                                              size_t bytes,
                                                                              unsigned int /*samples*/,
                                                                              unsigned int /*current_frame*/,
                                                                              void* client_data)
    {
        return static_cast<FlacWriter*> (client_data)->writeData (buffer, (int) bytes)
                ? FlacNamespace::FLAC__STREAM_ENCODER_WRITE_STATUS_OK
                : FlacNamespace::FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR;
    }

    static FlacNamespace::FLAC__StreamEncoderSeekStatus encodeSeekCallback (const FlacNamespace::FLAC__StreamEncoder*, FlacNamespace::FLAC__uint64, void*)
    {
        return FlacNamespace::FLAC__STREAM_ENCODER_SEEK_STATUS_UNSUPPORTED;
    }

    static FlacNamespace::FLAC__StreamEncoderTellStatus encodeTellCallback (const FlacNamespace::FLAC__StreamEncoder*, FlacNamespace::FLAC__uint64* absolute_byte_offset, void* client_data)
    {
        if (client_data == nullptr)
            return FlacNamespace::FLAC__STREAM_ENCODER_TELL_STATUS_UNSUPPORTED;

        *absolute_byte_offset = (FlacNamespace::FLAC__uint64) static_cast<FlacWriter*> (client_data)->output->getPosition();
        return FlacNamespace::FLAC__STREAM_ENCODER_TELL_STATUS_OK;
    }

    static void encodeMetadataCallback (const FlacNamespace::FLAC__StreamEncoder*, const FlacNamespace::FLAC__StreamMetadata* metadata, void* client_data)
    {
        static_cast<FlacWriter*> (client_data)->writeMetaData (metadata);
    }

    bool ok = false;

private:
    FlacNamespace::FLAC__StreamEncoder* encoder;
    int64 streamStartPos;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FlacWriter)
};


//==============================================================================
FlacAudioFormat::FlacAudioFormat()  : AudioFormat (flacFormatName, ".flac") {}
FlacAudioFormat::~FlacAudioFormat() {}

Array<int> FlacAudioFormat::getPossibleSampleRates()
{
    return { 8000, 11025, 12000, 16000, 22050, 32000, 44100, 48000,
             88200, 96000, 176400, 192000, 352800, 384000 };
}

Array<int> FlacAudioFormat::getPossibleBitDepths()
{
    return { 16, 24 };
}

bool FlacAudioFormat::canDoStereo()     { return true; }
bool FlacAudioFormat::canDoMono()       { return true; }
bool FlacAudioFormat::isCompressed()    { return true; }

AudioFormatReader* FlacAudioFormat::createReaderFor (InputStream* in, const bool deleteStreamIfOpeningFails)
{
    ScopedPointer<FlacReader> r (new FlacReader (in));

    if (r->sampleRate > 0)
        return r.release();

    if (! deleteStreamIfOpeningFails)
        r->input = nullptr;

    return nullptr;
}

AudioFormatWriter* FlacAudioFormat::createWriterFor (OutputStream* out,
                                                     double sampleRate,
                                                     unsigned int numberOfChannels,
                                                     int bitsPerSample,
                                                     const StringPairArray& /*metadataValues*/,
                                                     int qualityOptionIndex)
{
    if (out != nullptr && getPossibleBitDepths().contains (bitsPerSample))
    {
        ScopedPointer<FlacWriter> w (new FlacWriter (out, sampleRate, numberOfChannels,
                                                     (uint32) bitsPerSample, qualityOptionIndex));
        if (w->ok)
            return w.release();
    }

    return nullptr;
}

StringArray FlacAudioFormat::getQualityOptions()
{
    return { "0 (Fastest)", "1", "2", "3", "4", "5 (Default)","6", "7", "8 (Highest quality)" };
}

#endif

} // namespace juce
