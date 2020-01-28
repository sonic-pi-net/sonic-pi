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

#if JUCE_QUICKTIME && ! (JUCE_64BIT || JUCE_IOS)

} // (juce namespace)

#if ! JUCE_WINDOWS
 #include <QuickTime/Movies.h>
 #include <QuickTime/QTML.h>
 #include <QuickTime/QuickTimeComponents.h>
 #include <QuickTime/MediaHandlers.h>
 #include <QuickTime/ImageCodec.h>
#else
 #if JUCE_MSVC
  #pragma warning (push)
  #pragma warning (disable : 4100)
 #endif

 /* If you've got an include error here, you probably need to install the QuickTime SDK and
    add its header directory to your include path.

    Alternatively, if you don't need any QuickTime services, just set the JUCE_QUICKTIME flag to 0.
 */
 #undef SIZE_MAX
 #include <Movies.h>
 #include <QTML.h>
 #include <QuickTimeComponents.h>
 #include <MediaHandlers.h>
 #include <ImageCodec.h>
 #undef SIZE_MAX

 #if JUCE_MSVC
  #pragma warning (pop)
 #endif
#endif

namespace juce
{

bool juce_OpenQuickTimeMovieFromStream (InputStream* input, Movie& movie, Handle& dataHandle);

static const char* const quickTimeFormatName = "QuickTime file";

//==============================================================================
class QTAudioReader     : public AudioFormatReader
{
public:
    QTAudioReader (InputStream* const input_, const int trackNum_)
        : AudioFormatReader (input_, quickTimeFormatName),
          ok (false),
          movie (0),
          trackNum (trackNum_),
          lastSampleRead (0),
          lastThreadId (0),
          extractor (0),
          dataHandle (0)
    {
        JUCE_AUTORELEASEPOOL
        {
            bufferList.calloc (256, 1);

           #if JUCE_WINDOWS
            if (InitializeQTML (0) != noErr)
                return;
           #endif

            if (EnterMovies() != noErr)
                return;

            bool opened = juce_OpenQuickTimeMovieFromStream (input_, movie, dataHandle);

            if (! opened)
                return;

            {
                const int numTracks = GetMovieTrackCount (movie);
                int trackCount = 0;

                for (int i = 1; i <= numTracks; ++i)
                {
                    track = GetMovieIndTrack (movie, i);
                    media = GetTrackMedia (track);

                    OSType mediaType;
                    GetMediaHandlerDescription (media, &mediaType, 0, 0);

                    if (mediaType == SoundMediaType
                         && trackCount++ == trackNum_)
                    {
                        ok = true;
                        break;
                    }
                }
            }

            if (! ok)
                return;

            ok = false;

            lengthInSamples = GetMediaDecodeDuration (media);
            usesFloatingPointData = false;

            samplesPerFrame = (int) (GetMediaDecodeDuration (media) / GetMediaSampleCount (media));

            trackUnitsPerFrame = GetMovieTimeScale (movie) * samplesPerFrame
                                    / GetMediaTimeScale (media);

            MovieAudioExtractionBegin (movie, 0, &extractor);

            unsigned long output_layout_size;
            OSStatus err = MovieAudioExtractionGetPropertyInfo (extractor,
                                                                kQTPropertyClass_MovieAudioExtraction_Audio,
                                                                kQTMovieAudioExtractionAudioPropertyID_AudioChannelLayout,
                                                                0, &output_layout_size, 0);
            if (err != noErr)
                return;

            HeapBlock<AudioChannelLayout> qt_audio_channel_layout;
            qt_audio_channel_layout.calloc (output_layout_size, 1);

            MovieAudioExtractionGetProperty (extractor,
                                             kQTPropertyClass_MovieAudioExtraction_Audio,
                                             kQTMovieAudioExtractionAudioPropertyID_AudioChannelLayout,
                                             output_layout_size, qt_audio_channel_layout, 0);

            qt_audio_channel_layout[0].mChannelLayoutTag = kAudioChannelLayoutTag_Stereo;

            MovieAudioExtractionSetProperty (extractor,
                                             kQTPropertyClass_MovieAudioExtraction_Audio,
                                             kQTMovieAudioExtractionAudioPropertyID_AudioChannelLayout,
                                             output_layout_size,
                                             qt_audio_channel_layout);

            err = MovieAudioExtractionGetProperty (extractor,
                                                   kQTPropertyClass_MovieAudioExtraction_Audio,
                                                   kQTMovieAudioExtractionAudioPropertyID_AudioStreamBasicDescription,
                                                   sizeof (inputStreamDesc),
                                                   &inputStreamDesc, 0);
            if (err != noErr)
                return;

            inputStreamDesc.mFormatFlags = kAudioFormatFlagIsSignedInteger
                                            | kAudioFormatFlagIsPacked
                                            | kAudioFormatFlagsNativeEndian;
            inputStreamDesc.mBitsPerChannel = sizeof (SInt16) * 8;
            inputStreamDesc.mChannelsPerFrame = jmin ((UInt32) 2, inputStreamDesc.mChannelsPerFrame);
            inputStreamDesc.mBytesPerFrame = sizeof (SInt16) * inputStreamDesc.mChannelsPerFrame;
            inputStreamDesc.mBytesPerPacket = inputStreamDesc.mBytesPerFrame;

            err = MovieAudioExtractionSetProperty (extractor,
                                                   kQTPropertyClass_MovieAudioExtraction_Audio,
                                                   kQTMovieAudioExtractionAudioPropertyID_AudioStreamBasicDescription,
                                                   sizeof (inputStreamDesc),
                                                   &inputStreamDesc);
            if (err != noErr)
                return;

            Boolean allChannelsDiscrete = false;
            err = MovieAudioExtractionSetProperty (extractor,
                                                   kQTPropertyClass_MovieAudioExtraction_Movie,
                                                   kQTMovieAudioExtractionMoviePropertyID_AllChannelsDiscrete,
                                                   sizeof (allChannelsDiscrete),
                                                   &allChannelsDiscrete);

            if (err != noErr)
                return;

            bufferList->mNumberBuffers = 1;
            bufferList->mBuffers[0].mNumberChannels = inputStreamDesc.mChannelsPerFrame;
            bufferList->mBuffers[0].mDataByteSize =  jmax ((UInt32) 4096, (UInt32) (samplesPerFrame * (int) inputStreamDesc.mBytesPerFrame) + 16);

            dataBuffer.malloc (bufferList->mBuffers[0].mDataByteSize);
            bufferList->mBuffers[0].mData = dataBuffer;

            sampleRate = inputStreamDesc.mSampleRate;
            bitsPerSample = 16;
            numChannels = inputStreamDesc.mChannelsPerFrame;

            detachThread();
            ok = true;
        }
    }

    ~QTAudioReader()
    {
        JUCE_AUTORELEASEPOOL
        {
            checkThreadIsAttached();

            if (dataHandle != nullptr)
                DisposeHandle (dataHandle);

            if (extractor != nullptr)
            {
                MovieAudioExtractionEnd (extractor);
                extractor = nullptr;
            }

            DisposeMovie (movie);

           #if JUCE_MAC
            ExitMoviesOnThread();
           #endif
        }
    }

    bool readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                      int64 startSampleInFile, int numSamples)
    {
        JUCE_AUTORELEASEPOOL
        {
            checkThreadIsAttached();
            bool readOk = true;

            while (numSamples > 0)
            {
                if (lastSampleRead != startSampleInFile)
                {
                    TimeRecord time;
                    time.scale = (TimeScale) inputStreamDesc.mSampleRate;
                    time.base = 0;
                    time.value.hi = 0;
                    time.value.lo = (UInt32) startSampleInFile;

                    OSStatus err = MovieAudioExtractionSetProperty (extractor,
                                                                    kQTPropertyClass_MovieAudioExtraction_Movie,
                                                                    kQTMovieAudioExtractionMoviePropertyID_CurrentTime,
                                                                    sizeof (time), &time);

                    if (err != noErr)
                    {
                        readOk = false;
                        break;
                    }
                }

                int framesToDo = jmin (numSamples, (int) (bufferList->mBuffers[0].mDataByteSize / inputStreamDesc.mBytesPerFrame));
                bufferList->mBuffers[0].mDataByteSize = inputStreamDesc.mBytesPerFrame * (UInt32) framesToDo;

                UInt32 outFlags = 0;
                UInt32 actualNumFrames = (UInt32) framesToDo;
                OSStatus err = MovieAudioExtractionFillBuffer (extractor, &actualNumFrames, bufferList, &outFlags);
                if (err != noErr)
                {
                    readOk = false;
                    break;
                }

                lastSampleRead = startSampleInFile + actualNumFrames;
                const int samplesReceived = (int) actualNumFrames;

                for (int j = numDestChannels; --j >= 0;)
                {
                    if (destSamples[j] != nullptr)
                    {
                        const short* src = ((const short*) bufferList->mBuffers[0].mData) + j;

                        for (int i = 0; i < samplesReceived; ++i)
                        {
                            destSamples[j][startOffsetInDestBuffer + i] = (*src << 16);
                            src += numChannels;
                        }
                    }
                }

                startOffsetInDestBuffer += samplesReceived;
                startSampleInFile += samplesReceived;
                numSamples -= samplesReceived;

                if (((outFlags & kQTMovieAudioExtractionComplete) != 0 || samplesReceived == 0) && numSamples > 0)
                {
                    for (int j = numDestChannels; --j >= 0;)
                        if (destSamples[j] != nullptr)
                            zeromem (destSamples[j] + startOffsetInDestBuffer, sizeof (int) * (size_t) numSamples);

                    break;
                }
            }

            detachThread();
            return readOk;
        }
    }

    bool ok;

private:
    Movie movie;
    Media media;
    Track track;
    const int trackNum;
    double trackUnitsPerFrame;
    int samplesPerFrame;
    int64 lastSampleRead;
    Thread::ThreadID lastThreadId;
    MovieAudioExtractionRef extractor;
    AudioStreamBasicDescription inputStreamDesc;
    HeapBlock<AudioBufferList> bufferList;
    HeapBlock<char> dataBuffer;
    Handle dataHandle;

    //==============================================================================
    void checkThreadIsAttached()
    {
       #if JUCE_MAC
        if (Thread::getCurrentThreadId() != lastThreadId)
            EnterMoviesOnThread (0);
        AttachMovieToCurrentThread (movie);
       #endif
    }

    void detachThread()
    {
       #if JUCE_MAC
        DetachMovieFromCurrentThread (movie);
       #endif
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (QTAudioReader)
};


//==============================================================================
QuickTimeAudioFormat::QuickTimeAudioFormat()  : AudioFormat (quickTimeFormatName, ".mov .mp3 .mp4 .m4a")
{
}

QuickTimeAudioFormat::~QuickTimeAudioFormat()
{
}

Array<int> QuickTimeAudioFormat::getPossibleSampleRates()    { return Array<int>(); }
Array<int> QuickTimeAudioFormat::getPossibleBitDepths()      { return Array<int>(); }

bool QuickTimeAudioFormat::canDoStereo()    { return true; }
bool QuickTimeAudioFormat::canDoMono()      { return true; }

//==============================================================================
AudioFormatReader* QuickTimeAudioFormat::createReaderFor (InputStream* sourceStream,
                                                          const bool deleteStreamIfOpeningFails)
{
    ScopedPointer<QTAudioReader> r (new QTAudioReader (sourceStream, 0));

    if (r->ok)
        return r.release();

    if (! deleteStreamIfOpeningFails)
        r->input = 0;

    return nullptr;
}

AudioFormatWriter* QuickTimeAudioFormat::createWriterFor (OutputStream* /*streamToWriteTo*/,
                                                          double /*sampleRateToUse*/,
                                                          unsigned int /*numberOfChannels*/,
                                                          int /*bitsPerSample*/,
                                                          const StringPairArray& /*metadataValues*/,
                                                          int /*qualityOptionIndex*/)
{
    jassertfalse; // not yet implemented!
    return nullptr;
}

#endif
