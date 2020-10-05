/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

namespace
{

#ifndef JUCE_ALSA_LOGGING
 #define JUCE_ALSA_LOGGING 0
#endif

#if JUCE_ALSA_LOGGING
 #define JUCE_ALSA_LOG(dbgtext)   { juce::String tempDbgBuf ("ALSA: "); tempDbgBuf << dbgtext; Logger::writeToLog (tempDbgBuf); DBG (tempDbgBuf); }
 #define JUCE_CHECKED_RESULT(x)   (logErrorMessage (x, __LINE__))

 static int logErrorMessage (int err, int lineNum)
 {
    if (err < 0)
        JUCE_ALSA_LOG ("Error: line " << lineNum << ": code " << err << " (" << snd_strerror (err) << ")");

    return err;
 }
#else
 #define JUCE_ALSA_LOG(x)         {}
 #define JUCE_CHECKED_RESULT(x)   (x)
#endif

#define JUCE_ALSA_FAILED(x)  failed (x)

static void getDeviceSampleRates (snd_pcm_t* handle, Array<double>& rates)
{
    const int ratesToTry[] = { 22050, 32000, 44100, 48000, 88200, 96000, 176400, 192000, 0 };

    snd_pcm_hw_params_t* hwParams;
    snd_pcm_hw_params_alloca (&hwParams);

    for (int i = 0; ratesToTry[i] != 0; ++i)
    {
        if (snd_pcm_hw_params_any (handle, hwParams) >= 0
             && snd_pcm_hw_params_test_rate (handle, hwParams, (unsigned int) ratesToTry[i], 0) == 0)
        {
            rates.addIfNotAlreadyThere ((double) ratesToTry[i]);
        }
    }
}

static void getDeviceNumChannels (snd_pcm_t* handle, unsigned int* minChans, unsigned int* maxChans)
{
    snd_pcm_hw_params_t *params;
    snd_pcm_hw_params_alloca (&params);

    if (snd_pcm_hw_params_any (handle, params) >= 0)
    {
        snd_pcm_hw_params_get_channels_min (params, minChans);
        snd_pcm_hw_params_get_channels_max (params, maxChans);

        JUCE_ALSA_LOG ("getDeviceNumChannels: " << (int) *minChans << " " << (int) *maxChans);

        // some virtual devices (dmix for example) report 10000 channels , we have to clamp these values
        *maxChans = jmin (*maxChans, 256u);
        *minChans = jmin (*minChans, *maxChans);
    }
    else
    {
        JUCE_ALSA_LOG ("getDeviceNumChannels failed");
    }
}

static void getDeviceProperties (const String& deviceID,
                                 unsigned int& minChansOut,
                                 unsigned int& maxChansOut,
                                 unsigned int& minChansIn,
                                 unsigned int& maxChansIn,
                                 Array<double>& rates,
                                 bool testOutput,
                                 bool testInput)
{
    minChansOut = maxChansOut = minChansIn = maxChansIn = 0;

    if (deviceID.isEmpty())
        return;

    JUCE_ALSA_LOG ("getDeviceProperties(" << deviceID.toUTF8().getAddress() << ")");

    snd_pcm_info_t* info;
    snd_pcm_info_alloca (&info);

    if (testOutput)
    {
        snd_pcm_t* pcmHandle;

        if (JUCE_CHECKED_RESULT (snd_pcm_open (&pcmHandle, deviceID.toUTF8().getAddress(), SND_PCM_STREAM_PLAYBACK, SND_PCM_NONBLOCK)) >= 0)
        {
            getDeviceNumChannels (pcmHandle, &minChansOut, &maxChansOut);
            getDeviceSampleRates (pcmHandle, rates);

            snd_pcm_close (pcmHandle);
        }
    }

    if (testInput)
    {
        snd_pcm_t* pcmHandle;

        if (JUCE_CHECKED_RESULT (snd_pcm_open (&pcmHandle, deviceID.toUTF8(), SND_PCM_STREAM_CAPTURE, SND_PCM_NONBLOCK) >= 0))
        {
            getDeviceNumChannels (pcmHandle, &minChansIn, &maxChansIn);

            if (rates.size() == 0)
                getDeviceSampleRates (pcmHandle, rates);

            snd_pcm_close (pcmHandle);
        }
    }
}

static void ensureMinimumNumBitsSet (BigInteger& chans, int minNumChans)
{
    int i = 0;

    while (chans.countNumberOfSetBits() < minNumChans)
        chans.setBit (i++);
}

static void silentErrorHandler (const char*, int, const char*, int, const char*,...) {}

//==============================================================================
class ALSADevice
{
public:
    ALSADevice (const String& devID, bool forInput)
        : handle (nullptr),
          bitDepth (16),
          numChannelsRunning (0),
          latency (0),
          deviceID (devID),
          isInput (forInput),
          isInterleaved (true)
    {
        JUCE_ALSA_LOG ("snd_pcm_open (" << deviceID.toUTF8().getAddress() << ", forInput=" << (int) forInput << ")");

        int err = snd_pcm_open (&handle, deviceID.toUTF8(),
                                forInput ? SND_PCM_STREAM_CAPTURE : SND_PCM_STREAM_PLAYBACK,
                                SND_PCM_ASYNC);
        if (err < 0)
        {
            if (-err == EBUSY)
                error << "The device \"" << deviceID << "\" is busy (another application is using it).";
            else if (-err == ENOENT)
                error << "The device \"" << deviceID << "\" is not available.";
            else
                error << "Could not open " << (forInput ? "input" : "output") << " device \"" << deviceID
                      << "\": " << snd_strerror(err) << " (" << err << ")";

            JUCE_ALSA_LOG ("snd_pcm_open failed; " << error);
        }
    }

    ~ALSADevice()
    {
        closeNow();
    }

    void closeNow()
    {
        if (handle != nullptr)
        {
            snd_pcm_close (handle);
            handle = nullptr;
        }
    }

    bool setParameters (unsigned int sampleRate, int numChannels, int bufferSize)
    {
        if (handle == nullptr)
            return false;

        JUCE_ALSA_LOG ("ALSADevice::setParameters(" << deviceID << ", "
                         << (int) sampleRate << ", " << numChannels << ", " << bufferSize << ")");

        snd_pcm_hw_params_t* hwParams;
        snd_pcm_hw_params_alloca (&hwParams);

        if (snd_pcm_hw_params_any (handle, hwParams) < 0)
        {
            // this is the error message that aplay returns when an error happens here,
            // it is a bit more explicit that "Invalid parameter"
            error = "Broken configuration for this PCM: no configurations available";
            return false;
        }

        if (snd_pcm_hw_params_set_access (handle, hwParams, SND_PCM_ACCESS_RW_INTERLEAVED) >= 0) // works better for plughw..
            isInterleaved = true;
        else if (snd_pcm_hw_params_set_access (handle, hwParams, SND_PCM_ACCESS_RW_NONINTERLEAVED) >= 0)
            isInterleaved = false;
        else
        {
            jassertfalse;
            return false;
        }

        enum { isFloatBit = 1 << 16, isLittleEndianBit = 1 << 17, onlyUseLower24Bits = 1 << 18 };

        const int formatsToTry[] = { SND_PCM_FORMAT_FLOAT_LE,   32 | isFloatBit | isLittleEndianBit,
                                     SND_PCM_FORMAT_FLOAT_BE,   32 | isFloatBit,
                                     SND_PCM_FORMAT_S32_LE,     32 | isLittleEndianBit,
                                     SND_PCM_FORMAT_S32_BE,     32,
                                     SND_PCM_FORMAT_S24_3LE,    24 | isLittleEndianBit,
                                     SND_PCM_FORMAT_S24_3BE,    24,
                                     SND_PCM_FORMAT_S24_LE,     32 | isLittleEndianBit | onlyUseLower24Bits,
                                     SND_PCM_FORMAT_S16_LE,     16 | isLittleEndianBit,
                                     SND_PCM_FORMAT_S16_BE,     16 };
        bitDepth = 0;

        for (int i = 0; i < numElementsInArray (formatsToTry); i += 2)
        {
            if (snd_pcm_hw_params_set_format (handle, hwParams, (_snd_pcm_format) formatsToTry [i]) >= 0)
            {
                const int type = formatsToTry [i + 1];
                bitDepth = type & 255;

                converter.reset (createConverter (isInput, bitDepth,
                                                  (type & isFloatBit) != 0,
                                                  (type & isLittleEndianBit) != 0,
                                                  (type & onlyUseLower24Bits) != 0,
                                                  numChannels,
                                                  isInterleaved));
                break;
            }
        }

        if (bitDepth == 0)
        {
            error = "device doesn't support a compatible PCM format";
            JUCE_ALSA_LOG ("Error: " + error);
            return false;
        }

        int dir = 0;
        unsigned int periods = 4;
        snd_pcm_uframes_t samplesPerPeriod = (snd_pcm_uframes_t) bufferSize;

        if (JUCE_ALSA_FAILED (snd_pcm_hw_params_set_rate_near (handle, hwParams, &sampleRate, nullptr))
            || JUCE_ALSA_FAILED (snd_pcm_hw_params_set_channels (handle, hwParams, (unsigned int ) numChannels))
            || JUCE_ALSA_FAILED (snd_pcm_hw_params_set_periods_near (handle, hwParams, &periods, &dir))
            || JUCE_ALSA_FAILED (snd_pcm_hw_params_set_period_size_near (handle, hwParams, &samplesPerPeriod, &dir))
            || JUCE_ALSA_FAILED (snd_pcm_hw_params (handle, hwParams)))
        {
            return false;
        }

        snd_pcm_uframes_t frames = 0;

        if (JUCE_ALSA_FAILED (snd_pcm_hw_params_get_period_size (hwParams, &frames, &dir))
             || JUCE_ALSA_FAILED (snd_pcm_hw_params_get_periods (hwParams, &periods, &dir)))
            latency = 0;
        else
            latency = (int) frames * ((int) periods - 1); // (this is the method JACK uses to guess the latency..)

        JUCE_ALSA_LOG ("frames: " << (int) frames << ", periods: " << (int) periods
                          << ", samplesPerPeriod: " << (int) samplesPerPeriod);

        snd_pcm_sw_params_t* swParams;
        snd_pcm_sw_params_alloca (&swParams);
        snd_pcm_uframes_t boundary;

        if (JUCE_ALSA_FAILED (snd_pcm_sw_params_current (handle, swParams))
            || JUCE_ALSA_FAILED (snd_pcm_sw_params_get_boundary (swParams, &boundary))
            || JUCE_ALSA_FAILED (snd_pcm_sw_params_set_silence_threshold (handle, swParams, 0))
            || JUCE_ALSA_FAILED (snd_pcm_sw_params_set_silence_size (handle, swParams, boundary))
            || JUCE_ALSA_FAILED (snd_pcm_sw_params_set_start_threshold (handle, swParams, samplesPerPeriod))
            || JUCE_ALSA_FAILED (snd_pcm_sw_params_set_stop_threshold (handle, swParams, boundary))
            || JUCE_ALSA_FAILED (snd_pcm_sw_params (handle, swParams)))
        {
            return false;
        }

       #if JUCE_ALSA_LOGGING
        // enable this to dump the config of the devices that get opened
        snd_output_t* out;
        snd_output_stdio_attach (&out, stderr, 0);
        snd_pcm_hw_params_dump (hwParams, out);
        snd_pcm_sw_params_dump (swParams, out);
       #endif

        numChannelsRunning = numChannels;

        return true;
    }

    //==============================================================================
    bool writeToOutputDevice (AudioBuffer<float>& outputChannelBuffer, const int numSamples)
    {
        jassert (numChannelsRunning <= outputChannelBuffer.getNumChannels());
        float* const* const data = outputChannelBuffer.getArrayOfWritePointers();
        snd_pcm_sframes_t numDone = 0;

        if (isInterleaved)
        {
            scratch.ensureSize ((size_t) ((int) sizeof (float) * numSamples * numChannelsRunning), false);

            for (int i = 0; i < numChannelsRunning; ++i)
                converter->convertSamples (scratch.getData(), i, data[i], 0, numSamples);

            numDone = snd_pcm_writei (handle, scratch.getData(), (snd_pcm_uframes_t) numSamples);
        }
        else
        {
            for (int i = 0; i < numChannelsRunning; ++i)
                converter->convertSamples (data[i], data[i], numSamples);

            numDone = snd_pcm_writen (handle, (void**) data, (snd_pcm_uframes_t) numSamples);
        }

        if (numDone < 0)
        {
            if (numDone == -(EPIPE))
                underrunCount++;

            if (JUCE_ALSA_FAILED (snd_pcm_recover (handle, (int) numDone, 1 /* silent */)))
                return false;
        }

        if (numDone < numSamples)
            JUCE_ALSA_LOG ("Did not write all samples: numDone: " << numDone << ", numSamples: " << numSamples);

        return true;
    }

    bool readFromInputDevice (AudioBuffer<float>& inputChannelBuffer, const int numSamples)
    {
        jassert (numChannelsRunning <= inputChannelBuffer.getNumChannels());
        float* const* const data = inputChannelBuffer.getArrayOfWritePointers();

        if (isInterleaved)
        {
            scratch.ensureSize ((size_t) ((int) sizeof (float) * numSamples * numChannelsRunning), false);
            scratch.fillWith (0); // (not clearing this data causes warnings in valgrind)

            auto num = snd_pcm_readi (handle, scratch.getData(), (snd_pcm_uframes_t) numSamples);

            if (num < 0)
            {
                if (num == -(EPIPE))
                    overrunCount++;

                if (JUCE_ALSA_FAILED (snd_pcm_recover (handle, (int) num, 1 /* silent */)))
                    return false;
            }


            if (num < numSamples)
                JUCE_ALSA_LOG ("Did not read all samples: num: " << num << ", numSamples: " << numSamples);

            for (int i = 0; i < numChannelsRunning; ++i)
                converter->convertSamples (data[i], 0, scratch.getData(), i, numSamples);
        }
        else
        {
            auto num = snd_pcm_readn (handle, (void**) data, (snd_pcm_uframes_t) numSamples);

            if (num < 0)
            {
                if (num == -(EPIPE))
                    overrunCount++;

                if (JUCE_ALSA_FAILED (snd_pcm_recover (handle, (int) num, 1 /* silent */)))
                    return false;
            }

            if (num < numSamples)
                JUCE_ALSA_LOG ("Did not read all samples: num: " << num << ", numSamples: " << numSamples);

            for (int i = 0; i < numChannelsRunning; ++i)
                converter->convertSamples (data[i], data[i], numSamples);
        }

        return true;
    }

    //==============================================================================
    snd_pcm_t* handle;
    String error;
    int bitDepth, numChannelsRunning, latency;
    int underrunCount = 0, overrunCount = 0;

private:
    //==============================================================================
    String deviceID;
    const bool isInput;
    bool isInterleaved;
    MemoryBlock scratch;
    std::unique_ptr<AudioData::Converter> converter;

    //==============================================================================
    template <class SampleType>
    struct ConverterHelper
    {
        static AudioData::Converter* createConverter (const bool forInput, const bool isLittleEndian, const int numInterleavedChannels, bool interleaved)
        {
            if (interleaved)
                return create<AudioData::Interleaved> (forInput, isLittleEndian, numInterleavedChannels);

            return create<AudioData::NonInterleaved> (forInput, isLittleEndian, numInterleavedChannels);
        }

    private:
        template <class InterleavedType>
        static AudioData::Converter* create (const bool forInput, const bool isLittleEndian, const int numInterleavedChannels)
        {
            if (forInput)
            {
                using DestType = AudioData::Pointer <AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::NonConst>;

                if (isLittleEndian)
                    return new AudioData::ConverterInstance <AudioData::Pointer <SampleType, AudioData::LittleEndian, InterleavedType, AudioData::Const>, DestType> (numInterleavedChannels, 1);

                return new AudioData::ConverterInstance <AudioData::Pointer <SampleType, AudioData::BigEndian, InterleavedType, AudioData::Const>, DestType> (numInterleavedChannels, 1);
            }

            using SourceType = AudioData::Pointer <AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::Const>;

            if (isLittleEndian)
                return new AudioData::ConverterInstance <SourceType, AudioData::Pointer <SampleType, AudioData::LittleEndian, InterleavedType, AudioData::NonConst>> (1, numInterleavedChannels);

            return new AudioData::ConverterInstance <SourceType, AudioData::Pointer <SampleType, AudioData::BigEndian, InterleavedType, AudioData::NonConst>> (1, numInterleavedChannels);
        }
    };

    static AudioData::Converter* createConverter (bool forInput, int bitDepth,
                                                  bool isFloat, bool isLittleEndian, bool useOnlyLower24Bits,
                                                  int numInterleavedChannels,
                                                  bool interleaved)
    {
        JUCE_ALSA_LOG ("format: bitDepth=" << bitDepth << ", isFloat=" << (int) isFloat
                        << ", isLittleEndian=" << (int) isLittleEndian << ", numChannels=" << numInterleavedChannels);

        if (isFloat)         return ConverterHelper <AudioData::Float32>::createConverter (forInput, isLittleEndian, numInterleavedChannels, interleaved);
        if (bitDepth == 16)  return ConverterHelper <AudioData::Int16>  ::createConverter (forInput, isLittleEndian, numInterleavedChannels, interleaved);
        if (bitDepth == 24)  return ConverterHelper <AudioData::Int24>  ::createConverter (forInput, isLittleEndian, numInterleavedChannels, interleaved);

        jassert (bitDepth == 32);

        if (useOnlyLower24Bits)
            return ConverterHelper <AudioData::Int24in32>::createConverter (forInput, isLittleEndian, numInterleavedChannels, interleaved);

        return ConverterHelper <AudioData::Int32>::createConverter (forInput, isLittleEndian, numInterleavedChannels, interleaved);
    }

    //==============================================================================
    bool failed (const int errorNum)
    {
        if (errorNum >= 0)
            return false;

        error = snd_strerror (errorNum);
        JUCE_ALSA_LOG ("ALSA error: " << error);
        return true;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ALSADevice)
};

//==============================================================================
class ALSAThread  : public Thread
{
public:
    ALSAThread (const String& inputDeviceID, const String& outputDeviceID)
        : Thread ("JUCE ALSA"),
          inputId (inputDeviceID),
          outputId (outputDeviceID)
    {
        initialiseRatesAndChannels();
    }

    ~ALSAThread() override
    {
        close();
    }

    void open (BigInteger inputChannels,
               BigInteger outputChannels,
               double newSampleRate,
               int newBufferSize)
    {
        close();

        error.clear();
        sampleRate = newSampleRate;
        bufferSize = newBufferSize;

        int maxInputsRequested = inputChannels.getHighestBit() + 1;
        maxInputsRequested = jmax ((int) minChansIn, jmin ((int) maxChansIn, maxInputsRequested));

        inputChannelBuffer.setSize (maxInputsRequested, bufferSize);
        inputChannelBuffer.clear();
        inputChannelDataForCallback.clear();
        currentInputChans.clear();

        if (inputChannels.getHighestBit() >= 0)
        {
            for (int i = 0; i < maxInputsRequested; ++i)
            {
                if (inputChannels[i])
                {
                    inputChannelDataForCallback.add (inputChannelBuffer.getReadPointer (i));
                    currentInputChans.setBit (i);
                }
            }
        }

        ensureMinimumNumBitsSet (outputChannels, (int) minChansOut);

        int maxOutputsRequested = outputChannels.getHighestBit() + 1;
        maxOutputsRequested = jmax ((int) minChansOut, jmin ((int) maxChansOut, maxOutputsRequested));

        outputChannelBuffer.setSize (maxOutputsRequested, bufferSize);
        outputChannelBuffer.clear();
        outputChannelDataForCallback.clear();
        currentOutputChans.clear();

        // Note that the input device is opened before an output, because we've heard
        // of drivers where doing it in the reverse order mysteriously fails.. If this
        // order also causes problems, let us know and we'll see if we can find a compromise!

        if (inputChannelDataForCallback.size() > 0 && inputId.isNotEmpty())
        {
            inputDevice.reset (new ALSADevice (inputId, true));

            if (inputDevice->error.isNotEmpty())
            {
                error = inputDevice->error;
                inputDevice.reset();
                return;
            }

            ensureMinimumNumBitsSet (currentInputChans, (int) minChansIn);

            if (! inputDevice->setParameters ((unsigned int) sampleRate,
                                              jlimit ((int) minChansIn, (int) maxChansIn, currentInputChans.getHighestBit() + 1),
                                              bufferSize))
            {
                error = inputDevice->error;
                inputDevice.reset();
                return;
            }

            inputLatency = inputDevice->latency;
        }

        if (outputChannels.getHighestBit() >= 0)
        {
            for (int i = 0; i < maxOutputsRequested; ++i)
            {
                if (outputChannels[i])
                {
                    outputChannelDataForCallback.add (outputChannelBuffer.getWritePointer (i));
                    currentOutputChans.setBit (i);
                }
            }
        }

        if (outputChannelDataForCallback.size() > 0 && outputId.isNotEmpty())
        {
            outputDevice.reset (new ALSADevice (outputId, false));

            if (outputDevice->error.isNotEmpty())
            {
                error = outputDevice->error;
                outputDevice.reset();
                return;
            }

            if (! outputDevice->setParameters ((unsigned int) sampleRate,
                                               jlimit ((int) minChansOut, (int) maxChansOut,
                                                       currentOutputChans.getHighestBit() + 1),
                                               bufferSize))
            {
                error = outputDevice->error;
                outputDevice.reset();
                return;
            }

            outputLatency = outputDevice->latency;
        }

        if (outputDevice == nullptr && inputDevice == nullptr)
        {
            error = "no channels";
            return;
        }

        if (outputDevice != nullptr && inputDevice != nullptr)
            snd_pcm_link (outputDevice->handle, inputDevice->handle);

        if (inputDevice != nullptr && JUCE_ALSA_FAILED (snd_pcm_prepare (inputDevice->handle)))
            return;

        if (outputDevice != nullptr && JUCE_ALSA_FAILED (snd_pcm_prepare (outputDevice->handle)))
            return;

        startThread (9);

        int count = 1000;

        while (numCallbacks == 0)
        {
            sleep (5);

            if (--count < 0 || ! isThreadRunning())
            {
                error = "device didn't start";
                break;
            }
        }
    }

    void close()
    {
        if (isThreadRunning())
        {
            // problem: when pulseaudio is suspended (with pasuspend) , the ALSAThread::run is just stuck in
            // snd_pcm_writei -- no error, no nothing it just stays stuck. So the only way I found to exit "nicely"
            // (that is without the "killing thread by force" of stopThread) , is to just call snd_pcm_close from
            // here which will cause the thread to resume, and exit
            signalThreadShouldExit();

            const int callbacksToStop = numCallbacks;

            if ((! waitForThreadToExit (400)) && audioIoInProgress && numCallbacks == callbacksToStop)
            {
                JUCE_ALSA_LOG ("Thread is stuck in i/o.. Is pulseaudio suspended?");

                if (outputDevice != nullptr) outputDevice->closeNow();
                if (inputDevice != nullptr) inputDevice->closeNow();
            }
        }

        stopThread (6000);

        inputDevice.reset();
        outputDevice.reset();

        inputChannelBuffer.setSize (1, 1);
        outputChannelBuffer.setSize (1, 1);

        numCallbacks = 0;
    }

    void setCallback (AudioIODeviceCallback* const newCallback) noexcept
    {
        const ScopedLock sl (callbackLock);
        callback = newCallback;
    }

    void run() override
    {
        while (! threadShouldExit())
        {
            if (inputDevice != nullptr && inputDevice->handle != nullptr)
            {
                if (outputDevice == nullptr || outputDevice->handle == nullptr)
                {
                    JUCE_ALSA_FAILED (snd_pcm_wait (inputDevice->handle, 2000));

                    if (threadShouldExit())
                        break;

                    auto avail = snd_pcm_avail_update (inputDevice->handle);

                    if (avail < 0)
                        JUCE_ALSA_FAILED (snd_pcm_recover (inputDevice->handle, (int) avail, 0));
                }

                audioIoInProgress = true;

                if (! inputDevice->readFromInputDevice (inputChannelBuffer, bufferSize))
                {
                    JUCE_ALSA_LOG ("Read failure");
                    break;
                }

                audioIoInProgress = false;
            }

            if (threadShouldExit())
                break;

            {
                const ScopedLock sl (callbackLock);
                ++numCallbacks;

                if (callback != nullptr)
                {
                    callback->audioDeviceIOCallback (inputChannelDataForCallback.getRawDataPointer(),
                                                     inputChannelDataForCallback.size(),
                                                     outputChannelDataForCallback.getRawDataPointer(),
                                                     outputChannelDataForCallback.size(),
                                                     bufferSize);
                }
                else
                {
                    for (int i = 0; i < outputChannelDataForCallback.size(); ++i)
                        zeromem (outputChannelDataForCallback[i], (size_t) bufferSize * sizeof (float));
                }
            }

            if (outputDevice != nullptr && outputDevice->handle != nullptr)
            {
                JUCE_ALSA_FAILED (snd_pcm_wait (outputDevice->handle, 2000));

                if (threadShouldExit())
                    break;

                auto avail = snd_pcm_avail_update (outputDevice->handle);

                if (avail < 0)
                    JUCE_ALSA_FAILED (snd_pcm_recover (outputDevice->handle, (int) avail, 0));

                audioIoInProgress = true;

                if (! outputDevice->writeToOutputDevice (outputChannelBuffer, bufferSize))
                {
                    JUCE_ALSA_LOG ("write failure");
                    break;
                }

                audioIoInProgress = false;
            }
        }

        audioIoInProgress = false;
    }

    int getBitDepth() const noexcept
    {
        if (outputDevice != nullptr)
            return outputDevice->bitDepth;

        if (inputDevice != nullptr)
            return inputDevice->bitDepth;

        return 16;
    }

    int getXRunCount() const noexcept
    {
        int result = 0;

        if (outputDevice != nullptr)
            result += outputDevice->underrunCount;

        if (inputDevice != nullptr)
            result += inputDevice->overrunCount;

        return result;
    }

    //==============================================================================
    String error;
    double sampleRate = 0;
    int bufferSize = 0, outputLatency = 0, inputLatency = 0;
    BigInteger currentInputChans, currentOutputChans;

    Array<double> sampleRates;
    StringArray channelNamesOut, channelNamesIn;
    AudioIODeviceCallback* callback = nullptr;

private:
    //==============================================================================
    const String inputId, outputId;
    std::unique_ptr<ALSADevice> outputDevice, inputDevice;
    std::atomic<int> numCallbacks { 0 };
    bool audioIoInProgress = false;

    CriticalSection callbackLock;

    AudioBuffer<float> inputChannelBuffer, outputChannelBuffer;
    Array<const float*> inputChannelDataForCallback;
    Array<float*> outputChannelDataForCallback;

    unsigned int minChansOut = 0, maxChansOut = 0;
    unsigned int minChansIn = 0, maxChansIn = 0;

    bool failed (const int errorNum)
    {
        if (errorNum >= 0)
            return false;

        error = snd_strerror (errorNum);
        JUCE_ALSA_LOG ("ALSA error: " << error);
        return true;
    }

    void initialiseRatesAndChannels()
    {
        sampleRates.clear();
        channelNamesOut.clear();
        channelNamesIn.clear();
        minChansOut = 0;
        maxChansOut = 0;
        minChansIn = 0;
        maxChansIn = 0;
        unsigned int dummy = 0;

        getDeviceProperties (inputId, dummy, dummy, minChansIn, maxChansIn, sampleRates, false, true);
        getDeviceProperties (outputId, minChansOut, maxChansOut, dummy, dummy, sampleRates, true, false);

        for (unsigned int i = 0; i < maxChansOut; ++i)
            channelNamesOut.add ("channel " + String ((int) i + 1));

        for (unsigned int i = 0; i < maxChansIn; ++i)
            channelNamesIn.add ("channel " + String ((int) i + 1));
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ALSAThread)
};


//==============================================================================
class ALSAAudioIODevice   : public AudioIODevice
{
public:
    ALSAAudioIODevice (const String& deviceName,
                       const String& deviceTypeName,
                       const String& inputDeviceID,
                       const String& outputDeviceID)
        : AudioIODevice (deviceName, deviceTypeName),
          inputId (inputDeviceID),
          outputId (outputDeviceID),
          internal (inputDeviceID, outputDeviceID)
    {
    }

    ~ALSAAudioIODevice() override
    {
        close();
    }

    StringArray getOutputChannelNames() override            { return internal.channelNamesOut; }
    StringArray getInputChannelNames() override             { return internal.channelNamesIn; }

    Array<double> getAvailableSampleRates() override        { return internal.sampleRates; }

    Array<int> getAvailableBufferSizes() override
    {
        Array<int> r;
        int n = 16;

        for (int i = 0; i < 50; ++i)
        {
            r.add (n);
            n += n < 64 ? 16
                        : (n < 512 ? 32
                                   : (n < 1024 ? 64
                                               : (n < 2048 ? 128 : 256)));
        }

        return r;
    }

    int getDefaultBufferSize() override                      { return 512; }

    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double sampleRate,
                 int bufferSizeSamples) override
    {
        close();

        if (bufferSizeSamples <= 0)
            bufferSizeSamples = getDefaultBufferSize();

        if (sampleRate <= 0)
        {
            for (int i = 0; i < internal.sampleRates.size(); ++i)
            {
                double rate = internal.sampleRates[i];

                if (rate >= 44100)
                {
                    sampleRate = rate;
                    break;
                }
            }
        }

        internal.open (inputChannels, outputChannels,
                       sampleRate, bufferSizeSamples);

        isOpen_ = internal.error.isEmpty();
        return internal.error;
    }

    void close() override
    {
        stop();
        internal.close();
        isOpen_ = false;
    }

    bool isOpen() override                           { return isOpen_; }
    bool isPlaying() override                        { return isStarted && internal.error.isEmpty(); }
    String getLastError() override                   { return internal.error; }

    int getCurrentBufferSizeSamples() override       { return internal.bufferSize; }
    double getCurrentSampleRate() override           { return internal.sampleRate; }
    int getCurrentBitDepth() override                { return internal.getBitDepth(); }

    BigInteger getActiveOutputChannels() const override    { return internal.currentOutputChans; }
    BigInteger getActiveInputChannels() const override     { return internal.currentInputChans; }

    int getOutputLatencyInSamples() override         { return internal.outputLatency; }
    int getInputLatencyInSamples() override          { return internal.inputLatency; }

    int getXRunCount() const noexcept override       { return internal.getXRunCount(); }

    void start (AudioIODeviceCallback* callback) override
    {
        if (! isOpen_)
            callback = nullptr;

        if (callback != nullptr)
            callback->audioDeviceAboutToStart (this);

        internal.setCallback (callback);

        isStarted = (callback != nullptr);
    }

    void stop() override
    {
        auto oldCallback = internal.callback;

        start (nullptr);

        if (oldCallback != nullptr)
            oldCallback->audioDeviceStopped();
    }

    String inputId, outputId;

private:
    bool isOpen_ = false, isStarted = false;
    ALSAThread internal;
};


//==============================================================================
class ALSAAudioIODeviceType  : public AudioIODeviceType
{
public:
    ALSAAudioIODeviceType (bool onlySoundcards, const String& deviceTypeName)
        : AudioIODeviceType (deviceTypeName),
          listOnlySoundcards (onlySoundcards)
    {
       #if ! JUCE_ALSA_LOGGING
        snd_lib_error_set_handler (&silentErrorHandler);
       #endif
    }

    ~ALSAAudioIODeviceType()
    {
       #if ! JUCE_ALSA_LOGGING
        snd_lib_error_set_handler (nullptr);
       #endif

        snd_config_update_free_global(); // prevent valgrind from screaming about alsa leaks
    }

    //==============================================================================
    void scanForDevices()
    {
        if (hasScanned)
            return;

        hasScanned = true;
        inputNames.clear();
        inputIds.clear();
        outputNames.clear();
        outputIds.clear();

        JUCE_ALSA_LOG ("scanForDevices()");

        if (listOnlySoundcards)
            enumerateAlsaSoundcards();
        else
            enumerateAlsaPCMDevices();

        inputNames.appendNumbersToDuplicates (false, true);
        outputNames.appendNumbersToDuplicates (false, true);
    }

    StringArray getDeviceNames (bool wantInputNames) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        return wantInputNames ? inputNames : outputNames;
    }

    int getDefaultDeviceIndex (bool forInput) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        auto idx = (forInput ? inputIds : outputIds).indexOf ("default");
        return idx >= 0 ? idx : 0;
    }

    bool hasSeparateInputsAndOutputs() const    { return true; }

    int getIndexOfDevice (AudioIODevice* device, bool asInput) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        if (auto* d = dynamic_cast<ALSAAudioIODevice*> (device))
            return asInput ? inputIds.indexOf (d->inputId)
                           : outputIds.indexOf (d->outputId);

        return -1;
    }

    AudioIODevice* createDevice (const String& outputDeviceName,
                                 const String& inputDeviceName)
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        auto inputIndex = inputNames.indexOf (inputDeviceName);
        auto outputIndex = outputNames.indexOf (outputDeviceName);

        String deviceName (outputIndex >= 0 ? outputDeviceName
                                            : inputDeviceName);

        if (inputIndex >= 0 || outputIndex >= 0)
            return new ALSAAudioIODevice (deviceName, getTypeName(),
                                          inputIds [inputIndex],
                                          outputIds [outputIndex]);

        return nullptr;
    }

private:
    //==============================================================================
    StringArray inputNames, outputNames, inputIds, outputIds;
    bool hasScanned = false;
    const bool listOnlySoundcards;

    bool testDevice (const String& id, const String& outputName, const String& inputName)
    {
        unsigned int minChansOut = 0, maxChansOut = 0;
        unsigned int minChansIn = 0, maxChansIn = 0;
        Array<double> rates;

        bool isInput = inputName.isNotEmpty(), isOutput = outputName.isNotEmpty();
        getDeviceProperties (id, minChansOut, maxChansOut, minChansIn, maxChansIn, rates, isOutput, isInput);

        isInput  = maxChansIn > 0;
        isOutput = maxChansOut > 0;

        if ((isInput || isOutput) && rates.size() > 0)
        {
            JUCE_ALSA_LOG ("testDevice: '" << id.toUTF8().getAddress() << "' -> isInput: "
                            << (int) isInput << ", isOutput: " << (int) isOutput);

            if (isInput)
            {
                inputNames.add (inputName);
                inputIds.add (id);
            }

            if (isOutput)
            {
                outputNames.add (outputName);
                outputIds.add (id);
            }

            return isInput || isOutput;
        }

        return false;
    }

    void enumerateAlsaSoundcards()
    {
        snd_ctl_t* handle = nullptr;
        snd_ctl_card_info_t* info = nullptr;
        snd_ctl_card_info_alloca (&info);

        int cardNum = -1;

        while (outputIds.size() + inputIds.size() <= 64)
        {
            snd_card_next (&cardNum);

            if (cardNum < 0)
                break;

            if (JUCE_CHECKED_RESULT (snd_ctl_open (&handle, ("hw:" + String (cardNum)).toRawUTF8(), SND_CTL_NONBLOCK)) >= 0)
            {
                if (JUCE_CHECKED_RESULT (snd_ctl_card_info (handle, info)) >= 0)
                {
                    String cardId (snd_ctl_card_info_get_id (info));

                    if (cardId.removeCharacters ("0123456789").isEmpty())
                        cardId = String (cardNum);

                    String cardName = snd_ctl_card_info_get_name (info);

                    if (cardName.isEmpty())
                        cardName = cardId;

                    int device = -1;

                    snd_pcm_info_t* pcmInfo;
                    snd_pcm_info_alloca (&pcmInfo);

                    for (;;)
                    {
                        if (snd_ctl_pcm_next_device (handle, &device) < 0 || device < 0)
                            break;

                        snd_pcm_info_set_device (pcmInfo, (unsigned int) device);

                        for (unsigned int subDevice = 0, nbSubDevice = 1; subDevice < nbSubDevice; ++subDevice)
                        {
                            snd_pcm_info_set_subdevice (pcmInfo, subDevice);
                            snd_pcm_info_set_stream (pcmInfo, SND_PCM_STREAM_CAPTURE);
                            const bool isInput = (snd_ctl_pcm_info (handle, pcmInfo) >= 0);

                            snd_pcm_info_set_stream (pcmInfo, SND_PCM_STREAM_PLAYBACK);
                            const bool isOutput = (snd_ctl_pcm_info (handle, pcmInfo) >= 0);

                            if (! (isInput || isOutput))
                                continue;

                            if (nbSubDevice == 1)
                                nbSubDevice = snd_pcm_info_get_subdevices_count (pcmInfo);

                            String id, name;

                            if (nbSubDevice == 1)
                            {
                                id << "hw:" << cardId << "," << device;
                                name << cardName << ", " << snd_pcm_info_get_name (pcmInfo);
                            }
                            else
                            {
                                id << "hw:" << cardId << "," << device << "," << (int) subDevice;
                                name << cardName << ", " << snd_pcm_info_get_name (pcmInfo)
                                     << " {" <<  snd_pcm_info_get_subdevice_name (pcmInfo) << "}";
                            }

                            JUCE_ALSA_LOG ("Soundcard ID: " << id << ", name: '" << name
                                            << ", isInput:"  << (int) isInput
                                            << ", isOutput:" << (int) isOutput << "\n");

                            if (isInput)
                            {
                                inputNames.add (name);
                                inputIds.add (id);
                            }

                            if (isOutput)
                            {
                                outputNames.add (name);
                                outputIds.add (id);
                            }
                        }
                    }
                }

                JUCE_CHECKED_RESULT (snd_ctl_close (handle));
            }
        }
    }

    /* Enumerates all ALSA output devices (as output by the command aplay -L)
       Does not try to open the devices (with "testDevice" for example),
       so that it also finds devices that are busy and not yet available.
    */
    void enumerateAlsaPCMDevices()
    {
        void** hints = nullptr;

        if (JUCE_CHECKED_RESULT (snd_device_name_hint (-1, "pcm", &hints)) == 0)
        {
            for (char** h = (char**) hints; *h; ++h)
            {
                const String id (hintToString (*h, "NAME"));
                const String description (hintToString (*h, "DESC"));
                const String ioid (hintToString (*h, "IOID"));

                JUCE_ALSA_LOG ("ID: " << id << "; desc: " << description << "; ioid: " << ioid);

                String ss = id.fromFirstOccurrenceOf ("=", false, false)
                              .upToFirstOccurrenceOf (",", false, false);

                if (id.isEmpty()
                     || id.startsWith ("default:") || id.startsWith ("sysdefault:")
                     || id.startsWith ("plughw:") || id == "null")
                    continue;

                String name (description.replace ("\n", "; "));

                if (name.isEmpty())
                    name = id;

                bool isOutput = (ioid != "Input");
                bool isInput  = (ioid != "Output");

                // alsa is stupid here, it advertises dmix and dsnoop as input/output devices, but
                // opening dmix as input, or dsnoop as output will trigger errors..
                isInput  = isInput  && ! id.startsWith ("dmix");
                isOutput = isOutput && ! id.startsWith ("dsnoop");

                if (isInput)
                {
                    inputNames.add (name);
                    inputIds.add (id);
                }

                if (isOutput)
                {
                    outputNames.add (name);
                    outputIds.add (id);
                }
            }

            snd_device_name_free_hint (hints);
        }

        // sometimes the "default" device is not listed, but it is nice to see it explicitly in the list
        if (! outputIds.contains ("default"))
            testDevice ("default", "Default ALSA Output", "Default ALSA Input");

        // same for the pulseaudio plugin
        if (! outputIds.contains ("pulse"))
            testDevice ("pulse", "Pulseaudio output", "Pulseaudio input");

        // make sure the default device is listed first, and followed by the pulse device (if present)
        auto idx = outputIds.indexOf ("pulse");
        outputIds.move (idx, 0);
        outputNames.move (idx, 0);

        idx = inputIds.indexOf ("pulse");
        inputIds.move (idx, 0);
        inputNames.move (idx, 0);

        idx = outputIds.indexOf ("default");
        outputIds.move (idx, 0);
        outputNames.move (idx, 0);

        idx = inputIds.indexOf ("default");
        inputIds.move (idx, 0);
        inputNames.move (idx, 0);
    }

    static String hintToString (const void* hints, const char* type)
    {
        char* hint = snd_device_name_get_hint (hints, type);
        auto s = String::fromUTF8 (hint);
        ::free (hint);
        return s;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ALSAAudioIODeviceType)
};

}

//==============================================================================
AudioIODeviceType* createAudioIODeviceType_ALSA_Soundcards()
{
    return new ALSAAudioIODeviceType (true, "ALSA HW");
}

AudioIODeviceType* createAudioIODeviceType_ALSA_PCMDevices()
{
    return new ALSAAudioIODeviceType (false, "ALSA");
}

AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_ALSA()
{
    return createAudioIODeviceType_ALSA_PCMDevices();
}

} // namespace juce
