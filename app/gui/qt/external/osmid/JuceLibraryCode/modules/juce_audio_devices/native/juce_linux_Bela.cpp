/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

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

//==============================================================================
class BelaAudioIODevice   : public AudioIODevice
{
public:
    BelaAudioIODevice()  : AudioIODevice (BelaAudioIODevice::belaTypeName,
                                          BelaAudioIODevice::belaTypeName)
    {
        Bela_defaultSettings (&defaultSettings);
    }

    ~BelaAudioIODevice() {}

    //==============================================================================
    StringArray getOutputChannelNames() override           { return { "Out #1", "Out #2" }; }
    StringArray getInputChannelNames() override            { return { "In #1",  "In #2" }; }
    Array<double> getAvailableSampleRates() override       { return { 44100.0 }; }
    Array<int> getAvailableBufferSizes() override          { /* TODO: */ return { getDefaultBufferSize() }; }
    int getDefaultBufferSize() override                    { return defaultSettings.periodSize; }

    //==============================================================================
    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double sampleRate,
                 int bufferSizeSamples) override
    {
        if (sampleRate != 44100.0 && sampleRate != 0.0)
        {
            lastError = "Bela audio outputs only support 44.1 kHz sample rate";
            return lastError;
        }

        settings = defaultSettings;

        auto numIns = getNumContiguousSetBits (inputChannels);
        auto numOuts = getNumContiguousSetBits (outputChannels);

        settings.useAnalog            = 0;
        settings.useDigital           = 0;
        settings.numAudioInChannels   = numIns;
        settings.numAudioOutChannels  = numOuts;
        settings.detectUnderruns      = 1;
        settings.setup                = setupCallback;
        settings.render               = renderCallback;
        settings.cleanup              = cleanupCallback;
        settings.interleave           = 1;

        if (bufferSizeSamples > 0)
            settings.periodSize = bufferSizeSamples;

        isBelaOpen = false;
        isRunning  = false;
        callback   = nullptr;
        underruns  = 0;

        if (Bela_initAudio (&settings, this) != 0 || ! isBelaOpen)
        {
            lastError = "Bela_initAutio failed";
            return lastError;
        }

        actualNumberOfInputs  = jmin (numIns, actualNumberOfInputs);
        actualNumberOfOutputs = jmin (numOuts, actualNumberOfOutputs);

        audioInBuffer.setSize (actualNumberOfInputs, actualBufferSize);
        channelInBuffer.calloc (actualNumberOfInputs);

        audioOutBuffer.setSize (actualNumberOfOutputs, actualBufferSize);
        channelOutBuffer.calloc (actualNumberOfOutputs);

        return {};
    }

    void close() override
    {
        stop();

        if (isBelaOpen)
        {
            Bela_cleanupAudio();

            isBelaOpen = false;
            callback = nullptr;
            underruns = 0;

            actualBufferSize = 0;
            actualNumberOfInputs = 0;
            actualNumberOfOutputs = 0;

            audioInBuffer.setSize (0, 0);
            channelInBuffer.free();

            audioOutBuffer.setSize (0, 0);
            channelOutBuffer.free();
        }
    }

    bool isOpen() override   { return isBelaOpen; }

    void start (AudioIODeviceCallback* newCallback) override
    {
        if (! isBelaOpen)
            return;

        if (isRunning)
        {
            if (newCallback != callback)
            {
                if (newCallback != nullptr)
                    newCallback->audioDeviceAboutToStart (this);

                {
                    ScopedLock lock (callbackLock);
                    std::swap (callback, newCallback);
                }

                if (newCallback != nullptr)
                    newCallback->audioDeviceStopped();
            }
        }
        else
        {
            audioInBuffer.clear();
            audioOutBuffer.clear();

            callback = newCallback;
            isRunning = (Bela_startAudio() == 0);

            if (callback != nullptr)
            {
                if (isRunning)
                {
                    callback->audioDeviceAboutToStart (this);
                }
                else
                {
                    lastError = "Bela_StartAudio failed";
                    callback->audioDeviceError (lastError);
                }
            }
        }
    }

    void stop() override
    {
        AudioIODeviceCallback* oldCallback = nullptr;

        if (callback != nullptr)
        {
            ScopedLock lock (callbackLock);
            std::swap (callback, oldCallback);
        }

        isRunning = false;
        Bela_stopAudio();

        if (oldCallback != nullptr)
            oldCallback->audioDeviceStopped();
    }

    bool isPlaying() override         { return isRunning; }
    String getLastError() override    { return lastError; }

    //==============================================================================
    int getCurrentBufferSizeSamples() override            { return actualBufferSize; }
    double getCurrentSampleRate() override                { return 44100.0; }
    int getCurrentBitDepth() override                     { return 24; }
    BigInteger getActiveOutputChannels() const override   { BigInteger b; b.setRange (0, actualNumberOfOutputs, true); return b; }
    BigInteger getActiveInputChannels() const override    { BigInteger b; b.setRange (0, actualNumberOfInputs, true);  return b; }
    int getOutputLatencyInSamples() override              { /* TODO */ return 0; }
    int getInputLatencyInSamples() override               { /* TODO */ return 0; }
    int getXRunCount() const noexcept                     { return underruns; }

    //==============================================================================
    static const char* const belaTypeName;

private:
    //==============================================================================
    bool setup (BelaContext& context)
    {
        actualBufferSize      = context.audioFrames;
        actualNumberOfInputs  = context.audioInChannels;
        actualNumberOfOutputs = context.audioOutChannels;
        isBelaOpen = true;
        firstCallback = true;

        ScopedLock lock (callbackLock);

        if (callback != nullptr)
            callback->audioDeviceAboutToStart (this);

        return true;
    }

    void render (BelaContext& context)
    {
        // check for xruns
        calculateXruns (context.audioFramesElapsed, context.audioFrames);

        ScopedLock lock (callbackLock);

        if (callback != nullptr)
        {
            jassert (context.audioFrames <= actualBufferSize);
            auto numSamples = jmin (context.audioFrames, actualBufferSize);
            auto interleaved = ((context.flags & BELA_FLAG_INTERLEAVED) != 0);
            auto numIns  = jmin (actualNumberOfInputs,  (int) context.audioInChannels);
            auto numOuts = jmin (actualNumberOfOutputs, (int) context.audioOutChannels);

            int ch;

            if (interleaved && context.audioInChannels > 1)
            {
                for (ch = 0; ch < numIns; ++ch)
                {
                    using DstSampleType = AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::NonConst>;
                    using SrcSampleType = AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::Interleaved,    AudioData::Const>;

                    channelInBuffer[ch] = audioInBuffer.getWritePointer (ch);
                    DstSampleType dstData (audioInBuffer.getWritePointer (ch));
                    SrcSampleType srcData (context.audioIn + ch, context.audioInChannels);
                    dstData.convertSamples (srcData, numSamples);
                }
            }
            else
            {
                for (ch = 0; ch < numIns; ++ch)
                    channelInBuffer[ch] = context.audioIn + (ch * numSamples);
            }

            for (; ch < actualNumberOfInputs; ++ch)
            {
                channelInBuffer[ch] = audioInBuffer.getWritePointer(ch);
                zeromem (audioInBuffer.getWritePointer (ch), sizeof (float) * numSamples);
            }

            for (int i = 0; i < actualNumberOfOutputs; ++i)
                channelOutBuffer[i] = ((interleaved && context.audioOutChannels > 1) || i >= context.audioOutChannels ? audioOutBuffer.getWritePointer (i)
                                                                                                                      : context.audioOut + (i * numSamples));

            callback->audioDeviceIOCallback (channelInBuffer.getData(), actualNumberOfInputs,
                                             channelOutBuffer.getData(), actualNumberOfOutputs,
                                             numSamples);

            if (interleaved && context.audioOutChannels > 1)
            {
                for (int i = 0; i < numOuts; ++i)
                {
                    using DstSampleType = AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::Interleaved,    AudioData::NonConst>;
                    using SrcSampleType = AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::Const>;

                    SrcSampleType srcData (channelOutBuffer[i]);
                    DstSampleType dstData (context.audioOut + i, context.audioOutChannels);

                    dstData.convertSamples (srcData, numSamples);
                }
            }
        }
    }

    void cleanup (BelaContext&)
    {
        ScopedLock lock (callbackLock);

        if (callback != nullptr)
            callback->audioDeviceStopped();
    }


    //==============================================================================
    uint64_t expectedElapsedAudioSamples = 0;
    int underruns = 0;
    bool firstCallback = false;

    void calculateXruns (uint64_t audioFramesElapsed, uint32_t numSamples)
    {
        if (audioFramesElapsed > expectedElapsedAudioSamples && ! firstCallback)
            ++underruns;

        firstCallback = false;
        expectedElapsedAudioSamples = audioFramesElapsed + numSamples;
    }

    //==============================================================================
    static int getNumContiguousSetBits (const BigInteger& value) noexcept
    {
        int bit = 0;

        while (value[bit])
            ++bit;

        return bit;
    }

    //==============================================================================
    static bool setupCallback   (BelaContext* context, void* userData) noexcept    { return static_cast<BelaAudioIODevice*> (userData)->setup (*context); }
    static void renderCallback  (BelaContext* context, void* userData) noexcept    { static_cast<BelaAudioIODevice*> (userData)->render (*context); }
    static void cleanupCallback (BelaContext* context, void* userData) noexcept    { static_cast<BelaAudioIODevice*> (userData)->cleanup (*context); }

    //==============================================================================
    BelaInitSettings defaultSettings, settings;
    bool isBelaOpen = false, isRunning = false;

    CriticalSection callbackLock;
    AudioIODeviceCallback* callback = nullptr;

    String lastError;
    uint32_t actualBufferSize = 0;
    int actualNumberOfInputs = 0, actualNumberOfOutputs = 0;

    AudioBuffer<float> audioInBuffer, audioOutBuffer;
    HeapBlock<const float*> channelInBuffer;
    HeapBlock<float*> channelOutBuffer;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BelaAudioIODevice)
};

const char* const BelaAudioIODevice::belaTypeName = "Bela Analog";

//==============================================================================
struct BelaAudioIODeviceType  : public AudioIODeviceType
{
    BelaAudioIODeviceType() : AudioIODeviceType ("Bela") {}

    // TODO: support analog outputs
    StringArray getDeviceNames (bool) const override                       { return StringArray (BelaAudioIODevice::belaTypeName); }
    void scanForDevices() override                                         {}
    int getDefaultDeviceIndex (bool) const override                        { return 0; }
    int getIndexOfDevice (AudioIODevice* device, bool) const override      { return device != nullptr ? 0 : -1; }
    bool hasSeparateInputsAndOutputs() const override                      { return false; }

    AudioIODevice* createDevice (const String& outputName, const String& inputName) override
    {
        if (outputName == BelaAudioIODevice::belaTypeName || inputName == BelaAudioIODevice::belaTypeName)
            return new BelaAudioIODevice();

        return nullptr;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BelaAudioIODeviceType)
};

//==============================================================================
AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_Bela()
{
    return new BelaAudioIODeviceType();
}

} // namespace juce
