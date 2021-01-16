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

#if JUCE_COREAUDIO_LOGGING_ENABLED
 #define JUCE_COREAUDIOLOG(a) { String camsg ("CoreAudio: "); camsg << a; Logger::writeToLog (camsg); }
#else
 #define JUCE_COREAUDIOLOG(a)
#endif

JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE ("-Wnonnull")

//==============================================================================
struct SystemVol
{
    SystemVol (AudioObjectPropertySelector selector) noexcept
        : outputDeviceID (kAudioObjectUnknown)
    {
        addr.mScope    = kAudioObjectPropertyScopeGlobal;
        addr.mElement  = kAudioObjectPropertyElementMaster;
        addr.mSelector = kAudioHardwarePropertyDefaultOutputDevice;

        if (AudioObjectHasProperty (kAudioObjectSystemObject, &addr))
        {
            UInt32 deviceIDSize = sizeof (outputDeviceID);
            OSStatus status = AudioObjectGetPropertyData (kAudioObjectSystemObject, &addr, 0, nullptr, &deviceIDSize, &outputDeviceID);

            if (status == noErr)
            {
                addr.mElement  = kAudioObjectPropertyElementMaster;
                addr.mSelector = selector;
                addr.mScope    = kAudioDevicePropertyScopeOutput;

                if (! AudioObjectHasProperty (outputDeviceID, &addr))
                    outputDeviceID = kAudioObjectUnknown;
            }
        }
    }

    float getGain() const noexcept
    {
        Float32 gain = 0;

        if (outputDeviceID != kAudioObjectUnknown)
        {
            UInt32 size = sizeof (gain);
            AudioObjectGetPropertyData (outputDeviceID, &addr,  0, nullptr, &size, &gain);
        }

        return (float) gain;
    }

    bool setGain (float gain) const noexcept
    {
        if (outputDeviceID != kAudioObjectUnknown && canSetVolume())
        {
            Float32 newVolume = gain;
            UInt32 size = sizeof (newVolume);

            return AudioObjectSetPropertyData (outputDeviceID, &addr, 0, nullptr, size, &newVolume) == noErr;
        }

        return false;
    }

    bool isMuted() const noexcept
    {
        UInt32 muted = 0;

        if (outputDeviceID != kAudioObjectUnknown)
        {
            UInt32 size = sizeof (muted);
            AudioObjectGetPropertyData (outputDeviceID, &addr, 0, nullptr, &size, &muted);
        }

        return muted != 0;
    }

    bool setMuted (bool mute) const noexcept
    {
        if (outputDeviceID != kAudioObjectUnknown && canSetVolume())
        {
            UInt32 newMute = mute ? 1 : 0;
            UInt32 size = sizeof (newMute);

            return AudioObjectSetPropertyData (outputDeviceID, &addr, 0, nullptr, size, &newMute) == noErr;
        }

        return false;
    }

private:
    AudioDeviceID outputDeviceID;
    AudioObjectPropertyAddress addr;

    bool canSetVolume() const noexcept
    {
        Boolean isSettable = NO;
        return AudioObjectIsPropertySettable (outputDeviceID, &addr, &isSettable) == noErr && isSettable;
    }
};

JUCE_END_IGNORE_WARNINGS_GCC_LIKE

#define JUCE_SYSTEMAUDIOVOL_IMPLEMENTED 1
float JUCE_CALLTYPE SystemAudioVolume::getGain()              { return SystemVol (kAudioHardwareServiceDeviceProperty_VirtualMasterVolume).getGain(); }
bool  JUCE_CALLTYPE SystemAudioVolume::setGain (float gain)   { return SystemVol (kAudioHardwareServiceDeviceProperty_VirtualMasterVolume).setGain (gain); }
bool  JUCE_CALLTYPE SystemAudioVolume::isMuted()              { return SystemVol (kAudioDevicePropertyMute).isMuted(); }
bool  JUCE_CALLTYPE SystemAudioVolume::setMuted (bool mute)   { return SystemVol (kAudioDevicePropertyMute).setMuted (mute); }

//==============================================================================
struct CoreAudioClasses
{

class CoreAudioIODeviceType;
class CoreAudioIODevice;

//==============================================================================
class CoreAudioInternal  : private Timer
{
public:
    CoreAudioInternal (CoreAudioIODevice& d, AudioDeviceID id, bool input, bool output)
       : owner (d),
         deviceID (id),
         isInputDevice  (input),
         isOutputDevice (output)
    {
        jassert (deviceID != 0);

        updateDetailsFromDevice();
        JUCE_COREAUDIOLOG ("Creating CoreAudioInternal\n"
                           << (isInputDevice  ? ("    inputDeviceId "  + String (deviceID) + "\n") : "")
                           << (isOutputDevice ? ("    outputDeviceId " + String (deviceID) + "\n") : "")
                           << getDeviceDetails().joinIntoString ("\n    "));

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioObjectPropertySelectorWildcard;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectAddPropertyListener (deviceID, &pa, deviceListenerProc, this);
    }

    ~CoreAudioInternal() override
    {
        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioObjectPropertySelectorWildcard;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectRemovePropertyListener (deviceID, &pa, deviceListenerProc, this);

        stop (false);
    }

    void allocateTempBuffers()
    {
        auto tempBufSize = bufferSize + 4;
        audioBuffer.calloc ((numInputChans + numOutputChans) * tempBufSize);

        tempInputBuffers.calloc  (numInputChans + 2);
        tempOutputBuffers.calloc (numOutputChans + 2);

        int count = 0;
        for (int i = 0; i < numInputChans;  ++i)  tempInputBuffers[i]  = audioBuffer + count++ * tempBufSize;
        for (int i = 0; i < numOutputChans; ++i)  tempOutputBuffers[i] = audioBuffer + count++ * tempBufSize;
    }

    struct CallbackDetailsForChannel
    {
        int streamNum;
        int dataOffsetSamples;
        int dataStrideSamples;
    };

    // returns the number of actual available channels
    StringArray getChannelInfo (bool input, Array<CallbackDetailsForChannel>& newChannelInfo) const
    {
        StringArray newNames;
        int chanNum = 0;
        UInt32 size;

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioDevicePropertyStreamConfiguration;
        pa.mScope = input ? kAudioDevicePropertyScopeInput : kAudioDevicePropertyScopeOutput;
        pa.mElement = kAudioObjectPropertyElementMaster;

        if (OK (AudioObjectGetPropertyDataSize (deviceID, &pa, 0, nullptr, &size)))
        {
            HeapBlock<AudioBufferList> bufList;
            bufList.calloc (size, 1);

            if (OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, bufList)))
            {
                const int numStreams = (int) bufList->mNumberBuffers;

                for (int i = 0; i < numStreams; ++i)
                {
                    auto& b = bufList->mBuffers[i];

                    for (unsigned int j = 0; j < b.mNumberChannels; ++j)
                    {
                        String name;
                        NSString* nameNSString = nil;
                        size = sizeof (nameNSString);

                        pa.mSelector = kAudioObjectPropertyElementName;
                        pa.mElement = (AudioObjectPropertyElement) chanNum + 1;

                        if (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &nameNSString) == noErr)
                        {
                            name = nsStringToJuce (nameNSString);
                            [nameNSString release];
                        }

                        if ((input ? activeInputChans : activeOutputChans) [chanNum])
                        {
                            CallbackDetailsForChannel info = { i, (int) j, (int) b.mNumberChannels };
                            newChannelInfo.add (info);
                        }

                        if (name.isEmpty())
                            name << (input ? "Input " : "Output ") << (chanNum + 1);

                        newNames.add (name);
                        ++chanNum;
                    }
                }
            }
        }

        return newNames;
    }

    Array<double> getSampleRatesFromDevice() const
    {
        Array<double> newSampleRates;

        AudioObjectPropertyAddress pa;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementMaster;
        pa.mSelector = kAudioDevicePropertyAvailableNominalSampleRates;
        UInt32 size = 0;

        if (OK (AudioObjectGetPropertyDataSize (deviceID, &pa, 0, nullptr, &size)))
        {
            HeapBlock<AudioValueRange> ranges;
            ranges.calloc (size, 1);

            if (OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, ranges)))
            {
                for (auto r : { 8000, 11025, 16000, 22050, 32000,
                                44100, 48000, 88200, 96000, 176400,
                                192000, 352800, 384000, 705600, 768000 })
                {
                    auto rate = (double) r;

                    for (int j = size / (int) sizeof (AudioValueRange); --j >= 0;)
                    {
                        if (rate >= ranges[j].mMinimum - 2 && rate <= ranges[j].mMaximum + 2)
                        {
                            newSampleRates.add (rate);
                            break;
                        }
                    }
                }
            }
        }

        if (newSampleRates.isEmpty() && sampleRate > 0)
            newSampleRates.add (sampleRate);

        return newSampleRates;
    }

    Array<int> getBufferSizesFromDevice() const
    {
        Array<int> newBufferSizes;

        AudioObjectPropertyAddress pa;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementMaster;
        pa.mSelector = kAudioDevicePropertyBufferFrameSizeRange;
        UInt32 size = 0;

        if (OK (AudioObjectGetPropertyDataSize (deviceID, &pa, 0, nullptr, &size)))
        {
            HeapBlock<AudioValueRange> ranges;
            ranges.calloc (size, 1);

            if (OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, ranges)))
            {
                newBufferSizes.add ((int) (ranges[0].mMinimum + 15) & ~15);

                for (int i = 32; i <= 2048; i += 32)
                {
                    for (int j = size / (int) sizeof (AudioValueRange); --j >= 0;)
                    {
                        if (i >= ranges[j].mMinimum && i <= ranges[j].mMaximum)
                        {
                            newBufferSizes.addIfNotAlreadyThere (i);
                            break;
                        }
                    }
                }

                if (bufferSize > 0)
                    newBufferSizes.addIfNotAlreadyThere (bufferSize);
            }
        }

        if (newBufferSizes.isEmpty() && bufferSize > 0)
            newBufferSizes.add (bufferSize);

        return newBufferSizes;
    }

    int getLatencyFromDevice (AudioObjectPropertyScope scope) const
    {
        UInt32 latency = 0;
        UInt32 size = sizeof (latency);
        AudioObjectPropertyAddress pa;
        pa.mElement = kAudioObjectPropertyElementMaster;
        pa.mSelector = kAudioDevicePropertyLatency;
        pa.mScope = scope;
        AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &latency);

        UInt32 safetyOffset = 0;
        size = sizeof (safetyOffset);
        pa.mSelector = kAudioDevicePropertySafetyOffset;
        AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &safetyOffset);

        return (int) (latency + safetyOffset);
    }

    int getBitDepthFromDevice (AudioObjectPropertyScope scope) const
    {
        AudioObjectPropertyAddress pa;
        pa.mElement = kAudioObjectPropertyElementMaster;
        pa.mSelector = kAudioStreamPropertyPhysicalFormat;
        pa.mScope = scope;

        AudioStreamBasicDescription asbd;
        UInt32 size = sizeof (asbd);

        if (OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &asbd)))
            return (int) asbd.mBitsPerChannel;

        return 0;
    }

    int getFrameSizeFromDevice() const
    {
        AudioObjectPropertyAddress pa;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementMaster;
        pa.mSelector = kAudioDevicePropertyBufferFrameSize;

        UInt32 framesPerBuf = (UInt32) bufferSize;
        UInt32 size = sizeof (framesPerBuf);
        AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &framesPerBuf);
        return (int) framesPerBuf;
    }

    bool isDeviceAlive() const
    {
        AudioObjectPropertyAddress pa;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementMaster;
        pa.mSelector = kAudioDevicePropertyDeviceIsAlive;

        UInt32 isAlive = 0;
        UInt32 size = sizeof (isAlive);
        return deviceID != 0
                && OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &isAlive))
                && isAlive != 0;
    }

    bool updateDetailsFromDevice()
    {
        stopTimer();

        if (! isDeviceAlive())
            return false;

        // this collects all the new details from the device without any locking, then
        // locks + swaps them afterwards.

        auto newSampleRate = getNominalSampleRate();
        auto newBufferSize = getFrameSizeFromDevice();

        auto newBufferSizes = getBufferSizesFromDevice();
        auto newSampleRates = getSampleRatesFromDevice();

        auto newInputLatency  = getLatencyFromDevice (kAudioDevicePropertyScopeInput);
        auto newOutputLatency = getLatencyFromDevice (kAudioDevicePropertyScopeOutput);

        Array<CallbackDetailsForChannel> newInChans, newOutChans;
        auto newInNames  = isInputDevice  ? getChannelInfo (true,  newInChans)  : StringArray();
        auto newOutNames = isOutputDevice ? getChannelInfo (false, newOutChans) : StringArray();

        auto inputBitDepth  = isInputDevice  ? getBitDepthFromDevice (kAudioDevicePropertyScopeInput)  : 0;
        auto outputBitDepth = isOutputDevice ? getBitDepthFromDevice (kAudioDevicePropertyScopeOutput) : 0;
        auto newBitDepth = jmax (inputBitDepth, outputBitDepth);

        {
            const ScopedLock sl (callbackLock);

            bitDepth = newBitDepth > 0 ? newBitDepth : 32;

            if (newSampleRate > 0)
                sampleRate = newSampleRate;

            inputLatency  = newInputLatency;
            outputLatency = newOutputLatency;
            bufferSize = newBufferSize;

            sampleRates.swapWith (newSampleRates);
            bufferSizes.swapWith (newBufferSizes);

            inChanNames.swapWith (newInNames);
            outChanNames.swapWith (newOutNames);

            inputChannelInfo.swapWith (newInChans);
            outputChannelInfo.swapWith (newOutChans);

            numInputChans  = inputChannelInfo.size();
            numOutputChans = outputChannelInfo.size();

            allocateTempBuffers();
        }

        return true;
    }

    StringArray getDeviceDetails()
    {
        StringArray result;

        String availableSampleRates ("Available sample rates:");

        for (auto& s : sampleRates)
            availableSampleRates << " " << s;

        result.add (availableSampleRates);
        result.add ("Sample rate: " + String (sampleRate));
        String availableBufferSizes ("Available buffer sizes:");

        for (auto& b : bufferSizes)
            availableBufferSizes << " " << b;

        result.add (availableBufferSizes);
        result.add ("Buffer size: " + String (bufferSize));
        result.add ("Bit depth: " + String (bitDepth));
        result.add ("Input latency: " + String (inputLatency));
        result.add ("Output latency: " + String (outputLatency));
        result.add ("Input channel names: "  +  inChanNames.joinIntoString (" "));
        result.add ("Output channel names: " + outChanNames.joinIntoString (" "));

        return result;
    }

    //==============================================================================
    StringArray getSources (bool input)
    {
        StringArray s;
        HeapBlock<OSType> types;
        auto num = getAllDataSourcesForDevice (deviceID, types);

        for (int i = 0; i < num; ++i)
        {
            AudioValueTranslation avt;
            char buffer[256];

            avt.mInputData = &(types[i]);
            avt.mInputDataSize = sizeof (UInt32);
            avt.mOutputData = buffer;
            avt.mOutputDataSize = 256;

            UInt32 transSize = sizeof (avt);

            AudioObjectPropertyAddress pa;
            pa.mSelector = kAudioDevicePropertyDataSourceNameForID;
            pa.mScope = input ? kAudioDevicePropertyScopeInput : kAudioDevicePropertyScopeOutput;
            pa.mElement = kAudioObjectPropertyElementMaster;

            if (OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &transSize, &avt)))
                s.add (buffer);
        }

        return s;
    }

    int getCurrentSourceIndex (bool input) const
    {
        OSType currentSourceID = 0;
        UInt32 size = sizeof (currentSourceID);
        int result = -1;

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioDevicePropertyDataSource;
        pa.mScope = input ? kAudioDevicePropertyScopeInput : kAudioDevicePropertyScopeOutput;
        pa.mElement = kAudioObjectPropertyElementMaster;

        if (deviceID != 0)
        {
            if (OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &currentSourceID)))
            {
                HeapBlock<OSType> types;
                auto num = getAllDataSourcesForDevice (deviceID, types);

                for (int i = 0; i < num; ++i)
                {
                    if (types[num] == currentSourceID)
                    {
                        result = i;
                        break;
                    }
                }
            }
        }

        return result;
    }

    void setCurrentSourceIndex (int index, bool input)
    {
        if (deviceID != 0)
        {
            HeapBlock<OSType> types;
            auto num = getAllDataSourcesForDevice (deviceID, types);

            if (isPositiveAndBelow (index, num))
            {
                AudioObjectPropertyAddress pa;
                pa.mSelector = kAudioDevicePropertyDataSource;
                pa.mScope = input ? kAudioDevicePropertyScopeInput : kAudioDevicePropertyScopeOutput;
                pa.mElement = kAudioObjectPropertyElementMaster;

                OSType typeId = types[index];

                OK (AudioObjectSetPropertyData (deviceID, &pa, 0, nullptr, sizeof (typeId), &typeId));
            }
        }
    }

    double getNominalSampleRate() const
    {
        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioDevicePropertyNominalSampleRate;
        pa.mScope = kAudioObjectPropertyScopeGlobal;
        pa.mElement = kAudioObjectPropertyElementMaster;
        Float64 sr = 0;
        UInt32 size = (UInt32) sizeof (sr);
        return OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &sr)) ? (double) sr : 0.0;
    }

    bool setNominalSampleRate (double newSampleRate) const
    {
        if (std::abs (getNominalSampleRate() - newSampleRate) < 1.0)
            return true;

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioDevicePropertyNominalSampleRate;
        pa.mScope = kAudioObjectPropertyScopeGlobal;
        pa.mElement = kAudioObjectPropertyElementMaster;
        Float64 sr = newSampleRate;
        return OK (AudioObjectSetPropertyData (deviceID, &pa, 0, nullptr, sizeof (sr), &sr));
    }

    //==============================================================================
    String reopen (const BigInteger& inputChannels,
                   const BigInteger& outputChannels,
                   double newSampleRate, int bufferSizeSamples)
    {
        String error;
        callbacksAllowed = false;
        stopTimer();

        stop (false);

        updateDetailsFromDevice();

        activeInputChans = inputChannels;
        activeInputChans.setRange (inChanNames.size(),
                                   activeInputChans.getHighestBit() + 1 - inChanNames.size(),
                                   false);

        activeOutputChans = outputChannels;
        activeOutputChans.setRange (outChanNames.size(),
                                    activeOutputChans.getHighestBit() + 1 - outChanNames.size(),
                                    false);

        numInputChans = activeInputChans.countNumberOfSetBits();
        numOutputChans = activeOutputChans.countNumberOfSetBits();

        if (! setNominalSampleRate (newSampleRate))
        {
            updateDetailsFromDevice();
            error = "Couldn't change sample rate";
        }
        else
        {
            // change buffer size
            AudioObjectPropertyAddress pa;
            pa.mSelector = kAudioDevicePropertyBufferFrameSize;
            pa.mScope = kAudioObjectPropertyScopeGlobal;
            pa.mElement = kAudioObjectPropertyElementMaster;
            UInt32 framesPerBuf = (UInt32) bufferSizeSamples;

            if (! OK (AudioObjectSetPropertyData (deviceID, &pa, 0, nullptr, sizeof (framesPerBuf), &framesPerBuf)))
            {
                updateDetailsFromDevice();
                error = "Couldn't change buffer size";
            }
            else
            {
                // Annoyingly, after changing the rate and buffer size, some devices fail to
                // correctly report their new settings until some random time in the future, so
                // after calling updateDetailsFromDevice, we need to manually bodge these values
                // to make sure we're using the correct numbers..
                updateDetailsFromDevice();
                sampleRate = newSampleRate;
                bufferSize = bufferSizeSamples;

                if (sampleRates.size() == 0)
                    error = "Device has no available sample-rates";
                else if (bufferSizes.size() == 0)
                    error = "Device has no available buffer-sizes";
            }
        }

        callbacksAllowed = true;
        return error;
    }

    bool start()
    {
        if (! started)
        {
            callback = nullptr;

            if (deviceID != 0)
            {
                if (OK (AudioDeviceCreateIOProcID (deviceID, audioIOProc, this, &audioProcID)))
                {
                    if (OK (AudioDeviceStart (deviceID, audioIOProc)))
                    {
                        started = true;
                    }
                    else
                    {
                        OK (AudioDeviceDestroyIOProcID (deviceID, audioProcID));
                        audioProcID = {};
                    }
                }
            }
        }

        return started;
    }

    void setCallback (AudioIODeviceCallback* cb)
    {
        const ScopedLock sl (callbackLock);
        callback = cb;
    }

    void stop (bool leaveInterruptRunning)
    {
        {
            const ScopedLock sl (callbackLock);
            callback = nullptr;
        }

        if (started && (deviceID != 0) && ! leaveInterruptRunning)
        {
            OK (AudioDeviceStop (deviceID, audioIOProc));
            OK (AudioDeviceDestroyIOProcID (deviceID, audioProcID));
            audioProcID = {};

            started = false;

            { const ScopedLock sl (callbackLock); }

            // wait until it's definitely stopped calling back..
            for (int i = 40; --i >= 0;)
            {
                Thread::sleep (50);

                UInt32 running = 0;
                UInt32 size = sizeof (running);

                AudioObjectPropertyAddress pa;
                pa.mSelector = kAudioDevicePropertyDeviceIsRunning;
                pa.mScope = kAudioObjectPropertyScopeWildcard;
                pa.mElement = kAudioObjectPropertyElementMaster;

                OK (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, &running));

                if (running == 0)
                    break;
            }

            const ScopedLock sl (callbackLock);
        }
    }

    double getSampleRate() const  { return sampleRate; }
    int getBufferSize() const     { return bufferSize; }

    void audioCallback (const AudioBufferList* inInputData,
                        AudioBufferList* outOutputData)
    {
        const ScopedLock sl (callbackLock);

        if (callback != nullptr)
        {
            for (int i = numInputChans; --i >= 0;)
            {
                auto& info = inputChannelInfo.getReference(i);
                auto dest = tempInputBuffers[i];
                auto src = ((const float*) inInputData->mBuffers[info.streamNum].mData) + info.dataOffsetSamples;
                auto stride = info.dataStrideSamples;

                if (stride != 0) // if this is zero, info is invalid
                {
                    for (int j = bufferSize; --j >= 0;)
                    {
                        *dest++ = *src;
                        src += stride;
                    }
                }
            }

            callback->audioDeviceIOCallback (const_cast<const float**> (tempInputBuffers.get()),
                                             numInputChans,
                                             tempOutputBuffers,
                                             numOutputChans,
                                             bufferSize);

            for (int i = numOutputChans; --i >= 0;)
            {
                auto& info = outputChannelInfo.getReference (i);
                auto src = tempOutputBuffers[i];
                auto dest = ((float*) outOutputData->mBuffers[info.streamNum].mData) + info.dataOffsetSamples;
                auto stride = info.dataStrideSamples;

                if (stride != 0) // if this is zero, info is invalid
                {
                    for (int j = bufferSize; --j >= 0;)
                    {
                        *dest = *src++;
                        dest += stride;
                    }
                }
            }
        }
        else
        {
            for (UInt32 i = 0; i < outOutputData->mNumberBuffers; ++i)
                zeromem (outOutputData->mBuffers[i].mData,
                         outOutputData->mBuffers[i].mDataByteSize);
        }
    }

    // called by callbacks
    void deviceDetailsChanged()
    {
        if (callbacksAllowed.get() == 1)
            startTimer (100);
    }

    void timerCallback() override
    {
        JUCE_COREAUDIOLOG ("Device changed");

        stopTimer();
        auto oldSampleRate = sampleRate;
        auto oldBufferSize = bufferSize;

        if (! updateDetailsFromDevice())
            owner.stopInternal();
        else if ((oldBufferSize != bufferSize || oldSampleRate != sampleRate) && owner.shouldRestartDevice())
            owner.restart();
    }

    //==============================================================================
    CoreAudioIODevice& owner;
    int inputLatency  = 0;
    int outputLatency = 0;
    int bitDepth = 32;
    int xruns = 0;
    BigInteger activeInputChans, activeOutputChans;
    StringArray inChanNames, outChanNames;
    Array<double> sampleRates;
    Array<int> bufferSizes;
    AudioIODeviceCallback* callback = nullptr;
    AudioDeviceIOProcID audioProcID = {};

private:
    CriticalSection callbackLock;
    AudioDeviceID deviceID;
    bool started = false;
    double sampleRate = 0;
    int bufferSize = 512;
    HeapBlock<float> audioBuffer;
    int numInputChans  = 0;
    int numOutputChans = 0;
    Atomic<int> callbacksAllowed { 1 };
    const bool isInputDevice, isOutputDevice;

    Array<CallbackDetailsForChannel> inputChannelInfo, outputChannelInfo;
    HeapBlock<float*> tempInputBuffers, tempOutputBuffers;

    //==============================================================================
    static OSStatus audioIOProc (AudioDeviceID /*inDevice*/,
                                 const AudioTimeStamp* /*inNow*/,
                                 const AudioBufferList* inInputData,
                                 const AudioTimeStamp* /*inInputTime*/,
                                 AudioBufferList* outOutputData,
                                 const AudioTimeStamp* /*inOutputTime*/,
                                 void* device)
    {
        static_cast<CoreAudioInternal*> (device)->audioCallback (inInputData, outOutputData);
        return noErr;
    }

    static OSStatus deviceListenerProc (AudioDeviceID /*inDevice*/, UInt32 /*inLine*/,
                                        const AudioObjectPropertyAddress* pa, void* inClientData)
    {
        auto intern = static_cast<CoreAudioInternal*> (inClientData);

        switch (pa->mSelector)
        {
            case kAudioDeviceProcessorOverload:
                intern->xruns++;
                break;

            case kAudioDevicePropertyBufferSize:
            case kAudioDevicePropertyBufferFrameSize:
            case kAudioDevicePropertyNominalSampleRate:
            case kAudioDevicePropertyStreamFormat:
            case kAudioDevicePropertyDeviceIsAlive:
            case kAudioStreamPropertyPhysicalFormat:
                intern->deviceDetailsChanged();
                break;

            case kAudioDevicePropertyDeviceHasChanged:
            case kAudioObjectPropertyOwnedObjects:
                intern->owner.restart();

                if (intern->owner.deviceType != nullptr)
                    intern->owner.deviceType->triggerAsyncAudioDeviceListChange();
                break;

            case kAudioDevicePropertyBufferSizeRange:
            case kAudioDevicePropertyVolumeScalar:
            case kAudioDevicePropertyMute:
            case kAudioDevicePropertyPlayThru:
            case kAudioDevicePropertyDataSource:
            case kAudioDevicePropertyDeviceIsRunning:
                break;
        }

        return noErr;
    }

    //==============================================================================
    static int getAllDataSourcesForDevice (AudioDeviceID deviceID, HeapBlock<OSType>& types)
    {
        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioDevicePropertyDataSources;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementMaster;
        UInt32 size = 0;

        if (deviceID != 0
             && AudioObjectGetPropertyDataSize (deviceID, &pa, 0, nullptr, &size) == noErr)
        {
            types.calloc (size, 1);

            if (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, types) == noErr)
                return size / (int) sizeof (OSType);
        }

        return 0;
    }

    bool OK (const OSStatus errorCode) const
    {
        if (errorCode == noErr)
            return true;

        const String errorMessage ("CoreAudio error: " + String::toHexString ((int) errorCode));
        JUCE_COREAUDIOLOG (errorMessage);

        if (callback != nullptr)
            callback->audioDeviceError (errorMessage);

        return false;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CoreAudioInternal)
};


//==============================================================================
class CoreAudioIODevice   : public AudioIODevice,
                            private Timer
{
public:
    CoreAudioIODevice (CoreAudioIODeviceType* dt,
                       const String& deviceName,
                       AudioDeviceID inputDeviceId, int inputIndex_,
                       AudioDeviceID outputDeviceId, int outputIndex_)
        : AudioIODevice (deviceName, "CoreAudio"),
          deviceType (dt),
          inputIndex (inputIndex_),
          outputIndex (outputIndex_)
    {
        CoreAudioInternal* device = nullptr;

        if (outputDeviceId == 0 || outputDeviceId == inputDeviceId)
        {
            jassert (inputDeviceId != 0);
            device = new CoreAudioInternal (*this, inputDeviceId, true, outputDeviceId != 0);
        }
        else
        {
            device = new CoreAudioInternal (*this, outputDeviceId, false, true);
        }

        jassert (device != nullptr);
        internal.reset (device);

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioObjectPropertySelectorWildcard;
        pa.mScope    = kAudioObjectPropertyScopeWildcard;
        pa.mElement  = kAudioObjectPropertyElementWildcard;

        AudioObjectAddPropertyListener (kAudioObjectSystemObject, &pa, hardwareListenerProc, internal.get());
    }

    ~CoreAudioIODevice() override
    {
        close();

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioObjectPropertySelectorWildcard;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectRemovePropertyListener (kAudioObjectSystemObject, &pa, hardwareListenerProc, internal.get());
    }

    StringArray getOutputChannelNames() override        { return internal->outChanNames; }
    StringArray getInputChannelNames() override         { return internal->inChanNames; }

    bool isOpen() override                              { return isOpen_; }

    Array<double> getAvailableSampleRates() override    { return internal->sampleRates; }
    Array<int> getAvailableBufferSizes() override       { return internal->bufferSizes; }

    double getCurrentSampleRate() override              { return internal->getSampleRate(); }
    int getCurrentBitDepth() override                   { return internal->bitDepth; }
    int getCurrentBufferSizeSamples() override          { return internal->getBufferSize(); }
    int getXRunCount() const noexcept override          { return internal->xruns; }

    int getDefaultBufferSize() override
    {
        int best = 0;

        for (int i = 0; best < 512 && i < internal->bufferSizes.size(); ++i)
            best = internal->bufferSizes.getUnchecked(i);

        if (best == 0)
            best = 512;

        return best;
    }

    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double sampleRate, int bufferSizeSamples) override
    {
        isOpen_ = true;
        internal->xruns = 0;

        inputChannelsRequested = inputChannels;
        outputChannelsRequested = outputChannels;

        if (bufferSizeSamples <= 0)
            bufferSizeSamples = getDefaultBufferSize();

        if (sampleRate <= 0)
            sampleRate = internal->getNominalSampleRate();

        lastError = internal->reopen (inputChannels, outputChannels, sampleRate, bufferSizeSamples);
        JUCE_COREAUDIOLOG ("Opened: " << getName());

        isOpen_ = lastError.isEmpty();

        return lastError;
    }

    void close() override
    {
        isOpen_ = false;
        internal->stop (false);
    }

    BigInteger getActiveOutputChannels() const override     { return internal->activeOutputChans; }
    BigInteger getActiveInputChannels() const override      { return internal->activeInputChans; }

    int getOutputLatencyInSamples() override
    {
        // this seems like a good guess at getting the latency right - comparing
        // this with a round-trip measurement, it gets it to within a few millisecs
        // for the built-in mac soundcard
        return internal->outputLatency;
    }

    int getInputLatencyInSamples() override
    {
        return internal->inputLatency;
    }

    void start (AudioIODeviceCallback* callback) override
    {
        if (! isStarted)
        {
            if (callback != nullptr)
                callback->audioDeviceAboutToStart (this);

            isStarted = internal->start();

            if (isStarted)
            {
                internal->setCallback (callback);
                previousCallback = callback;
            }
        }
    }

    void stop() override
    {
        restartDevice = false;

        if (isStarted)
        {
            auto lastCallback = internal->callback;

            isStarted = false;
            internal->stop (true);

            if (lastCallback != nullptr)
                lastCallback->audioDeviceStopped();
        }
    }

    void stopInternal()
    {
        stop();
        restartDevice = true;
    }

    bool isPlaying() override
    {
        if (internal->callback == nullptr)
            isStarted = false;

        return isStarted;
    }

    String getLastError() override
    {
        return lastError;
    }

    void audioDeviceListChanged()
    {
        if (deviceType != nullptr)
            deviceType->audioDeviceListChanged();
    }

    void restart()
    {
        if (deviceWrapperRestartCallback != nullptr)
        {
            deviceWrapperRestartCallback();
        }
        else
        {
            {
                const ScopedLock sl (closeLock);

                if (isStarted)
                {
                    if (internal->callback != nullptr)
                        previousCallback = internal->callback;

                    stopInternal();
                }
            }

            startTimer (100);
        }
    }

    bool setCurrentSampleRate (double newSampleRate)
    {
        return internal->setNominalSampleRate (newSampleRate);
    }

    void setDeviceWrapperRestartCallback (const std::function<void()>& cb)
    {
        deviceWrapperRestartCallback = cb;
    }

    bool shouldRestartDevice() const noexcept    { return restartDevice; }

    WeakReference<CoreAudioIODeviceType> deviceType;
    int inputIndex, outputIndex;

private:
    std::unique_ptr<CoreAudioInternal> internal;
    bool isOpen_ = false, isStarted = false, restartDevice = true;
    String lastError;
    AudioIODeviceCallback* previousCallback = nullptr;
    std::function<void()> deviceWrapperRestartCallback = nullptr;
    BigInteger inputChannelsRequested, outputChannelsRequested;
    CriticalSection closeLock;

    void timerCallback() override
    {
        stopTimer();

        stopInternal();

        internal->updateDetailsFromDevice();

        open (inputChannelsRequested, outputChannelsRequested,
              getCurrentSampleRate(), getCurrentBufferSizeSamples());
        start (previousCallback);
    }

    static OSStatus hardwareListenerProc (AudioDeviceID /*inDevice*/, UInt32 /*inLine*/, const AudioObjectPropertyAddress* pa, void* inClientData)
    {
        switch (pa->mSelector)
        {
            case kAudioHardwarePropertyDevices:
                static_cast<CoreAudioInternal*> (inClientData)->deviceDetailsChanged();
                break;

            case kAudioHardwarePropertyDefaultOutputDevice:
            case kAudioHardwarePropertyDefaultInputDevice:
            case kAudioHardwarePropertyDefaultSystemOutputDevice:
                break;
        }

        return noErr;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CoreAudioIODevice)
};

//==============================================================================
class AudioIODeviceCombiner    : public AudioIODevice,
                                 private Thread,
                                 private Timer
{
public:
    AudioIODeviceCombiner (const String& deviceName, CoreAudioIODeviceType* deviceType)
        : AudioIODevice (deviceName, "CoreAudio"),
          Thread (deviceName),
          owner (deviceType)
    {
    }

    ~AudioIODeviceCombiner() override
    {
        close();
        devices.clear();
    }

    void addDevice (CoreAudioIODevice* device, bool useInputs, bool useOutputs)
    {
        jassert (device != nullptr);
        jassert (! isOpen());
        jassert (! device->isOpen());
        devices.add (new DeviceWrapper (*this, device, useInputs, useOutputs));

        if (currentSampleRate == 0)
            currentSampleRate = device->getCurrentSampleRate();

        if (currentBufferSize == 0)
            currentBufferSize = device->getCurrentBufferSizeSamples();
    }

    Array<AudioIODevice*> getDevices() const
    {
        Array<AudioIODevice*> devs;

        for (auto* d : devices)
            devs.add (d->device.get());

        return devs;
    }

    StringArray getOutputChannelNames() override
    {
        StringArray names;

        for (auto* d : devices)
            names.addArray (d->getOutputChannelNames());

        names.appendNumbersToDuplicates (false, true);
        return names;
    }

    StringArray getInputChannelNames() override
    {
        StringArray names;

        for (auto* d : devices)
            names.addArray (d->getInputChannelNames());

        names.appendNumbersToDuplicates (false, true);
        return names;
    }

    Array<double> getAvailableSampleRates() override
    {
        Array<double> commonRates;
        bool first = true;

        for (auto* d : devices)
        {
            auto rates = d->device->getAvailableSampleRates();

            if (first)
            {
                first = false;
                commonRates = rates;
            }
            else
            {
                commonRates.removeValuesNotIn (rates);
            }
        }

        return commonRates;
    }

    Array<int> getAvailableBufferSizes() override
    {
        Array<int> commonSizes;
        bool first = true;

        for (auto* d : devices)
        {
            auto sizes = d->device->getAvailableBufferSizes();

            if (first)
            {
                first = false;
                commonSizes = sizes;
            }
            else
            {
                commonSizes.removeValuesNotIn (sizes);
            }
        }

        return commonSizes;
    }

    bool isOpen() override                          { return active; }
    bool isPlaying() override                       { return callback != nullptr; }
    double getCurrentSampleRate() override          { return currentSampleRate; }
    int getCurrentBufferSizeSamples() override      { return currentBufferSize; }

    int getCurrentBitDepth() override
    {
        int depth = 32;

        for (auto* d : devices)
            depth = jmin (depth, d->device->getCurrentBitDepth());

        return depth;
    }

    int getDefaultBufferSize() override
    {
        int size = 0;

        for (auto* d : devices)
            size = jmax (size, d->device->getDefaultBufferSize());

        return size;
    }

    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double sampleRate, int bufferSize) override
    {
        inputChannelsRequested = inputChannels;
        outputChannelsRequested = outputChannels;
        sampleRateRequested = sampleRate;
        bufferSizeRequested = bufferSize;

        close();
        active = true;

        if (bufferSize <= 0)
            bufferSize = getDefaultBufferSize();

        if (sampleRate <= 0)
        {
            auto rates = getAvailableSampleRates();

            for (int i = 0; i < rates.size() && sampleRate < 44100.0; ++i)
                sampleRate = rates.getUnchecked(i);
        }

        currentSampleRate = sampleRate;
        currentBufferSize = bufferSize;

        const int fifoSize = bufferSize * 3 + 1;
        int totalInputChanIndex = 0, totalOutputChanIndex = 0;
        int chanIndex = 0;

        for (auto* d : devices)
        {
            BigInteger ins (inputChannels >> totalInputChanIndex);
            BigInteger outs (outputChannels >> totalOutputChanIndex);

            int numIns  = d->getInputChannelNames().size();
            int numOuts = d->getOutputChannelNames().size();

            totalInputChanIndex += numIns;
            totalOutputChanIndex += numOuts;

            String err = d->open (ins, outs, sampleRate, bufferSize,
                                  chanIndex, fifoSize);

            if (err.isNotEmpty())
            {
                close();
                lastError = err;
                return err;
            }

            chanIndex += d->numInputChans + d->numOutputChans;
        }

        fifos.setSize (chanIndex, fifoSize);
        fifoReadPointers  = fifos.getArrayOfReadPointers();
        fifoWritePointers = fifos.getArrayOfWritePointers();
        fifos.clear();
        startThread (9);
        threadInitialised.wait();

        return {};
    }

    void close() override
    {
        stop();
        stopThread (10000);
        fifos.clear();
        active = false;

        for (auto* d : devices)
            d->close();
    }

    void restart (AudioIODeviceCallback* cb)
    {
        const ScopedLock sl (closeLock);

        close();

        auto newSampleRate = sampleRateRequested;
        auto newBufferSize = bufferSizeRequested;

        for (auto* d : devices)
        {
            auto deviceSampleRate = d->getCurrentSampleRate();

            if (deviceSampleRate != sampleRateRequested)
            {
                if (! getAvailableSampleRates().contains (deviceSampleRate))
                    return;

                for (auto* d2 : devices)
                    if (d2 != d)
                        d2->setCurrentSampleRate (deviceSampleRate);

                newSampleRate = deviceSampleRate;
                break;
            }
        }

        for (auto* d : devices)
        {
            auto deviceBufferSize = d->getCurrentBufferSizeSamples();

            if (deviceBufferSize != bufferSizeRequested)
            {
                if (! getAvailableBufferSizes().contains (deviceBufferSize))
                    return;

                newBufferSize = deviceBufferSize;
                break;
            }
        }

        open (inputChannelsRequested, outputChannelsRequested,
              newSampleRate, newBufferSize);

        start (cb);
    }

    void restartAsync()
    {
        {
            const ScopedLock sl (closeLock);

            if (active)
            {
                if (callback != nullptr)
                    previousCallback = callback;

                close();
            }
        }

        startTimer (100);
    }

    BigInteger getActiveOutputChannels() const override
    {
        BigInteger chans;
        int start = 0;

        for (auto* d : devices)
        {
            auto numChans = d->getOutputChannelNames().size();

            if (numChans > 0)
            {
                chans |= (d->device->getActiveOutputChannels() << start);
                start += numChans;
            }
        }

        return chans;
    }

    BigInteger getActiveInputChannels() const override
    {
        BigInteger chans;
        int start = 0;

        for (auto* d : devices)
        {
            auto numChans = d->getInputChannelNames().size();

            if (numChans > 0)
            {
                chans |= (d->device->getActiveInputChannels() << start);
                start += numChans;
            }
        }

        return chans;
    }

    int getOutputLatencyInSamples() override
    {
        int lat = 0;

        for (auto* d : devices)
            lat = jmax (lat, d->device->getOutputLatencyInSamples());

        return lat + currentBufferSize * 2;
    }

    int getInputLatencyInSamples() override
    {
        int lat = 0;

        for (auto* d : devices)
            lat = jmax (lat, d->device->getInputLatencyInSamples());

        return lat + currentBufferSize * 2;
    }

    void start (AudioIODeviceCallback* newCallback) override
    {
        if (callback != newCallback)
        {
            stop();
            fifos.clear();

            for (auto* d : devices)
                d->start();

            if (newCallback != nullptr)
                newCallback->audioDeviceAboutToStart (this);

            const ScopedLock sl (callbackLock);
            callback = newCallback;
            previousCallback = callback;
        }
    }

    void stop() override    { shutdown ({}); }

    String getLastError() override
    {
        return lastError;
    }

private:
    WeakReference<CoreAudioIODeviceType> owner;
    CriticalSection callbackLock;
    AudioIODeviceCallback* callback = nullptr;
    AudioIODeviceCallback* previousCallback = nullptr;
    double currentSampleRate = 0;
    int currentBufferSize = 0;
    bool active = false;
    String lastError;
    AudioBuffer<float> fifos;
    const float** fifoReadPointers = nullptr;
    float** fifoWritePointers = nullptr;
    WaitableEvent threadInitialised;
    CriticalSection closeLock;

    BigInteger inputChannelsRequested, outputChannelsRequested;
    double sampleRateRequested = 44100;
    int bufferSizeRequested = 512;

    void run() override
    {
        auto numSamples = currentBufferSize;

        AudioBuffer<float> buffer (fifos.getNumChannels(), numSamples);
        buffer.clear();

        Array<const float*> inputChans;
        Array<float*> outputChans;

        for (auto* d : devices)
        {
            for (int j = 0; j < d->numInputChans; ++j)   inputChans.add  (buffer.getReadPointer  (d->inputIndex  + j));
            for (int j = 0; j < d->numOutputChans; ++j)  outputChans.add (buffer.getWritePointer (d->outputIndex + j));
        }

        auto numInputChans  = inputChans.size();
        auto numOutputChans = outputChans.size();

        inputChans.add (nullptr);
        outputChans.add (nullptr);

        auto blockSizeMs = jmax (1, (int) (1000 * numSamples / currentSampleRate));

        jassert (numInputChans + numOutputChans == buffer.getNumChannels());

        threadInitialised.signal();

        while (! threadShouldExit())
        {
            readInput (buffer, numSamples, blockSizeMs);

            bool didCallback = true;

            {
                const ScopedLock sl (callbackLock);

                if (callback != nullptr)
                    callback->audioDeviceIOCallback ((const float**) inputChans.getRawDataPointer(), numInputChans,
                                                     outputChans.getRawDataPointer(), numOutputChans, numSamples);
                else
                    didCallback = false;
            }

            if (didCallback)
            {
                pushOutputData (buffer, numSamples, blockSizeMs);
            }
            else
            {
                for (int i = 0; i < numOutputChans; ++i)
                    FloatVectorOperations::clear (outputChans[i], numSamples);

                reset();
            }
        }
    }

    void timerCallback() override
    {
        stopTimer();

        restart (previousCallback);
    }

    void shutdown (const String& error)
    {
        AudioIODeviceCallback* lastCallback = nullptr;

        {
            const ScopedLock sl (callbackLock);
            std::swap (callback, lastCallback);
        }

        for (auto* d : devices)
            d->device->stopInternal();

        if (lastCallback != nullptr)
        {
            if (error.isNotEmpty())
                lastCallback->audioDeviceError (error);
            else
                lastCallback->audioDeviceStopped();
        }
    }

    void reset()
    {
        for (auto* d : devices)
            d->reset();
    }

    void underrun()
    {
    }

    void readInput (AudioBuffer<float>& buffer, const int numSamples, const int blockSizeMs)
    {
        for (auto* d : devices)
            d->done = (d->numInputChans == 0);

        for (int tries = 5;;)
        {
            bool anyRemaining = false;

            for (auto* d : devices)
            {
                if (! d->done)
                {
                    if (d->isInputReady (numSamples))
                    {
                        d->readInput (buffer, numSamples);
                        d->done = true;
                    }
                    else
                        anyRemaining = true;
                }
            }

            if (! anyRemaining)
                return;

            if (--tries == 0)
                break;

            wait (blockSizeMs);
        }

        for (auto* d : devices)
            if (! d->done)
                for (int i = 0; i < d->numInputChans; ++i)
                    buffer.clear (d->inputIndex + i, 0, numSamples);
    }

    void pushOutputData (AudioBuffer<float>& buffer, const int numSamples, const int blockSizeMs)
    {
        for (auto* d : devices)
            d->done = (d->numOutputChans == 0);

        for (int tries = 5;;)
        {
            bool anyRemaining = false;

            for (auto* d : devices)
            {
                if (! d->done)
                {
                    if (d->isOutputReady (numSamples))
                    {
                        d->pushOutputData (buffer, numSamples);
                        d->done = true;
                    }
                    else
                        anyRemaining = true;
                }
            }

            if ((! anyRemaining) || --tries == 0)
                return;

            wait (blockSizeMs);
        }
    }

    void handleAudioDeviceAboutToStart (AudioIODevice* device)
    {
        const ScopedLock sl (callbackLock);

        auto newSampleRate = device->getCurrentSampleRate();
        auto commonRates = getAvailableSampleRates();

        if (! commonRates.contains (newSampleRate))
        {
            commonRates.sort();

            if (newSampleRate < commonRates.getFirst() || newSampleRate > commonRates.getLast())
            {
                newSampleRate = jlimit (commonRates.getFirst(), commonRates.getLast(), newSampleRate);
            }
            else
            {
                for (auto it = commonRates.begin(); it < commonRates.end() - 1; ++it)
                {
                    if (it[0] < newSampleRate && it[1] > newSampleRate)
                    {
                        newSampleRate = newSampleRate - it[0] < it[1] - newSampleRate ? it[0] : it[1];
                        break;
                    }
                }
            }
        }

        currentSampleRate = newSampleRate;
        bool anySampleRateChanges = false;

        for (auto* d : devices)
        {
            if (d->getCurrentSampleRate() != currentSampleRate)
            {
                d->setCurrentSampleRate (currentSampleRate);
                anySampleRateChanges = true;
            }
        }

        if (anySampleRateChanges && owner != nullptr)
            owner->audioDeviceListChanged();

        if (callback != nullptr)
            callback->audioDeviceAboutToStart (device);
    }

    void handleAudioDeviceStopped()                            { shutdown ({}); }
    void handleAudioDeviceError (const String& errorMessage)   { shutdown (errorMessage.isNotEmpty() ? errorMessage : String ("unknown")); }

    //==============================================================================
    struct DeviceWrapper  : private AudioIODeviceCallback
    {
        DeviceWrapper (AudioIODeviceCombiner& cd, CoreAudioIODevice* d, bool useIns, bool useOuts)
            : owner (cd), device (d),
              useInputs (useIns), useOutputs (useOuts)
        {
            d->setDeviceWrapperRestartCallback ([this] { owner.restartAsync(); });
        }

        ~DeviceWrapper() override
        {
            close();
        }

        String open (const BigInteger& inputChannels, const BigInteger& outputChannels,
                     double sampleRate, int bufferSize, int channelIndex, int fifoSize)
        {
            inputFifo.setTotalSize (fifoSize);
            outputFifo.setTotalSize (fifoSize);
            inputFifo.reset();
            outputFifo.reset();

            auto err = device->open (useInputs  ? inputChannels  : BigInteger(),
                                     useOutputs ? outputChannels : BigInteger(),
                                     sampleRate, bufferSize);

            numInputChans  = useInputs  ? device->getActiveInputChannels().countNumberOfSetBits()  : 0;
            numOutputChans = useOutputs ? device->getActiveOutputChannels().countNumberOfSetBits() : 0;

            inputIndex = channelIndex;
            outputIndex = channelIndex + numInputChans;

            return err;
        }

        void close()
        {
            device->close();
        }

        void start()
        {
            reset();
            device->start (this);
        }

        void reset()
        {
            inputFifo.reset();
            outputFifo.reset();
        }

        StringArray getOutputChannelNames() const  { return useOutputs ? device->getOutputChannelNames() : StringArray(); }
        StringArray getInputChannelNames()  const  { return useInputs  ? device->getInputChannelNames()  : StringArray(); }

        bool isInputReady (int numSamples) const noexcept
        {
            return numInputChans == 0 || inputFifo.getNumReady() >= numSamples;
        }

        void readInput (AudioBuffer<float>& destBuffer, int numSamples)
        {
            if (numInputChans == 0)
                return;

            int start1, size1, start2, size2;
            inputFifo.prepareToRead (numSamples, start1, size1, start2, size2);

            for (int i = 0; i < numInputChans; ++i)
            {
                auto index = inputIndex + i;
                auto dest = destBuffer.getWritePointer (index);
                auto src = owner.fifoReadPointers[index];

                if (size1 > 0)  FloatVectorOperations::copy (dest,         src + start1, size1);
                if (size2 > 0)  FloatVectorOperations::copy (dest + size1, src + start2, size2);
            }

            inputFifo.finishedRead (size1 + size2);
        }

        bool isOutputReady (int numSamples) const noexcept
        {
            return numOutputChans == 0 || outputFifo.getFreeSpace() >= numSamples;
        }

        void pushOutputData (AudioBuffer<float>& srcBuffer, int numSamples)
        {
            if (numOutputChans == 0)
                return;

            int start1, size1, start2, size2;
            outputFifo.prepareToWrite (numSamples, start1, size1, start2, size2);

            for (int i = 0; i < numOutputChans; ++i)
            {
                auto index = outputIndex + i;
                auto dest = owner.fifoWritePointers[index];
                auto src = srcBuffer.getReadPointer (index);

                if (size1 > 0)  FloatVectorOperations::copy (dest + start1, src,         size1);
                if (size2 > 0)  FloatVectorOperations::copy (dest + start2, src + size1, size2);
            }

            outputFifo.finishedWrite (size1 + size2);
        }

        void audioDeviceIOCallback (const float** inputChannelData, int numInputChannels,
                                    float** outputChannelData, int numOutputChannels,
                                    int numSamples) override
        {
            if (numInputChannels > 0)
            {
                int start1, size1, start2, size2;
                inputFifo.prepareToWrite (numSamples, start1, size1, start2, size2);

                if (size1 + size2 < numSamples)
                {
                    inputFifo.reset();
                    inputFifo.prepareToWrite (numSamples, start1, size1, start2, size2);
                }

                for (int i = 0; i < numInputChannels; ++i)
                {
                    auto dest = owner.fifoWritePointers[inputIndex + i];
                    auto src = inputChannelData[i];

                    if (size1 > 0)  FloatVectorOperations::copy (dest + start1, src,         size1);
                    if (size2 > 0)  FloatVectorOperations::copy (dest + start2, src + size1, size2);
                }

                auto totalSize = size1 + size2;
                inputFifo.finishedWrite (totalSize);

                if (numSamples > totalSize)
                {
                    auto samplesRemaining = numSamples - totalSize;

                    for (int i = 0; i < numInputChans; ++i)
                        FloatVectorOperations::clear (owner.fifoWritePointers[inputIndex + i] + totalSize, samplesRemaining);

                    owner.underrun();
                }
            }

            if (numOutputChannels > 0)
            {
                int start1, size1, start2, size2;
                outputFifo.prepareToRead (numSamples, start1, size1, start2, size2);

                if (size1 + size2 < numSamples)
                {
                    Thread::sleep (1);
                    outputFifo.prepareToRead (numSamples, start1, size1, start2, size2);
                }

                for (int i = 0; i < numOutputChannels; ++i)
                {
                    auto dest = outputChannelData[i];
                    auto src = owner.fifoReadPointers[outputIndex + i];

                    if (size1 > 0)  FloatVectorOperations::copy (dest,         src + start1, size1);
                    if (size2 > 0)  FloatVectorOperations::copy (dest + size1, src + start2, size2);
                }

                auto totalSize = size1 + size2;
                outputFifo.finishedRead (totalSize);

                if (numSamples > totalSize)
                {
                    auto samplesRemaining = numSamples - totalSize;

                    for (int i = 0; i < numOutputChannels; ++i)
                        FloatVectorOperations::clear (outputChannelData[i] + totalSize, samplesRemaining);

                    owner.underrun();
                }
            }

            owner.notify();
        }

        double getCurrentSampleRate()                        { return device->getCurrentSampleRate(); }
        bool   setCurrentSampleRate (double newSampleRate)   { return device->setCurrentSampleRate (newSampleRate); }
        int  getCurrentBufferSizeSamples()                   { return device->getCurrentBufferSizeSamples(); }

        void audioDeviceAboutToStart (AudioIODevice* d) override      { owner.handleAudioDeviceAboutToStart (d); }
        void audioDeviceStopped() override                            { owner.handleAudioDeviceStopped(); }
        void audioDeviceError (const String& errorMessage) override   { owner.handleAudioDeviceError (errorMessage); }

        AudioIODeviceCombiner& owner;
        std::unique_ptr<CoreAudioIODevice> device;
        int inputIndex = 0, numInputChans = 0, outputIndex = 0, numOutputChans = 0;
        bool useInputs = false, useOutputs = false;
        AbstractFifo inputFifo { 32 }, outputFifo { 32 };
        bool done = false;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (DeviceWrapper)
    };

    OwnedArray<DeviceWrapper> devices;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (AudioIODeviceCombiner)
};


//==============================================================================
class CoreAudioIODeviceType  : public AudioIODeviceType,
                               private AsyncUpdater
{
public:
    CoreAudioIODeviceType()  : AudioIODeviceType ("CoreAudio")
    {
        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioHardwarePropertyDevices;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectAddPropertyListener (kAudioObjectSystemObject, &pa, hardwareListenerProc, this);
    }

    ~CoreAudioIODeviceType() override
    {
        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioHardwarePropertyDevices;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectRemovePropertyListener (kAudioObjectSystemObject, &pa, hardwareListenerProc, this);
    }

    //==============================================================================
    void scanForDevices() override
    {
        hasScanned = true;

        inputDeviceNames.clear();
        outputDeviceNames.clear();
        inputIds.clear();
        outputIds.clear();

        UInt32 size;

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioHardwarePropertyDevices;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementMaster;

        if (AudioObjectGetPropertyDataSize (kAudioObjectSystemObject, &pa, 0, nullptr, &size) == noErr)
        {
            HeapBlock<AudioDeviceID> devs;
            devs.calloc (size, 1);

            if (AudioObjectGetPropertyData (kAudioObjectSystemObject, &pa, 0, nullptr, &size, devs) == noErr)
            {
                auto num = (int) size / (int) sizeof (AudioDeviceID);

                for (int i = 0; i < num; ++i)
                {
                    char name[1024];
                    size = sizeof (name);
                    pa.mSelector = kAudioDevicePropertyDeviceName;

                    if (AudioObjectGetPropertyData (devs[i], &pa, 0, nullptr, &size, name) == noErr)
                    {
                        auto nameString = String::fromUTF8 (name, (int) strlen (name));
                        auto numIns  = getNumChannels (devs[i], true);
                        auto numOuts = getNumChannels (devs[i], false);

                        if (numIns > 0)
                        {
                            inputDeviceNames.add (nameString);
                            inputIds.add (devs[i]);
                        }

                        if (numOuts > 0)
                        {
                            outputDeviceNames.add (nameString);
                            outputIds.add (devs[i]);
                        }
                    }
                }
            }
        }

        inputDeviceNames.appendNumbersToDuplicates (false, true);
        outputDeviceNames.appendNumbersToDuplicates (false, true);
    }

    StringArray getDeviceNames (bool wantInputNames) const override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        return wantInputNames ? inputDeviceNames
                              : outputDeviceNames;
    }

    int getDefaultDeviceIndex (bool forInput) const override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        AudioDeviceID deviceID;
        UInt32 size = sizeof (deviceID);

        // if they're asking for any input channels at all, use the default input, so we
        // get the built-in mic rather than the built-in output with no inputs..

        AudioObjectPropertyAddress pa;
        pa.mSelector = forInput ? kAudioHardwarePropertyDefaultInputDevice
                                : kAudioHardwarePropertyDefaultOutputDevice;
        pa.mScope    = kAudioObjectPropertyScopeWildcard;
        pa.mElement  = kAudioObjectPropertyElementMaster;

        if (AudioObjectGetPropertyData (kAudioObjectSystemObject, &pa, 0, nullptr, &size, &deviceID) == noErr)
        {
            if (forInput)
            {
                for (int i = inputIds.size(); --i >= 0;)
                    if (inputIds[i] == deviceID)
                        return i;
            }
            else
            {
                for (int i = outputIds.size(); --i >= 0;)
                    if (outputIds[i] == deviceID)
                        return i;
            }
        }

        return 0;
    }

    int getIndexOfDevice (AudioIODevice* device, bool asInput) const override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        if (auto* d = dynamic_cast<CoreAudioIODevice*> (device))
            return asInput ? d->inputIndex
                           : d->outputIndex;

        if (auto* d = dynamic_cast<AudioIODeviceCombiner*> (device))
        {
            for (auto* dev : d->getDevices())
            {
                auto index = getIndexOfDevice (dev, asInput);

                if (index >= 0)
                    return index;
            }
        }

        return -1;
    }

    bool hasSeparateInputsAndOutputs() const override    { return true; }

    AudioIODevice* createDevice (const String& outputDeviceName,
                                 const String& inputDeviceName) override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        auto inputIndex  = inputDeviceNames.indexOf (inputDeviceName);
        auto outputIndex = outputDeviceNames.indexOf (outputDeviceName);

        auto inputDeviceID  = inputIds[inputIndex];
        auto outputDeviceID = outputIds[outputIndex];

        if (inputDeviceID == 0 && outputDeviceID == 0)
            return nullptr;

        auto combinedName = outputDeviceName.isEmpty() ? inputDeviceName
                                                       : outputDeviceName;

        if (inputDeviceID == outputDeviceID)
            return new CoreAudioIODevice (this, combinedName, inputDeviceID, inputIndex, outputDeviceID, outputIndex);

        std::unique_ptr<CoreAudioIODevice> in, out;

        if (inputDeviceID != 0)
            in.reset (new CoreAudioIODevice (this, inputDeviceName, inputDeviceID, inputIndex, 0, -1));

        if (outputDeviceID != 0)
            out.reset (new CoreAudioIODevice (this, outputDeviceName, 0, -1, outputDeviceID, outputIndex));

        if (in == nullptr)   return out.release();
        if (out == nullptr)  return in.release();

        std::unique_ptr<AudioIODeviceCombiner> combo (new AudioIODeviceCombiner (combinedName, this));
        combo->addDevice (in.release(),  true, false);
        combo->addDevice (out.release(), false, true);
        return combo.release();
    }

    void audioDeviceListChanged()
    {
        scanForDevices();
        callDeviceChangeListeners();
    }

    void triggerAsyncAudioDeviceListChange()
    {
        triggerAsyncUpdate();
    }

    //==============================================================================
private:
    StringArray inputDeviceNames, outputDeviceNames;
    Array<AudioDeviceID> inputIds, outputIds;

    bool hasScanned = false;

    static int getNumChannels (AudioDeviceID deviceID, bool input)
    {
        int total = 0;
        UInt32 size;

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioDevicePropertyStreamConfiguration;
        pa.mScope = input ? kAudioDevicePropertyScopeInput : kAudioDevicePropertyScopeOutput;
        pa.mElement = kAudioObjectPropertyElementMaster;

        if (AudioObjectGetPropertyDataSize (deviceID, &pa, 0, nullptr, &size) == noErr)
        {
            HeapBlock<AudioBufferList> bufList;
            bufList.calloc (size, 1);

            if (AudioObjectGetPropertyData (deviceID, &pa, 0, nullptr, &size, bufList) == noErr)
            {
                auto numStreams = (int) bufList->mNumberBuffers;

                for (int i = 0; i < numStreams; ++i)
                    total += bufList->mBuffers[i].mNumberChannels;
            }
        }

        return total;
    }

    static OSStatus hardwareListenerProc (AudioDeviceID, UInt32, const AudioObjectPropertyAddress*, void* clientData)
    {
        static_cast<CoreAudioIODeviceType*> (clientData)->triggerAsyncAudioDeviceListChange();
        return noErr;
    }

    void handleAsyncUpdate() override
    {
        audioDeviceListChanged();
    }

    JUCE_DECLARE_WEAK_REFERENCEABLE (CoreAudioIODeviceType)
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CoreAudioIODeviceType)
};

};

//==============================================================================
AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_CoreAudio()
{
    return new CoreAudioClasses::CoreAudioIODeviceType();
}

#undef JUCE_COREAUDIOLOG

} // namespace juce
