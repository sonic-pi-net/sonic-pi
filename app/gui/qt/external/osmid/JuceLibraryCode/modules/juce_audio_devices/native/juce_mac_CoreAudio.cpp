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

#if JUCE_COREAUDIO_LOGGING_ENABLED
 #define JUCE_COREAUDIOLOG(a) { String camsg ("CoreAudio: "); camsg << a; Logger::writeToLog (camsg); }
#else
 #define JUCE_COREAUDIOLOG(a)
#endif

#ifdef __clang__
 #pragma clang diagnostic push
 #pragma clang diagnostic ignored "-Wnonnull" // aovid some spurious 10.11 SDK warnings

 // The AudioHardwareService stuff was deprecated in 10.11 but there's no replacement yet,
 // so we'll have to silence the warnings here and revisit it in a future OS version..
 #if ((defined (MAC_OS_X_VERSION_10_12) && MAC_OS_X_VERSION_MIN_REQUIRED <= MAC_OS_X_VERSION_10_12) \
   || (defined (MAC_OS_X_VERSION_10_11) && MAC_OS_X_VERSION_MIN_REQUIRED <= MAC_OS_X_VERSION_10_11))
  #pragma clang diagnostic ignored "-Wdeprecated-declarations"
 #endif
#endif

//==============================================================================
struct SystemVol
{
    SystemVol (AudioObjectPropertySelector selector) noexcept
        : outputDeviceID (kAudioObjectUnknown)
    {
        addr.mScope    = kAudioObjectPropertyScopeGlobal;
        addr.mElement  = kAudioObjectPropertyElementMaster;
        addr.mSelector = kAudioHardwarePropertyDefaultOutputDevice;

        if (AudioHardwareServiceHasProperty (kAudioObjectSystemObject, &addr))
        {
            UInt32 deviceIDSize = sizeof (outputDeviceID);
            OSStatus status = AudioHardwareServiceGetPropertyData (kAudioObjectSystemObject, &addr, 0,
                                                                   nullptr, &deviceIDSize, &outputDeviceID);

            if (status == noErr)
            {
                addr.mElement  = kAudioObjectPropertyElementMaster;
                addr.mSelector = selector;
                addr.mScope    = kAudioDevicePropertyScopeOutput;

                if (! AudioHardwareServiceHasProperty (outputDeviceID, &addr))
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
            AudioHardwareServiceGetPropertyData (outputDeviceID, &addr,
                                                 0, nullptr, &size, &gain);
        }

        return (float) gain;
    }

    bool setGain (float gain) const noexcept
    {
        if (outputDeviceID != kAudioObjectUnknown && canSetVolume())
        {
            Float32 newVolume = gain;
            UInt32 size = sizeof (newVolume);

            return AudioHardwareServiceSetPropertyData (outputDeviceID, &addr, 0, nullptr,
                                                        size, &newVolume) == noErr;
        }

        return false;
    }

    bool isMuted() const noexcept
    {
        UInt32 muted = 0;

        if (outputDeviceID != kAudioObjectUnknown)
        {
            UInt32 size = sizeof (muted);
            AudioHardwareServiceGetPropertyData (outputDeviceID, &addr,
                                                 0, nullptr, &size, &muted);
        }

        return muted != 0;
    }

    bool setMuted (bool mute) const noexcept
    {
        if (outputDeviceID != kAudioObjectUnknown && canSetVolume())
        {
            UInt32 newMute = mute ? 1 : 0;
            UInt32 size = sizeof (newMute);

            return AudioHardwareServiceSetPropertyData (outputDeviceID, &addr, 0, nullptr,
                                                        size, &newMute) == noErr;
        }

        return false;
    }

private:
    AudioDeviceID outputDeviceID;
    AudioObjectPropertyAddress addr;

    bool canSetVolume() const noexcept
    {
        Boolean isSettable = NO;
        return AudioHardwareServiceIsPropertySettable (outputDeviceID, &addr, &isSettable) == noErr
                 && isSettable;
    }
};

#ifdef __clang__
 #pragma clang diagnostic pop
#endif

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

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioObjectPropertySelectorWildcard;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectAddPropertyListener (deviceID, &pa, deviceListenerProc, this);
    }

    ~CoreAudioInternal()
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
    StringArray getChannelInfo (const bool input, Array<CallbackDetailsForChannel>& newChannelInfo) const
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
                static const double possibleRates[] = { 44100.0, 48000.0, 88200.0, 96000.0, 176400.0, 192000.0, 384000.0 };

                for (int i = 0; i < numElementsInArray (possibleRates); ++i)
                {
                    for (int j = size / (int) sizeof (AudioValueRange); --j >= 0;)
                    {
                        if (possibleRates[i] >= ranges[j].mMinimum - 2 && possibleRates[i] <= ranges[j].mMaximum + 2)
                        {
                            newSampleRates.add (possibleRates[i]);
                            break;
                        }
                    }
                }
            }
        }

        if (newSampleRates.size() == 0 && sampleRate > 0)
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

        if (newBufferSizes.size() == 0 && bufferSize > 0)
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

        const double newSampleRate = getNominalSampleRate();
        const int newBufferSize = getFrameSizeFromDevice();

        Array<int>    newBufferSizes = getBufferSizesFromDevice();
        Array<double> newSampleRates = getSampleRatesFromDevice();

        const int newInputLatency  = getLatencyFromDevice (kAudioDevicePropertyScopeInput);
        const int newOutputLatency = getLatencyFromDevice (kAudioDevicePropertyScopeOutput);

        Array<CallbackDetailsForChannel> newInChans, newOutChans;
        auto newInNames  = isInputDevice  ? getChannelInfo (true,  newInChans)  : StringArray();
        auto newOutNames = isOutputDevice ? getChannelInfo (false, newOutChans) : StringArray();

        const int newBitDepth = jmax (getBitDepthFromDevice (kAudioDevicePropertyScopeInput),
                                      getBitDepthFromDevice (kAudioDevicePropertyScopeOutput));

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

            allocateTempBuffers();
        }

        return true;
    }

    //==============================================================================
    StringArray getSources (bool input)
    {
        StringArray s;
        HeapBlock<OSType> types;
        const int num = getAllDataSourcesForDevice (deviceID, types);

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
                const int num = getAllDataSourcesForDevice (deviceID, types);

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
            const int num = getAllDataSourcesForDevice (deviceID, types);

            if (isPositiveAndBelow (index, num))
            {
                AudioObjectPropertyAddress pa;
                pa.mSelector = kAudioDevicePropertyDataSource;
                pa.mScope = input ? kAudioDevicePropertyScopeInput : kAudioDevicePropertyScopeOutput;
                pa.mElement = kAudioObjectPropertyElementMaster;

                OSType typeId = types[index];

                OK (AudioObjectSetPropertyData (deviceID, &pa, 0, 0, sizeof (typeId), &typeId));
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
        return OK (AudioObjectSetPropertyData (deviceID, &pa, 0, 0, sizeof (sr), &sr));
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

            if (! OK (AudioObjectSetPropertyData (deviceID, &pa, 0, 0, sizeof (framesPerBuf), &framesPerBuf)))
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
                        audioProcID = 0;
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

        if (started
             && (deviceID != 0)
             && ! leaveInterruptRunning)
        {
            OK (AudioDeviceStop (deviceID, audioIOProc));
            OK (AudioDeviceDestroyIOProcID (deviceID, audioProcID));
            audioProcID = 0;

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
                const CallbackDetailsForChannel& info = inputChannelInfo.getReference(i);
                float* dest = tempInputBuffers [i];
                const float* src = ((const float*) inInputData->mBuffers[info.streamNum].mData)
                                    + info.dataOffsetSamples;
                const int stride = info.dataStrideSamples;

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
                const CallbackDetailsForChannel& info = outputChannelInfo.getReference(i);
                const float* src = tempOutputBuffers [i];
                float* dest = ((float*) outOutputData->mBuffers[info.streamNum].mData)
                                + info.dataOffsetSamples;
                const int stride = info.dataStrideSamples;

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
        if (callbacksAllowed)
            startTimer (100);
    }

    void timerCallback() override
    {
        JUCE_COREAUDIOLOG ("Device changed");

        stopTimer();
        const double oldSampleRate = sampleRate;
        const int oldBufferSize = bufferSize;

        if (! updateDetailsFromDevice())
            owner.stop();
        else if (oldBufferSize != bufferSize || oldSampleRate != sampleRate)
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
    AudioDeviceIOProcID audioProcID = 0;

private:
    CriticalSection callbackLock;
    AudioDeviceID deviceID;
    bool started = false;
    double sampleRate = 0;
    int bufferSize = 512;
    HeapBlock<float> audioBuffer;
    int numInputChans  = 0;
    int numOutputChans = 0;
    bool callbacksAllowed = true;
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

    static OSStatus deviceListenerProc (AudioDeviceID /*inDevice*/, UInt32 /*inLine*/, const AudioObjectPropertyAddress* pa, void* inClientData)
    {
        CoreAudioInternal* const intern = static_cast<CoreAudioInternal*> (inClientData);

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

            case kAudioObjectPropertyOwnedObjects:
                intern->stop (false);
                intern->owner.deviceType.triggerAsyncAudioDeviceListChange();
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
class CoreAudioIODevice   : public AudioIODevice
{
public:
    CoreAudioIODevice (CoreAudioIODeviceType& dt,
                       const String& deviceName,
                       AudioDeviceID inputDeviceId, const int inputIndex_,
                       AudioDeviceID outputDeviceId, const int outputIndex_)
        : AudioIODevice (deviceName, "CoreAudio"),
          deviceType (dt),
          inputIndex (inputIndex_),
          outputIndex (outputIndex_),
          isOpen_ (false),
          isStarted (false)
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

        internal = device;

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioObjectPropertySelectorWildcard;
        pa.mScope    = kAudioObjectPropertyScopeWildcard;
        pa.mElement  = kAudioObjectPropertyElementWildcard;

        AudioObjectAddPropertyListener (kAudioObjectSystemObject, &pa, hardwareListenerProc, internal);
    }

    ~CoreAudioIODevice()
    {
        close();

        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioObjectPropertySelectorWildcard;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectRemovePropertyListener (kAudioObjectSystemObject, &pa, hardwareListenerProc, internal);
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
        if (bufferSizeSamples <= 0)
            bufferSizeSamples = getDefaultBufferSize();

        lastError = internal->reopen (inputChannels, outputChannels, sampleRate, bufferSizeSamples);

        JUCE_COREAUDIOLOG ("Opened: " << getName());
        JUCE_COREAUDIOLOG ("Latencies: " << getInputLatencyInSamples() << ' ' << getOutputLatencyInSamples());

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
                internal->setCallback (callback);
        }
    }

    void stop() override
    {
        if (isStarted)
        {
            AudioIODeviceCallback* const lastCallback = internal->callback;

            isStarted = false;
            internal->stop (true);

            if (lastCallback != nullptr)
                lastCallback->audioDeviceStopped();
        }
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
        deviceType.audioDeviceListChanged();
    }

    void restart()
    {
        JUCE_COREAUDIOLOG ("Restarting");
        AudioIODeviceCallback* oldCallback = internal->callback;
        stop();
        start (oldCallback);
    }

    bool setCurrentSampleRate (double newSampleRate)
    {
        return internal->setNominalSampleRate (newSampleRate);
    }

    CoreAudioIODeviceType& deviceType;
    int inputIndex, outputIndex;

private:
    ScopedPointer<CoreAudioInternal> internal;
    bool isOpen_, isStarted;
    String lastError;

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
                                 private Thread
{
public:
    AudioIODeviceCombiner (const String& deviceName, CoreAudioIODeviceType& deviceType)
        : AudioIODevice (deviceName, "CoreAudio"),
          Thread (deviceName),
          owner (deviceType)
    {
    }

    ~AudioIODeviceCombiner()
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

        for (int i = 0; i < devices.size(); ++i)
            devs.add (devices.getUnchecked(i)->device);

        return devs;
    }

    StringArray getOutputChannelNames() override
    {
        StringArray names;

        for (int i = 0; i < devices.size(); ++i)
            names.addArray (devices.getUnchecked(i)->getOutputChannelNames());

        names.appendNumbersToDuplicates (false, true);
        return names;
    }

    StringArray getInputChannelNames() override
    {
        StringArray names;

        for (int i = 0; i < devices.size(); ++i)
            names.addArray (devices.getUnchecked(i)->getInputChannelNames());

        names.appendNumbersToDuplicates (false, true);
        return names;
    }

    Array<double> getAvailableSampleRates() override
    {
        Array<double> commonRates;

        for (int i = 0; i < devices.size(); ++i)
        {
            Array<double> rates (devices.getUnchecked(i)->device->getAvailableSampleRates());

            if (i == 0)
                commonRates = rates;
            else
                commonRates.removeValuesNotIn (rates);
        }

        return commonRates;
    }

    Array<int> getAvailableBufferSizes() override
    {
        Array<int> commonSizes;

        for (int i = 0; i < devices.size(); ++i)
        {
            Array<int> sizes (devices.getUnchecked(i)->device->getAvailableBufferSizes());

            if (i == 0)
                commonSizes = sizes;
            else
                commonSizes.removeValuesNotIn (sizes);
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

        for (int i = 0; i < devices.size(); ++i)
            depth = jmin (depth, devices.getUnchecked(i)->device->getCurrentBitDepth());

        return depth;
    }

    int getDefaultBufferSize() override
    {
        int size = 0;

        for (int i = 0; i < devices.size(); ++i)
            size = jmax (size, devices.getUnchecked(i)->device->getDefaultBufferSize());

        return size;
    }

    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double sampleRate, int bufferSize) override
    {
        close();
        active = true;

        if (bufferSize <= 0)
            bufferSize = getDefaultBufferSize();

        if (sampleRate <= 0)
        {
            Array<double> rates (getAvailableSampleRates());

            for (int i = 0; i < rates.size() && sampleRate < 44100.0; ++i)
                sampleRate = rates.getUnchecked(i);
        }

        currentSampleRate = sampleRate;
        currentBufferSize = bufferSize;

        const int fifoSize = bufferSize * 3 + 1;
        int totalInputChanIndex = 0, totalOutputChanIndex = 0;
        int chanIndex = 0;

        for (int i = 0; i < devices.size(); ++i)
        {
            DeviceWrapper& d = *devices.getUnchecked(i);

            BigInteger ins (inputChannels >> totalInputChanIndex);
            BigInteger outs (outputChannels >> totalOutputChanIndex);

            int numIns  = d.getInputChannelNames().size();
            int numOuts = d.getOutputChannelNames().size();

            totalInputChanIndex += numIns;
            totalOutputChanIndex += numOuts;

            String err = d.open (ins, outs, sampleRate, bufferSize,
                                 chanIndex, fifoSize);

            if (err.isNotEmpty())
            {
                close();
                lastError = err;
                return err;
            }

            chanIndex += d.numInputChans + d.numOutputChans;
        }

        fifos.setSize (chanIndex, fifoSize);
        fifos.clear();
        startThread (9);

        return {};
    }

    void close() override
    {
        stop();
        stopThread (10000);
        fifos.clear();
        active = false;

        for (int i = 0; i < devices.size(); ++i)
            devices.getUnchecked(i)->close();
    }

    BigInteger getActiveOutputChannels() const override
    {
        BigInteger chans;
        int start = 0;

        for (int i = 0; i < devices.size(); ++i)
        {
            const int numChans = devices.getUnchecked(i)->getOutputChannelNames().size();

            if (numChans > 0)
            {
                chans |= (devices.getUnchecked(i)->device->getActiveOutputChannels() << start);
                start += numChans;
            }
        }

        return chans;
    }

    BigInteger getActiveInputChannels() const override
    {
        BigInteger chans;
        int start = 0;

        for (int i = 0; i < devices.size(); ++i)
        {
            const int numChans = devices.getUnchecked(i)->getInputChannelNames().size();

            if (numChans > 0)
            {
                chans |= (devices.getUnchecked(i)->device->getActiveInputChannels() << start);
                start += numChans;
            }
        }

        return chans;
    }

    int getOutputLatencyInSamples() override
    {
        int lat = 0;

        for (int i = 0; i < devices.size(); ++i)
            lat = jmax (lat, devices.getUnchecked(i)->device->getOutputLatencyInSamples());

        return lat + currentBufferSize * 2;
    }

    int getInputLatencyInSamples() override
    {
        int lat = 0;

        for (int i = 0; i < devices.size(); ++i)
            lat = jmax (lat, devices.getUnchecked(i)->device->getInputLatencyInSamples());

        return lat + currentBufferSize * 2;
    }

    void start (AudioIODeviceCallback* newCallback) override
    {
        if (callback != newCallback)
        {
            stop();
            fifos.clear();

            for (int i = 0; i < devices.size(); ++i)
                devices.getUnchecked(i)->start();

            if (newCallback != nullptr)
                newCallback->audioDeviceAboutToStart (this);

            const ScopedLock sl (callbackLock);
            callback = newCallback;
        }
    }

    void stop() override    { shutdown ({}); }

    String getLastError() override
    {
        return lastError;
    }

private:
    CoreAudioIODeviceType& owner;
    CriticalSection callbackLock;
    AudioIODeviceCallback* callback = nullptr;
    double currentSampleRate = 0;
    int currentBufferSize = 0;
    bool active = false;
    String lastError;

    AudioSampleBuffer fifos;

    void run() override
    {
        const int numSamples = currentBufferSize;

        AudioSampleBuffer buffer (fifos.getNumChannels(), numSamples);
        buffer.clear();

        Array<const float*> inputChans;
        Array<float*> outputChans;

        for (int i = 0; i < devices.size(); ++i)
        {
            DeviceWrapper& d = *devices.getUnchecked(i);

            for (int j = 0; j < d.numInputChans; ++j)   inputChans.add  (buffer.getReadPointer  (d.inputIndex  + j));
            for (int j = 0; j < d.numOutputChans; ++j)  outputChans.add (buffer.getWritePointer (d.outputIndex + j));
        }

        const int numInputChans  = inputChans.size();
        const int numOutputChans = outputChans.size();

        inputChans.add (nullptr);
        outputChans.add (nullptr);

        const int blockSizeMs = jmax (1, (int) (1000 * numSamples / currentSampleRate));

        jassert (numInputChans + numOutputChans == buffer.getNumChannels());

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

    void shutdown (const String& error)
    {
        AudioIODeviceCallback* lastCallback = nullptr;

        {
            const ScopedLock sl (callbackLock);
            std::swap (callback, lastCallback);
        }

        for (int i = 0; i < devices.size(); ++i)
            devices.getUnchecked(i)->device->stop();

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
        for (int i = 0; i < devices.size(); ++i)
            devices.getUnchecked(i)->reset();
    }

    void underrun()
    {
    }

    void readInput (AudioSampleBuffer& buffer, const int numSamples, const int blockSizeMs)
    {
        for (int i = 0; i < devices.size(); ++i)
        {
            DeviceWrapper& d = *devices.getUnchecked(i);
            d.done = (d.numInputChans == 0);
        }

        for (int tries = 5;;)
        {
            bool anyRemaining = false;

            for (int i = 0; i < devices.size(); ++i)
            {
                DeviceWrapper& d = *devices.getUnchecked(i);

                if (! d.done)
                {
                    if (d.isInputReady (numSamples))
                    {
                        d.readInput (buffer, numSamples);
                        d.done = true;
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

        for (int j = 0; j < devices.size(); ++j)
        {
            DeviceWrapper& d = *devices.getUnchecked(j);

            if (! d.done)
                for (int i = 0; i < d.numInputChans; ++i)
                    buffer.clear (d.inputIndex + i, 0, numSamples);
        }
    }

    void pushOutputData (AudioSampleBuffer& buffer, const int numSamples, const int blockSizeMs)
    {
        for (int i = 0; i < devices.size(); ++i)
        {
            DeviceWrapper& d = *devices.getUnchecked(i);
            d.done = (d.numOutputChans == 0);
        }

        for (int tries = 5;;)
        {
            bool anyRemaining = false;

            for (int i = 0; i < devices.size(); ++i)
            {
                DeviceWrapper& d = *devices.getUnchecked(i);

                if (! d.done)
                {
                    if (d.isOutputReady (numSamples))
                    {
                        d.pushOutputData (buffer, numSamples);
                        d.done = true;
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
                newSampleRate = jlimit (commonRates.getFirst(), commonRates.getLast(), newSampleRate);
            else
                for (auto it = commonRates.begin(); it < commonRates.end() - 1; ++it)
                    if (it[0] < newSampleRate && it[1] > newSampleRate)
                    {
                        newSampleRate = newSampleRate - it[0] < it[1] - newSampleRate ? it[0] : it[1];
                        break;
                    }
        }
        currentSampleRate = newSampleRate;

        bool anySampleRateChanges = false;
        for (int i = 0; i < devices.size(); ++i)
            if (devices.getUnchecked(i)->getCurrentSampleRate() != currentSampleRate)
            {
                devices.getUnchecked(i)->setCurrentSampleRate (currentSampleRate);
                anySampleRateChanges = true;
            }

        if (anySampleRateChanges)
            owner.audioDeviceListChanged();

        if (callback != nullptr)
            callback->audioDeviceAboutToStart (device);
    }

    void handleAudioDeviceStopped()                            { shutdown ({}); }
    void handleAudioDeviceError (const String& errorMessage)   { shutdown (errorMessage.isNotEmpty() ? errorMessage : String ("unknown")); }

    //==============================================================================
    struct DeviceWrapper  : private AudioIODeviceCallback
    {
        DeviceWrapper (AudioIODeviceCombiner& cd, CoreAudioIODevice* d, bool useIns, bool useOuts)
            : owner (cd), device (d), inputIndex (0), outputIndex (0),
              useInputs (useIns), useOutputs (useOuts),
              inputFifo (32), outputFifo (32), done (false)
        {
        }

        ~DeviceWrapper()
        {
            close();
        }

        String open (const BigInteger& inputChannels, const BigInteger& outputChannels,
                     double sampleRate, int bufferSize,
                     int channelIndex,
                     int fifoSize)
        {
            inputFifo.setTotalSize (fifoSize);
            outputFifo.setTotalSize (fifoSize);
            inputFifo.reset();
            outputFifo.reset();

            String err (device->open (useInputs  ? inputChannels  : BigInteger(),
                                      useOutputs ? outputChannels : BigInteger(),
                                      sampleRate, bufferSize));

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

        void readInput (AudioSampleBuffer& destBuffer, int numSamples)
        {
            if (numInputChans == 0)
                return;

            int start1, size1, start2, size2;
            inputFifo.prepareToRead (numSamples, start1, size1, start2, size2);

            for (int i = 0; i < numInputChans; ++i)
            {
                const int index = inputIndex + i;
                float* const dest = destBuffer.getWritePointer (index);
                const float* const src = owner.fifos.getReadPointer (index);

                if (size1 > 0)  FloatVectorOperations::copy (dest,         src + start1, size1);
                if (size2 > 0)  FloatVectorOperations::copy (dest + size1, src + start2, size2);
            }

            inputFifo.finishedRead (size1 + size2);
        }

        bool isOutputReady (int numSamples) const noexcept
        {
            return numOutputChans == 0 || outputFifo.getFreeSpace() >= numSamples;
        }

        void pushOutputData (AudioSampleBuffer& srcBuffer, int numSamples)
        {
            if (numOutputChans == 0)
                return;

            int start1, size1, start2, size2;
            outputFifo.prepareToWrite (numSamples, start1, size1, start2, size2);

            for (int i = 0; i < numOutputChans; ++i)
            {
                const int index = outputIndex + i;
                float* const dest = owner.fifos.getWritePointer (index);
                const float* const src = srcBuffer.getReadPointer (index);

                if (size1 > 0)  FloatVectorOperations::copy (dest + start1, src,         size1);
                if (size2 > 0)  FloatVectorOperations::copy (dest + start2, src + size1, size2);
            }

            outputFifo.finishedWrite (size1 + size2);
        }

        void audioDeviceIOCallback (const float** inputChannelData, int numInputChannels,
                                    float** outputChannelData, int numOutputChannels,
                                    int numSamples) override
        {
            AudioSampleBuffer& buf = owner.fifos;

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
                    float* const dest = buf.getWritePointer (inputIndex + i);
                    const float* const src = inputChannelData[i];

                    if (size1 > 0)  FloatVectorOperations::copy (dest + start1, src,         size1);
                    if (size2 > 0)  FloatVectorOperations::copy (dest + start2, src + size1, size2);
                }

                inputFifo.finishedWrite (size1 + size2);

                if (numSamples > size1 + size2)
                {
                    for (int i = 0; i < numInputChans; ++i)
                        buf.clear (inputIndex + i, size1 + size2, numSamples - (size1 + size2));

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
                    float* const dest = outputChannelData[i];
                    const float* const src = buf.getReadPointer (outputIndex + i);

                    if (size1 > 0)  FloatVectorOperations::copy (dest,         src + start1, size1);
                    if (size2 > 0)  FloatVectorOperations::copy (dest + size1, src + start2, size2);
                }

                outputFifo.finishedRead (size1 + size2);

                if (numSamples > size1 + size2)
                {
                    for (int i = 0; i < numOutputChannels; ++i)
                        FloatVectorOperations::clear (outputChannelData[i] + (size1 + size2), numSamples - (size1 + size2));

                    owner.underrun();
                }
            }

            owner.notify();
        }

        double getCurrentSampleRate()                        { return device->getCurrentSampleRate(); }
        bool   setCurrentSampleRate (double newSampleRate)   { return device->setCurrentSampleRate (newSampleRate); }

        void audioDeviceAboutToStart (AudioIODevice* d) override      { owner.handleAudioDeviceAboutToStart (d); }
        void audioDeviceStopped() override                            { owner.handleAudioDeviceStopped(); }
        void audioDeviceError (const String& errorMessage) override   { owner.handleAudioDeviceError (errorMessage); }

        AudioIODeviceCombiner& owner;
        ScopedPointer<CoreAudioIODevice> device;
        int inputIndex, numInputChans, outputIndex, numOutputChans;
        bool useInputs, useOutputs;
        AbstractFifo inputFifo, outputFifo;
        bool done;

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
    CoreAudioIODeviceType()
        : AudioIODeviceType ("CoreAudio"),
          hasScanned (false)
    {
        AudioObjectPropertyAddress pa;
        pa.mSelector = kAudioHardwarePropertyDevices;
        pa.mScope = kAudioObjectPropertyScopeWildcard;
        pa.mElement = kAudioObjectPropertyElementWildcard;

        AudioObjectAddPropertyListener (kAudioObjectSystemObject, &pa, hardwareListenerProc, this);
    }

    ~CoreAudioIODeviceType()
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
                const int num = size / (int) sizeof (AudioDeviceID);
                for (int i = 0; i < num; ++i)
                {
                    char name [1024];
                    size = sizeof (name);
                    pa.mSelector = kAudioDevicePropertyDeviceName;

                    if (AudioObjectGetPropertyData (devs[i], &pa, 0, nullptr, &size, name) == noErr)
                    {
                        const String nameString (String::fromUTF8 (name, (int) strlen (name)));
                        const int numIns = getNumChannels (devs[i], true);
                        const int numOuts = getNumChannels (devs[i], false);

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

        if (CoreAudioIODevice* const d = dynamic_cast<CoreAudioIODevice*> (device))
            return asInput ? d->inputIndex
                           : d->outputIndex;

        if (AudioIODeviceCombiner* const d = dynamic_cast<AudioIODeviceCombiner*> (device))
        {
            const Array<AudioIODevice*> devs (d->getDevices());

            for (int i = 0; i < devs.size(); ++i)
            {
                const int index = getIndexOfDevice (devs.getUnchecked(i), asInput);

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

        const int inputIndex  = inputDeviceNames.indexOf (inputDeviceName);
        const int outputIndex = outputDeviceNames.indexOf (outputDeviceName);

        AudioDeviceID inputDeviceID  = inputIds [inputIndex];
        AudioDeviceID outputDeviceID = outputIds [outputIndex];

        if (inputDeviceID == 0 && outputDeviceID == 0)
            return nullptr;

        String combinedName (outputDeviceName.isEmpty() ? inputDeviceName : outputDeviceName);

        if (inputDeviceID == outputDeviceID)
            return new CoreAudioIODevice (*this, combinedName, inputDeviceID, inputIndex, outputDeviceID, outputIndex);

        ScopedPointer<CoreAudioIODevice> in, out;

        if (inputDeviceID != 0)
            in = new CoreAudioIODevice (*this, inputDeviceName, inputDeviceID, inputIndex, 0, -1);

        if (outputDeviceID != 0)
            out = new CoreAudioIODevice (*this, outputDeviceName, 0, -1, outputDeviceID, outputIndex);

        if (in == nullptr)   return out.release();
        if (out == nullptr)  return in.release();

        ScopedPointer<AudioIODeviceCombiner> combo (new AudioIODeviceCombiner (combinedName, *this));
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

    bool hasScanned;

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
                const int numStreams = (int) bufList->mNumberBuffers;

                for (int i = 0; i < numStreams; ++i)
                {
                    const ::AudioBuffer& b = bufList->mBuffers[i];
                    total += b.mNumberChannels;
                }
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
