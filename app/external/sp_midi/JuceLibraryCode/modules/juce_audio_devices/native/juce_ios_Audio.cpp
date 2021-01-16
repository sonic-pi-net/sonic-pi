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

class iOSAudioIODevice;

static const char* const iOSAudioDeviceName = "iOS Audio";

//==============================================================================
struct AudioSessionHolder
{
    AudioSessionHolder();
    ~AudioSessionHolder();

    void handleStatusChange (bool enabled, const char* reason) const;
    void handleRouteChange (AVAudioSessionRouteChangeReason reason);

    Array<iOSAudioIODevice::Pimpl*> activeDevices;
    Array<iOSAudioIODeviceType*> activeDeviceTypes;

    id nativeSession;
};

static const char* getRoutingChangeReason (AVAudioSessionRouteChangeReason reason) noexcept
{
    switch (reason)
    {
        case AVAudioSessionRouteChangeReasonNewDeviceAvailable:         return "New device available";
        case AVAudioSessionRouteChangeReasonOldDeviceUnavailable:       return "Old device unavailable";
        case AVAudioSessionRouteChangeReasonCategoryChange:             return "Category change";
        case AVAudioSessionRouteChangeReasonOverride:                   return "Override";
        case AVAudioSessionRouteChangeReasonWakeFromSleep:              return "Wake from sleep";
        case AVAudioSessionRouteChangeReasonNoSuitableRouteForCategory: return "No suitable route for category";
        case AVAudioSessionRouteChangeReasonRouteConfigurationChange:   return "Route configuration change";
        case AVAudioSessionRouteChangeReasonUnknown:
        default:                                                        return "Unknown";
    }
}

bool getNotificationValueForKey (NSNotification* notification, NSString* key, NSUInteger& value) noexcept
{
    if (notification != nil)
    {
        if (NSDictionary* userInfo = [notification userInfo])
        {
            if (NSNumber* number = [userInfo objectForKey: key])
            {
                value = [number unsignedIntegerValue];
                return true;
            }
        }
    }

    jassertfalse;
    return false;
}

} // juce namespace

//==============================================================================
@interface iOSAudioSessionNative  : NSObject
{
@private
    juce::AudioSessionHolder* audioSessionHolder;
};

- (id) init: (juce::AudioSessionHolder*) holder;
- (void) dealloc;

- (void) audioSessionChangedInterruptionType: (NSNotification*) notification;
- (void) handleMediaServicesReset;
- (void) handleMediaServicesLost;
- (void) handleRouteChange: (NSNotification*) notification;
@end

@implementation iOSAudioSessionNative

- (id) init: (juce::AudioSessionHolder*) holder
{
    self = [super init];

    if (self != nil)
    {
        audioSessionHolder = holder;

        auto session = [AVAudioSession sharedInstance];
        auto centre = [NSNotificationCenter defaultCenter];

        [centre addObserver: self
                   selector: @selector (audioSessionChangedInterruptionType:)
                       name: AVAudioSessionInterruptionNotification
                     object: session];

        [centre addObserver: self
                   selector: @selector (handleMediaServicesLost)
                       name: AVAudioSessionMediaServicesWereLostNotification
                     object: session];

        [centre addObserver: self
                   selector: @selector (handleMediaServicesReset)
                       name: AVAudioSessionMediaServicesWereResetNotification
                     object: session];

        [centre addObserver: self
                   selector: @selector (handleRouteChange:)
                       name: AVAudioSessionRouteChangeNotification
                     object: session];
    }
    else
    {
        jassertfalse;
    }

    return self;
}

- (void) dealloc
{
    [[NSNotificationCenter defaultCenter] removeObserver: self];
    [super dealloc];
}

- (void) audioSessionChangedInterruptionType: (NSNotification*) notification
{
    NSUInteger value;

    if (juce::getNotificationValueForKey (notification, AVAudioSessionInterruptionTypeKey, value))
    {
        switch ((AVAudioSessionInterruptionType) value)
        {
            case AVAudioSessionInterruptionTypeBegan:
                audioSessionHolder->handleStatusChange (false, "AVAudioSessionInterruptionTypeBegan");
                break;

            case AVAudioSessionInterruptionTypeEnded:
                audioSessionHolder->handleStatusChange (true, "AVAudioSessionInterruptionTypeEnded");
                break;

            // No default so the code doesn't compile if this enum is extended.
        }
    }
}

- (void) handleMediaServicesReset
{
    audioSessionHolder->handleStatusChange (true, "AVAudioSessionMediaServicesWereResetNotification");
}

- (void) handleMediaServicesLost
{
    audioSessionHolder->handleStatusChange (false, "AVAudioSessionMediaServicesWereLostNotification");
}

- (void) handleRouteChange: (NSNotification*) notification
{
    NSUInteger value;

    if (juce::getNotificationValueForKey (notification, AVAudioSessionRouteChangeReasonKey, value))
        audioSessionHolder->handleRouteChange ((AVAudioSessionRouteChangeReason) value);
}

@end

//==============================================================================
#if JUCE_MODULE_AVAILABLE_juce_graphics
 #include <juce_graphics/native/juce_mac_CoreGraphicsHelpers.h>
#endif

namespace juce {

#ifndef JUCE_IOS_AUDIO_LOGGING
 #define JUCE_IOS_AUDIO_LOGGING 0
#endif

#if JUCE_IOS_AUDIO_LOGGING
 #define JUCE_IOS_AUDIO_LOG(x)  DBG(x)
#else
 #define JUCE_IOS_AUDIO_LOG(x)
#endif

static void logNSError (NSError* e)
{
    if (e != nil)
    {
        JUCE_IOS_AUDIO_LOG ("iOS Audio error: " << [e.localizedDescription UTF8String]);
        jassertfalse;
    }
}

#define JUCE_NSERROR_CHECK(X)     { NSError* error = nil; X; logNSError (error); }

//==============================================================================
class iOSAudioIODeviceType  : public AudioIODeviceType,
                              public AsyncUpdater
{
public:
    iOSAudioIODeviceType();
    ~iOSAudioIODeviceType() override;

    void scanForDevices() override;
    StringArray getDeviceNames (bool) const override;
    int getDefaultDeviceIndex (bool) const override;
    int getIndexOfDevice (AudioIODevice*, bool) const override;
    bool hasSeparateInputsAndOutputs() const override;
    AudioIODevice* createDevice (const String&, const String&) override;

private:
    void handleRouteChange (AVAudioSessionRouteChangeReason);

    void handleAsyncUpdate() override;

    friend struct AudioSessionHolder;
    friend struct iOSAudioIODevice::Pimpl;

    SharedResourcePointer<AudioSessionHolder> sessionHolder;

    JUCE_DECLARE_WEAK_REFERENCEABLE (iOSAudioIODeviceType)
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (iOSAudioIODeviceType)
};

//==============================================================================
struct iOSAudioIODevice::Pimpl      : public AudioPlayHead,
                                      public AsyncUpdater
{
    Pimpl (iOSAudioIODeviceType* ioDeviceType, iOSAudioIODevice& ioDevice)
        : deviceType (ioDeviceType),
          owner (ioDevice)
    {
        JUCE_IOS_AUDIO_LOG ("Creating iOS audio device");

        // We need to activate the audio session here to obtain the available sample rates and buffer sizes,
        // but if we don't set a category first then background audio will always be stopped. This category
        // may be changed later.
        setAudioSessionCategory (AVAudioSessionCategoryPlayAndRecord);

        setAudioSessionActive (true);
        updateHardwareInfo();
        channelData.reconfigure ({}, {});
        setAudioSessionActive (false);

        sessionHolder->activeDevices.add (this);
    }

    ~Pimpl() override
    {
        sessionHolder->activeDevices.removeFirstMatchingValue (this);

        close();
    }

    static void setAudioSessionCategory (NSString* category)
    {
        NSUInteger options = 0;

       #if ! JUCE_DISABLE_AUDIO_MIXING_WITH_OTHER_APPS
        options |= AVAudioSessionCategoryOptionMixWithOthers; // Alternatively AVAudioSessionCategoryOptionDuckOthers
       #endif

        if (category == AVAudioSessionCategoryPlayAndRecord)
            options |= (AVAudioSessionCategoryOptionDefaultToSpeaker
                      | AVAudioSessionCategoryOptionAllowBluetooth
                      | AVAudioSessionCategoryOptionAllowBluetoothA2DP);

        JUCE_NSERROR_CHECK ([[AVAudioSession sharedInstance] setCategory: category
                                                             withOptions: options
                                                                   error: &error]);
    }

    static void setAudioSessionActive (bool enabled)
    {
        JUCE_NSERROR_CHECK ([[AVAudioSession sharedInstance] setActive: enabled
                                                                 error: &error]);
    }

    int getBufferSize (const double currentSampleRate)
    {
        return roundToInt (currentSampleRate * [AVAudioSession sharedInstance].IOBufferDuration);
    }

    int tryBufferSize (const double currentSampleRate, const int newBufferSize)
    {
        NSTimeInterval bufferDuration = currentSampleRate > 0 ? (NSTimeInterval) ((newBufferSize + 1) / currentSampleRate) : 0.0;

        auto session = [AVAudioSession sharedInstance];
        JUCE_NSERROR_CHECK ([session setPreferredIOBufferDuration: bufferDuration
                                                            error: &error]);

        return getBufferSize (currentSampleRate);
    }

    void updateAvailableBufferSizes()
    {
        availableBufferSizes.clear();

        auto newBufferSize = tryBufferSize (sampleRate, 64);
        jassert (newBufferSize > 0);

        const auto longestBufferSize  = tryBufferSize (sampleRate, 4096);

        while (newBufferSize <= longestBufferSize)
        {
            availableBufferSizes.add (newBufferSize);
            newBufferSize *= 2;
        }

        // Sometimes the largest supported buffer size is not a power of 2
        availableBufferSizes.addIfNotAlreadyThere (longestBufferSize);

        bufferSize = tryBufferSize (sampleRate, bufferSize);

       #if JUCE_IOS_AUDIO_LOGGING
        {
            String info ("Available buffer sizes:");

            for (auto size : availableBufferSizes)
                info << " " << size;

            JUCE_IOS_AUDIO_LOG (info);
        }
       #endif

        JUCE_IOS_AUDIO_LOG ("Buffer size after detecting available buffer sizes: " << bufferSize);
    }

    double trySampleRate (double rate)
    {
        auto session = [AVAudioSession sharedInstance];
        JUCE_NSERROR_CHECK ([session setPreferredSampleRate: rate
                                                      error: &error]);

        return session.sampleRate;
    }

    // Important: the supported audio sample rates change on the iPhone 6S
    // depending on whether the headphones are plugged in or not!
    void updateAvailableSampleRates()
    {
        availableSampleRates.clear();

        AudioUnitRemovePropertyListenerWithUserData (audioUnit,
                                                     kAudioUnitProperty_StreamFormat,
                                                     dispatchAudioUnitPropertyChange,
                                                     this);

        const double lowestRate = trySampleRate (4000);
        availableSampleRates.add (lowestRate);
        const double highestRate = trySampleRate (192000);

        JUCE_IOS_AUDIO_LOG ("Lowest supported sample rate: "  << lowestRate);
        JUCE_IOS_AUDIO_LOG ("Highest supported sample rate: " << highestRate);

        for (double rate = lowestRate + 1000; rate < highestRate; rate += 1000)
        {
            const double supportedRate = trySampleRate (rate);
            JUCE_IOS_AUDIO_LOG ("Trying a sample rate of " << rate << ", got " << supportedRate);
            availableSampleRates.addIfNotAlreadyThere (supportedRate);
            rate = jmax (rate, supportedRate);
        }

        availableSampleRates.addIfNotAlreadyThere (highestRate);

        // Restore the original values.
        sampleRate = trySampleRate (sampleRate);
        bufferSize = tryBufferSize (sampleRate, bufferSize);

        AudioUnitAddPropertyListener (audioUnit,
                                      kAudioUnitProperty_StreamFormat,
                                      dispatchAudioUnitPropertyChange,
                                      this);

        // Check the current stream format in case things have changed whilst we
        // were going through the sample rates
        handleStreamFormatChange();

       #if JUCE_IOS_AUDIO_LOGGING
        {
            String info ("Available sample rates:");

            for (auto rate : availableSampleRates)
                info << " " << rate;

            JUCE_IOS_AUDIO_LOG (info);
        }
       #endif

        JUCE_IOS_AUDIO_LOG ("Sample rate after detecting available sample rates: " << sampleRate);
    }

    void updateHardwareInfo (bool forceUpdate = false)
    {
        if (! forceUpdate && ! hardwareInfoNeedsUpdating.compareAndSetBool (false, true))
            return;

        JUCE_IOS_AUDIO_LOG ("Updating hardware info");

        updateAvailableSampleRates();
        updateAvailableBufferSizes();

        if (deviceType != nullptr)
            deviceType->callDeviceChangeListeners();
    }

    void setTargetSampleRateAndBufferSize()
    {
        JUCE_IOS_AUDIO_LOG ("Setting target sample rate: " << targetSampleRate);
        sampleRate = trySampleRate (targetSampleRate);
        JUCE_IOS_AUDIO_LOG ("Actual sample rate: " << sampleRate);

        JUCE_IOS_AUDIO_LOG ("Setting target buffer size: " << targetBufferSize);
        bufferSize = tryBufferSize (sampleRate, targetBufferSize);
        JUCE_IOS_AUDIO_LOG ("Actual buffer size: " << bufferSize);
    }

    String open (const BigInteger& inputChannelsWanted,
                 const BigInteger& outputChannelsWanted,
                 double sampleRateWanted, int bufferSizeWanted)
    {
        close();

        firstHostTime = true;
        lastNumFrames = 0;
        xrun = 0;
        lastError.clear();

        requestedInputChannels  = inputChannelsWanted;
        requestedOutputChannels = outputChannelsWanted;
        targetSampleRate = sampleRateWanted;
        targetBufferSize = bufferSizeWanted > 0 ? bufferSizeWanted : defaultBufferSize;

        JUCE_IOS_AUDIO_LOG ("Opening audio device:"
                            <<  " inputChannelsWanted: "  << requestedInputChannels .toString (2)
                            << ", outputChannelsWanted: " << requestedOutputChannels.toString (2)
                            << ", targetSampleRate: " << targetSampleRate
                            << ", targetBufferSize: " << targetBufferSize);

        setAudioSessionActive (true);
        setAudioSessionCategory (requestedInputChannels > 0 ? AVAudioSessionCategoryPlayAndRecord
                                                            : AVAudioSessionCategoryPlayback);
        channelData.reconfigure (requestedInputChannels, requestedOutputChannels);
        updateHardwareInfo (true);
        setTargetSampleRateAndBufferSize();
        fixAudioRouteIfSetToReceiver();

        isRunning = true;

        if (! createAudioUnit())
        {
            lastError = "Couldn't open the device";
            return lastError;
        }

        const ScopedLock sl (callbackLock);

        AudioOutputUnitStart (audioUnit);

        if (callback != nullptr)
            callback->audioDeviceAboutToStart (&owner);

        return lastError;
    }

    void close()
    {
        stop();

        if (isRunning)
        {
            isRunning = false;

            if (audioUnit != nullptr)
            {
                AudioOutputUnitStart (audioUnit);
                AudioComponentInstanceDispose (audioUnit);
                audioUnit = nullptr;
            }

            setAudioSessionActive (false);
        }
    }

    void start (AudioIODeviceCallback* newCallback)
    {
        if (isRunning && callback != newCallback)
        {
            if (newCallback != nullptr)
                newCallback->audioDeviceAboutToStart (&owner);

            const ScopedLock sl (callbackLock);
            callback = newCallback;
        }
    }

    void stop()
    {
        if (isRunning)
        {
            AudioIODeviceCallback* lastCallback;

            {
                const ScopedLock sl (callbackLock);
                lastCallback = callback;
                callback = nullptr;
            }

            if (lastCallback != nullptr)
                lastCallback->audioDeviceStopped();
        }
    }

    bool setAudioPreprocessingEnabled (bool enable)
    {
        auto session = [AVAudioSession sharedInstance];

        NSString* mode = (enable ? AVAudioSessionModeDefault
                                 : AVAudioSessionModeMeasurement);

        JUCE_NSERROR_CHECK ([session setMode: mode
                                       error: &error]);

        return session.mode == mode;
    }

    //==============================================================================
    bool canControlTransport() override                    { return interAppAudioConnected; }

    void transportPlay (bool shouldSartPlaying) override
    {
        if (! canControlTransport())
            return;

        HostCallbackInfo callbackInfo;
        fillHostCallbackInfo (callbackInfo);

        Boolean hostIsPlaying = NO;
        OSStatus err = callbackInfo.transportStateProc2 (callbackInfo.hostUserData,
                                                         &hostIsPlaying,
                                                         nullptr,
                                                         nullptr,
                                                         nullptr,
                                                         nullptr,
                                                         nullptr,
                                                         nullptr);

        ignoreUnused (err);
        jassert (err == noErr);

        if (hostIsPlaying != shouldSartPlaying)
            handleAudioTransportEvent (kAudioUnitRemoteControlEvent_TogglePlayPause);
    }

    void transportRecord (bool shouldStartRecording) override
    {
        if (! canControlTransport())
            return;

        HostCallbackInfo callbackInfo;
        fillHostCallbackInfo (callbackInfo);

        Boolean hostIsRecording = NO;
        OSStatus err = callbackInfo.transportStateProc2 (callbackInfo.hostUserData,
                                                         nullptr,
                                                         &hostIsRecording,
                                                         nullptr,
                                                         nullptr,
                                                         nullptr,
                                                         nullptr,
                                                         nullptr);
        ignoreUnused (err);
        jassert (err == noErr);

        if (hostIsRecording != shouldStartRecording)
            handleAudioTransportEvent (kAudioUnitRemoteControlEvent_ToggleRecord);
    }

    void transportRewind() override
    {
        if (canControlTransport())
            handleAudioTransportEvent (kAudioUnitRemoteControlEvent_Rewind);
    }

    bool getCurrentPosition (CurrentPositionInfo& result) override
    {
        if (! canControlTransport())
            return false;

        zerostruct (result);

        HostCallbackInfo callbackInfo;
        fillHostCallbackInfo (callbackInfo);

        if (callbackInfo.hostUserData == nullptr)
            return false;

        Boolean hostIsPlaying               = NO;
        Boolean hostIsRecording             = NO;
        Float64 hostCurrentSampleInTimeLine = 0;
        Boolean hostIsCycling               = NO;
        Float64 hostCycleStartBeat          = 0;
        Float64 hostCycleEndBeat            = 0;
        OSStatus err = callbackInfo.transportStateProc2 (callbackInfo.hostUserData,
                                                         &hostIsPlaying,
                                                         &hostIsRecording,
                                                         nullptr,
                                                         &hostCurrentSampleInTimeLine,
                                                         &hostIsCycling,
                                                         &hostCycleStartBeat,
                                                         &hostCycleEndBeat);
        if (err == kAUGraphErr_CannotDoInCurrentContext)
            return false;

        jassert (err == noErr);

        result.timeInSamples = (int64) hostCurrentSampleInTimeLine;
        result.isPlaying     = hostIsPlaying;
        result.isRecording   = hostIsRecording;
        result.isLooping     = hostIsCycling;
        result.ppqLoopStart  = hostCycleStartBeat;
        result.ppqLoopEnd    = hostCycleEndBeat;

        result.timeInSeconds = result.timeInSamples / sampleRate;

        Float64 hostBeat = 0;
        Float64 hostTempo = 0;
        err = callbackInfo.beatAndTempoProc (callbackInfo.hostUserData,
                                             &hostBeat,
                                             &hostTempo);
        jassert (err == noErr);

        result.ppqPosition = hostBeat;
        result.bpm         = hostTempo;

        Float32 hostTimeSigNumerator = 0;
        UInt32 hostTimeSigDenominator = 0;
        Float64 hostCurrentMeasureDownBeat = 0;
        err = callbackInfo.musicalTimeLocationProc (callbackInfo.hostUserData,
                                                    nullptr,
                                                    &hostTimeSigNumerator,
                                                    &hostTimeSigDenominator,
                                                    &hostCurrentMeasureDownBeat);
        jassert (err == noErr);

        result.ppqPositionOfLastBarStart = hostCurrentMeasureDownBeat;
        result.timeSigNumerator          = (int) hostTimeSigNumerator;
        result.timeSigDenominator        = (int) hostTimeSigDenominator;

        result.frameRate = AudioPlayHead::fpsUnknown;

        return true;
    }

    //==============================================================================
   #if JUCE_MODULE_AVAILABLE_juce_graphics
    JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE ("-Wdeprecated-declarations")
    Image getIcon (int size)
    {
        if (interAppAudioConnected)
        {
            UIImage* hostUIImage = AudioOutputUnitGetHostIcon (audioUnit, size);
            if (hostUIImage != nullptr)
                return juce_createImageFromUIImage (hostUIImage);
        }
        return Image();
    }
    JUCE_END_IGNORE_WARNINGS_GCC_LIKE
   #endif

    void switchApplication()
    {
        if (! interAppAudioConnected)
            return;

        CFURLRef hostUrl;
        UInt32 dataSize = sizeof (hostUrl);
        OSStatus err = AudioUnitGetProperty(audioUnit,
                                            kAudioUnitProperty_PeerURL,
                                            kAudioUnitScope_Global,
                                            0,
                                            &hostUrl,
                                            &dataSize);
        if (err == noErr)
        {
           #if (! defined __IPHONE_10_0) || (__IPHONE_OS_VERSION_MIN_REQUIRED < __IPHONE_10_0)
            [[UIApplication sharedApplication] openURL: (NSURL*)hostUrl];
           #else
            [[UIApplication sharedApplication] openURL: (NSURL*)hostUrl options: @{} completionHandler: nil];
           #endif
        }
    }

    //==============================================================================
    void invokeAudioDeviceErrorCallback (const String& reason)
    {
        const ScopedLock sl (callbackLock);

        if (callback != nullptr)
            callback->audioDeviceError (reason);
    }

    void handleStatusChange (bool enabled, const char* reason)
    {
        const ScopedLock myScopedLock (callbackLock);

        JUCE_IOS_AUDIO_LOG ("handleStatusChange: enabled: " << (int) enabled << ", reason: " << reason);

        isRunning = enabled;
        setAudioSessionActive (enabled);

        if (enabled)
            AudioOutputUnitStart (audioUnit);
        else
            AudioOutputUnitStop (audioUnit);

        if (! enabled)
            invokeAudioDeviceErrorCallback (reason);
    }

    void handleRouteChange (AVAudioSessionRouteChangeReason reason)
    {
        const ScopedLock myScopedLock (callbackLock);

        const String reasonString (getRoutingChangeReason (reason));
        JUCE_IOS_AUDIO_LOG ("handleRouteChange: " << reasonString);

        if (isRunning)
            invokeAudioDeviceErrorCallback (reasonString);

        switch (reason)
        {
        case AVAudioSessionRouteChangeReasonCategoryChange:
        case AVAudioSessionRouteChangeReasonOverride:
        case AVAudioSessionRouteChangeReasonRouteConfigurationChange:
            break;
        case AVAudioSessionRouteChangeReasonUnknown:
        case AVAudioSessionRouteChangeReasonNewDeviceAvailable:
        case AVAudioSessionRouteChangeReasonOldDeviceUnavailable:
        case AVAudioSessionRouteChangeReasonWakeFromSleep:
        case AVAudioSessionRouteChangeReasonNoSuitableRouteForCategory:
        {
            hardwareInfoNeedsUpdating = true;
            triggerAsyncUpdate();
            break;
        }

        // No default so the code doesn't compile if this enum is extended.
        }
    }

    void handleAudioUnitPropertyChange (AudioUnit,
                                        AudioUnitPropertyID propertyID,
                                        AudioUnitScope scope,
                                        AudioUnitElement element)
    {
        ignoreUnused (scope);
        ignoreUnused (element);
        JUCE_IOS_AUDIO_LOG ("handleAudioUnitPropertyChange: propertyID: " << String (propertyID)
                                                            << " scope: " << String (scope)
                                                          << " element: " << String (element));

        switch (propertyID)
        {
            case kAudioUnitProperty_IsInterAppConnected:
                handleInterAppAudioConnectionChange();
                return;
            case kAudioUnitProperty_StreamFormat:
                handleStreamFormatChange();
                return;
            default:
                jassertfalse;
        }
    }

    void handleInterAppAudioConnectionChange()
    {
        UInt32 connected;
        UInt32 dataSize = sizeof (connected);
        OSStatus err = AudioUnitGetProperty (audioUnit, kAudioUnitProperty_IsInterAppConnected,
                                             kAudioUnitScope_Global, 0, &connected, &dataSize);
        ignoreUnused (err);
        jassert (err == noErr);

        JUCE_IOS_AUDIO_LOG ("handleInterAppAudioConnectionChange: " << (connected ? "connected"
                                                                                  : "disconnected"));

        if (connected != interAppAudioConnected)
        {
            const ScopedLock myScopedLock (callbackLock);

            interAppAudioConnected = connected;

            UIApplicationState appstate = [UIApplication sharedApplication].applicationState;
            bool inForeground = (appstate != UIApplicationStateBackground);

            if (interAppAudioConnected || inForeground)
            {
                setAudioSessionActive (true);
                AudioOutputUnitStart (audioUnit);

                if (callback != nullptr)
                    callback->audioDeviceAboutToStart (&owner);
            }
            else if (! inForeground)
            {
                AudioOutputUnitStop (audioUnit);
                setAudioSessionActive (false);

                if (callback != nullptr)
                    callback->audioDeviceStopped();
            }
        }
    }

    //==============================================================================
    OSStatus process (AudioUnitRenderActionFlags* flags, const AudioTimeStamp* time,
                      const UInt32 numFrames, AudioBufferList* data)
    {
        OSStatus err = noErr;

        recordXruns (time, numFrames);

        const bool useInput = channelData.areInputChannelsAvailable();

        if (useInput)
            err = AudioUnitRender (audioUnit, flags, time, 1, numFrames, data);

        const auto channelDataSize = sizeof (float) * numFrames;

        const ScopedTryLock stl (callbackLock);

        if (stl.isLocked() && callback != nullptr)
        {
            if ((int) numFrames > channelData.getFloatBufferSize())
                channelData.setFloatBufferSize ((int) numFrames);

            float** const inputData = channelData.audioData.getArrayOfWritePointers();
            float** const outputData = inputData + channelData.inputs->numActiveChannels;

            if (useInput)
            {
                for (int c = 0; c < channelData.inputs->numActiveChannels; ++c)
                {
                    auto channelIndex = channelData.inputs->activeChannelIndices[c];
                    memcpy (inputData[c], (float*) data->mBuffers[channelIndex].mData, channelDataSize);
                }
            }
            else
            {
                for (int c = 0; c < channelData.inputs->numActiveChannels; ++c)
                    zeromem (inputData[c], channelDataSize);
            }

            callback->audioDeviceIOCallback ((const float**) inputData,  channelData.inputs ->numActiveChannels,
                                                             outputData, channelData.outputs->numActiveChannels,
                                             (int) numFrames);

            for (int c = 0; c < channelData.outputs->numActiveChannels; ++c)
            {
                auto channelIndex = channelData.outputs->activeChannelIndices[c];
                memcpy (data->mBuffers[channelIndex].mData, outputData[c], channelDataSize);
            }

            for (auto c : channelData.outputs->inactiveChannelIndices)
                zeromem (data->mBuffers[c].mData, channelDataSize);
        }
        else
        {
            for (uint32 c = 0; c < data->mNumberBuffers; ++c)
                zeromem (data->mBuffers[c].mData, channelDataSize);
        }

        return err;
    }

    void recordXruns (const AudioTimeStamp* time, UInt32 numFrames)
    {
        if (time != nullptr && (time->mFlags & kAudioTimeStampSampleTimeValid) != 0)
        {
            if (! firstHostTime)
            {
                if ((time->mSampleTime - lastSampleTime) != lastNumFrames)
                    xrun++;
            }
            else
                firstHostTime = false;

            lastSampleTime = time->mSampleTime;
        }
        else
            firstHostTime = true;

        lastNumFrames = numFrames;
    }

    //==============================================================================
    static OSStatus processStatic (void* client, AudioUnitRenderActionFlags* flags, const AudioTimeStamp* time,
                                   UInt32 /*busNumber*/, UInt32 numFrames, AudioBufferList* data)
    {
        return static_cast<Pimpl*> (client)->process (flags, time, numFrames, data);
    }

    //==============================================================================
    bool createAudioUnit()
    {
        JUCE_IOS_AUDIO_LOG ("Creating the audio unit");

        if (audioUnit != nullptr)
        {
            AudioComponentInstanceDispose (audioUnit);
            audioUnit = nullptr;
        }

        AudioComponentDescription desc;
        desc.componentType = kAudioUnitType_Output;
        desc.componentSubType = kAudioUnitSubType_RemoteIO;
        desc.componentManufacturer = kAudioUnitManufacturer_Apple;
        desc.componentFlags = 0;
        desc.componentFlagsMask = 0;

        AudioComponent comp = AudioComponentFindNext (nullptr, &desc);
        AudioComponentInstanceNew (comp, &audioUnit);

        if (audioUnit == nullptr)
            return false;

       #if JucePlugin_Enable_IAA
        AudioComponentDescription appDesc;
        appDesc.componentType = JucePlugin_IAAType;
        appDesc.componentSubType = JucePlugin_IAASubType;
        appDesc.componentManufacturer = JucePlugin_ManufacturerCode;
        appDesc.componentFlags = 0;
        appDesc.componentFlagsMask = 0;
        OSStatus err = AudioOutputUnitPublish (&appDesc,
                                               CFSTR(JucePlugin_IAAName),
                                               JucePlugin_VersionCode,
                                               audioUnit);

        // This assert will be hit if the Inter-App Audio entitlement has not
        // been enabled, or the description being published with
        // AudioOutputUnitPublish is different from any in the AudioComponents
        // array in this application's .plist file.
        jassert (err == noErr);

        err = AudioUnitAddPropertyListener (audioUnit,
                                            kAudioUnitProperty_IsInterAppConnected,
                                            dispatchAudioUnitPropertyChange,
                                            this);
        jassert (err == noErr);

        AudioOutputUnitMIDICallbacks midiCallbacks;
        midiCallbacks.userData = this;
        midiCallbacks.MIDIEventProc = midiEventCallback;
        midiCallbacks.MIDISysExProc = midiSysExCallback;
        err = AudioUnitSetProperty (audioUnit,
                                    kAudioOutputUnitProperty_MIDICallbacks,
                                    kAudioUnitScope_Global,
                                    0,
                                    &midiCallbacks,
                                    sizeof (midiCallbacks));
        jassert (err == noErr);
       #endif

        if (channelData.areInputChannelsAvailable())
        {
            const UInt32 one = 1;
            AudioUnitSetProperty (audioUnit, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input, 1, &one, sizeof (one));
        }

        {
            AURenderCallbackStruct inputProc;
            inputProc.inputProc = processStatic;
            inputProc.inputProcRefCon = this;
            AudioUnitSetProperty (audioUnit, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input, 0, &inputProc, sizeof (inputProc));
        }

        {
            AudioStreamBasicDescription format;
            zerostruct (format);
            format.mSampleRate = sampleRate;
            format.mFormatID = kAudioFormatLinearPCM;
            format.mFormatFlags = kAudioFormatFlagIsFloat | kAudioFormatFlagIsNonInterleaved | kAudioFormatFlagsNativeEndian | kLinearPCMFormatFlagIsPacked;
            format.mBitsPerChannel = 8 * sizeof (float);
            format.mFramesPerPacket = 1;
            format.mChannelsPerFrame = (UInt32) jmax (channelData.inputs->numHardwareChannels, channelData.outputs->numHardwareChannels);
            format.mBytesPerFrame = format.mBytesPerPacket = sizeof (float);

            AudioUnitSetProperty (audioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input,  0, &format, sizeof (format));
            AudioUnitSetProperty (audioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &format, sizeof (format));
        }

        AudioUnitInitialize (audioUnit);

        {
            // Querying the kAudioUnitProperty_MaximumFramesPerSlice property after calling AudioUnitInitialize
            // seems to be more reliable than calling it before.
            UInt32 framesPerSlice, dataSize = sizeof (framesPerSlice);

            if (AudioUnitGetProperty (audioUnit, kAudioUnitProperty_MaximumFramesPerSlice,
                                      kAudioUnitScope_Global, 0, &framesPerSlice, &dataSize) == noErr
                    && dataSize == sizeof (framesPerSlice)
                    && static_cast<int> (framesPerSlice) != bufferSize)
            {
                JUCE_IOS_AUDIO_LOG ("Internal buffer size: " << String (framesPerSlice));
                channelData.setFloatBufferSize (static_cast<int> (framesPerSlice));
            }
        }

        AudioUnitAddPropertyListener (audioUnit, kAudioUnitProperty_StreamFormat, dispatchAudioUnitPropertyChange, this);

        return true;
    }

    void fillHostCallbackInfo (HostCallbackInfo& callbackInfo)
    {
        zerostruct (callbackInfo);
        UInt32 dataSize = sizeof (HostCallbackInfo);
        OSStatus err = AudioUnitGetProperty (audioUnit,
                                             kAudioUnitProperty_HostCallbacks,
                                             kAudioUnitScope_Global,
                                             0,
                                             &callbackInfo,
                                             &dataSize);
        ignoreUnused (err);
        jassert (err == noErr);
    }

    void handleAudioTransportEvent (AudioUnitRemoteControlEvent event)
    {
        OSStatus err = AudioUnitSetProperty (audioUnit, kAudioOutputUnitProperty_RemoteControlToHost,
                                             kAudioUnitScope_Global, 0, &event, sizeof (event));
        ignoreUnused (err);
        jassert (err == noErr);
    }

    // If the routing is set to go through the receiver (i.e. the speaker, but quiet), this re-routes it
    // to make it loud. Needed because by default when using an input + output, the output is kept quiet.
    static void fixAudioRouteIfSetToReceiver()
    {
        auto session = [AVAudioSession sharedInstance];
        auto route = session.currentRoute;

        for (AVAudioSessionPortDescription* port in route.outputs)
        {
            if ([port.portName isEqualToString: @"Receiver"])
            {
                JUCE_NSERROR_CHECK ([session overrideOutputAudioPort: AVAudioSessionPortOverrideSpeaker
                                                               error: &error]);
                setAudioSessionActive (true);
            }
        }
    }

    void restart()
    {
        const ScopedLock sl (callbackLock);

        updateHardwareInfo();
        setTargetSampleRateAndBufferSize();

        if (isRunning)
        {
            if (audioUnit != nullptr)
            {
                AudioComponentInstanceDispose (audioUnit);
                audioUnit = nullptr;

                if (callback != nullptr)
                    callback->audioDeviceStopped();
            }

            channelData.reconfigure (requestedInputChannels, requestedOutputChannels);

            createAudioUnit();

            if (audioUnit != nullptr)
            {
                isRunning = true;

                if (callback != nullptr)
                    callback->audioDeviceAboutToStart (&owner);

                AudioOutputUnitStart (audioUnit);
            }
        }
    }

    void handleAsyncUpdate() override
    {
        restart();
    }

    void handleStreamFormatChange()
    {
        AudioStreamBasicDescription desc;
        zerostruct (desc);
        UInt32 dataSize = sizeof (desc);
        AudioUnitGetProperty (audioUnit,
                              kAudioUnitProperty_StreamFormat,
                              kAudioUnitScope_Output,
                              0,
                              &desc,
                              &dataSize);

        if (desc.mSampleRate != 0 && desc.mSampleRate != sampleRate)
        {
            JUCE_IOS_AUDIO_LOG ("Stream format has changed: Sample rate " << desc.mSampleRate);
            triggerAsyncUpdate();
        }
    }

    static void dispatchAudioUnitPropertyChange (void* data, AudioUnit unit, AudioUnitPropertyID propertyID,
                                                 AudioUnitScope scope, AudioUnitElement element)
    {
        static_cast<Pimpl*> (data)->handleAudioUnitPropertyChange (unit, propertyID, scope, element);
    }

    static double getTimestampForMIDI()
    {
        return Time::getMillisecondCounter() / 1000.0;
    }

    static void midiEventCallback (void *client, UInt32 status, UInt32 data1, UInt32 data2, UInt32)
    {
        return static_cast<Pimpl*> (client)->handleMidiMessage (MidiMessage ((int) status,
                                                                             (int) data1,
                                                                             (int) data2,
                                                                             getTimestampForMIDI()));
    }

    static void midiSysExCallback (void *client, const UInt8 *data, UInt32 length)
    {
        return static_cast<Pimpl*> (client)->handleMidiMessage (MidiMessage (data, (int) length, getTimestampForMIDI()));
    }

    void handleMidiMessage (MidiMessage msg)
    {
        if (messageCollector != nullptr)
            messageCollector->addMessageToQueue (msg);
    }

    struct IOChannelData
    {
        class IOChannelConfig
        {
        public:
            IOChannelConfig (const bool isInput, const BigInteger requiredChannels)
                : hardwareChannelNames (getHardwareChannelNames (isInput)),
                  numHardwareChannels (hardwareChannelNames.size()),
                  areChannelsAccessible ((! isInput) || [AVAudioSession sharedInstance].isInputAvailable),
                  activeChannels (limitRequiredChannelsToHardware (numHardwareChannels, requiredChannels)),
                  numActiveChannels (activeChannels.countNumberOfSetBits()),
                  activeChannelIndices (getActiveChannelIndices (activeChannels)),
                  inactiveChannelIndices (getInactiveChannelIndices (activeChannelIndices, numHardwareChannels))
            {
               #if JUCE_IOS_AUDIO_LOGGING
                {
                    String info;

                    info << "Number of hardware channels: " << numHardwareChannels
                         << ", Hardware channel names:";

                    for (auto& name : hardwareChannelNames)
                        info << " \"" << name << "\"";

                    info << ", Are channels available: " << (areChannelsAccessible ? "yes" : "no")
                         << ", Active channel indices:";

                    for (auto i : activeChannelIndices)
                        info << " " << i;

                    info << ", Inactive channel indices:";

                    for (auto i : inactiveChannelIndices)
                        info << " " << i;

                    JUCE_IOS_AUDIO_LOG ((isInput ? "Input" : "Output") << " channel configuration: {" << info << "}");
                }
               #endif
            }

            const StringArray hardwareChannelNames;
            const int numHardwareChannels;
            const bool areChannelsAccessible;

            const BigInteger activeChannels;
            const int numActiveChannels;

            const Array<int> activeChannelIndices, inactiveChannelIndices;

        private:
            static StringArray getHardwareChannelNames (const bool isInput)
            {
                StringArray result;

                auto route = [AVAudioSession sharedInstance].currentRoute;

                for (AVAudioSessionPortDescription* port in (isInput ? route.inputs : route.outputs))
                {
                    for (AVAudioSessionChannelDescription* desc in port.channels)
                        result.add (nsStringToJuce (desc.channelName));
                }

                // A fallback for the iOS simulator and older iOS versions
                if (result.isEmpty())
                    return { "Left", "Right" };

                return result;
            }

            static BigInteger limitRequiredChannelsToHardware (const int numHardwareChannelsAvailable,
                                                               BigInteger requiredChannels)
            {
                requiredChannels.setRange (numHardwareChannelsAvailable,
                                           requiredChannels.getHighestBit() + 1,
                                           false);

                return requiredChannels;
            }

            static Array<int> getActiveChannelIndices (const BigInteger activeChannelsToIndex)
            {
                Array<int> result;

                auto index = activeChannelsToIndex.findNextSetBit (0);

                while (index != -1)
                {
                    result.add (index);
                    index = activeChannelsToIndex.findNextSetBit (++index);
                }

                return result;
            }

            static Array<int> getInactiveChannelIndices (const Array<int>& activeIndices, int numChannels)
            {
                Array<int> result;

                auto nextActiveChannel = activeIndices.begin();

                for (int i = 0; i < numChannels; ++i)
                    if (nextActiveChannel != activeIndices.end() && i == *nextActiveChannel)
                        ++nextActiveChannel;
                    else
                        result.add (i);

                return result;
            }
        };

        void reconfigure (const BigInteger requiredInputChannels,
                          const BigInteger requiredOutputChannels)
        {
            inputs .reset (new IOChannelConfig (true,  requiredInputChannels));
            outputs.reset (new IOChannelConfig (false, requiredOutputChannels));

            audioData.setSize (inputs->numActiveChannels + outputs->numActiveChannels,
                               audioData.getNumSamples());
        }

        int getFloatBufferSize() const
        {
            return audioData.getNumSamples();
        }

        void setFloatBufferSize (const int newSize)
        {
            audioData.setSize (audioData.getNumChannels(), newSize);
        }

        bool areInputChannelsAvailable() const
        {
            return inputs->areChannelsAccessible && inputs->numActiveChannels > 0;
        }

        std::unique_ptr<IOChannelConfig> inputs;
        std::unique_ptr<IOChannelConfig> outputs;

        AudioBuffer<float> audioData { 0, 0 };
    };

    IOChannelData channelData;

    BigInteger requestedInputChannels, requestedOutputChannels;

    bool isRunning = false;

    AudioIODeviceCallback* callback = nullptr;

    String lastError;

   #if TARGET_IPHONE_SIMULATOR
    static constexpr int defaultBufferSize = 512;
   #else
    static constexpr int defaultBufferSize = 256;
   #endif
    int targetBufferSize = defaultBufferSize, bufferSize = targetBufferSize;

    double targetSampleRate = 44100.0, sampleRate = targetSampleRate;

    Array<double> availableSampleRates;
    Array<int> availableBufferSizes;

    bool interAppAudioConnected = false;

    MidiMessageCollector* messageCollector = nullptr;

    WeakReference<iOSAudioIODeviceType> deviceType;
    iOSAudioIODevice& owner;

    CriticalSection callbackLock;

    Atomic<bool> hardwareInfoNeedsUpdating { true };

    AudioUnit audioUnit {};

    SharedResourcePointer<AudioSessionHolder> sessionHolder;

    bool firstHostTime;
    Float64 lastSampleTime;
    unsigned int lastNumFrames;
    int xrun;

    JUCE_DECLARE_NON_COPYABLE (Pimpl)
};

//==============================================================================
iOSAudioIODevice::iOSAudioIODevice (iOSAudioIODeviceType* ioDeviceType, const String&, const String&)
    : AudioIODevice (iOSAudioDeviceName, iOSAudioDeviceName),
      pimpl (new Pimpl (ioDeviceType, *this))
{
}

//==============================================================================
String iOSAudioIODevice::open (const BigInteger& inChans, const BigInteger& outChans,
                               double requestedSampleRate, int requestedBufferSize)
{
    return pimpl->open (inChans, outChans, requestedSampleRate, requestedBufferSize);
}

void iOSAudioIODevice::close()                                      { pimpl->close(); }

void iOSAudioIODevice::start (AudioIODeviceCallback* callbackToUse) { pimpl->start (callbackToUse); }
void iOSAudioIODevice::stop()                                       { pimpl->stop(); }

Array<double> iOSAudioIODevice::getAvailableSampleRates()           { return pimpl->availableSampleRates; }
Array<int> iOSAudioIODevice::getAvailableBufferSizes()              { return pimpl->availableBufferSizes; }

bool iOSAudioIODevice::setAudioPreprocessingEnabled (bool enabled)  { return pimpl->setAudioPreprocessingEnabled (enabled); }

bool iOSAudioIODevice::isPlaying()                                  { return pimpl->isRunning && pimpl->callback != nullptr; }
bool iOSAudioIODevice::isOpen()                                     { return pimpl->isRunning; }
String iOSAudioIODevice::getLastError()                             { return pimpl->lastError; }

StringArray iOSAudioIODevice::getOutputChannelNames()               { return pimpl->channelData.outputs->hardwareChannelNames; }
StringArray iOSAudioIODevice::getInputChannelNames()                { return pimpl->channelData.inputs->areChannelsAccessible ? pimpl->channelData.inputs->hardwareChannelNames : StringArray(); }

int iOSAudioIODevice::getDefaultBufferSize()                        { return pimpl->defaultBufferSize; }
int iOSAudioIODevice::getCurrentBufferSizeSamples()                 { return pimpl->bufferSize; }

double iOSAudioIODevice::getCurrentSampleRate()                     { return pimpl->sampleRate; }

int iOSAudioIODevice::getCurrentBitDepth()                          { return 16; }

BigInteger iOSAudioIODevice::getActiveInputChannels() const         { return pimpl->channelData.inputs->activeChannels; }
BigInteger iOSAudioIODevice::getActiveOutputChannels() const        { return pimpl->channelData.outputs->activeChannels; }

int iOSAudioIODevice::getInputLatencyInSamples()                    { return roundToInt (pimpl->sampleRate * [AVAudioSession sharedInstance].inputLatency); }
int iOSAudioIODevice::getOutputLatencyInSamples()                   { return roundToInt (pimpl->sampleRate * [AVAudioSession sharedInstance].outputLatency); }
int iOSAudioIODevice::getXRunCount() const noexcept                 { return pimpl->xrun; }

void iOSAudioIODevice::setMidiMessageCollector (MidiMessageCollector* collector) { pimpl->messageCollector = collector; }
AudioPlayHead* iOSAudioIODevice::getAudioPlayHead() const           { return pimpl.get(); }

bool iOSAudioIODevice::isInterAppAudioConnected() const             { return pimpl->interAppAudioConnected; }
#if JUCE_MODULE_AVAILABLE_juce_graphics
Image iOSAudioIODevice::getIcon (int size)                          { return pimpl->getIcon (size); }
#endif
void iOSAudioIODevice::switchApplication()                          { return pimpl->switchApplication(); }

//==============================================================================
iOSAudioIODeviceType::iOSAudioIODeviceType()
    : AudioIODeviceType (iOSAudioDeviceName)
{
    sessionHolder->activeDeviceTypes.add (this);
}

iOSAudioIODeviceType::~iOSAudioIODeviceType()
{
    sessionHolder->activeDeviceTypes.removeFirstMatchingValue (this);
}

// The list of devices is updated automatically
void iOSAudioIODeviceType::scanForDevices() {}
StringArray iOSAudioIODeviceType::getDeviceNames (bool) const             { return { iOSAudioDeviceName }; }
int iOSAudioIODeviceType::getDefaultDeviceIndex (bool) const              { return 0; }
int iOSAudioIODeviceType::getIndexOfDevice (AudioIODevice*, bool) const   { return 0; }
bool iOSAudioIODeviceType::hasSeparateInputsAndOutputs() const            { return false; }

AudioIODevice* iOSAudioIODeviceType::createDevice (const String& outputDeviceName, const String& inputDeviceName)
{
    return new iOSAudioIODevice (this, outputDeviceName, inputDeviceName);
}

void iOSAudioIODeviceType::handleRouteChange (AVAudioSessionRouteChangeReason)
{
    triggerAsyncUpdate();
}

void iOSAudioIODeviceType::handleAsyncUpdate()
{
    callDeviceChangeListeners();
}

//==============================================================================
AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_iOSAudio()
{
    return new iOSAudioIODeviceType();
}

//==============================================================================
AudioSessionHolder::AudioSessionHolder()    { nativeSession = [[iOSAudioSessionNative alloc] init: this]; }
AudioSessionHolder::~AudioSessionHolder()   { [nativeSession release]; }

void AudioSessionHolder::handleStatusChange (bool enabled, const char* reason) const
{
    for (auto device: activeDevices)
        device->handleStatusChange (enabled, reason);
}

void AudioSessionHolder::handleRouteChange (AVAudioSessionRouteChangeReason reason)
{
    for (auto device: activeDevices)
        device->handleRouteChange (reason);

    for (auto deviceType: activeDeviceTypes)
        deviceType->handleRouteChange (reason);
}

#undef JUCE_NSERROR_CHECK

} // namespace juce
