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

class iOSAudioIODevice;

static const char* const iOSAudioDeviceName = "iOS Audio";

//==============================================================================
struct AudioSessionHolder      : public AsyncUpdater
{
    AudioSessionHolder();
    ~AudioSessionHolder();

    void handleAsyncUpdate() override;

    void handleStatusChange (bool enabled, const char* reason) const;
    void handleRouteChange (const char* reason);

    CriticalSection routeChangeLock;
    String lastRouteChangeReason;
    Array<iOSAudioIODevice*> activeDevices;

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
        audioSessionHolder->handleRouteChange (juce::getRoutingChangeReason ((AVAudioSessionRouteChangeReason) value));
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
struct iOSAudioIODevice::Pimpl      : public AudioPlayHead,
                                      public AsyncUpdater
{
    Pimpl (iOSAudioIODevice& ioDevice)
        : owner (ioDevice)
    {
        sessionHolder->activeDevices.add (&owner);

        updateSampleRateAndAudioInput();
    }

    ~Pimpl()
    {
        sessionHolder->activeDevices.removeFirstMatchingValue (&owner);

        close();
    }

    static void setAudioSessionActive (bool enabled)
    {
        JUCE_NSERROR_CHECK ([[AVAudioSession sharedInstance] setActive: enabled
                                                                 error: &error]);
    }

    static double trySampleRate (double rate)
    {
        auto session = [AVAudioSession sharedInstance];
        JUCE_NSERROR_CHECK ([session setPreferredSampleRate: rate
                                                      error: &error]);
        return session.sampleRate;
    }

    Array<double> getAvailableSampleRates()
    {
        const ScopedLock sl (callbackLock);

        Array<double> rates;

        // Important: the supported audio sample rates change on the iPhone 6S
        // depending on whether the headphones are plugged in or not!
        setAudioSessionActive (true);

        AudioUnitRemovePropertyListenerWithUserData (audioUnit,
                                                     kAudioUnitProperty_StreamFormat,
                                                     dispatchAudioUnitPropertyChange,
                                                     this);

        const double lowestRate = trySampleRate (4000);
        const double highestRate = trySampleRate (192000);

        for (double rate = lowestRate; rate <= highestRate; rate += 1000)
        {
            const double supportedRate = trySampleRate (rate);

            if (rates.addIfNotAlreadyThere (supportedRate))
                JUCE_IOS_AUDIO_LOG ("available rate = " + String (supportedRate, 0) + "Hz");

            rate = jmax (rate, supportedRate);
        }

        trySampleRate (sampleRate);
        updateCurrentBufferSize();

        AudioUnitAddPropertyListener (audioUnit,
                                      kAudioUnitProperty_StreamFormat,
                                      dispatchAudioUnitPropertyChange,
                                      this);

        return rates;
    }

    Array<int> getAvailableBufferSizes()
    {
        Array<int> r;

        for (int i = 6; i < 13; ++i)
            r.add (1 << i);

        return r;
    }

    void updateSampleRateAndAudioInput()
    {
        auto session = [AVAudioSession sharedInstance];
        sampleRate = session.sampleRate;
        audioInputIsAvailable = session.isInputAvailable;
        actualBufferSize = roundToInt (sampleRate * session.IOBufferDuration);

        JUCE_IOS_AUDIO_LOG ("AVAudioSession: sampleRate: " << sampleRate
                            << " Hz, audioInputAvailable: " << (int) audioInputIsAvailable
                            << ", buffer size: " << actualBufferSize);
    }

    String open (const BigInteger& inputChannelsWanted,
                 const BigInteger& outputChannelsWanted,
                 double targetSampleRate, int bufferSize)
    {
        close();

        firstHostTime = true;
        lastNumFrames = 0;
        xrun = 0;
        lastError.clear();
        preferredBufferSize = bufferSize <= 0 ? defaultBufferSize : bufferSize;

        //  xxx set up channel mapping

        activeOutputChans = outputChannelsWanted;
        activeOutputChans.setRange (2, activeOutputChans.getHighestBit(), false);
        numOutputChannels = activeOutputChans.countNumberOfSetBits();
        monoOutputChannelNumber = activeOutputChans.findNextSetBit (0);

        activeInputChans = inputChannelsWanted;
        activeInputChans.setRange (2, activeInputChans.getHighestBit(), false);
        numInputChannels = activeInputChans.countNumberOfSetBits();
        monoInputChannelNumber = activeInputChans.findNextSetBit (0);

        setAudioSessionActive (true);

        // Set the session category & options:
        auto session = [AVAudioSession sharedInstance];

        const bool useInputs = (numInputChannels > 0 && audioInputIsAvailable);

        NSString* category = (useInputs ? AVAudioSessionCategoryPlayAndRecord : AVAudioSessionCategoryPlayback);

        NSUInteger options = AVAudioSessionCategoryOptionMixWithOthers; // Alternatively AVAudioSessionCategoryOptionDuckOthers
        if (useInputs) // These options are only valid for category = PlayAndRecord
            options |= (AVAudioSessionCategoryOptionDefaultToSpeaker | AVAudioSessionCategoryOptionAllowBluetooth);

        JUCE_NSERROR_CHECK ([session setCategory: category
                                     withOptions: options
                                           error: &error]);

        fixAudioRouteIfSetToReceiver();

        // Set the sample rate
        trySampleRate (targetSampleRate);
        updateSampleRateAndAudioInput();
        updateCurrentBufferSize();

        prepareFloatBuffers (actualBufferSize);

        isRunning = true;
        handleRouteChange ("Started AudioUnit");

        lastError = (audioUnit != 0 ? "" : "Couldn't open the device");

        setAudioSessionActive (true);

        return lastError;
    }

    void close()
    {
        if (isRunning)
        {
            isRunning = false;

            if (audioUnit != 0)
            {
                AudioOutputUnitStart (audioUnit);
                AudioComponentInstanceDispose (audioUnit);
                audioUnit = 0;
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

        NSString* mode = (enable ? AVAudioSessionModeMeasurement
                                 : AVAudioSessionModeDefault);

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
           #if (! defined __IPHONE_OS_VERSION_MIN_REQUIRED) || (! defined __IPHONE_10_0) || (__IPHONE_OS_VERSION_MIN_REQUIRED < __IPHONE_10_0)
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

    void handleRouteChange (const char* reason)
    {
        const ScopedLock myScopedLock (callbackLock);

        JUCE_IOS_AUDIO_LOG ("handleRouteChange: reason: " << reason);

        fixAudioRouteIfSetToReceiver();

        if (isRunning)
            invokeAudioDeviceErrorCallback (reason);

        restart();
    }

    void handleAudioUnitPropertyChange (AudioUnit,
                                        AudioUnitPropertyID propertyID,
                                        AudioUnitScope scope,
                                        AudioUnitElement element)
    {
        JUCE_IOS_AUDIO_LOG ("handleAudioUnitPropertyChange: propertyID: " << String (propertyID)
                                                            << " scope: " << String (scope)
                                                          << " element: " << String (element));

        switch (propertyID)
        {
            case kAudioUnitProperty_IsInterAppConnected:
                handleInterAppAudioConnectionChange();
                return;
            case kAudioUnitProperty_StreamFormat:
                if (scope == kAudioUnitScope_Output && element == 0)
                    handleStreamFormatChange();

                return;
            default:
                jassertfalse;
                return;
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
            }
        }
    }

    //==============================================================================
    void prepareFloatBuffers (int bufferSize)
    {
        if (numInputChannels + numOutputChannels > 0)
        {
            floatData.setSize (numInputChannels + numOutputChannels, bufferSize);
            zeromem (inputChannels, sizeof (inputChannels));
            zeromem (outputChannels, sizeof (outputChannels));

            for (int i = 0; i < numInputChannels; ++i)
                inputChannels[i] = floatData.getWritePointer (i);

            for (int i = 0; i < numOutputChannels; ++i)
                outputChannels[i] = floatData.getWritePointer (i + numInputChannels);
        }
    }

    //==============================================================================
    OSStatus process (AudioUnitRenderActionFlags* flags, const AudioTimeStamp* time,
                      const UInt32 numFrames, AudioBufferList* data)
    {
        OSStatus err = noErr;

        recordXruns (time, numFrames);

        if (audioInputIsAvailable && numInputChannels > 0)
            err = AudioUnitRender (audioUnit, flags, time, 1, numFrames, data);

        const ScopedTryLock stl (callbackLock);

        if (stl.isLocked() && callback != nullptr)
        {
            if ((int) numFrames > floatData.getNumSamples())
                prepareFloatBuffers ((int) numFrames);

            if (audioInputIsAvailable && numInputChannels > 0)
            {
                short* shortData = (short*) data->mBuffers[0].mData;

                if (numInputChannels >= 2)
                {
                    for (UInt32 i = 0; i < numFrames; ++i)
                    {
                        inputChannels[0][i] = *shortData++ * (1.0f / 32768.0f);
                        inputChannels[1][i] = *shortData++ * (1.0f / 32768.0f);
                    }
                }
                else
                {
                    if (monoInputChannelNumber > 0)
                        ++shortData;

                    for (UInt32 i = 0; i < numFrames; ++i)
                    {
                        inputChannels[0][i] = *shortData++ * (1.0f / 32768.0f);
                        ++shortData;
                    }
                }
            }
            else
            {
                for (int i = numInputChannels; --i >= 0;)
                    zeromem (inputChannels[i], sizeof (float) * numFrames);
            }

            callback->audioDeviceIOCallback ((const float**) inputChannels, numInputChannels,
                                             outputChannels, numOutputChannels, (int) numFrames);

            short* const shortData = (short*) data->mBuffers[0].mData;
            int n = 0;

            if (numOutputChannels >= 2)
            {
                for (UInt32 i = 0; i < numFrames; ++i)
                {
                    shortData [n++] = (short) (outputChannels[0][i] * 32767.0f);
                    shortData [n++] = (short) (outputChannels[1][i] * 32767.0f);
                }
            }
            else if (numOutputChannels == 1)
            {
                for (UInt32 i = 0; i < numFrames; ++i)
                {
                    const short s = (short) (outputChannels[monoOutputChannelNumber][i] * 32767.0f);
                    shortData [n++] = s;
                    shortData [n++] = s;
                }
            }
            else
            {
                zeromem (data->mBuffers[0].mData, 2 * sizeof (short) * numFrames);
            }
        }
        else
        {
            zeromem (data->mBuffers[0].mData, 2 * sizeof (short) * numFrames);
        }

        return err;
    }

    void updateCurrentBufferSize()
    {
        NSTimeInterval bufferDuration = sampleRate > 0 ? (NSTimeInterval) ((preferredBufferSize + 1) / sampleRate) : 0.0;

        JUCE_NSERROR_CHECK ([[AVAudioSession sharedInstance] setPreferredIOBufferDuration: bufferDuration
                                                                                    error: &error]);
        updateSampleRateAndAudioInput();
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
    void resetFormat (const int numChannels) noexcept
    {
        zerostruct (format);
        format.mFormatID = kAudioFormatLinearPCM;
        format.mFormatFlags = kLinearPCMFormatFlagIsSignedInteger | kLinearPCMFormatFlagIsPacked | kAudioFormatFlagsNativeEndian;
        format.mBitsPerChannel = 8 * sizeof (short);
        format.mChannelsPerFrame = (UInt32) numChannels;
        format.mFramesPerPacket = 1;
        format.mBytesPerFrame = format.mBytesPerPacket = (UInt32) numChannels * sizeof (short);
    }

    bool createAudioUnit()
    {
        if (audioUnit != 0)
        {
            AudioComponentInstanceDispose (audioUnit);
            audioUnit = 0;
        }

        resetFormat (2);

        AudioComponentDescription desc;
        desc.componentType = kAudioUnitType_Output;
        desc.componentSubType = kAudioUnitSubType_RemoteIO;
        desc.componentManufacturer = kAudioUnitManufacturer_Apple;
        desc.componentFlags = 0;
        desc.componentFlagsMask = 0;

        AudioComponent comp = AudioComponentFindNext (0, &desc);
        AudioComponentInstanceNew (comp, &audioUnit);

        if (audioUnit == 0)
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
       #endif

        if (numInputChannels > 0)
        {
            const UInt32 one = 1;
            AudioUnitSetProperty (audioUnit, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input, 1, &one, sizeof (one));
        }

        {
            AudioChannelLayout layout;
            layout.mChannelBitmap = 0;
            layout.mNumberChannelDescriptions = 0;
            layout.mChannelLayoutTag = kAudioChannelLayoutTag_Stereo;
            AudioUnitSetProperty (audioUnit, kAudioUnitProperty_AudioChannelLayout, kAudioUnitScope_Input,  0, &layout, sizeof (layout));
            AudioUnitSetProperty (audioUnit, kAudioUnitProperty_AudioChannelLayout, kAudioUnitScope_Output, 0, &layout, sizeof (layout));
        }

        {
            AURenderCallbackStruct inputProc;
            inputProc.inputProc = processStatic;
            inputProc.inputProcRefCon = this;
            AudioUnitSetProperty (audioUnit, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input, 0, &inputProc, sizeof (inputProc));
        }

        AudioUnitSetProperty (audioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input,  0, &format, sizeof (format));
        AudioUnitSetProperty (audioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &format, sizeof (format));

        UInt32 framesPerSlice;
        UInt32 dataSize = sizeof (framesPerSlice);

        AudioUnitInitialize (audioUnit);

        updateCurrentBufferSize();

        if (AudioUnitGetProperty (audioUnit, kAudioUnitProperty_MaximumFramesPerSlice,
                                  kAudioUnitScope_Global, 0, &framesPerSlice, &dataSize) == noErr
            && dataSize == sizeof (framesPerSlice) && static_cast<int> (framesPerSlice) != actualBufferSize)
        {
            prepareFloatBuffers (static_cast<int> (framesPerSlice));
        }

        AudioUnitAddPropertyListener (audioUnit, kAudioUnitProperty_StreamFormat,          dispatchAudioUnitPropertyChange, this);

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

        for (AVAudioSessionPortDescription* port in route.inputs)
        {
            ignoreUnused (port);
            JUCE_IOS_AUDIO_LOG ("AVAudioSession: input: " << [port.description UTF8String]);
        }

        for (AVAudioSessionPortDescription* port in route.outputs)
        {
            JUCE_IOS_AUDIO_LOG ("AVAudioSession: output: " << [port.description UTF8String]);

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
        if (isRunning)
        {
            updateSampleRateAndAudioInput();
            updateCurrentBufferSize();
            createAudioUnit();

            setAudioSessionActive (true);

            if (audioUnit != 0)
            {
                UInt32 formatSize = sizeof (format);
                AudioUnitGetProperty (audioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &format, &formatSize);
                AudioOutputUnitStart (audioUnit);
            }

            if (callback != nullptr)
            {
                callback->audioDeviceStopped();
                callback->audioDeviceAboutToStart (&owner);
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
        AudioUnitGetProperty(audioUnit,
                             kAudioUnitProperty_StreamFormat,
                             kAudioUnitScope_Output,
                             0,
                             &desc,
                             &dataSize);

        if (desc.mSampleRate != sampleRate)
        {
            JUCE_IOS_AUDIO_LOG ("handleStreamFormatChange: sample rate " << desc.mSampleRate);
            triggerAsyncUpdate();
        }
    }

    static void dispatchAudioUnitPropertyChange (void* data, AudioUnit unit, AudioUnitPropertyID propertyID,
                                                 AudioUnitScope scope, AudioUnitElement element)
    {
        static_cast<Pimpl*> (data)->handleAudioUnitPropertyChange (unit, propertyID, scope, element);
    }

    void handleMidiMessage (MidiMessage msg)
    {
        if (messageCollector != nullptr)
            messageCollector->addMessageToQueue (msg);
    }

    static void midiEventCallback (void *client, UInt32 status, UInt32 data1, UInt32 data2, UInt32)
    {
        return static_cast<Pimpl*> (client)->handleMidiMessage (MidiMessage ((int) status,
                                                                             (int) data1,
                                                                             (int) data2,
                                                                             Time::getMillisecondCounter() / 1000.0));
    }

    bool isRunning = false;
    AudioIODeviceCallback* callback = nullptr;

    String lastError;

    bool audioInputIsAvailable = false;

    const int defaultBufferSize =
   #if TARGET_IPHONE_SIMULATOR
    512;
   #else
    256;
   #endif
    double sampleRate = 0;
    int numInputChannels = 2, numOutputChannels = 2;
    int preferredBufferSize = 0, actualBufferSize = 0;

    bool interAppAudioConnected = false;

    BigInteger activeOutputChans, activeInputChans;

    MidiMessageCollector* messageCollector = nullptr;

    iOSAudioIODevice& owner;
    SharedResourcePointer<AudioSessionHolder> sessionHolder;
    CriticalSection callbackLock;

    AudioStreamBasicDescription format;
    AudioUnit audioUnit {};

    AudioSampleBuffer floatData;
    float* inputChannels[3];
    float* outputChannels[3];
    bool monoInputChannelNumber, monoOutputChannelNumber;

    bool firstHostTime;
    Float64 lastSampleTime;
    unsigned int lastNumFrames;
    int xrun;

    JUCE_DECLARE_NON_COPYABLE (Pimpl)
};


//==============================================================================
iOSAudioIODevice::iOSAudioIODevice (const String& deviceName)
    : AudioIODevice (deviceName, iOSAudioDeviceName),
      pimpl (new Pimpl (*this))
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

Array<double> iOSAudioIODevice::getAvailableSampleRates()           { return pimpl->getAvailableSampleRates(); }
Array<int> iOSAudioIODevice::getAvailableBufferSizes()              { return pimpl->getAvailableBufferSizes(); }

bool iOSAudioIODevice::setAudioPreprocessingEnabled (bool enabled)  { return pimpl->setAudioPreprocessingEnabled (enabled); }

bool iOSAudioIODevice::isPlaying()                                  { return pimpl->isRunning && pimpl->callback != nullptr; }
bool iOSAudioIODevice::isOpen()                                     { return pimpl->isRunning; }
String iOSAudioIODevice::getLastError()                             { return pimpl->lastError; }

StringArray iOSAudioIODevice::getOutputChannelNames()               { return { "Left", "Right" }; }
StringArray iOSAudioIODevice::getInputChannelNames()                { return pimpl->audioInputIsAvailable ? getOutputChannelNames() : StringArray(); }

int iOSAudioIODevice::getDefaultBufferSize()                        { return pimpl->defaultBufferSize; }
int iOSAudioIODevice::getCurrentBufferSizeSamples()                 { return pimpl->actualBufferSize; }

double iOSAudioIODevice::getCurrentSampleRate()                     { return pimpl->sampleRate; }

int iOSAudioIODevice::getCurrentBitDepth()                          { return 16; }

BigInteger iOSAudioIODevice::getActiveOutputChannels() const        { return pimpl->activeOutputChans; }
BigInteger iOSAudioIODevice::getActiveInputChannels() const         { return pimpl->activeInputChans; }

int iOSAudioIODevice::getOutputLatencyInSamples()                   { return roundToInt (pimpl->sampleRate * [AVAudioSession sharedInstance].outputLatency); }
int iOSAudioIODevice::getInputLatencyInSamples()                    { return roundToInt (pimpl->sampleRate * [AVAudioSession sharedInstance].inputLatency); }
int iOSAudioIODevice::getXRunCount() const noexcept                 { return pimpl->xrun; }

void iOSAudioIODevice::setMidiMessageCollector (MidiMessageCollector* collector) { pimpl->messageCollector = collector; }
AudioPlayHead* iOSAudioIODevice::getAudioPlayHead() const           { return pimpl; }

bool iOSAudioIODevice::isInterAppAudioConnected() const             { return pimpl->interAppAudioConnected; }
#if JUCE_MODULE_AVAILABLE_juce_graphics
Image iOSAudioIODevice::getIcon (int size)                          { return pimpl->getIcon (size); }
#endif
void iOSAudioIODevice::switchApplication()                          { return pimpl->switchApplication(); }

//==============================================================================
struct iOSAudioIODeviceType  : public AudioIODeviceType
{
    iOSAudioIODeviceType()  : AudioIODeviceType (iOSAudioDeviceName) {}

    void scanForDevices() {}
    StringArray getDeviceNames (bool /*wantInputNames*/) const       { return StringArray (iOSAudioDeviceName); }
    int getDefaultDeviceIndex (bool /*forInput*/) const              { return 0; }
    int getIndexOfDevice (AudioIODevice* d, bool /*asInput*/) const  { return d != nullptr ? 0 : -1; }
    bool hasSeparateInputsAndOutputs() const                         { return false; }

    AudioIODevice* createDevice (const String& outputDeviceName, const String& inputDeviceName)
    {
        if (outputDeviceName.isNotEmpty() || inputDeviceName.isNotEmpty())
            return new iOSAudioIODevice (outputDeviceName.isNotEmpty() ? outputDeviceName : inputDeviceName);

        return nullptr;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (iOSAudioIODeviceType)
};

//==============================================================================
AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_iOSAudio()
{
    return new iOSAudioIODeviceType();
}

//==============================================================================
AudioSessionHolder::AudioSessionHolder()    { nativeSession = [[iOSAudioSessionNative alloc] init: this]; }
AudioSessionHolder::~AudioSessionHolder()   { [nativeSession release]; }

void AudioSessionHolder::handleAsyncUpdate()
{
    const ScopedLock sl (routeChangeLock);
    for (auto device: activeDevices)
        device->pimpl->handleRouteChange (lastRouteChangeReason.toRawUTF8());
}

void AudioSessionHolder::handleStatusChange (bool enabled, const char* reason) const
{
    for (auto device: activeDevices)
        device->pimpl->handleStatusChange (enabled, reason);
}

void AudioSessionHolder::handleRouteChange (const char* reason)
{
    const ScopedLock sl (routeChangeLock);
    lastRouteChangeReason = reason;
    triggerAsyncUpdate();
}

#undef JUCE_NSERROR_CHECK

} // namespace juce
