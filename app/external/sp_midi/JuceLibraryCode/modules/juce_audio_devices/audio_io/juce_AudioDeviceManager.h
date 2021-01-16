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

//==============================================================================
/**
    Manages the state of some audio and midi i/o devices.

    This class keeps tracks of a currently-selected audio device, through
    with which it continuously streams data from an audio callback, as well as
    one or more midi inputs.

    The idea is that your application will create one global instance of this object,
    and let it take care of creating and deleting specific types of audio devices
    internally. So when the device is changed, your callbacks will just keep running
    without having to worry about this.

    The manager can save and reload all of its device settings as XML, which
    makes it very easy for you to save and reload the audio setup of your
    application.

    And to make it easy to let the user change its settings, there's a component
    to do just that - the AudioDeviceSelectorComponent class, which contains a set of
    device selection/sample-rate/latency controls.

    To use an AudioDeviceManager, create one, and use initialise() to set it up. Then
    call addAudioCallback() to register your audio callback with it, and use that to process
    your audio data.

    The manager also acts as a handy hub for incoming midi messages, allowing a
    listener to register for messages from either a specific midi device, or from whatever
    the current default midi input device is. The listener then doesn't have to worry about
    re-registering with different midi devices if they are changed or deleted.

    And yet another neat trick is that amount of CPU time being used is measured and
    available with the getCpuUsage() method.

    The AudioDeviceManager is a ChangeBroadcaster, and will send a change message to
    listeners whenever one of its settings is changed.

    @see AudioDeviceSelectorComponent, AudioIODevice, AudioIODeviceType

    @tags{Audio}
*/
class JUCE_API  AudioDeviceManager  : public ChangeBroadcaster
{
public:
    //==============================================================================
    /** Creates a default AudioDeviceManager.

        Initially no audio device will be selected. You should call the initialise() method
        and register an audio callback with setAudioCallback() before it'll be able to
        actually make any noise.
    */
    AudioDeviceManager();

    /** Destructor. */
    ~AudioDeviceManager() override;

    //==============================================================================
    /**
        This structure holds a set of properties describing the current audio setup.

        An AudioDeviceManager uses this class to save/load its current settings, and to
        specify your preferred options when opening a device.

        @see AudioDeviceManager::setAudioDeviceSetup(), AudioDeviceManager::initialise()
    */
    struct JUCE_API  AudioDeviceSetup
    {
        /** The name of the audio device used for output.
            The name has to be one of the ones listed by the AudioDeviceManager's currently
            selected device type.
            This may be the same as the input device.
            An empty string indicates the default device.
        */
        String outputDeviceName;

        /** The name of the audio device used for input.
            This may be the same as the output device.
            An empty string indicates the default device.
        */
        String inputDeviceName;

        /** The current sample rate.
            This rate is used for both the input and output devices.
            A value of 0 indicates that you don't care what rate is used, and the
            device will choose a sensible rate for you.
        */
        double sampleRate = 0;

        /** The buffer size, in samples.
            This buffer size is used for both the input and output devices.
            A value of 0 indicates the default buffer size.
        */
        int bufferSize = 0;

        /** The set of active input channels.
            The bits that are set in this array indicate the channels of the
            input device that are active.
            If useDefaultInputChannels is true, this value is ignored.
        */
        BigInteger inputChannels;

        /** If this is true, it indicates that the inputChannels array
            should be ignored, and instead, the device's default channels
            should be used.
        */
        bool useDefaultInputChannels = true;

        /** The set of active output channels.
            The bits that are set in this array indicate the channels of the
            input device that are active.
            If useDefaultOutputChannels is true, this value is ignored.
        */
        BigInteger outputChannels;

        /** If this is true, it indicates that the outputChannels array
            should be ignored, and instead, the device's default channels
            should be used.
        */
        bool useDefaultOutputChannels = true;

        bool operator== (const AudioDeviceSetup&) const;
        bool operator!= (const AudioDeviceSetup&) const;
    };


    //==============================================================================
    /** Opens a set of audio devices ready for use.

        This will attempt to open either a default audio device, or one that was
        previously saved as XML.

        @param numInputChannelsNeeded       the maximum number of input channels your app would like to
                                            use (the actual number of channels opened may be less than
                                            the number requested)
        @param numOutputChannelsNeeded      the maximum number of output channels your app would like to
                                            use (the actual number of channels opened may be less than
                                            the number requested)
        @param savedState                   either a previously-saved state that was produced
                                            by createStateXml(), or nullptr if you want the manager
                                            to choose the best device to open.
        @param selectDefaultDeviceOnFailure if true, then if the device specified in the XML
                                            fails to open, then a default device will be used
                                            instead. If false, then on failure, no device is
                                            opened.
        @param preferredDefaultDeviceName   if this is not empty, and there's a device with this
                                            name, then that will be used as the default device
                                            (assuming that there wasn't one specified in the XML).
                                            The string can actually be a simple wildcard, containing "*"
                                            and "?" characters
        @param preferredSetupOptions        if this is non-null, the structure will be used as the
                                            set of preferred settings when opening the device. If you
                                            use this parameter, the preferredDefaultDeviceName
                                            field will be ignored

        @returns an error message if anything went wrong, or an empty string if it worked ok.
    */
    String initialise (int numInputChannelsNeeded,
                       int numOutputChannelsNeeded,
                       const XmlElement* savedState,
                       bool selectDefaultDeviceOnFailure,
                       const String& preferredDefaultDeviceName = String(),
                       const AudioDeviceSetup* preferredSetupOptions = nullptr);

    /** Resets everything to a default device setup, clearing any stored settings. */
    String initialiseWithDefaultDevices (int numInputChannelsNeeded,
                                         int numOutputChannelsNeeded);

    /** Returns some XML representing the current state of the manager.

        This stores the current device, its samplerate, block size, etc, and
        can be restored later with initialise().

        Note that this can return a null pointer if no settings have been explicitly changed
        (i.e. if the device manager has just been left in its default state).
    */
    std::unique_ptr<XmlElement> createStateXml() const;

    //==============================================================================
    /** Returns the current device properties that are in use.
        @see setAudioDeviceSetup
    */
    AudioDeviceSetup getAudioDeviceSetup() const;

    /** Returns the current device properties that are in use.
        This is an old method, kept around for compatibility, but you should prefer the new
        version which returns the result rather than taking an out-parameter.
        @see getAudioDeviceSetup()
    */
    void getAudioDeviceSetup (AudioDeviceSetup& result) const;

    /** Changes the current device or its settings.

        If you want to change a device property, like the current sample rate or
        block size, you can call getAudioDeviceSetup() to retrieve the current
        settings, then tweak the appropriate fields in the AudioDeviceSetup structure,
        and pass it back into this method to apply the new settings.

        @param newSetup             the settings that you'd like to use
        @param treatAsChosenDevice  if this is true and if the device opens correctly, these new
                                    settings will be taken as having been explicitly chosen by the
                                    user, and the next time createStateXml() is called, these settings
                                    will be returned. If it's false, then the device is treated as a
                                    temporary or default device, and a call to createStateXml() will
                                    return either the last settings that were made with treatAsChosenDevice
                                    as true, or the last XML settings that were passed into initialise().
        @returns an error message if anything went wrong, or an empty string if it worked ok.

        @see getAudioDeviceSetup
    */
    String setAudioDeviceSetup (const AudioDeviceSetup& newSetup, bool treatAsChosenDevice);


    /** Returns the currently-active audio device. */
    AudioIODevice* getCurrentAudioDevice() const noexcept               { return currentAudioDevice.get(); }

    /** Returns the type of audio device currently in use.
        @see setCurrentAudioDeviceType
    */
    String getCurrentAudioDeviceType() const                            { return currentDeviceType; }

    /** Returns the currently active audio device type object.
        Don't keep a copy of this pointer - it's owned by the device manager and could
        change at any time.
    */
    AudioIODeviceType* getCurrentDeviceTypeObject() const;

    /** Changes the class of audio device being used.

        This switches between, e.g. ASIO and DirectSound. On the Mac you probably won't ever call
        this because there's only one type: CoreAudio.

        For a list of types, see getAvailableDeviceTypes().
    */
    void setCurrentAudioDeviceType (const String& type, bool treatAsChosenDevice);

    /** Closes the currently-open device.
        You can call restartLastAudioDevice() later to reopen it in the same state
        that it was just in.
    */
    void closeAudioDevice();

    /** Tries to reload the last audio device that was running.

        Note that this only reloads the last device that was running before
        closeAudioDevice() was called - it doesn't reload any kind of saved-state,
        and can only be called after a device has been opened with SetAudioDevice().

        If a device is already open, this call will do nothing.
    */
    void restartLastAudioDevice();

    //==============================================================================
    /** Registers an audio callback to be used.

        The manager will redirect callbacks from whatever audio device is currently
        in use to all registered callback objects. If more than one callback is
        active, they will all be given the same input data, and their outputs will
        be summed.

        If necessary, this method will invoke audioDeviceAboutToStart() on the callback
        object before returning.

        To remove a callback, use removeAudioCallback().
    */
    void addAudioCallback (AudioIODeviceCallback* newCallback);

    /** Deregisters a previously added callback.

        If necessary, this method will invoke audioDeviceStopped() on the callback
        object before returning.

        @see addAudioCallback
    */
    void removeAudioCallback (AudioIODeviceCallback* callback);

    //==============================================================================
    /** Returns the average proportion of available CPU being spent inside the audio callbacks.
        @returns  A value between 0 and 1.0 to indicate the approximate proportion of CPU
                  time spent in the callbacks.
    */
    double getCpuUsage() const;

    //==============================================================================
    /** Enables or disables a midi input device.

        The list of devices can be obtained with the MidiInput::getAvailableDevices() method.

        Any incoming messages from enabled input devices will be forwarded on to all the
        listeners that have been registered with the addMidiInputDeviceCallback() method. They
        can either register for messages from a particular device, or from just the "default"
        midi input.

        Routing the midi input via an AudioDeviceManager means that when a listener
        registers for the default midi input, this default device can be changed by the
        manager without the listeners having to know about it or re-register.

        It also means that a listener can stay registered for a midi input that is disabled
        or not present, so that when the input is re-enabled, the listener will start
        receiving messages again.

        @see addMidiInputDeviceCallback, isMidiInputDeviceEnabled
    */
    void setMidiInputDeviceEnabled (const String& deviceIdentifier, bool enabled);

    /** Returns true if a given midi input device is being used.

        @see setMidiInputDeviceEnabled
    */
    bool isMidiInputDeviceEnabled (const String& deviceIdentifier) const;

    /** Registers a listener for callbacks when midi events arrive from a midi input.

        The device identifier can be empty to indicate that it wants to receive all incoming
        events from all the enabled MIDI inputs. Or it can be the identifier of one of the
        MIDI input devices if it just wants the events from that device. (see
        MidiInput::getAvailableDevices() for the list of devices).

        Only devices which are enabled (see the setMidiInputDeviceEnabled() method) will have their
        events forwarded on to listeners.
    */
    void addMidiInputDeviceCallback (const String& deviceIdentifier,
                                     MidiInputCallback* callback);

    /** Removes a listener that was previously registered with addMidiInputDeviceCallback(). */
    void removeMidiInputDeviceCallback (const String& deviceIdentifier,
                                        MidiInputCallback* callback);

    //==============================================================================
    /** Sets a midi output device to use as the default.

        The list of devices can be obtained with the MidiOutput::getAvailableDevices() method.

        The specified device will be opened automatically and can be retrieved with the
        getDefaultMidiOutput() method.

        Pass in an empty string to deselect all devices. For the default device, you
        can use MidiOutput::getDefaultDevice().

        @see getDefaultMidiOutput, getDefaultMidiOutputIdentifier
    */
    void setDefaultMidiOutputDevice (const String& deviceIdentifier);

    /** Returns the name of the default midi output.

        @see setDefaultMidiOutputDevice, getDefaultMidiOutput
    */
    const String& getDefaultMidiOutputIdentifier() const noexcept   { return defaultMidiOutputDeviceInfo.identifier; }

    /** Returns the current default midi output device. If no device has been selected, or the
        device can't be opened, this will return nullptr.

        @see getDefaultMidiOutputIdentifier
    */
    MidiOutput* getDefaultMidiOutput() const noexcept               { return defaultMidiOutput.get(); }

    //==============================================================================
    /** Returns a list of the types of device supported. */
    const OwnedArray<AudioIODeviceType>& getAvailableDeviceTypes();

    /** Creates a list of available types.

        This will add a set of new AudioIODeviceType objects to the specified list, to
        represent each available types of device.

        You can override this if your app needs to do something specific, like avoid
        using DirectSound devices, etc.
    */
    virtual void createAudioDeviceTypes (OwnedArray<AudioIODeviceType>& types);

    /** Adds a new device type to the list of types. */
    void addAudioDeviceType (std::unique_ptr<AudioIODeviceType> newDeviceType);

    /** Removes a previously added device type from the manager. */
    void removeAudioDeviceType (AudioIODeviceType* deviceTypeToRemove);

    //==============================================================================
    /** Plays a beep through the current audio device.

        This is here to allow the audio setup UI panels to easily include a "test"
        button so that the user can check where the audio is coming from.
    */
    void playTestSound();

    //==============================================================================
    /**
        A simple reference-counted struct that holds a level-meter value that can be read
        using getCurrentLevel().

        This is used to ensure that the level processing code is only executed when something
        holds a reference to one of these objects and will be bypassed otherwise.

        @see getInputLevelGetter, getOutputLevelGetter
    */
    struct LevelMeter    : public ReferenceCountedObject
    {
        LevelMeter() noexcept;
        double getCurrentLevel() const noexcept;

        using Ptr = ReferenceCountedObjectPtr<LevelMeter>;

    private:
        friend class AudioDeviceManager;

        Atomic<float> level { 0 };
        void updateLevel (const float* const*, int numChannels, int numSamples) noexcept;
    };

    /** Returns a reference-counted object that can be used to get the current input level.

        You need to store this object locally to ensure that the reference count is incremented
        and decremented properly. The current input level value can be read using getCurrentLevel().
    */
    LevelMeter::Ptr getInputLevelGetter() noexcept          { return inputLevelGetter; }

    /** Returns a reference-counted object that can be used to get the current output level.

        You need to store this object locally to ensure that the reference count is incremented
        and decremented properly. The current output level value can be read using getCurrentLevel().
    */
    LevelMeter::Ptr getOutputLevelGetter() noexcept         { return outputLevelGetter; }

    //==============================================================================
    /** Returns the a lock that can be used to synchronise access to the audio callback.
        Obviously while this is locked, you're blocking the audio thread from running, so
        it must only be used for very brief periods when absolutely necessary.
    */
    CriticalSection& getAudioCallbackLock() noexcept        { return audioCallbackLock; }

    /** Returns the a lock that can be used to synchronise access to the midi callback.
        Obviously while this is locked, you're blocking the midi system from running, so
        it must only be used for very brief periods when absolutely necessary.
    */
    CriticalSection& getMidiCallbackLock() noexcept         { return midiCallbackLock; }

    //==============================================================================
    /** Returns the number of under- or over runs reported.

        This method will use the underlying device's native getXRunCount if it supports
        it. Otherwise it will estimate the number of under-/overruns by measuring the
        time it spent in the audio callback.
    */
    int getXRunCount() const noexcept;

    //==============================================================================
    /** Deprecated. */
    void setMidiInputEnabled (const String&, bool);
    /** Deprecated. */
    bool isMidiInputEnabled (const String&) const;
    /** Deprecated. */
    void addMidiInputCallback (const String&, MidiInputCallback*);
    /** Deprecated. */
    void removeMidiInputCallback (const String&, MidiInputCallback*);
    /** Deprecated. */
    void setDefaultMidiOutput (const String&);
    /** Deprecated. */
    const String& getDefaultMidiOutputName() const noexcept  { return defaultMidiOutputDeviceInfo.name; }

private:
    //==============================================================================
    OwnedArray<AudioIODeviceType> availableDeviceTypes;
    OwnedArray<AudioDeviceSetup> lastDeviceTypeConfigs;

    AudioDeviceSetup currentSetup;
    std::unique_ptr<AudioIODevice> currentAudioDevice;
    Array<AudioIODeviceCallback*> callbacks;
    int numInputChansNeeded = 0, numOutputChansNeeded = 2;
    String preferredDeviceName, currentDeviceType;
    std::unique_ptr<XmlElement> lastExplicitSettings;
    mutable bool listNeedsScanning = true;
    AudioBuffer<float> tempBuffer;

    struct MidiCallbackInfo
    {
        String deviceIdentifier;
        MidiInputCallback* callback;
    };

    Array<MidiDeviceInfo> midiDeviceInfosFromXml;
    std::vector<std::unique_ptr<MidiInput>> enabledMidiInputs;
    Array<MidiCallbackInfo> midiCallbacks;

    MidiDeviceInfo defaultMidiOutputDeviceInfo;
    std::unique_ptr<MidiOutput> defaultMidiOutput;
    CriticalSection audioCallbackLock, midiCallbackLock;

    std::unique_ptr<AudioBuffer<float>> testSound;
    int testSoundPosition = 0;

    AudioProcessLoadMeasurer loadMeasurer;

    LevelMeter::Ptr inputLevelGetter   { new LevelMeter() },
                    outputLevelGetter  { new LevelMeter() };

    //==============================================================================
    class CallbackHandler;
    std::unique_ptr<CallbackHandler> callbackHandler;

    void audioDeviceIOCallbackInt (const float** inputChannelData, int totalNumInputChannels,
                                   float** outputChannelData, int totalNumOutputChannels, int numSamples);
    void audioDeviceAboutToStartInt (AudioIODevice*);
    void audioDeviceStoppedInt();
    void audioDeviceErrorInt (const String&);
    void handleIncomingMidiMessageInt (MidiInput*, const MidiMessage&);
    void audioDeviceListChanged();

    String restartDevice (int blockSizeToUse, double sampleRateToUse,
                          const BigInteger& ins, const BigInteger& outs);
    void stopDevice();

    void updateXml();

    void createDeviceTypesIfNeeded();
    void scanDevicesIfNeeded();
    void deleteCurrentDevice();
    double chooseBestSampleRate (double preferred) const;
    int chooseBestBufferSize (int preferred) const;
    void insertDefaultDeviceNames (AudioDeviceSetup&) const;
    String initialiseDefault (const String& preferredDefaultDeviceName, const AudioDeviceSetup*);
    String initialiseFromXML (const XmlElement&, bool selectDefaultDeviceOnFailure,
                              const String& preferredDefaultDeviceName, const AudioDeviceSetup*);

    AudioIODeviceType* findType (const String& inputName, const String& outputName);
    AudioIODeviceType* findType (const String& typeName);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (AudioDeviceManager)
};

} // namespace juce
