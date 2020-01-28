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

AudioDeviceManager::AudioDeviceSetup::AudioDeviceSetup()
    : sampleRate (0),
      bufferSize (0),
      useDefaultInputChannels (true),
      useDefaultOutputChannels (true)
{
}

bool AudioDeviceManager::AudioDeviceSetup::operator== (const AudioDeviceManager::AudioDeviceSetup& other) const
{
    return outputDeviceName == other.outputDeviceName
            && inputDeviceName == other.inputDeviceName
            && sampleRate == other.sampleRate
            && bufferSize == other.bufferSize
            && inputChannels == other.inputChannels
            && useDefaultInputChannels == other.useDefaultInputChannels
            && outputChannels == other.outputChannels
            && useDefaultOutputChannels == other.useDefaultOutputChannels;
}

//==============================================================================
class AudioDeviceManager::CallbackHandler  : public AudioIODeviceCallback,
                                             public MidiInputCallback,
                                             public AudioIODeviceType::Listener
{
public:
    CallbackHandler (AudioDeviceManager& adm) noexcept  : owner (adm) {}

private:
    void audioDeviceIOCallback (const float** ins, int numIns, float** outs, int numOuts, int numSamples) override
    {
        owner.audioDeviceIOCallbackInt (ins, numIns, outs, numOuts, numSamples);
    }

    void audioDeviceAboutToStart (AudioIODevice* device) override
    {
        owner.audioDeviceAboutToStartInt (device);
    }

    void audioDeviceStopped() override
    {
        owner.audioDeviceStoppedInt();
    }

    void audioDeviceError (const String& message) override
    {
        owner.audioDeviceErrorInt (message);
    }

    void handleIncomingMidiMessage (MidiInput* source, const MidiMessage& message) override
    {
        owner.handleIncomingMidiMessageInt (source, message);
    }

    void audioDeviceListChanged() override
    {
        owner.audioDeviceListChanged();
    }

    AudioDeviceManager& owner;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CallbackHandler)
};

//==============================================================================
AudioDeviceManager::AudioDeviceManager()
    : numInputChansNeeded (0),
      numOutputChansNeeded (2),
      listNeedsScanning (true),
      testSoundPosition (0),
      cpuUsageMs (0),
      timeToCpuScale (0)
{
    callbackHandler = new CallbackHandler (*this);
}

AudioDeviceManager::~AudioDeviceManager()
{
    currentAudioDevice = nullptr;
    defaultMidiOutput = nullptr;
}

//==============================================================================
void AudioDeviceManager::createDeviceTypesIfNeeded()
{
    if (availableDeviceTypes.size() == 0)
    {
        OwnedArray<AudioIODeviceType> types;
        createAudioDeviceTypes (types);

        for (int i = 0; i < types.size(); ++i)
            addAudioDeviceType (types.getUnchecked(i));

        types.clear (false);

        if (AudioIODeviceType* first = availableDeviceTypes.getFirst())
            currentDeviceType = first->getTypeName();
    }
}

const OwnedArray<AudioIODeviceType>& AudioDeviceManager::getAvailableDeviceTypes()
{
    scanDevicesIfNeeded();
    return availableDeviceTypes;
}

void AudioDeviceManager::audioDeviceListChanged()
{
    if (currentAudioDevice != nullptr)
    {
        currentSetup.sampleRate     = currentAudioDevice->getCurrentSampleRate();
        currentSetup.bufferSize     = currentAudioDevice->getCurrentBufferSizeSamples();
        currentSetup.inputChannels  = currentAudioDevice->getActiveInputChannels();
        currentSetup.outputChannels = currentAudioDevice->getActiveOutputChannels();
    }

    sendChangeMessage();
}

//==============================================================================
static void addIfNotNull (OwnedArray<AudioIODeviceType>& list, AudioIODeviceType* const device)
{
    if (device != nullptr)
        list.add (device);
}

void AudioDeviceManager::createAudioDeviceTypes (OwnedArray<AudioIODeviceType>& list)
{
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_WASAPI (false));
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_WASAPI (true));
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_DirectSound());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_ASIO());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_CoreAudio());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_iOSAudio());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_ALSA());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_JACK());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_OpenSLES());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_Android());
}

void AudioDeviceManager::addAudioDeviceType (AudioIODeviceType* newDeviceType)
{
    if (newDeviceType != nullptr)
    {
        jassert (lastDeviceTypeConfigs.size() == availableDeviceTypes.size());
        availableDeviceTypes.add (newDeviceType);
        lastDeviceTypeConfigs.add (new AudioDeviceSetup());

        newDeviceType->addListener (callbackHandler);
    }
}

static bool deviceListContains (AudioIODeviceType* type, bool isInput, const String& name)
{
    StringArray devices (type->getDeviceNames (isInput));

    for (int i = devices.size(); --i >= 0;)
        if (devices[i].trim().equalsIgnoreCase (name.trim()))
            return true;

    return false;
}

//==============================================================================
String AudioDeviceManager::initialise (const int numInputChannelsNeeded,
                                       const int numOutputChannelsNeeded,
                                       const XmlElement* const xml,
                                       const bool selectDefaultDeviceOnFailure,
                                       const String& preferredDefaultDeviceName,
                                       const AudioDeviceSetup* preferredSetupOptions)
{
    scanDevicesIfNeeded();

    numInputChansNeeded = numInputChannelsNeeded;
    numOutputChansNeeded = numOutputChannelsNeeded;

    if (xml != nullptr && xml->hasTagName ("DEVICESETUP"))
        return initialiseFromXML (*xml, selectDefaultDeviceOnFailure,
                                  preferredDefaultDeviceName, preferredSetupOptions);

    return initialiseDefault (preferredDefaultDeviceName, preferredSetupOptions);
}

String AudioDeviceManager::initialiseDefault (const String& preferredDefaultDeviceName,
                                              const AudioDeviceSetup* preferredSetupOptions)
{
    AudioDeviceSetup setup;

    if (preferredSetupOptions != nullptr)
    {
        setup = *preferredSetupOptions;
    }
    else if (preferredDefaultDeviceName.isNotEmpty())
    {
        for (int j = availableDeviceTypes.size(); --j >= 0;)
        {
            AudioIODeviceType* const type = availableDeviceTypes.getUnchecked(j);

            const StringArray outs (type->getDeviceNames (false));

            for (int i = 0; i < outs.size(); ++i)
            {
                if (outs[i].matchesWildcard (preferredDefaultDeviceName, true))
                {
                    setup.outputDeviceName = outs[i];
                    break;
                }
            }

            const StringArray ins (type->getDeviceNames (true));

            for (int i = 0; i < ins.size(); ++i)
            {
                if (ins[i].matchesWildcard (preferredDefaultDeviceName, true))
                {
                    setup.inputDeviceName = ins[i];
                    break;
                }
            }
        }
    }

    insertDefaultDeviceNames (setup);
    return setAudioDeviceSetup (setup, false);
}

String AudioDeviceManager::initialiseFromXML (const XmlElement& xml,
                                              const bool selectDefaultDeviceOnFailure,
                                              const String& preferredDefaultDeviceName,
                                              const AudioDeviceSetup* preferredSetupOptions)
{
    lastExplicitSettings = new XmlElement (xml);

    String error;
    AudioDeviceSetup setup;

    if (preferredSetupOptions != nullptr)
        setup = *preferredSetupOptions;

    if (xml.getStringAttribute ("audioDeviceName").isNotEmpty())
    {
        setup.inputDeviceName = setup.outputDeviceName
            = xml.getStringAttribute ("audioDeviceName");
    }
    else
    {
        setup.inputDeviceName  = xml.getStringAttribute ("audioInputDeviceName");
        setup.outputDeviceName = xml.getStringAttribute ("audioOutputDeviceName");
    }

    currentDeviceType = xml.getStringAttribute ("deviceType");

    if (findType (currentDeviceType) == nullptr)
    {
        if (AudioIODeviceType* const type = findType (setup.inputDeviceName, setup.outputDeviceName))
            currentDeviceType = type->getTypeName();
        else if (availableDeviceTypes.size() > 0)
            currentDeviceType = availableDeviceTypes.getUnchecked(0)->getTypeName();
    }

    setup.bufferSize = xml.getIntAttribute ("audioDeviceBufferSize", setup.bufferSize);
    setup.sampleRate = xml.getDoubleAttribute ("audioDeviceRate", setup.sampleRate);

    setup.inputChannels .parseString (xml.getStringAttribute ("audioDeviceInChans",  "11"), 2);
    setup.outputChannels.parseString (xml.getStringAttribute ("audioDeviceOutChans", "11"), 2);

    setup.useDefaultInputChannels  = ! xml.hasAttribute ("audioDeviceInChans");
    setup.useDefaultOutputChannels = ! xml.hasAttribute ("audioDeviceOutChans");

    error = setAudioDeviceSetup (setup, true);

    midiInsFromXml.clear();

    forEachXmlChildElementWithTagName (xml, c, "MIDIINPUT")
        midiInsFromXml.add (c->getStringAttribute ("name"));

    const StringArray allMidiIns (MidiInput::getDevices());

    for (int i = allMidiIns.size(); --i >= 0;)
        setMidiInputEnabled (allMidiIns[i], midiInsFromXml.contains (allMidiIns[i]));

    if (error.isNotEmpty() && selectDefaultDeviceOnFailure)
        error = initialise (numInputChansNeeded, numOutputChansNeeded,
                            nullptr, false, preferredDefaultDeviceName);

    setDefaultMidiOutput (xml.getStringAttribute ("defaultMidiOutput"));

    return error;
}

String AudioDeviceManager::initialiseWithDefaultDevices (int numInputChannelsNeeded,
                                                         int numOutputChannelsNeeded)
{
    lastExplicitSettings = nullptr;

    return initialise (numInputChannelsNeeded, numOutputChannelsNeeded,
                       nullptr, false, String(), nullptr);
}

void AudioDeviceManager::insertDefaultDeviceNames (AudioDeviceSetup& setup) const
{
    if (AudioIODeviceType* type = getCurrentDeviceTypeObject())
    {
        if (setup.outputDeviceName.isEmpty())
            setup.outputDeviceName = type->getDeviceNames (false) [type->getDefaultDeviceIndex (false)];

        if (setup.inputDeviceName.isEmpty())
            setup.inputDeviceName = type->getDeviceNames (true) [type->getDefaultDeviceIndex (true)];
    }
}

XmlElement* AudioDeviceManager::createStateXml() const
{
    return lastExplicitSettings.createCopy();
}

//==============================================================================
void AudioDeviceManager::scanDevicesIfNeeded()
{
    if (listNeedsScanning)
    {
        listNeedsScanning = false;

        createDeviceTypesIfNeeded();

        for (int i = availableDeviceTypes.size(); --i >= 0;)
            availableDeviceTypes.getUnchecked(i)->scanForDevices();
    }
}

AudioIODeviceType* AudioDeviceManager::findType (const String& typeName)
{
    scanDevicesIfNeeded();

    for (int i = availableDeviceTypes.size(); --i >= 0;)
        if (availableDeviceTypes.getUnchecked(i)->getTypeName() == typeName)
            return availableDeviceTypes.getUnchecked(i);

    return nullptr;
}

AudioIODeviceType* AudioDeviceManager::findType (const String& inputName, const String& outputName)
{
    scanDevicesIfNeeded();

    for (int i = availableDeviceTypes.size(); --i >= 0;)
    {
        AudioIODeviceType* const type = availableDeviceTypes.getUnchecked(i);

        if ((inputName.isNotEmpty() && deviceListContains (type, true, inputName))
             || (outputName.isNotEmpty() && deviceListContains (type, false, outputName)))
        {
            return type;
        }
    }

    return nullptr;
}

void AudioDeviceManager::getAudioDeviceSetup (AudioDeviceSetup& setup) const
{
    setup = currentSetup;
}

void AudioDeviceManager::deleteCurrentDevice()
{
    currentAudioDevice = nullptr;
    currentSetup.inputDeviceName.clear();
    currentSetup.outputDeviceName.clear();
}

void AudioDeviceManager::setCurrentAudioDeviceType (const String& type,
                                                    const bool treatAsChosenDevice)
{
    for (int i = 0; i < availableDeviceTypes.size(); ++i)
    {
        if (availableDeviceTypes.getUnchecked(i)->getTypeName() == type
             && currentDeviceType != type)
        {
            if (currentAudioDevice != nullptr)
            {
                closeAudioDevice();
                Thread::sleep (1500); // allow a moment for OS devices to sort themselves out, to help
                                      // avoid things like DirectSound/ASIO clashes
            }

            currentDeviceType = type;

            AudioDeviceSetup s (*lastDeviceTypeConfigs.getUnchecked(i));
            insertDefaultDeviceNames (s);

            setAudioDeviceSetup (s, treatAsChosenDevice);

            sendChangeMessage();
            break;
        }
    }
}

AudioIODeviceType* AudioDeviceManager::getCurrentDeviceTypeObject() const
{
    for (int i = 0; i < availableDeviceTypes.size(); ++i)
        if (availableDeviceTypes.getUnchecked(i)->getTypeName() == currentDeviceType)
            return availableDeviceTypes.getUnchecked(i);

    return availableDeviceTypes[0];
}

String AudioDeviceManager::setAudioDeviceSetup (const AudioDeviceSetup& newSetup,
                                                const bool treatAsChosenDevice)
{
    jassert (&newSetup != &currentSetup);    // this will have no effect

    if (newSetup == currentSetup && currentAudioDevice != nullptr)
        return {};

    if (! (newSetup == currentSetup))
        sendChangeMessage();

    stopDevice();

    const String newInputDeviceName  (numInputChansNeeded  == 0 ? String() : newSetup.inputDeviceName);
    const String newOutputDeviceName (numOutputChansNeeded == 0 ? String() : newSetup.outputDeviceName);

    String error;
    AudioIODeviceType* type = getCurrentDeviceTypeObject();

    if (type == nullptr || (newInputDeviceName.isEmpty() && newOutputDeviceName.isEmpty()))
    {
        deleteCurrentDevice();

        if (treatAsChosenDevice)
            updateXml();

        return {};
    }

    if (currentSetup.inputDeviceName != newInputDeviceName
         || currentSetup.outputDeviceName != newOutputDeviceName
         || currentAudioDevice == nullptr)
    {
        deleteCurrentDevice();
        scanDevicesIfNeeded();

        if (newOutputDeviceName.isNotEmpty() && ! deviceListContains (type, false, newOutputDeviceName))
            return "No such device: " + newOutputDeviceName;

        if (newInputDeviceName.isNotEmpty() && ! deviceListContains (type, true, newInputDeviceName))
            return "No such device: " + newInputDeviceName;

        currentAudioDevice = type->createDevice (newOutputDeviceName, newInputDeviceName);

        if (currentAudioDevice == nullptr)
            error = "Can't open the audio device!\n\n"
                    "This may be because another application is currently using the same device - "
                    "if so, you should close any other applications and try again!";
        else
            error = currentAudioDevice->getLastError();

        if (error.isNotEmpty())
        {
            deleteCurrentDevice();
            return error;
        }

        if (newSetup.useDefaultInputChannels)
        {
            inputChannels.clear();
            inputChannels.setRange (0, numInputChansNeeded, true);
        }

        if (newSetup.useDefaultOutputChannels)
        {
            outputChannels.clear();
            outputChannels.setRange (0, numOutputChansNeeded, true);
        }

        if (newInputDeviceName.isEmpty())  inputChannels.clear();
        if (newOutputDeviceName.isEmpty()) outputChannels.clear();
    }

    if (! newSetup.useDefaultInputChannels)    inputChannels  = newSetup.inputChannels;
    if (! newSetup.useDefaultOutputChannels)   outputChannels = newSetup.outputChannels;

    currentSetup = newSetup;

    currentSetup.sampleRate = chooseBestSampleRate (newSetup.sampleRate);
    currentSetup.bufferSize = chooseBestBufferSize (newSetup.bufferSize);

    error = currentAudioDevice->open (inputChannels,
                                      outputChannels,
                                      currentSetup.sampleRate,
                                      currentSetup.bufferSize);

    if (error.isEmpty())
    {
        currentDeviceType = currentAudioDevice->getTypeName();

        currentAudioDevice->start (callbackHandler);

        currentSetup.sampleRate     = currentAudioDevice->getCurrentSampleRate();
        currentSetup.bufferSize     = currentAudioDevice->getCurrentBufferSizeSamples();
        currentSetup.inputChannels  = currentAudioDevice->getActiveInputChannels();
        currentSetup.outputChannels = currentAudioDevice->getActiveOutputChannels();

        for (int i = 0; i < availableDeviceTypes.size(); ++i)
            if (availableDeviceTypes.getUnchecked (i)->getTypeName() == currentDeviceType)
                *(lastDeviceTypeConfigs.getUnchecked (i)) = currentSetup;

        if (treatAsChosenDevice)
            updateXml();
    }
    else
    {
        deleteCurrentDevice();
    }

    return error;
}

double AudioDeviceManager::chooseBestSampleRate (double rate) const
{
    jassert (currentAudioDevice != nullptr);

    const Array<double> rates (currentAudioDevice->getAvailableSampleRates());

    if (rate > 0 && rates.contains (rate))
        return rate;

    rate = currentAudioDevice->getCurrentSampleRate();

    if (rate > 0 && rates.contains (rate))
        return rate;

    double lowestAbove44 = 0.0;

    for (int i = rates.size(); --i >= 0;)
    {
        const double sr = rates[i];

        if (sr >= 44100.0 && (lowestAbove44 < 1.0 || sr < lowestAbove44))
            lowestAbove44 = sr;
    }

    if (lowestAbove44 > 0.0)
        return lowestAbove44;

    return rates[0];
}

int AudioDeviceManager::chooseBestBufferSize (int bufferSize) const
{
    jassert (currentAudioDevice != nullptr);

    if (bufferSize > 0 && currentAudioDevice->getAvailableBufferSizes().contains (bufferSize))
        return bufferSize;

    return currentAudioDevice->getDefaultBufferSize();
}

void AudioDeviceManager::stopDevice()
{
    if (currentAudioDevice != nullptr)
        currentAudioDevice->stop();

    testSound = nullptr;
}

void AudioDeviceManager::closeAudioDevice()
{
    stopDevice();
    currentAudioDevice = nullptr;
}

void AudioDeviceManager::restartLastAudioDevice()
{
    if (currentAudioDevice == nullptr)
    {
        if (currentSetup.inputDeviceName.isEmpty()
              && currentSetup.outputDeviceName.isEmpty())
        {
            // This method will only reload the last device that was running
            // before closeAudioDevice() was called - you need to actually open
            // one first, with setAudioDevice().
            jassertfalse;
            return;
        }

        AudioDeviceSetup s (currentSetup);
        setAudioDeviceSetup (s, false);
    }
}

void AudioDeviceManager::updateXml()
{
    lastExplicitSettings = new XmlElement ("DEVICESETUP");

    lastExplicitSettings->setAttribute ("deviceType", currentDeviceType);
    lastExplicitSettings->setAttribute ("audioOutputDeviceName", currentSetup.outputDeviceName);
    lastExplicitSettings->setAttribute ("audioInputDeviceName", currentSetup.inputDeviceName);

    if (currentAudioDevice != nullptr)
    {
        lastExplicitSettings->setAttribute ("audioDeviceRate", currentAudioDevice->getCurrentSampleRate());

        if (currentAudioDevice->getDefaultBufferSize() != currentAudioDevice->getCurrentBufferSizeSamples())
            lastExplicitSettings->setAttribute ("audioDeviceBufferSize", currentAudioDevice->getCurrentBufferSizeSamples());

        if (! currentSetup.useDefaultInputChannels)
            lastExplicitSettings->setAttribute ("audioDeviceInChans", currentSetup.inputChannels.toString (2));

        if (! currentSetup.useDefaultOutputChannels)
            lastExplicitSettings->setAttribute ("audioDeviceOutChans", currentSetup.outputChannels.toString (2));
    }

    for (int i = 0; i < enabledMidiInputs.size(); ++i)
        lastExplicitSettings->createNewChildElement ("MIDIINPUT")
                            ->setAttribute ("name", enabledMidiInputs[i]->getName());

    if (midiInsFromXml.size() > 0)
    {
        // Add any midi devices that have been enabled before, but which aren't currently
        // open because the device has been disconnected.
        const StringArray availableMidiDevices (MidiInput::getDevices());

        for (int i = 0; i < midiInsFromXml.size(); ++i)
            if (! availableMidiDevices.contains (midiInsFromXml[i], true))
                lastExplicitSettings->createNewChildElement ("MIDIINPUT")
                                    ->setAttribute ("name", midiInsFromXml[i]);
    }

    if (defaultMidiOutputName.isNotEmpty())
        lastExplicitSettings->setAttribute ("defaultMidiOutput", defaultMidiOutputName);
}

//==============================================================================
void AudioDeviceManager::addAudioCallback (AudioIODeviceCallback* newCallback)
{
    {
        const ScopedLock sl (audioCallbackLock);
        if (callbacks.contains (newCallback))
            return;
    }

    if (currentAudioDevice != nullptr && newCallback != nullptr)
        newCallback->audioDeviceAboutToStart (currentAudioDevice);

    const ScopedLock sl (audioCallbackLock);
    callbacks.add (newCallback);
}

void AudioDeviceManager::removeAudioCallback (AudioIODeviceCallback* callbackToRemove)
{
    if (callbackToRemove != nullptr)
    {
        bool needsDeinitialising = currentAudioDevice != nullptr;

        {
            const ScopedLock sl (audioCallbackLock);

            needsDeinitialising = needsDeinitialising && callbacks.contains (callbackToRemove);
            callbacks.removeFirstMatchingValue (callbackToRemove);
        }

        if (needsDeinitialising)
            callbackToRemove->audioDeviceStopped();
    }
}

void AudioDeviceManager::audioDeviceIOCallbackInt (const float** inputChannelData,
                                                   int numInputChannels,
                                                   float** outputChannelData,
                                                   int numOutputChannels,
                                                   int numSamples)
{
    const ScopedLock sl (audioCallbackLock);

    inputLevelMeter.updateLevel (inputChannelData, numInputChannels, numSamples);
    outputLevelMeter.updateLevel (const_cast<const float**> (outputChannelData), numOutputChannels, numSamples);

    if (callbacks.size() > 0)
    {
        const double callbackStartTime = Time::getMillisecondCounterHiRes();

        tempBuffer.setSize (jmax (1, numOutputChannels), jmax (1, numSamples), false, false, true);

        callbacks.getUnchecked(0)->audioDeviceIOCallback (inputChannelData, numInputChannels,
                                                          outputChannelData, numOutputChannels, numSamples);

        float** const tempChans = tempBuffer.getArrayOfWritePointers();

        for (int i = callbacks.size(); --i > 0;)
        {
            callbacks.getUnchecked(i)->audioDeviceIOCallback (inputChannelData, numInputChannels,
                                                              tempChans, numOutputChannels, numSamples);

            for (int chan = 0; chan < numOutputChannels; ++chan)
            {
                if (const float* const src = tempChans [chan])
                    if (float* const dst = outputChannelData [chan])
                        for (int j = 0; j < numSamples; ++j)
                            dst[j] += src[j];
            }
        }

        const double msTaken = Time::getMillisecondCounterHiRes() - callbackStartTime;
        const double filterAmount = 0.2;
        cpuUsageMs += filterAmount * (msTaken - cpuUsageMs);

        if (msTaken > msPerBlock)
            xruns++;
    }
    else
    {
        for (int i = 0; i < numOutputChannels; ++i)
            zeromem (outputChannelData[i], sizeof (float) * (size_t) numSamples);
    }

    if (testSound != nullptr)
    {
        const int numSamps = jmin (numSamples, testSound->getNumSamples() - testSoundPosition);
        const float* const src = testSound->getReadPointer (0, testSoundPosition);

        for (int i = 0; i < numOutputChannels; ++i)
            for (int j = 0; j < numSamps; ++j)
                outputChannelData [i][j] += src[j];

        testSoundPosition += numSamps;
        if (testSoundPosition >= testSound->getNumSamples())
            testSound = nullptr;
    }
}

void AudioDeviceManager::audioDeviceAboutToStartInt (AudioIODevice* const device)
{
    cpuUsageMs = 0;
    xruns = 0;

    const double sampleRate = device->getCurrentSampleRate();
    const int blockSize = device->getCurrentBufferSizeSamples();

    if (sampleRate > 0.0 && blockSize > 0)
    {
        msPerBlock = 1000.0 * blockSize / sampleRate;
        timeToCpuScale = (msPerBlock > 0.0) ? (1.0 / msPerBlock) : 0.0;
    }

    {
        const ScopedLock sl (audioCallbackLock);
        for (int i = callbacks.size(); --i >= 0;)
            callbacks.getUnchecked(i)->audioDeviceAboutToStart (device);
    }

    sendChangeMessage();
}

void AudioDeviceManager::audioDeviceStoppedInt()
{
    cpuUsageMs = 0;
    timeToCpuScale = 0;
    xruns = 0;
    sendChangeMessage();

    const ScopedLock sl (audioCallbackLock);
    for (int i = callbacks.size(); --i >= 0;)
        callbacks.getUnchecked(i)->audioDeviceStopped();
}

void AudioDeviceManager::audioDeviceErrorInt (const String& message)
{
    const ScopedLock sl (audioCallbackLock);
    for (int i = callbacks.size(); --i >= 0;)
        callbacks.getUnchecked(i)->audioDeviceError (message);
}

double AudioDeviceManager::getCpuUsage() const
{
    return jlimit (0.0, 1.0, timeToCpuScale * cpuUsageMs);
}

//==============================================================================
void AudioDeviceManager::setMidiInputEnabled (const String& name, const bool enabled)
{
    if (enabled != isMidiInputEnabled (name))
    {
        if (enabled)
        {
            const int index = MidiInput::getDevices().indexOf (name);

            if (index >= 0)
            {
                if (MidiInput* const midiIn = MidiInput::openDevice (index, callbackHandler))
                {
                    enabledMidiInputs.add (midiIn);
                    midiIn->start();
                }
            }
        }
        else
        {
            for (int i = enabledMidiInputs.size(); --i >= 0;)
                if (enabledMidiInputs[i]->getName() == name)
                    enabledMidiInputs.remove (i);
        }

        updateXml();
        sendChangeMessage();
    }
}

bool AudioDeviceManager::isMidiInputEnabled (const String& name) const
{
    for (int i = enabledMidiInputs.size(); --i >= 0;)
        if (enabledMidiInputs[i]->getName() == name)
            return true;

    return false;
}

void AudioDeviceManager::addMidiInputCallback (const String& name, MidiInputCallback* callbackToAdd)
{
    removeMidiInputCallback (name, callbackToAdd);

    if (name.isEmpty() || isMidiInputEnabled (name))
    {
        const ScopedLock sl (midiCallbackLock);

        MidiCallbackInfo mc;
        mc.deviceName = name;
        mc.callback = callbackToAdd;
        midiCallbacks.add (mc);
    }
}

void AudioDeviceManager::removeMidiInputCallback (const String& name, MidiInputCallback* callbackToRemove)
{
    for (int i = midiCallbacks.size(); --i >= 0;)
    {
        const MidiCallbackInfo& mc = midiCallbacks.getReference(i);

        if (mc.callback == callbackToRemove && mc.deviceName == name)
        {
            const ScopedLock sl (midiCallbackLock);
            midiCallbacks.remove (i);
        }
    }
}

void AudioDeviceManager::handleIncomingMidiMessageInt (MidiInput* source, const MidiMessage& message)
{
    if (! message.isActiveSense())
    {
        const ScopedLock sl (midiCallbackLock);

        for (int i = 0; i < midiCallbacks.size(); ++i)
        {
            const MidiCallbackInfo& mc = midiCallbacks.getReference(i);

            if (mc.deviceName.isEmpty() || mc.deviceName == source->getName())
                mc.callback->handleIncomingMidiMessage (source, message);
        }
    }
}

//==============================================================================
void AudioDeviceManager::setDefaultMidiOutput (const String& deviceName)
{
    if (defaultMidiOutputName != deviceName)
    {
        Array<AudioIODeviceCallback*> oldCallbacks;

        {
            const ScopedLock sl (audioCallbackLock);
            oldCallbacks.swapWith (callbacks);
        }

        if (currentAudioDevice != nullptr)
            for (int i = oldCallbacks.size(); --i >= 0;)
                oldCallbacks.getUnchecked(i)->audioDeviceStopped();

        defaultMidiOutput = nullptr;
        defaultMidiOutputName = deviceName;

        if (deviceName.isNotEmpty())
            defaultMidiOutput = MidiOutput::openDevice (MidiOutput::getDevices().indexOf (deviceName));

        if (currentAudioDevice != nullptr)
            for (int i = oldCallbacks.size(); --i >= 0;)
                oldCallbacks.getUnchecked(i)->audioDeviceAboutToStart (currentAudioDevice);

        {
            const ScopedLock sl (audioCallbackLock);
            oldCallbacks.swapWith (callbacks);
        }

        updateXml();
        sendChangeMessage();
    }
}

//==============================================================================
AudioDeviceManager::LevelMeter::LevelMeter() noexcept : level() {}

void AudioDeviceManager::LevelMeter::updateLevel (const float* const* channelData, int numChannels, int numSamples) noexcept
{
    if (enabled.get() != 0 && numChannels > 0)
    {
        for (int j = 0; j < numSamples; ++j)
        {
            float s = 0;

            for (int i = 0; i < numChannels; ++i)
                s += std::abs (channelData[i][j]);

            s /= (float) numChannels;

            const double decayFactor = 0.99992;

            if (s > level)
                level = s;
            else if (level > 0.001f)
                level *= decayFactor;
            else
                level = 0;
        }
    }
    else
    {
        level = 0;
    }
}

void AudioDeviceManager::LevelMeter::setEnabled (bool shouldBeEnabled) noexcept
{
    enabled.set (shouldBeEnabled ? 1 : 0);
    level = 0;
}

double AudioDeviceManager::LevelMeter::getCurrentLevel() const noexcept
{
    jassert (enabled.get() != 0); // you need to call setEnabled (true) before using this!
    return level;
}

void AudioDeviceManager::playTestSound()
{
    { // cunningly nested to swap, unlock and delete in that order.
        ScopedPointer<AudioSampleBuffer> oldSound;

        {
            const ScopedLock sl (audioCallbackLock);
            oldSound = testSound;
        }
    }

    testSoundPosition = 0;

    if (currentAudioDevice != nullptr)
    {
        const double sampleRate = currentAudioDevice->getCurrentSampleRate();
        const int soundLength = (int) sampleRate;

        const double frequency = 440.0;
        const float amplitude = 0.5f;

        const double phasePerSample = double_Pi * 2.0 / (sampleRate / frequency);

        AudioSampleBuffer* const newSound = new AudioSampleBuffer (1, soundLength);

        for (int i = 0; i < soundLength; ++i)
            newSound->setSample (0, i, amplitude * (float) std::sin (i * phasePerSample));

        newSound->applyGainRamp (0, 0, soundLength / 10, 0.0f, 1.0f);
        newSound->applyGainRamp (0, soundLength - soundLength / 4, soundLength / 4, 1.0f, 0.0f);

        const ScopedLock sl (audioCallbackLock);
        testSound = newSound;
    }
}

int AudioDeviceManager::getXRunCount() const noexcept
{
    auto deviceXRuns = (currentAudioDevice != nullptr ? currentAudioDevice->getXRunCount() : -1);
    return (deviceXRuns >= 0 ? deviceXRuns : xruns);
}

double AudioDeviceManager::getCurrentInputLevel() const noexcept    { return inputLevelMeter.getCurrentLevel(); }
double AudioDeviceManager::getCurrentOutputLevel() const noexcept   { return outputLevelMeter.getCurrentLevel(); }

void AudioDeviceManager::enableInputLevelMeasurement  (bool enable) noexcept  { inputLevelMeter.setEnabled (enable); }
void AudioDeviceManager::enableOutputLevelMeasurement (bool enable) noexcept  { outputLevelMeter.setEnabled (enable); }

} // namespace juce
