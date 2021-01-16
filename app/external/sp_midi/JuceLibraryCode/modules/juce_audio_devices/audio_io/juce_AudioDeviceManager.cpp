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

bool AudioDeviceManager::AudioDeviceSetup::operator!= (const AudioDeviceManager::AudioDeviceSetup& other) const
{
    return ! operator== (other);
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
{
    callbackHandler.reset (new CallbackHandler (*this));
}

AudioDeviceManager::~AudioDeviceManager()
{
    currentAudioDevice.reset();
    defaultMidiOutput.reset();
}

//==============================================================================
void AudioDeviceManager::createDeviceTypesIfNeeded()
{
    if (availableDeviceTypes.size() == 0)
    {
        OwnedArray<AudioIODeviceType> types;
        createAudioDeviceTypes (types);

        for (auto* t : types)
            addAudioDeviceType (std::unique_ptr<AudioIODeviceType> (t));

        types.clear (false);

        if (auto* first = availableDeviceTypes.getFirst())
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
        auto currentDeviceStillAvailable = [&]
        {
            auto currentTypeName = currentAudioDevice->getTypeName();
            auto currentDeviceName = currentAudioDevice->getName();

            for (auto* deviceType : availableDeviceTypes)
            {
                if (currentTypeName == deviceType->getTypeName())
                {
                    for (auto& deviceName : deviceType->getDeviceNames (true))
                        if (currentDeviceName == deviceName)
                            return true;

                    for (auto& deviceName : deviceType->getDeviceNames (false))
                        if (currentDeviceName == deviceName)
                            return true;
                }
            }

            return false;
        }();

        if (! currentDeviceStillAvailable)
        {
            closeAudioDevice();

            if (auto e = createStateXml())
                initialiseFromXML (*e, true, preferredDeviceName, &currentSetup);
            else
                initialiseDefault (preferredDeviceName, &currentSetup);
        }

        if (currentAudioDevice != nullptr)
        {
            currentSetup.sampleRate     = currentAudioDevice->getCurrentSampleRate();
            currentSetup.bufferSize     = currentAudioDevice->getCurrentBufferSizeSamples();
            currentSetup.inputChannels  = currentAudioDevice->getActiveInputChannels();
            currentSetup.outputChannels = currentAudioDevice->getActiveOutputChannels();
        }
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
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_Bela());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_ALSA());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_JACK());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_Oboe());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_OpenSLES());
    addIfNotNull (list, AudioIODeviceType::createAudioIODeviceType_Android());
}

void AudioDeviceManager::addAudioDeviceType (std::unique_ptr<AudioIODeviceType> newDeviceType)
{
    if (newDeviceType != nullptr)
    {
        jassert (lastDeviceTypeConfigs.size() == availableDeviceTypes.size());

        availableDeviceTypes.add (newDeviceType.release());
        lastDeviceTypeConfigs.add (new AudioDeviceSetup());

        availableDeviceTypes.getLast()->addListener (callbackHandler.get());
    }
}

void AudioDeviceManager::removeAudioDeviceType (AudioIODeviceType* deviceTypeToRemove)
{
    if (deviceTypeToRemove != nullptr)
    {
        jassert (lastDeviceTypeConfigs.size() == availableDeviceTypes.size());

        auto index = availableDeviceTypes.indexOf (deviceTypeToRemove);

        if (auto removed = std::unique_ptr<AudioIODeviceType> (availableDeviceTypes.removeAndReturn (index)))
        {
            removed->removeListener (callbackHandler.get());
            lastDeviceTypeConfigs.remove (index, true);
        }
    }
}

static bool deviceListContains (AudioIODeviceType* type, bool isInput, const String& name)
{
    for (auto& deviceName : type->getDeviceNames (isInput))
        if (deviceName.trim().equalsIgnoreCase (name.trim()))
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
    preferredDeviceName = preferredDefaultDeviceName;

    if (xml != nullptr && xml->hasTagName ("DEVICESETUP"))
        return initialiseFromXML (*xml, selectDefaultDeviceOnFailure,
                                  preferredDeviceName, preferredSetupOptions);

    return initialiseDefault (preferredDeviceName, preferredSetupOptions);
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
        for (auto* type : availableDeviceTypes)
        {
            for (auto& out : type->getDeviceNames (false))
            {
                if (out.matchesWildcard (preferredDefaultDeviceName, true))
                {
                    setup.outputDeviceName = out;
                    break;
                }
            }

            for (auto& in : type->getDeviceNames (true))
            {
                if (in.matchesWildcard (preferredDefaultDeviceName, true))
                {
                    setup.inputDeviceName = in;
                    break;
                }
            }
        }
    }

    insertDefaultDeviceNames (setup);
    return setAudioDeviceSetup (setup, false);
}

String AudioDeviceManager::initialiseFromXML (const XmlElement& xml,
                                              bool selectDefaultDeviceOnFailure,
                                              const String& preferredDefaultDeviceName,
                                              const AudioDeviceSetup* preferredSetupOptions)
{
    lastExplicitSettings.reset (new XmlElement (xml));

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
        if (auto* type = findType (setup.inputDeviceName, setup.outputDeviceName))
            currentDeviceType = type->getTypeName();
        else if (auto* firstType = availableDeviceTypes.getFirst())
            currentDeviceType = firstType->getTypeName();
    }

    setup.bufferSize = xml.getIntAttribute ("audioDeviceBufferSize", setup.bufferSize);
    setup.sampleRate = xml.getDoubleAttribute ("audioDeviceRate", setup.sampleRate);

    setup.inputChannels .parseString (xml.getStringAttribute ("audioDeviceInChans",  "11"), 2);
    setup.outputChannels.parseString (xml.getStringAttribute ("audioDeviceOutChans", "11"), 2);

    setup.useDefaultInputChannels  = ! xml.hasAttribute ("audioDeviceInChans");
    setup.useDefaultOutputChannels = ! xml.hasAttribute ("audioDeviceOutChans");

    error = setAudioDeviceSetup (setup, true);

    if (error.isNotEmpty() && selectDefaultDeviceOnFailure)
        error = initialise (numInputChansNeeded, numOutputChansNeeded, nullptr, false, preferredDefaultDeviceName);

    midiDeviceInfosFromXml.clear();
    enabledMidiInputs.clear();

    forEachXmlChildElementWithTagName (xml, c, "MIDIINPUT")
        midiDeviceInfosFromXml.add ({ c->getStringAttribute ("name"), c->getStringAttribute ("identifier") });

    auto isIdentifierAvailable = [] (const Array<MidiDeviceInfo>& available, const String& identifier)
    {
        for (auto& device : available)
            if (device.identifier == identifier)
                return true;

        return false;
    };

    auto getUpdatedIdentifierForName = [&] (const Array<MidiDeviceInfo>& available, const String& name) -> String
    {
        for (auto& device : available)
            if (device.name == name)
                return device.identifier;

        return {};
    };

    auto inputs = MidiInput::getAvailableDevices();

    for (auto& info : midiDeviceInfosFromXml)
    {
        if (isIdentifierAvailable (inputs, info.identifier))
        {
            setMidiInputDeviceEnabled (info.identifier, true);
        }
        else
        {
            auto identifier = getUpdatedIdentifierForName (inputs, info.name);

            if (identifier.isNotEmpty())
                setMidiInputDeviceEnabled (identifier, true);
        }
    }

    MidiDeviceInfo defaultOutputDeviceInfo (xml.getStringAttribute ("defaultMidiOutput"),
                                            xml.getStringAttribute ("defaultMidiOutputDevice"));

    auto outputs = MidiOutput::getAvailableDevices();

    if (isIdentifierAvailable (outputs, defaultOutputDeviceInfo.identifier))
    {
        setDefaultMidiOutputDevice (defaultOutputDeviceInfo.identifier);
    }
    else
    {
        auto identifier = getUpdatedIdentifierForName (outputs, defaultOutputDeviceInfo.name);

        if (identifier.isNotEmpty())
            setDefaultMidiOutputDevice (identifier);
    }

    return error;
}

String AudioDeviceManager::initialiseWithDefaultDevices (int numInputChannelsNeeded,
                                                         int numOutputChannelsNeeded)
{
    lastExplicitSettings.reset();

    return initialise (numInputChannelsNeeded, numOutputChannelsNeeded,
                       nullptr, false, {}, nullptr);
}

void AudioDeviceManager::insertDefaultDeviceNames (AudioDeviceSetup& setup) const
{
    if (auto* type = getCurrentDeviceTypeObject())
    {
        if (numOutputChansNeeded > 0 && setup.outputDeviceName.isEmpty())
            setup.outputDeviceName = type->getDeviceNames (false) [type->getDefaultDeviceIndex (false)];

        if (numInputChansNeeded > 0 && setup.inputDeviceName.isEmpty())
            setup.inputDeviceName = type->getDeviceNames (true) [type->getDefaultDeviceIndex (true)];
    }
}

std::unique_ptr<XmlElement> AudioDeviceManager::createStateXml() const
{
    if (lastExplicitSettings != nullptr)
        return std::make_unique<XmlElement> (*lastExplicitSettings);

    return {};
}

//==============================================================================
void AudioDeviceManager::scanDevicesIfNeeded()
{
    if (listNeedsScanning)
    {
        listNeedsScanning = false;

        createDeviceTypesIfNeeded();

        for (auto* type : availableDeviceTypes)
            type->scanForDevices();
    }
}

AudioIODeviceType* AudioDeviceManager::findType (const String& typeName)
{
    scanDevicesIfNeeded();

    for (auto* type : availableDeviceTypes)
        if (type->getTypeName() == typeName)
            return type;

    return {};
}

AudioIODeviceType* AudioDeviceManager::findType (const String& inputName, const String& outputName)
{
    scanDevicesIfNeeded();

    for (auto* type : availableDeviceTypes)
        if ((inputName.isNotEmpty() && deviceListContains (type, true, inputName))
             || (outputName.isNotEmpty() && deviceListContains (type, false, outputName)))
            return type;

    return {};
}

AudioDeviceManager::AudioDeviceSetup AudioDeviceManager::getAudioDeviceSetup() const
{
    return currentSetup;
}

void AudioDeviceManager::getAudioDeviceSetup (AudioDeviceSetup& setup) const
{
    setup = currentSetup;
}

void AudioDeviceManager::deleteCurrentDevice()
{
    currentAudioDevice.reset();
    currentSetup.inputDeviceName.clear();
    currentSetup.outputDeviceName.clear();
}

void AudioDeviceManager::setCurrentAudioDeviceType (const String& type, bool treatAsChosenDevice)
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
    for (auto* type : availableDeviceTypes)
        if (type->getTypeName() == currentDeviceType)
            return type;

    return availableDeviceTypes.getFirst();
}

static void updateSetupChannels (AudioDeviceManager::AudioDeviceSetup& setup, int defaultNumIns, int defaultNumOuts)
{
    auto updateChannels = [] (const String& deviceName, BigInteger& channels, int defaultNumChannels)
    {
        if (deviceName.isEmpty())
        {
            channels.clear();
        }
        else if (defaultNumChannels != -1)
        {
            channels.clear();
            channels.setRange (0, defaultNumChannels, true);
        }
    };

    updateChannels (setup.inputDeviceName,  setup.inputChannels,  setup.useDefaultInputChannels  ? defaultNumIns  : -1);
    updateChannels (setup.outputDeviceName, setup.outputChannels, setup.useDefaultOutputChannels ? defaultNumOuts : -1);
}

String AudioDeviceManager::setAudioDeviceSetup (const AudioDeviceSetup& newSetup,
                                                bool treatAsChosenDevice)
{
    jassert (&newSetup != &currentSetup);    // this will have no effect

    if (newSetup != currentSetup)
        sendChangeMessage();
    else if (currentAudioDevice != nullptr)
        return {};

    if (getCurrentDeviceTypeObject() == nullptr
        || (newSetup.inputDeviceName.isEmpty() && newSetup.outputDeviceName.isEmpty()))
    {
        deleteCurrentDevice();

        if (treatAsChosenDevice)
            updateXml();

        return {};
    }

    stopDevice();

    String error;

    if (currentSetup.inputDeviceName  != newSetup.inputDeviceName
     || currentSetup.outputDeviceName != newSetup.outputDeviceName
     || currentAudioDevice == nullptr)
    {
        deleteCurrentDevice();
        scanDevicesIfNeeded();

        auto* type = getCurrentDeviceTypeObject();

        if (newSetup.outputDeviceName.isNotEmpty() && ! deviceListContains (type, false, newSetup.outputDeviceName))
            return "No such device: " + newSetup.outputDeviceName;

        if (newSetup.inputDeviceName.isNotEmpty() && ! deviceListContains (type, true, newSetup.inputDeviceName))
            return "No such device: " + newSetup.inputDeviceName;

        currentAudioDevice.reset (type->createDevice (newSetup.outputDeviceName, newSetup.inputDeviceName));

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
    }

    currentSetup = newSetup;

    if (! currentSetup.useDefaultInputChannels)    numInputChansNeeded  = currentSetup.inputChannels.countNumberOfSetBits();
    if (! currentSetup.useDefaultOutputChannels)   numOutputChansNeeded = currentSetup.outputChannels.countNumberOfSetBits();

    updateSetupChannels (currentSetup, numInputChansNeeded, numOutputChansNeeded);

    if (currentSetup.inputChannels.isZero() && currentSetup.outputChannels.isZero())
    {
        if (treatAsChosenDevice)
            updateXml();

        return {};
    }

    currentSetup.sampleRate = chooseBestSampleRate (currentSetup.sampleRate);
    currentSetup.bufferSize = chooseBestBufferSize (currentSetup.bufferSize);

    error = currentAudioDevice->open (currentSetup.inputChannels,
                                      currentSetup.outputChannels,
                                      currentSetup.sampleRate,
                                      currentSetup.bufferSize);

    if (error.isEmpty())
    {
        currentDeviceType = currentAudioDevice->getTypeName();

        currentAudioDevice->start (callbackHandler.get());

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

    auto rates = currentAudioDevice->getAvailableSampleRates();

    if (rate > 0 && rates.contains (rate))
        return rate;

    rate = currentAudioDevice->getCurrentSampleRate();

    if (rate > 0 && rates.contains (rate))
        return rate;

    double lowestAbove44 = 0.0;

    for (int i = rates.size(); --i >= 0;)
    {
        auto sr = rates[i];

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

    testSound.reset();
}

void AudioDeviceManager::closeAudioDevice()
{
    stopDevice();
    currentAudioDevice.reset();
    loadMeasurer.reset();
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
    lastExplicitSettings.reset (new XmlElement ("DEVICESETUP"));

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

    for (auto& input : enabledMidiInputs)
    {
        auto* child = lastExplicitSettings->createNewChildElement ("MIDIINPUT");

        child->setAttribute ("name",       input->getName());
        child->setAttribute ("identifier", input->getIdentifier());
    }

    if (midiDeviceInfosFromXml.size() > 0)
    {
        // Add any midi devices that have been enabled before, but which aren't currently
        // open because the device has been disconnected.
        auto availableMidiDevices = MidiInput::getAvailableDevices();

        for (auto& d : midiDeviceInfosFromXml)
        {
            if (! availableMidiDevices.contains (d))
            {
                auto* child = lastExplicitSettings->createNewChildElement ("MIDIINPUT");

                child->setAttribute ("name",       d.name);
                child->setAttribute ("identifier", d.identifier);
            }
        }
    }

    if (defaultMidiOutputDeviceInfo != MidiDeviceInfo())
    {
        lastExplicitSettings->setAttribute ("defaultMidiOutput",       defaultMidiOutputDeviceInfo.name);
        lastExplicitSettings->setAttribute ("defaultMidiOutputDevice", defaultMidiOutputDeviceInfo.identifier);
    }
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
        newCallback->audioDeviceAboutToStart (currentAudioDevice.get());

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

    inputLevelGetter->updateLevel (inputChannelData, numInputChannels, numSamples);
    outputLevelGetter->updateLevel (const_cast<const float**> (outputChannelData), numOutputChannels, numSamples);

    if (callbacks.size() > 0)
    {
        AudioProcessLoadMeasurer::ScopedTimer timer (loadMeasurer);

        tempBuffer.setSize (jmax (1, numOutputChannels), jmax (1, numSamples), false, false, true);

        callbacks.getUnchecked(0)->audioDeviceIOCallback (inputChannelData, numInputChannels,
                                                          outputChannelData, numOutputChannels, numSamples);

        auto** tempChans = tempBuffer.getArrayOfWritePointers();

        for (int i = callbacks.size(); --i > 0;)
        {
            callbacks.getUnchecked(i)->audioDeviceIOCallback (inputChannelData, numInputChannels,
                                                              tempChans, numOutputChannels, numSamples);

            for (int chan = 0; chan < numOutputChannels; ++chan)
            {
                if (auto* src = tempChans [chan])
                    if (auto* dst = outputChannelData [chan])
                        for (int j = 0; j < numSamples; ++j)
                            dst[j] += src[j];
            }
        }
    }
    else
    {
        for (int i = 0; i < numOutputChannels; ++i)
            zeromem (outputChannelData[i], (size_t) numSamples * sizeof (float));
    }

    if (testSound != nullptr)
    {
        auto numSamps = jmin (numSamples, testSound->getNumSamples() - testSoundPosition);
        auto* src = testSound->getReadPointer (0, testSoundPosition);

        for (int i = 0; i < numOutputChannels; ++i)
            for (int j = 0; j < numSamps; ++j)
                outputChannelData [i][j] += src[j];

        testSoundPosition += numSamps;

        if (testSoundPosition >= testSound->getNumSamples())
            testSound.reset();
    }
}

void AudioDeviceManager::audioDeviceAboutToStartInt (AudioIODevice* const device)
{
    loadMeasurer.reset (device->getCurrentSampleRate(),
                        device->getCurrentBufferSizeSamples());

    {
        const ScopedLock sl (audioCallbackLock);

        for (int i = callbacks.size(); --i >= 0;)
            callbacks.getUnchecked(i)->audioDeviceAboutToStart (device);
    }

    sendChangeMessage();
}

void AudioDeviceManager::audioDeviceStoppedInt()
{
    sendChangeMessage();

    const ScopedLock sl (audioCallbackLock);

    loadMeasurer.reset();

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
    return loadMeasurer.getLoadAsProportion();
}

//==============================================================================
void AudioDeviceManager::setMidiInputDeviceEnabled (const String& identifier, bool enabled)
{
    if (enabled != isMidiInputDeviceEnabled (identifier))
    {
        if (enabled)
        {
            if (auto midiIn = MidiInput::openDevice (identifier, callbackHandler.get()))
            {
                enabledMidiInputs.push_back (std::move (midiIn));
                enabledMidiInputs.back()->start();
            }
        }
        else
        {
            auto removePredicate = [identifier] (const std::unique_ptr<MidiInput>& in) { return in->getIdentifier() == identifier; };
            enabledMidiInputs.erase (std::remove_if (std::begin (enabledMidiInputs), std::end (enabledMidiInputs), removePredicate),
                                     std::end (enabledMidiInputs));
        }

        updateXml();
        sendChangeMessage();
    }
}

bool AudioDeviceManager::isMidiInputDeviceEnabled (const String& identifier) const
{
    for (auto& mi : enabledMidiInputs)
        if (mi->getIdentifier() == identifier)
            return true;

    return false;
}

void AudioDeviceManager::addMidiInputDeviceCallback (const String& identifier, MidiInputCallback* callbackToAdd)
{
    removeMidiInputDeviceCallback (identifier, callbackToAdd);

    if (identifier.isEmpty() || isMidiInputDeviceEnabled (identifier))
    {
        const ScopedLock sl (midiCallbackLock);
        midiCallbacks.add ({ identifier, callbackToAdd });
    }
}

void AudioDeviceManager::removeMidiInputDeviceCallback (const String& identifier, MidiInputCallback* callbackToRemove)
{
    for (int i = midiCallbacks.size(); --i >= 0;)
    {
        auto& mc = midiCallbacks.getReference (i);

        if (mc.callback == callbackToRemove && mc.deviceIdentifier == identifier)
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

        for (auto& mc : midiCallbacks)
            if (mc.deviceIdentifier.isEmpty() || mc.deviceIdentifier == source->getIdentifier())
                mc.callback->handleIncomingMidiMessage (source, message);
    }
}

//==============================================================================
void AudioDeviceManager::setDefaultMidiOutputDevice (const String& identifier)
{
    if (defaultMidiOutputDeviceInfo.identifier != identifier)
    {
        Array<AudioIODeviceCallback*> oldCallbacks;

        {
            const ScopedLock sl (audioCallbackLock);
            oldCallbacks.swapWith (callbacks);
        }

        if (currentAudioDevice != nullptr)
            for (int i = oldCallbacks.size(); --i >= 0;)
                oldCallbacks.getUnchecked (i)->audioDeviceStopped();

        defaultMidiOutput.reset();

        if (identifier.isNotEmpty())
            defaultMidiOutput = MidiOutput::openDevice (identifier);

        if (defaultMidiOutput != nullptr)
            defaultMidiOutputDeviceInfo = defaultMidiOutput->getDeviceInfo();
        else
            defaultMidiOutputDeviceInfo = {};

        if (currentAudioDevice != nullptr)
            for (auto* c : oldCallbacks)
                c->audioDeviceAboutToStart (currentAudioDevice.get());

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
    if (getReferenceCount() <= 1)
        return;

    auto localLevel = level.get();

    if (numChannels > 0)
    {
        for (int j = 0; j < numSamples; ++j)
        {
            float s = 0;

            for (int i = 0; i < numChannels; ++i)
                s += std::abs (channelData[i][j]);

            s /= (float) numChannels;

            const float decayFactor = 0.99992f;

            if (s > localLevel)
                localLevel = s;
            else if (localLevel > 0.001f)
                localLevel *= decayFactor;
            else
                localLevel = 0;
        }
    }
    else
    {
        localLevel = 0;
    }

    level = localLevel;
}

double AudioDeviceManager::LevelMeter::getCurrentLevel() const noexcept
{
    jassert (getReferenceCount() > 1);
    return level.get();
}

void AudioDeviceManager::playTestSound()
{
    { // cunningly nested to swap, unlock and delete in that order.
        std::unique_ptr<AudioBuffer<float>> oldSound;

        {
            const ScopedLock sl (audioCallbackLock);
            std::swap (oldSound, testSound);
        }
    }

    testSoundPosition = 0;

    if (currentAudioDevice != nullptr)
    {
        auto sampleRate = currentAudioDevice->getCurrentSampleRate();
        auto soundLength = (int) sampleRate;

        double frequency = 440.0;
        float amplitude = 0.5f;

        auto phasePerSample = MathConstants<double>::twoPi / (sampleRate / frequency);

        std::unique_ptr<AudioBuffer<float>> newSound (new AudioBuffer<float> (1, soundLength));

        for (int i = 0; i < soundLength; ++i)
            newSound->setSample (0, i, amplitude * (float) std::sin (i * phasePerSample));

        newSound->applyGainRamp (0, 0, soundLength / 10, 0.0f, 1.0f);
        newSound->applyGainRamp (0, soundLength - soundLength / 4, soundLength / 4, 1.0f, 0.0f);

        {
            const ScopedLock sl (audioCallbackLock);
            std::swap (testSound, newSound);
        }
    }
}

int AudioDeviceManager::getXRunCount() const noexcept
{
    auto deviceXRuns = (currentAudioDevice != nullptr ? currentAudioDevice->getXRunCount() : -1);
    return jmax (0, deviceXRuns) + loadMeasurer.getXRunCount();
}

//==============================================================================
// Deprecated
void AudioDeviceManager::setMidiInputEnabled (const String& name, const bool enabled)
{
    for (auto& device : MidiInput::getAvailableDevices())
    {
        if (device.name == name)
        {
            setMidiInputDeviceEnabled (device.identifier, enabled);
            return;
        }
    }
}

bool AudioDeviceManager::isMidiInputEnabled (const String& name) const
{
    for (auto& device : MidiInput::getAvailableDevices())
        if (device.name == name)
            return isMidiInputDeviceEnabled (device.identifier);

    return false;
}

void AudioDeviceManager::addMidiInputCallback (const String& name, MidiInputCallback* callbackToAdd)
{
    if (name.isEmpty())
    {
        addMidiInputDeviceCallback ({}, callbackToAdd);
    }
    else
    {
        for (auto& device : MidiInput::getAvailableDevices())
        {
            if (device.name == name)
            {
                addMidiInputDeviceCallback (device.identifier, callbackToAdd);
                return;
            }
        }
    }
}

void AudioDeviceManager::removeMidiInputCallback (const String& name, MidiInputCallback* callbackToRemove)
{
    for (auto& device : MidiInput::getAvailableDevices())
    {
        if (device.name == name)
        {
            removeMidiInputDeviceCallback (device.identifier, callbackToRemove);
            return;
        }
    }
}

void AudioDeviceManager::setDefaultMidiOutput (const String& name)
{
    for (auto& device : MidiOutput::getAvailableDevices())
    {
        if (device.name == name)
        {
            setDefaultMidiOutputDevice (device.identifier);
            return;
        }
    }
}

} // namespace juce
