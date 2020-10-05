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

#ifndef JUCE_LOG_COREMIDI_ERRORS
 #define JUCE_LOG_COREMIDI_ERRORS 1
#endif

namespace CoreMidiHelpers
{
    //==============================================================================
    static bool checkError (OSStatus err, int lineNum)
    {
        if (err == noErr)
            return true;

       #if JUCE_LOG_COREMIDI_ERRORS
        Logger::writeToLog ("CoreMIDI error: " + String (lineNum) + " - " + String::toHexString ((int) err));
       #endif

        ignoreUnused (lineNum);
        return false;
    }

    #undef CHECK_ERROR
    #define CHECK_ERROR(a) CoreMidiHelpers::checkError (a, __LINE__)

    static MidiDeviceInfo getMidiObjectInfo (MIDIObjectRef entity)
    {
        MidiDeviceInfo info;

        {
            ScopedCFString str;

            if (CHECK_ERROR (MIDIObjectGetStringProperty (entity, kMIDIPropertyName, &str.cfString)))
                info.name = String::fromCFString (str.cfString);
        }

        SInt32 objectID = 0;

        if (CHECK_ERROR (MIDIObjectGetIntegerProperty (entity, kMIDIPropertyUniqueID, &objectID)))
        {
            info.identifier = String (objectID);
        }
        else
        {
            ScopedCFString str;

            if (CHECK_ERROR (MIDIObjectGetStringProperty (entity, kMIDIPropertyUniqueID, &str.cfString)))
                info.identifier = String::fromCFString (str.cfString);
        }

        return info;
    }

    static MidiDeviceInfo getEndpointInfo (MIDIEndpointRef endpoint, bool isExternal)
    {
        // NB: don't attempt to use nullptr for refs - it fails in some types of build.
        MIDIEntityRef entity = 0;
        MIDIEndpointGetEntity (endpoint, &entity);

        // probably virtual
        if (entity == 0)
            return getMidiObjectInfo (endpoint);

        auto result = getMidiObjectInfo (endpoint);

        // endpoint is empty - try the entity
        if (result == MidiDeviceInfo())
            result = getMidiObjectInfo (entity);

        // now consider the device
        MIDIDeviceRef device = 0;
        MIDIEntityGetDevice (entity, &device);

        if (device != 0)
        {
            auto info = getMidiObjectInfo (device);

            if (info != MidiDeviceInfo())
            {
                // if an external device has only one entity, throw away
                // the endpoint name and just use the device name
                if (isExternal && MIDIDeviceGetNumberOfEntities (device) < 2)
                {
                    result = info;
                }
                else if (! result.name.startsWithIgnoreCase (info.name))
                {
                    // prepend the device name and identifier to the entity's
                    result.name = (info.name + " " + result.name).trimEnd();
                    result.identifier = info.identifier + " " + result.identifier;
                }
            }
        }

        return result;
    }

    static MidiDeviceInfo getConnectedEndpointInfo (MIDIEndpointRef endpoint)
    {
        MidiDeviceInfo result;

        // Does the endpoint have connections?
        CFDataRef connections = nullptr;
        int numConnections = 0;

        MIDIObjectGetDataProperty (endpoint, kMIDIPropertyConnectionUniqueID, &connections);

        if (connections != nullptr)
        {
            numConnections = ((int) CFDataGetLength (connections)) / (int) sizeof (MIDIUniqueID);

            if (numConnections > 0)
            {
                auto* pid = reinterpret_cast<const SInt32*> (CFDataGetBytePtr (connections));

                for (int i = 0; i < numConnections; ++i, ++pid)
                {
                    auto id = (MIDIUniqueID) ByteOrder::swapIfLittleEndian ((uint32) *pid);
                    MIDIObjectRef connObject;
                    MIDIObjectType connObjectType;
                    auto err = MIDIObjectFindByUniqueID (id, &connObject, &connObjectType);

                    if (err == noErr)
                    {
                        MidiDeviceInfo deviceInfo;

                        if (connObjectType == kMIDIObjectType_ExternalSource
                             || connObjectType == kMIDIObjectType_ExternalDestination)
                        {
                            // Connected to an external device's endpoint (10.3 and later).
                            deviceInfo = getEndpointInfo (static_cast<MIDIEndpointRef> (connObject), true);
                        }
                        else
                        {
                            // Connected to an external device (10.2) (or something else, catch-all)
                            deviceInfo = getMidiObjectInfo (connObject);
                        }

                        if (deviceInfo != MidiDeviceInfo())
                        {
                            if (result.name.isNotEmpty())        result.name += ", ";
                            if (result.identifier.isNotEmpty())  result.identifier += ", ";

                            result.name       += deviceInfo.name;
                            result.identifier += deviceInfo.identifier;
                        }
                    }
                }
            }

            CFRelease (connections);
        }

        // Here, either the endpoint had no connections, or we failed to obtain names for them.
        if (result == MidiDeviceInfo())
            return getEndpointInfo (endpoint, false);

        return result;
    }

    static int createUniqueIDForMidiPort (String deviceName, bool isInput)
    {
        String uniqueID;

       #ifdef JucePlugin_CFBundleIdentifier
        uniqueID = JUCE_STRINGIFY (JucePlugin_CFBundleIdentifier);
       #else
        auto appBundle = File::getSpecialLocation (File::currentApplicationFile);
        ScopedCFString appBundlePath (appBundle.getFullPathName());

        if (auto bundleURL = CFURLCreateWithFileSystemPath (kCFAllocatorDefault, appBundlePath.cfString, kCFURLPOSIXPathStyle, true))
        {
            auto bundleRef = CFBundleCreate (kCFAllocatorDefault, bundleURL);
            CFRelease (bundleURL);

            if (bundleRef != nullptr)
            {
                if (auto bundleId = CFBundleGetIdentifier (bundleRef))
                    uniqueID = String::fromCFString (bundleId);

                CFRelease (bundleRef);
            }
        }
       #endif

        if (uniqueID.isEmpty())
            uniqueID = String (Random::getSystemRandom().nextInt (1024));

        uniqueID += "." + deviceName + (isInput ? ".input" : ".output");
        return uniqueID.hashCode();
    }

    static void enableSimulatorMidiSession()
    {
       #if TARGET_OS_SIMULATOR
        static bool hasEnabledNetworkSession = false;

        if (! hasEnabledNetworkSession)
        {
            MIDINetworkSession* session = [MIDINetworkSession defaultSession];
            session.enabled = YES;
            session.connectionPolicy = MIDINetworkConnectionPolicy_Anyone;

            hasEnabledNetworkSession = true;
        }
       #endif
    }

    static void globalSystemChangeCallback (const MIDINotification*, void*)
    {
        // TODO.. Should pass-on this notification..
    }

    static String getGlobalMidiClientName()
    {
        if (auto* app = JUCEApplicationBase::getInstance())
            return app->getApplicationName();

        return "JUCE";
    }

    static MIDIClientRef getGlobalMidiClient()
    {
        static MIDIClientRef globalMidiClient = 0;

        if (globalMidiClient == 0)
        {
            // Since OSX 10.6, the MIDIClientCreate function will only work
            // correctly when called from the message thread!
            JUCE_ASSERT_MESSAGE_THREAD

            enableSimulatorMidiSession();

            ScopedCFString name (getGlobalMidiClientName());
            CHECK_ERROR (MIDIClientCreate (name.cfString, &globalSystemChangeCallback, nullptr, &globalMidiClient));
        }

        return globalMidiClient;
    }

    static Array<MidiDeviceInfo> findDevices (bool forInput)
    {
        // It seems that OSX can be a bit picky about the thread that's first used to
        // search for devices. It's safest to use the message thread for calling this.
        JUCE_ASSERT_MESSAGE_THREAD

        if (getGlobalMidiClient() == 0)
        {
            jassertfalse;
            return {};
        }

        enableSimulatorMidiSession();

        Array<MidiDeviceInfo> devices;
        auto numDevices = (forInput ? MIDIGetNumberOfSources() : MIDIGetNumberOfDestinations());

        for (ItemCount i = 0; i < numDevices; ++i)
        {
            MidiDeviceInfo deviceInfo;

            if (auto dest = forInput ? MIDIGetSource (i) : MIDIGetDestination (i))
                deviceInfo = getConnectedEndpointInfo (dest);

            if (deviceInfo == MidiDeviceInfo())
                deviceInfo.name = deviceInfo.identifier = "<error>";

            devices.add (deviceInfo);
        }

        return devices;
    }

    //==============================================================================
    class MidiPortAndEndpoint
    {
    public:
        MidiPortAndEndpoint (MIDIPortRef p, MIDIEndpointRef ep) noexcept
            : port (p), endpoint (ep)
        {
        }

        ~MidiPortAndEndpoint() noexcept
        {
            if (port != 0)
                MIDIPortDispose (port);

            // if port == nullptr, it means we created the endpoint, so it's safe to delete it
            if (port == 0 && endpoint != 0)
                MIDIEndpointDispose (endpoint);
        }

        void send (const MIDIPacketList* packets) noexcept
        {
            if (port != 0)
                MIDISend (port, endpoint, packets);
            else
                MIDIReceived (endpoint, packets);
        }

        MIDIPortRef port;
        MIDIEndpointRef endpoint;
    };

    //==============================================================================
    struct MidiPortAndCallback;
    CriticalSection callbackLock;
    Array<MidiPortAndCallback*> activeCallbacks;

    struct MidiPortAndCallback
    {
        MidiPortAndCallback (MidiInputCallback& cb)  : callback (cb) {}

        ~MidiPortAndCallback()
        {
            active = false;

            {
                const ScopedLock sl (callbackLock);
                activeCallbacks.removeFirstMatchingValue (this);
            }

            if (portAndEndpoint != nullptr && portAndEndpoint->port != 0)
                CHECK_ERROR (MIDIPortDisconnectSource (portAndEndpoint->port, portAndEndpoint->endpoint));
        }

        void handlePackets (const MIDIPacketList* pktlist)
        {
            auto time = Time::getMillisecondCounterHiRes() * 0.001;

            const ScopedLock sl (callbackLock);

            if (activeCallbacks.contains (this) && active)
            {
                auto* packet = &pktlist->packet[0];

                for (unsigned int i = 0; i < pktlist->numPackets; ++i)
                {
                    auto len = readUnaligned<decltype (packet->length)> (&(packet->length));
                    concatenator.pushMidiData (packet->data, (int) len, time, input, callback);

                    packet = MIDIPacketNext (packet);
                }
            }
        }

        MidiInput* input = nullptr;
        std::unique_ptr<MidiPortAndEndpoint> portAndEndpoint;
        std::atomic<bool> active { false };

    private:
        MidiInputCallback& callback;
        MidiDataConcatenator concatenator { 2048 };
    };

    static void midiInputProc (const MIDIPacketList* pktlist, void* readProcRefCon, void* /*srcConnRefCon*/)
    {
        static_cast<MidiPortAndCallback*> (readProcRefCon)->handlePackets (pktlist);
    }

    static Array<MIDIEndpointRef> getEndpoints (bool isInput)
    {
        Array<MIDIEndpointRef> endpoints;
        auto numDevices = (isInput ? MIDIGetNumberOfSources() : MIDIGetNumberOfDestinations());

        for (ItemCount i = 0; i < numDevices; ++i)
            endpoints.add (isInput ? MIDIGetSource (i) : MIDIGetDestination (i));

        return endpoints;
    }
}

//==============================================================================
Array<MidiDeviceInfo> MidiInput::getAvailableDevices()
{
    return CoreMidiHelpers::findDevices (true);
}

MidiDeviceInfo MidiInput::getDefaultDevice()
{
    return getAvailableDevices().getFirst();
}

std::unique_ptr<MidiInput> MidiInput::openDevice (const String& deviceIdentifier, MidiInputCallback* callback)
{
    if (deviceIdentifier.isEmpty())
        return nullptr;

    using namespace CoreMidiHelpers;

    if (auto client = getGlobalMidiClient())
    {
        for (auto& endpoint : getEndpoints (true))
        {
            auto endpointInfo = getConnectedEndpointInfo (endpoint);

            if (deviceIdentifier == endpointInfo.identifier)
            {
                ScopedCFString cfName;

                if (CHECK_ERROR (MIDIObjectGetStringProperty (endpoint, kMIDIPropertyName, &cfName.cfString)))
                {
                    MIDIPortRef port;
                    auto mpc = std::make_unique<MidiPortAndCallback> (*callback);

                    if (CHECK_ERROR (MIDIInputPortCreate (client, cfName.cfString, midiInputProc, mpc.get(), &port)))
                    {
                        if (CHECK_ERROR (MIDIPortConnectSource (port, endpoint, nullptr)))
                        {
                            mpc->portAndEndpoint = std::make_unique<MidiPortAndEndpoint> (port, endpoint);

                            std::unique_ptr<MidiInput> midiInput (new MidiInput (endpointInfo.name, endpointInfo.identifier));

                            mpc->input = midiInput.get();
                            midiInput->internal = mpc.get();

                            const ScopedLock sl (callbackLock);
                            activeCallbacks.add (mpc.release());

                            return midiInput;
                        }
                        else
                        {
                            CHECK_ERROR (MIDIPortDispose (port));
                        }
                    }
                }
            }
        }
    }

    return {};
}

std::unique_ptr<MidiInput> MidiInput::createNewDevice (const String& deviceName, MidiInputCallback* callback)
{
    using namespace CoreMidiHelpers;
    jassert (callback != nullptr);

    if (auto client = getGlobalMidiClient())
    {
        auto mpc = std::make_unique<MidiPortAndCallback> (*callback);
        mpc->active = false;

        MIDIEndpointRef endpoint;
        ScopedCFString name (deviceName);

        auto err = MIDIDestinationCreate (client, name.cfString, midiInputProc, mpc.get(), &endpoint);

       #if JUCE_IOS
        if (err == kMIDINotPermitted)
        {
            // If you've hit this assertion then you probably haven't enabled the "Audio Background Capability"
            // setting in the iOS exporter for your app - this is required if you want to create a MIDI device!
            jassertfalse;
            return nullptr;
        }
       #endif

        if (CHECK_ERROR (err))
        {
            auto deviceIdentifier = createUniqueIDForMidiPort (deviceName, true);

            if (CHECK_ERROR (MIDIObjectSetIntegerProperty (endpoint, kMIDIPropertyUniqueID, (SInt32) deviceIdentifier)))
            {
                mpc->portAndEndpoint = std::make_unique<MidiPortAndEndpoint> ((UInt32) 0, endpoint);

                std::unique_ptr<MidiInput> midiInput (new MidiInput (deviceName, String (deviceIdentifier)));

                mpc->input = midiInput.get();
                midiInput->internal = mpc.get();

                const ScopedLock sl (callbackLock);
                activeCallbacks.add (mpc.release());

                return midiInput;
            }
        }
    }

    return {};
}

StringArray MidiInput::getDevices()
{
    StringArray deviceNames;

    for (auto& d : getAvailableDevices())
        deviceNames.add (d.name);

    return deviceNames;
}

int MidiInput::getDefaultDeviceIndex()
{
    return 0;
}

std::unique_ptr<MidiInput> MidiInput::openDevice (int index, MidiInputCallback* callback)
{
    return openDevice (getAvailableDevices()[index].identifier, callback);
}

MidiInput::MidiInput (const String& deviceName, const String& deviceIdentifier)
    : deviceInfo (deviceName, deviceIdentifier)
{
}

MidiInput::~MidiInput()
{
    delete static_cast<CoreMidiHelpers::MidiPortAndCallback*> (internal);
}

void MidiInput::start()
{
    const ScopedLock sl (CoreMidiHelpers::callbackLock);
    static_cast<CoreMidiHelpers::MidiPortAndCallback*> (internal)->active = true;
}

void MidiInput::stop()
{
    const ScopedLock sl (CoreMidiHelpers::callbackLock);
    static_cast<CoreMidiHelpers::MidiPortAndCallback*> (internal)->active = false;
}

//==============================================================================
Array<MidiDeviceInfo> MidiOutput::getAvailableDevices()
{
    return CoreMidiHelpers::findDevices (false);
}

MidiDeviceInfo MidiOutput::getDefaultDevice()
{
    return getAvailableDevices().getFirst();
}

std::unique_ptr<MidiOutput> MidiOutput::openDevice (const String& deviceIdentifier)
{
    if (deviceIdentifier.isEmpty())
        return nullptr;

    using namespace CoreMidiHelpers;

    if (auto client = getGlobalMidiClient())
    {
        for (auto& endpoint : getEndpoints (false))
        {
            auto endpointInfo = getConnectedEndpointInfo (endpoint);

            if (deviceIdentifier == endpointInfo.identifier)
            {
                ScopedCFString cfName;

                if (CHECK_ERROR (MIDIObjectGetStringProperty (endpoint, kMIDIPropertyName, &cfName.cfString)))
                {
                    MIDIPortRef port;

                    if (CHECK_ERROR (MIDIOutputPortCreate (client, cfName.cfString, &port)))
                    {
                        std::unique_ptr<MidiOutput> midiOutput (new MidiOutput (endpointInfo.name, endpointInfo.identifier));
                        midiOutput->internal = new MidiPortAndEndpoint (port, endpoint);

                        return midiOutput;
                    }
                }
            }
        }
    }

    return {};
}

std::unique_ptr<MidiOutput> MidiOutput::createNewDevice (const String& deviceName)
{
    using namespace CoreMidiHelpers;

    if (auto client = getGlobalMidiClient())
    {
        MIDIEndpointRef endpoint;

        ScopedCFString name (deviceName);

        auto err = MIDISourceCreate (client, name.cfString, &endpoint);

       #if JUCE_IOS
        if (err == kMIDINotPermitted)
        {
            // If you've hit this assertion then you probably haven't enabled the "Audio Background Capability"
            // setting in the iOS exporter for your app - this is required if you want to create a MIDI device!
            jassertfalse;
            return nullptr;
        }
       #endif

        if (CHECK_ERROR (err))
        {
            auto deviceIdentifier = createUniqueIDForMidiPort (deviceName, false);

            if (CHECK_ERROR (MIDIObjectSetIntegerProperty (endpoint, kMIDIPropertyUniqueID, (SInt32) deviceIdentifier)))
            {
                std::unique_ptr<MidiOutput> midiOutput (new MidiOutput (deviceName, String (deviceIdentifier)));
                midiOutput->internal = new MidiPortAndEndpoint (0, endpoint);

                return midiOutput;
            }
        }
    }

    return {};
}

StringArray MidiOutput::getDevices()
{
    StringArray deviceNames;

    for (auto& d : getAvailableDevices())
        deviceNames.add (d.name);

    return deviceNames;
}

int MidiOutput::getDefaultDeviceIndex()
{
    return 0;
}

std::unique_ptr<MidiOutput> MidiOutput::openDevice (int index)
{
    return openDevice (getAvailableDevices()[index].identifier);
}

MidiOutput::~MidiOutput()
{
    stopBackgroundThread();

    delete static_cast<CoreMidiHelpers::MidiPortAndEndpoint*> (internal);
}

void MidiOutput::sendMessageNow (const MidiMessage& message)
{
   #if JUCE_IOS
    const MIDITimeStamp timeStamp = mach_absolute_time();
   #else
    const MIDITimeStamp timeStamp = AudioGetCurrentHostTime();
   #endif

    HeapBlock<MIDIPacketList> allocatedPackets;
    MIDIPacketList stackPacket;
    auto* packetToSend = &stackPacket;
    auto dataSize = (size_t) message.getRawDataSize();

    if (message.isSysEx())
    {
        const int maxPacketSize = 256;
        int pos = 0, bytesLeft = (int) dataSize;
        const int numPackets = (bytesLeft + maxPacketSize - 1) / maxPacketSize;
        allocatedPackets.malloc ((size_t) (32 * (size_t) numPackets + dataSize), 1);
        packetToSend = allocatedPackets;
        packetToSend->numPackets = (UInt32) numPackets;

        auto* p = packetToSend->packet;

        for (int i = 0; i < numPackets; ++i)
        {
            p->timeStamp = timeStamp;
            p->length = (UInt16) jmin (maxPacketSize, bytesLeft);
            memcpy (p->data, message.getRawData() + pos, p->length);
            pos += p->length;
            bytesLeft -= p->length;
            p = MIDIPacketNext (p);
        }
    }
    else if (dataSize < 65536) // max packet size
    {
        auto stackCapacity = sizeof (stackPacket.packet->data);

        if (dataSize > stackCapacity)
        {
            allocatedPackets.malloc ((sizeof (MIDIPacketList) - stackCapacity) + dataSize, 1);
            packetToSend = allocatedPackets;
        }

        packetToSend->numPackets = 1;
        auto& p = *(packetToSend->packet);
        p.timeStamp = timeStamp;
        p.length = (UInt16) dataSize;
        memcpy (p.data, message.getRawData(), dataSize);
    }
    else
    {
        jassertfalse; // packet too large to send!
        return;
    }

    static_cast<CoreMidiHelpers::MidiPortAndEndpoint*> (internal)->send (packetToSend);
}

#undef CHECK_ERROR

} // namespace juce
