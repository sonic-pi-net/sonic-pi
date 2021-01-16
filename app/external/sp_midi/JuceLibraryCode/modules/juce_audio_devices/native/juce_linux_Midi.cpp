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

#if JUCE_ALSA

//==============================================================================
namespace
{

//==============================================================================
class AlsaClient  : public ReferenceCountedObject
{
public:
    AlsaClient()
    {
        jassert (instance == nullptr);

        snd_seq_open (&handle, "default", SND_SEQ_OPEN_DUPLEX, 0);

        if (handle != nullptr)
        {
            snd_seq_nonblock (handle, SND_SEQ_NONBLOCK);
            snd_seq_set_client_name (handle, getAlsaMidiName().toRawUTF8());
            clientId = snd_seq_client_id (handle);

            // It's good idea to pre-allocate a good number of elements
            ports.ensureStorageAllocated (32);
        }
    }

    ~AlsaClient()
    {
        jassert (instance != nullptr);
        instance = nullptr;

        if (handle != nullptr)
            snd_seq_close (handle);

        jassert (activeCallbacks.get() == 0);

        if (inputThread)
            inputThread->stopThread (3000);
    }

    static String getAlsaMidiName()
    {
        #ifdef JUCE_ALSA_MIDI_NAME
         return JUCE_ALSA_MIDI_NAME;
        #else
         if (auto* app = JUCEApplicationBase::getInstance())
             return app->getApplicationName();

         return "JUCE";
        #endif
    }

    using Ptr = ReferenceCountedObjectPtr<AlsaClient>;

    //==============================================================================
    // represents an input or output port of the supplied AlsaClient
    struct Port
    {
        Port (AlsaClient& c, bool forInput) noexcept
            : client (c), isInput (forInput)
        {}

        ~Port()
        {
            if (isValid())
            {
                if (isInput)
                    enableCallback (false);
                else
                    snd_midi_event_free (midiParser);

                snd_seq_delete_simple_port (client.get(), portId);
            }
        }

        void connectWith (int sourceClient, int sourcePort) const noexcept
        {
            if (isInput)
                snd_seq_connect_from (client.get(), portId, sourceClient, sourcePort);
            else
                snd_seq_connect_to (client.get(), portId, sourceClient, sourcePort);
        }

        bool isValid() const noexcept
        {
            return client.get() != nullptr && portId >= 0;
        }

        void setupInput (MidiInput* input, MidiInputCallback* cb)
        {
            jassert (cb != nullptr && input != nullptr);
            callback = cb;
            midiInput = input;
        }

        void setupOutput()
        {
            jassert (! isInput);
            snd_midi_event_new ((size_t) maxEventSize, &midiParser);
        }

        void enableCallback (bool enable)
        {
            if (callbackEnabled != enable)
            {
                callbackEnabled = enable;

                if (enable)
                    client.registerCallback();
                else
                    client.unregisterCallback();
            }
        }

        bool sendMessageNow (const MidiMessage& message)
        {
            if (message.getRawDataSize() > maxEventSize)
            {
                maxEventSize = message.getRawDataSize();
                snd_midi_event_free (midiParser);
                snd_midi_event_new ((size_t) maxEventSize, &midiParser);
            }

            snd_seq_event_t event;
            snd_seq_ev_clear (&event);

            auto numBytes = (long) message.getRawDataSize();
            auto* data = message.getRawData();

            auto seqHandle = client.get();
            bool success = true;

            while (numBytes > 0)
            {
                auto numSent = snd_midi_event_encode (midiParser, data, numBytes, &event);

                if (numSent <= 0)
                {
                    success = numSent == 0;
                    break;
                }

                numBytes -= numSent;
                data += numSent;

                snd_seq_ev_set_source (&event, (unsigned char) portId);
                snd_seq_ev_set_subs (&event);
                snd_seq_ev_set_direct (&event);

                if (snd_seq_event_output_direct (seqHandle, &event) < 0)
                {
                    success = false;
                    break;
                }
            }

            snd_midi_event_reset_encode (midiParser);
            return success;
        }


        bool operator== (const Port& lhs) const noexcept
        {
            return portId != -1 && portId == lhs.portId;
        }

        void createPort (const String& name, bool enableSubscription)
        {
            if (auto seqHandle = client.get())
            {
                const unsigned int caps =
                    isInput ? (SND_SEQ_PORT_CAP_WRITE | (enableSubscription ? SND_SEQ_PORT_CAP_SUBS_WRITE : 0))
                            : (SND_SEQ_PORT_CAP_READ  | (enableSubscription ? SND_SEQ_PORT_CAP_SUBS_READ : 0));

                portName = name;
                portId = snd_seq_create_simple_port (seqHandle, portName.toUTF8(), caps,
                                                     SND_SEQ_PORT_TYPE_MIDI_GENERIC |
                                                     SND_SEQ_PORT_TYPE_APPLICATION);
            }
        }

        void handleIncomingMidiMessage (const MidiMessage& message) const
        {
            callback->handleIncomingMidiMessage (midiInput, message);
        }

        void handlePartialSysexMessage (const uint8* messageData, int numBytesSoFar, double timeStamp)
        {
            callback->handlePartialSysexMessage (midiInput, messageData, numBytesSoFar, timeStamp);
        }

        AlsaClient& client;

        MidiInputCallback* callback = nullptr;
        snd_midi_event_t* midiParser = nullptr;
        MidiInput* midiInput = nullptr;

        String portName;

        int maxEventSize = 4096, portId = -1;
        bool callbackEnabled = false, isInput = false;
    };

    static Ptr getInstance()
    {
        if (instance == nullptr)
            instance = new AlsaClient();

        return instance;
    }

    void registerCallback()
    {
        if (inputThread == nullptr)
            inputThread.reset (new MidiInputThread (*this));

        if (++activeCallbacks == 1)
            inputThread->startThread();
    }

    void unregisterCallback()
    {
        jassert (activeCallbacks.get() > 0);

        if (--activeCallbacks == 0 && inputThread->isThreadRunning())
            inputThread->signalThreadShouldExit();
    }

    void handleIncomingMidiMessage (snd_seq_event* event, const MidiMessage& message)
    {
        if (event->dest.port < ports.size() && ports[event->dest.port]->callbackEnabled)
            ports[event->dest.port]->handleIncomingMidiMessage (message);
    }

    void handlePartialSysexMessage (snd_seq_event* event, const uint8* messageData, int numBytesSoFar, double timeStamp)
    {
        if (event->dest.port < ports.size()
            && ports[event->dest.port]->callbackEnabled)
            ports[event->dest.port]->handlePartialSysexMessage (messageData, numBytesSoFar, timeStamp);
    }

    snd_seq_t* get() const noexcept     { return handle; }
    int getId() const noexcept          { return clientId; }

    Port* createPort (const String& name, bool forInput, bool enableSubscription)
    {
        auto port = new Port (*this, forInput);
        port->createPort (name, enableSubscription);
        ports.set (port->portId, port);
        incReferenceCount();
        return port;
    }

    void deletePort (Port* port)
    {
        ports.set (port->portId, nullptr);
        decReferenceCount();
    }

private:
    snd_seq_t* handle = nullptr;
    int clientId = 0;
    OwnedArray<Port> ports;
    Atomic<int> activeCallbacks;
    CriticalSection callbackLock;

    static AlsaClient* instance;

    //==============================================================================
    class MidiInputThread   : public Thread
    {
    public:
        MidiInputThread (AlsaClient& c)
            : Thread ("JUCE MIDI Input"), client (c)
        {
            jassert (client.get() != nullptr);
        }

        void run() override
        {
            auto seqHandle = client.get();

            const int maxEventSize = 16 * 1024;
            snd_midi_event_t* midiParser;

            if (snd_midi_event_new (maxEventSize, &midiParser) >= 0)
            {
                auto numPfds = snd_seq_poll_descriptors_count (seqHandle, POLLIN);
                HeapBlock<pollfd> pfd (numPfds);
                snd_seq_poll_descriptors (seqHandle, pfd, (unsigned int) numPfds, POLLIN);

                HeapBlock<uint8> buffer (maxEventSize);

                while (! threadShouldExit())
                {
                    if (poll (pfd, (nfds_t) numPfds, 100) > 0) // there was a "500" here which is a bit long when we exit the program and have to wait for a timeout on this poll call
                    {
                        if (threadShouldExit())
                            break;

                        do
                        {
                            snd_seq_event_t* inputEvent = nullptr;

                            if (snd_seq_event_input (seqHandle, &inputEvent) >= 0)
                            {
                                // xxx what about SYSEXes that are too big for the buffer?
                                auto numBytes = snd_midi_event_decode (midiParser, buffer,
                                                                       maxEventSize, inputEvent);

                                snd_midi_event_reset_decode (midiParser);

                                concatenator.pushMidiData (buffer, (int) numBytes,
                                                           Time::getMillisecondCounter() * 0.001,
                                                           inputEvent, client);

                                snd_seq_free_event (inputEvent);
                            }
                        }
                        while (snd_seq_event_input_pending (seqHandle, 0) > 0);
                    }
                }

                snd_midi_event_free (midiParser);
            }
        }

    private:
        AlsaClient& client;
        MidiDataConcatenator concatenator { 2048 };
    };

    std::unique_ptr<MidiInputThread> inputThread;
};

AlsaClient* AlsaClient::instance = nullptr;

//==============================================================================
static String getFormattedPortIdentifier (int clientId, int portId)
{
    return String (clientId) + "-" + String (portId);
}

static AlsaClient::Port* iterateMidiClient (const AlsaClient::Ptr& client,
                                            snd_seq_client_info_t* clientInfo,
                                            bool forInput,
                                            Array<MidiDeviceInfo>& devices,
                                            const String& deviceIdentifierToOpen)
{
    AlsaClient::Port* port = nullptr;

    auto seqHandle = client->get();
    snd_seq_port_info_t* portInfo = nullptr;

    snd_seq_port_info_alloca (&portInfo);
    jassert (portInfo != nullptr);
    auto numPorts = snd_seq_client_info_get_num_ports (clientInfo);
    auto sourceClient = snd_seq_client_info_get_client (clientInfo);

    snd_seq_port_info_set_client (portInfo, sourceClient);
    snd_seq_port_info_set_port (portInfo, -1);

    while (--numPorts >= 0)
    {
        if (snd_seq_query_next_port (seqHandle, portInfo) == 0
            && (snd_seq_port_info_get_capability (portInfo)
                & (forInput ? SND_SEQ_PORT_CAP_SUBS_READ : SND_SEQ_PORT_CAP_SUBS_WRITE)) != 0)
        {
            String portName (snd_seq_port_info_get_name (portInfo));
            auto portID = snd_seq_port_info_get_port (portInfo);

            MidiDeviceInfo device (portName, getFormattedPortIdentifier (sourceClient, portID));
            devices.add (device);

            if (deviceIdentifierToOpen.isNotEmpty() && deviceIdentifierToOpen == device.identifier)
            {
                if (portID != -1)
                {
                    port = client->createPort (portName, forInput, false);
                    jassert (port->isValid());
                    port->connectWith (sourceClient, portID);
                    break;
                }
            }
        }
    }

    return port;
}

static AlsaClient::Port* iterateMidiDevices (bool forInput,
                                             Array<MidiDeviceInfo>& devices,
                                             const String& deviceIdentifierToOpen)
{
    AlsaClient::Port* port = nullptr;
    auto client = AlsaClient::getInstance();

    if (auto seqHandle = client->get())
    {
        snd_seq_system_info_t* systemInfo = nullptr;
        snd_seq_client_info_t* clientInfo = nullptr;

        snd_seq_system_info_alloca (&systemInfo);
        jassert (systemInfo != nullptr);

        if (snd_seq_system_info (seqHandle, systemInfo) == 0)
        {
            snd_seq_client_info_alloca (&clientInfo);
            jassert (clientInfo != nullptr);

            auto numClients = snd_seq_system_info_get_cur_clients (systemInfo);

            while (--numClients >= 0)
            {
                if (snd_seq_query_next_client (seqHandle, clientInfo) == 0)
                {
                    port = iterateMidiClient (client, clientInfo, forInput,
                                              devices, deviceIdentifierToOpen);

                    if (port != nullptr)
                        break;
                }
            }
        }
    }

    return port;
}

} // namespace

//==============================================================================
Array<MidiDeviceInfo> MidiInput::getAvailableDevices()
{
    Array<MidiDeviceInfo> devices;
    iterateMidiDevices (true, devices, {});

    return devices;
}

MidiDeviceInfo MidiInput::getDefaultDevice()
{
    return getAvailableDevices().getFirst();
}

std::unique_ptr<MidiInput> MidiInput::openDevice (const String& deviceIdentifier, MidiInputCallback* callback)
{
    if (deviceIdentifier.isEmpty())
        return {};

    Array<MidiDeviceInfo> devices;
    auto* port = iterateMidiDevices (true, devices, deviceIdentifier);

    if (port == nullptr || ! port->isValid())
        return {};

    jassert (port->isValid());

    std::unique_ptr<MidiInput> midiInput (new MidiInput (port->portName, deviceIdentifier));

    port->setupInput (midiInput.get(), callback);
    midiInput->internal = port;

    return midiInput;
}

std::unique_ptr<MidiInput> MidiInput::createNewDevice (const String& deviceName, MidiInputCallback* callback)
{
    auto client = AlsaClient::getInstance();
    auto* port = client->createPort (deviceName, true, true);

    if (port == nullptr || ! port->isValid())
        return {};

    std::unique_ptr<MidiInput> midiInput (new MidiInput (deviceName, getFormattedPortIdentifier (client->getId(), port->portId)));

    port->setupInput (midiInput.get(), callback);
    midiInput->internal = port;

    return midiInput;
}

StringArray MidiInput::getDevices()
{
    StringArray deviceNames;

    for (auto& d : getAvailableDevices())
        deviceNames.add (d.name);

    deviceNames.appendNumbersToDuplicates (true, true);

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
    stop();
    AlsaClient::getInstance()->deletePort (static_cast<AlsaClient::Port*> (internal));
}

void MidiInput::start()
{
    static_cast<AlsaClient::Port*> (internal)->enableCallback (true);
}

void MidiInput::stop()
{
    static_cast<AlsaClient::Port*> (internal)->enableCallback (false);
}

//==============================================================================
Array<MidiDeviceInfo> MidiOutput::getAvailableDevices()
{
    Array<MidiDeviceInfo> devices;
    iterateMidiDevices (false, devices, {});

    return devices;
}

MidiDeviceInfo MidiOutput::getDefaultDevice()
{
    return getAvailableDevices().getFirst();
}

std::unique_ptr<MidiOutput> MidiOutput::openDevice (const String& deviceIdentifier)
{
    if (deviceIdentifier.isEmpty())
        return {};

    Array<MidiDeviceInfo> devices;
    auto* port = iterateMidiDevices (false, devices, deviceIdentifier);

    if (port == nullptr || ! port->isValid())
        return {};

    std::unique_ptr<MidiOutput> midiOutput (new MidiOutput (port->portName, deviceIdentifier));

    port->setupOutput();
    midiOutput->internal = port;

    return midiOutput;
}

std::unique_ptr<MidiOutput> MidiOutput::createNewDevice (const String& deviceName)
{
    auto client = AlsaClient::getInstance();
    auto* port = client->createPort (deviceName, false, true);

    if (port == nullptr || ! port->isValid())
        return {};

    std::unique_ptr<MidiOutput> midiOutput (new MidiOutput (deviceName, getFormattedPortIdentifier (client->getId(), port->portId)));

    port->setupOutput();
    midiOutput->internal = port;

    return midiOutput;
}

StringArray MidiOutput::getDevices()
{
    StringArray deviceNames;

    for (auto& d : getAvailableDevices())
        deviceNames.add (d.name);

    deviceNames.appendNumbersToDuplicates (true, true);

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
    AlsaClient::getInstance()->deletePort (static_cast<AlsaClient::Port*> (internal));
}

void MidiOutput::sendMessageNow (const MidiMessage& message)
{
    static_cast<AlsaClient::Port*> (internal)->sendMessageNow (message);
}

//==============================================================================
#else

// (These are just stub functions if ALSA is unavailable...)
MidiInput::MidiInput (const String& deviceName, const String& deviceID)
    : deviceInfo (deviceName, deviceID)
{
}

MidiInput::~MidiInput()                                                                   {}
void MidiInput::start()                                                                   {}
void MidiInput::stop()                                                                    {}
Array<MidiDeviceInfo> MidiInput::getAvailableDevices()                                    { return {}; }
MidiDeviceInfo MidiInput::getDefaultDevice()                                              { return {}; }
std::unique_ptr<MidiInput> MidiInput::openDevice (const String&, MidiInputCallback*)      { return {}; }
std::unique_ptr<MidiInput> MidiInput::createNewDevice (const String&, MidiInputCallback*) { return {}; }
StringArray MidiInput::getDevices()                                                       { return {}; }
int MidiInput::getDefaultDeviceIndex()                                                    { return 0;}
std::unique_ptr<MidiInput> MidiInput::openDevice (int, MidiInputCallback*)                { return {}; }

MidiOutput::~MidiOutput()                                                                 {}
void MidiOutput::sendMessageNow (const MidiMessage&)                                      {}
Array<MidiDeviceInfo> MidiOutput::getAvailableDevices()                                   { return {}; }
MidiDeviceInfo MidiOutput::getDefaultDevice()                                             { return {}; }
std::unique_ptr<MidiOutput> MidiOutput::openDevice (const String&)                        { return {}; }
std::unique_ptr<MidiOutput> MidiOutput::createNewDevice (const String&)                   { return {}; }
StringArray MidiOutput::getDevices()                                                      { return {}; }
int MidiOutput::getDefaultDeviceIndex()                                                   { return 0;}
std::unique_ptr<MidiOutput> MidiOutput::openDevice (int)                                  { return {}; }

#endif

} // namespace juce
