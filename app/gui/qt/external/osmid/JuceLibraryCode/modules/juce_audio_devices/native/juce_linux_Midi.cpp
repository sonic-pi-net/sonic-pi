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

#if JUCE_ALSA

// You can define these strings in your app if you want to override the default names:
#ifndef JUCE_ALSA_MIDI_NAME
 #define JUCE_ALSA_MIDI_NAME  JUCEApplicationBase::getInstance()->getApplicationName().toUTF8()
#endif

//==============================================================================
namespace
{

//==============================================================================
class AlsaClient : public ReferenceCountedObject
{
public:
    typedef ReferenceCountedObjectPtr<AlsaClient> Ptr;

    //==============================================================================
    // represents an input or output port of the supplied AlsaClient
    class Port
    {
    public:
        Port (AlsaClient& c, bool forInput) noexcept
            : portId (-1),
              callbackEnabled (false),
              client (c),
              isInput (forInput),
              callback (nullptr),
              maxEventSize (4 * 1024),
              midiInput (nullptr)
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

        void setupInput(MidiInput* input, MidiInputCallback* cb)
        {
            jassert (cb && input);

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

            long numBytes = (long) message.getRawDataSize();
            const uint8* data = message.getRawData();

            snd_seq_t* seqHandle = client.get();
            bool success = true;

            while (numBytes > 0)
            {
                const long numSent = snd_midi_event_encode (midiParser, data, numBytes, &event);

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

        int portId;
        bool callbackEnabled;

    private:
        friend class AlsaClient;

        AlsaClient& client;
        bool isInput;
        MidiInputCallback* callback;
        snd_midi_event_t* midiParser;
        int maxEventSize;
        MidiInput* midiInput;

        void createPort (const String& name, bool enableSubscription)
        {
            if (snd_seq_t* seqHandle = client.get())
            {
                const unsigned int caps =
                    isInput
                    ? (SND_SEQ_PORT_CAP_WRITE | (enableSubscription ? SND_SEQ_PORT_CAP_SUBS_WRITE : 0))
                    : (SND_SEQ_PORT_CAP_WRITE | (enableSubscription ? SND_SEQ_PORT_CAP_SUBS_READ : 0));
                portId = snd_seq_create_simple_port (seqHandle, name.toUTF8(), caps,
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
            inputThread = new MidiInputThread (*this);

        if (++activeCallbacks - 1 == 0)
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
        if (event->dest.port < ports.size()
            && ports[event->dest.port]->callbackEnabled)
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
        Port* port = new Port (*this, forInput);
        port->createPort (name, enableSubscription);
        ports.set (port->portId, port);
        incReferenceCount();
        return port;
    }

    void deletePort (Port* port)
    {
        ports.remove (port->portId);
        decReferenceCount();
    }

private:
    snd_seq_t* handle;
    int clientId;
    OwnedArray<Port> ports;
    Atomic<int> activeCallbacks;
    CriticalSection callbackLock;

    static AlsaClient* instance;

    //==============================================================================
    friend class ReferenceCountedObjectPtr<AlsaClient>;
    friend struct ContainerDeletePolicy<AlsaClient>;

    AlsaClient()
        : handle (nullptr),
          inputThread (nullptr)
    {
        jassert (instance == nullptr);

        snd_seq_open (&handle, "default", SND_SEQ_OPEN_DUPLEX, 0);
        snd_seq_nonblock (handle, SND_SEQ_NONBLOCK);
        snd_seq_set_client_name (handle, JUCE_ALSA_MIDI_NAME);
        clientId = snd_seq_client_id(handle);

        // It's good idea to pre-allocate a good number of elements
        ports.ensureStorageAllocated (32);
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

    //==============================================================================
    class MidiInputThread   : public Thread
    {
    public:
        MidiInputThread (AlsaClient& c)
            : Thread ("Juce MIDI Input"), client (c), concatenator (2048)
        {
            jassert (client.get() != nullptr);
        }

        void run() override
        {
            const int maxEventSize = 16 * 1024;
            snd_midi_event_t* midiParser;
            snd_seq_t* seqHandle = client.get();

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
                                const long numBytes = snd_midi_event_decode (midiParser, buffer,
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
        MidiDataConcatenator concatenator;
    };

    ScopedPointer<MidiInputThread> inputThread;
};

AlsaClient* AlsaClient::instance = nullptr;

//==============================================================================
static AlsaClient::Port* iterateMidiClient (const AlsaClient::Ptr& client,
                                            snd_seq_client_info_t* clientInfo,
                                            const bool forInput,
                                            StringArray& deviceNamesFound,
                                            const int deviceIndexToOpen)
{
    AlsaClient::Port* port = nullptr;

    snd_seq_t* seqHandle = client->get();
    snd_seq_port_info_t* portInfo = nullptr;

    snd_seq_port_info_alloca (&portInfo);
    jassert (portInfo);
    int numPorts = snd_seq_client_info_get_num_ports (clientInfo);
    const int sourceClient = snd_seq_client_info_get_client (clientInfo);

    snd_seq_port_info_set_client (portInfo, sourceClient);
    snd_seq_port_info_set_port (portInfo, -1);

    while (--numPorts >= 0)
    {
        if (snd_seq_query_next_port (seqHandle, portInfo) == 0
            && (snd_seq_port_info_get_capability (portInfo)
                & (forInput ? SND_SEQ_PORT_CAP_SUBS_WRITE : SND_SEQ_PORT_CAP_SUBS_READ)) != 0)
        {
            const String portName = snd_seq_port_info_get_name(portInfo);

            deviceNamesFound.add (portName);

            if (deviceNamesFound.size() == deviceIndexToOpen + 1)
            {
                const int sourcePort = snd_seq_port_info_get_port (portInfo);
                if (sourcePort != -1)
                {
                    port = client->createPort (portName, forInput, false);
                    jassert (port->isValid());
                    port->connectWith (sourceClient, sourcePort);
                    break;
                }
            }
        }
    }

    return port;
}

static AlsaClient::Port* iterateMidiDevices (const bool forInput,
                                             StringArray& deviceNamesFound,
                                             const int deviceIndexToOpen)
{
    AlsaClient::Port* port = nullptr;
    const AlsaClient::Ptr client (AlsaClient::getInstance());

    if (snd_seq_t* const seqHandle = client->get())
    {
        snd_seq_system_info_t* systemInfo = nullptr;
        snd_seq_client_info_t* clientInfo = nullptr;

        snd_seq_system_info_alloca (&systemInfo);
        jassert(systemInfo);
        if (snd_seq_system_info (seqHandle, systemInfo) == 0)
        {
            snd_seq_client_info_alloca (&clientInfo);
            jassert(clientInfo);
            int numClients = snd_seq_system_info_get_cur_clients (systemInfo);

            while (--numClients >= 0)
            {
                if (snd_seq_query_next_client (seqHandle, clientInfo) == 0)
                {
                    const int sourceClient = snd_seq_client_info_get_client (clientInfo);
                    if (sourceClient != client->getId()
                        && sourceClient != SND_SEQ_CLIENT_SYSTEM)
                    {
                        port = iterateMidiClient (client, clientInfo, forInput,
                                                  deviceNamesFound, deviceIndexToOpen);
                        if (port)
                            break;
                    }
                }
            }
        }
    }

    deviceNamesFound.appendNumbersToDuplicates (true, true);

    return port;
}

} // namespace

StringArray MidiOutput::getDevices()
{
    StringArray devices;
    iterateMidiDevices (false, devices, -1);
    return devices;
}

int MidiOutput::getDefaultDeviceIndex()
{
    return 0;
}

MidiOutput* MidiOutput::openDevice (int deviceIndex)
{
    MidiOutput* newDevice = nullptr;

    StringArray devices;
    AlsaClient::Port* port = iterateMidiDevices (false, devices, deviceIndex);

    if (port == nullptr)
        return nullptr;

    jassert (port->isValid());

    newDevice = new MidiOutput (devices [deviceIndex]);
    port->setupOutput();
    newDevice->internal = port;

    return newDevice;
}

MidiOutput* MidiOutput::createNewDevice (const String& deviceName)
{
    MidiOutput* newDevice = nullptr;

    const AlsaClient::Ptr client (AlsaClient::getInstance());

    AlsaClient::Port* port = client->createPort (deviceName, false, true);

    jassert (port->isValid());

    newDevice = new MidiOutput (deviceName);
    port->setupOutput();
    newDevice->internal = port;

    return newDevice;
}

MidiOutput::~MidiOutput()
{
    stopBackgroundThread();

    AlsaClient::Ptr client (AlsaClient::getInstance());
    client->deletePort (static_cast<AlsaClient::Port*> (internal));
}

void MidiOutput::sendMessageNow (const MidiMessage& message)
{
    static_cast<AlsaClient::Port*> (internal)->sendMessageNow (message);
}

//==============================================================================
MidiInput::MidiInput (const String& nm)
    : name (nm), internal (nullptr)
{
}

MidiInput::~MidiInput()
{
    stop();
    AlsaClient::Ptr client (AlsaClient::getInstance());
    client->deletePort (static_cast<AlsaClient::Port*> (internal));
}

void MidiInput::start()
{
    static_cast<AlsaClient::Port*> (internal)->enableCallback (true);
}

void MidiInput::stop()
{
    static_cast<AlsaClient::Port*> (internal)->enableCallback (false);
}

int MidiInput::getDefaultDeviceIndex()
{
    return 0;
}

StringArray MidiInput::getDevices()
{
    StringArray devices;
    iterateMidiDevices (true, devices, -1);
    return devices;
}

MidiInput* MidiInput::openDevice (int deviceIndex, MidiInputCallback* callback)
{
    MidiInput* newDevice = nullptr;

    StringArray devices;
    AlsaClient::Port* port = iterateMidiDevices (true, devices, deviceIndex);

    if (port == nullptr)
        return nullptr;

    jassert (port->isValid());

    newDevice = new MidiInput (devices [deviceIndex]);
    port->setupInput (newDevice, callback);
    newDevice->internal = port;

    return newDevice;
}

MidiInput* MidiInput::createNewDevice (const String& deviceName, MidiInputCallback* callback)
{
    MidiInput* newDevice = nullptr;

    AlsaClient::Ptr client (AlsaClient::getInstance());

    AlsaClient::Port* port = client->createPort (deviceName, true, true);

    jassert (port->isValid());

    newDevice = new MidiInput (deviceName);
    port->setupInput (newDevice, callback);
    newDevice->internal = port;

    return newDevice;
}


//==============================================================================
#else

// (These are just stub functions if ALSA is unavailable...)

StringArray MidiOutput::getDevices()                                { return {}; }
int MidiOutput::getDefaultDeviceIndex()                             { return 0; }
MidiOutput* MidiOutput::openDevice (int)                            { return nullptr; }
MidiOutput* MidiOutput::createNewDevice (const String&)             { return nullptr; }
MidiOutput::~MidiOutput()   {}
void MidiOutput::sendMessageNow (const MidiMessage&)    {}

MidiInput::MidiInput (const String& nm) : name (nm), internal (nullptr)  {}
MidiInput::~MidiInput() {}
void MidiInput::start() {}
void MidiInput::stop()  {}
int MidiInput::getDefaultDeviceIndex()      { return 0; }
StringArray MidiInput::getDevices()         { return {}; }
MidiInput* MidiInput::openDevice (int, MidiInputCallback*)                  { return nullptr; }
MidiInput* MidiInput::createNewDevice (const String&, MidiInputCallback*)   { return nullptr; }

#endif

} // namespace juce
