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

static void* juce_libjackHandle = nullptr;

static void* juce_loadJackFunction (const char* const name)
{
    if (juce_libjackHandle == nullptr)
        return nullptr;

    return dlsym (juce_libjackHandle, name);
}

#define JUCE_DECL_JACK_FUNCTION(return_type, fn_name, argument_types, arguments)  \
  return_type fn_name argument_types                                              \
  {                                                                               \
      typedef return_type (*fn_type) argument_types;                              \
      static fn_type fn = (fn_type) juce_loadJackFunction (#fn_name);             \
      return (fn != nullptr) ? ((*fn) arguments) : (return_type) 0;               \
  }

#define JUCE_DECL_VOID_JACK_FUNCTION(fn_name, argument_types, arguments)          \
  void fn_name argument_types                                                     \
  {                                                                               \
      typedef void (*fn_type) argument_types;                                     \
      static fn_type fn = (fn_type) juce_loadJackFunction (#fn_name);             \
      if (fn != nullptr) (*fn) arguments;                                         \
  }

//==============================================================================
JUCE_DECL_JACK_FUNCTION (jack_client_t*, jack_client_open, (const char* client_name, jack_options_t options, jack_status_t* status, ...), (client_name, options, status));
JUCE_DECL_JACK_FUNCTION (int, jack_client_close, (jack_client_t *client), (client));
JUCE_DECL_JACK_FUNCTION (int, jack_activate, (jack_client_t* client), (client));
JUCE_DECL_JACK_FUNCTION (int, jack_deactivate, (jack_client_t* client), (client));
JUCE_DECL_JACK_FUNCTION (jack_nframes_t, jack_get_buffer_size, (jack_client_t* client), (client));
JUCE_DECL_JACK_FUNCTION (jack_nframes_t, jack_get_sample_rate, (jack_client_t* client), (client));
JUCE_DECL_VOID_JACK_FUNCTION (jack_on_shutdown, (jack_client_t* client, void (*function)(void* arg), void* arg), (client, function, arg));
JUCE_DECL_JACK_FUNCTION (void* , jack_port_get_buffer, (jack_port_t* port, jack_nframes_t nframes), (port, nframes));
JUCE_DECL_JACK_FUNCTION (jack_nframes_t, jack_port_get_total_latency, (jack_client_t* client, jack_port_t* port), (client, port));
JUCE_DECL_JACK_FUNCTION (jack_port_t* , jack_port_register, (jack_client_t* client, const char* port_name, const char* port_type, unsigned long flags, unsigned long buffer_size), (client, port_name, port_type, flags, buffer_size));
JUCE_DECL_VOID_JACK_FUNCTION (jack_set_error_function, (void (*func)(const char*)), (func));
JUCE_DECL_JACK_FUNCTION (int, jack_set_process_callback, (jack_client_t* client, JackProcessCallback process_callback, void* arg), (client, process_callback, arg));
JUCE_DECL_JACK_FUNCTION (const char**, jack_get_ports, (jack_client_t* client, const char* port_name_pattern, const char* type_name_pattern, unsigned long flags), (client, port_name_pattern, type_name_pattern, flags));
JUCE_DECL_JACK_FUNCTION (int, jack_connect, (jack_client_t* client, const char* source_port, const char* destination_port), (client, source_port, destination_port));
JUCE_DECL_JACK_FUNCTION (const char*, jack_port_name, (const jack_port_t* port), (port));
JUCE_DECL_JACK_FUNCTION (void*, jack_set_port_connect_callback, (jack_client_t* client, JackPortConnectCallback connect_callback, void* arg), (client, connect_callback, arg));
JUCE_DECL_JACK_FUNCTION (jack_port_t* , jack_port_by_id, (jack_client_t* client, jack_port_id_t port_id), (client, port_id));
JUCE_DECL_JACK_FUNCTION (int, jack_port_connected, (const jack_port_t* port), (port));
JUCE_DECL_JACK_FUNCTION (int, jack_port_connected_to, (const jack_port_t* port, const char* port_name), (port, port_name));
JUCE_DECL_JACK_FUNCTION (int, jack_set_xrun_callback, (jack_client_t* client, JackXRunCallback xrun_callback, void* arg), (client, xrun_callback, arg));

#if JUCE_DEBUG
 #define JACK_LOGGING_ENABLED 1
#endif

#if JACK_LOGGING_ENABLED
namespace
{
    void jack_Log (const String& s)
    {
        std::cerr << s << std::endl;
    }

    const char* getJackErrorMessage (const jack_status_t status)
    {
        if (status & JackServerFailed
             || status & JackServerError)   return "Unable to connect to JACK server";
        if (status & JackVersionError)      return "Client's protocol version does not match";
        if (status & JackInvalidOption)     return "The operation contained an invalid or unsupported option";
        if (status & JackNameNotUnique)     return "The desired client name was not unique";
        if (status & JackNoSuchClient)      return "Requested client does not exist";
        if (status & JackInitFailure)       return "Unable to initialize client";
        return nullptr;
    }
}
 #define JUCE_JACK_LOG_STATUS(x)    { if (const char* m = getJackErrorMessage (x)) jack_Log (m); }
 #define JUCE_JACK_LOG(x)           jack_Log(x)
#else
 #define JUCE_JACK_LOG_STATUS(x)    {}
 #define JUCE_JACK_LOG(x)           {}
#endif


//==============================================================================
#ifndef JUCE_JACK_CLIENT_NAME
 #define JUCE_JACK_CLIENT_NAME "JUCEJack"
#endif

struct JackPortIterator
{
    JackPortIterator (jack_client_t* const client, const bool forInput)
        : ports (nullptr), index (-1)
    {
        if (client != nullptr)
            ports = juce::jack_get_ports (client, nullptr, nullptr,
                                          forInput ? JackPortIsOutput : JackPortIsInput);
                                            // (NB: This looks like it's the wrong way round, but it is correct!)
    }

    ~JackPortIterator()
    {
        ::free (ports);
    }

    bool next()
    {
        if (ports == nullptr || ports [index + 1] == nullptr)
            return false;

        name = CharPointer_UTF8 (ports[++index]);
        clientName = name.upToFirstOccurrenceOf (":", false, false);
        return true;
    }

    const char** ports;
    int index;
    String name;
    String clientName;
};

class JackAudioIODeviceType;
static Array<JackAudioIODeviceType*> activeDeviceTypes;

//==============================================================================
class JackAudioIODevice   : public AudioIODevice
{
public:
    JackAudioIODevice (const String& deviceName,
                       const String& inId,
                       const String& outId)
        : AudioIODevice (deviceName, "JACK"),
          inputId (inId),
          outputId (outId),
          deviceIsOpen (false),
          callback (nullptr),
          totalNumberOfInputChannels (0),
          totalNumberOfOutputChannels (0)
    {
        jassert (deviceName.isNotEmpty());

        jack_status_t status;
        client = juce::jack_client_open (JUCE_JACK_CLIENT_NAME, JackNoStartServer, &status);

        if (client == nullptr)
        {
            JUCE_JACK_LOG_STATUS (status);
        }
        else
        {
            juce::jack_set_error_function (errorCallback);

            // open input ports
            const StringArray inputChannels (getInputChannelNames());
            for (int i = 0; i < inputChannels.size(); ++i)
            {
                String inputName;
                inputName << "in_" << ++totalNumberOfInputChannels;

                inputPorts.add (juce::jack_port_register (client, inputName.toUTF8(),
                                                          JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0));
            }

            // open output ports
            const StringArray outputChannels (getOutputChannelNames());
            for (int i = 0; i < outputChannels.size(); ++i)
            {
                String outputName;
                outputName << "out_" << ++totalNumberOfOutputChannels;

                outputPorts.add (juce::jack_port_register (client, outputName.toUTF8(),
                                                           JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0));
            }

            inChans.calloc (totalNumberOfInputChannels + 2);
            outChans.calloc (totalNumberOfOutputChannels + 2);
        }
    }

    ~JackAudioIODevice()
    {
        close();
        if (client != nullptr)
        {
            juce::jack_client_close (client);
            client = nullptr;
        }
    }

    StringArray getChannelNames (bool forInput) const
    {
        StringArray names;

        for (JackPortIterator i (client, forInput); i.next();)
            if (i.clientName == getName())
                names.add (i.name.fromFirstOccurrenceOf (":", false, false));

        return names;
    }

    StringArray getOutputChannelNames() override         { return getChannelNames (false); }
    StringArray getInputChannelNames() override          { return getChannelNames (true); }

    Array<double> getAvailableSampleRates() override
    {
        Array<double> rates;

        if (client != nullptr)
            rates.add (juce::jack_get_sample_rate (client));

        return rates;
    }

    Array<int> getAvailableBufferSizes() override
    {
        Array<int> sizes;

        if (client != nullptr)
            sizes.add (juce::jack_get_buffer_size (client));

        return sizes;
    }

    int getDefaultBufferSize() override             { return getCurrentBufferSizeSamples(); }
    int getCurrentBufferSizeSamples() override      { return client != nullptr ? juce::jack_get_buffer_size (client) : 0; }
    double getCurrentSampleRate() override          { return client != nullptr ? juce::jack_get_sample_rate (client) : 0; }


    String open (const BigInteger& inputChannels, const BigInteger& outputChannels,
                 double /* sampleRate */, int /* bufferSizeSamples */) override
    {
        if (client == nullptr)
        {
            lastError = "No JACK client running";
            return lastError;
        }

        lastError.clear();
        close();

        xruns = 0;
        juce::jack_set_process_callback (client, processCallback, this);
        juce::jack_set_port_connect_callback (client, portConnectCallback, this);
        juce::jack_on_shutdown (client, shutdownCallback, this);
        juce::jack_set_xrun_callback (client, xrunCallback, this);
        juce::jack_activate (client);
        deviceIsOpen = true;

        if (! inputChannels.isZero())
        {
            for (JackPortIterator i (client, true); i.next();)
            {
                if (inputChannels [i.index] && i.clientName == getName())
                {
                    int error = juce::jack_connect (client, i.ports[i.index], juce::jack_port_name ((jack_port_t*) inputPorts[i.index]));
                    if (error != 0)
                        JUCE_JACK_LOG ("Cannot connect input port " + String (i.index) + " (" + i.name + "), error " + String (error));
                }
            }
        }

        if (! outputChannels.isZero())
        {
            for (JackPortIterator i (client, false); i.next();)
            {
                if (outputChannels [i.index] && i.clientName == getName())
                {
                    int error = juce::jack_connect (client, juce::jack_port_name ((jack_port_t*) outputPorts[i.index]), i.ports[i.index]);
                    if (error != 0)
                        JUCE_JACK_LOG ("Cannot connect output port " + String (i.index) + " (" + i.name + "), error " + String (error));
                }
            }
        }

        updateActivePorts();

        return lastError;
    }

    void close() override
    {
        stop();

        if (client != nullptr)
        {
            juce::jack_deactivate (client);

            juce::jack_set_xrun_callback (client, xrunCallback, nullptr);
            juce::jack_set_process_callback (client, processCallback, nullptr);
            juce::jack_set_port_connect_callback (client, portConnectCallback, nullptr);
            juce::jack_on_shutdown (client, shutdownCallback, nullptr);
        }

        deviceIsOpen = false;
    }

    void start (AudioIODeviceCallback* newCallback) override
    {
        if (deviceIsOpen && newCallback != callback)
        {
            if (newCallback != nullptr)
                newCallback->audioDeviceAboutToStart (this);

            AudioIODeviceCallback* const oldCallback = callback;

            {
                const ScopedLock sl (callbackLock);
                callback = newCallback;
            }

            if (oldCallback != nullptr)
                oldCallback->audioDeviceStopped();
        }
    }

    void stop() override
    {
        start (nullptr);
    }

    bool isOpen() override                           { return deviceIsOpen; }
    bool isPlaying() override                        { return callback != nullptr; }
    int getCurrentBitDepth() override                { return 32; }
    String getLastError() override                   { return lastError; }
    int getXRunCount() const noexcept override       { return xruns; }

    BigInteger getActiveOutputChannels() const override  { return activeOutputChannels; }
    BigInteger getActiveInputChannels()  const override  { return activeInputChannels;  }

    int getOutputLatencyInSamples() override
    {
        int latency = 0;

        for (int i = 0; i < outputPorts.size(); i++)
            latency = jmax (latency, (int) juce::jack_port_get_total_latency (client, (jack_port_t*) outputPorts [i]));

        return latency;
    }

    int getInputLatencyInSamples() override
    {
        int latency = 0;

        for (int i = 0; i < inputPorts.size(); i++)
            latency = jmax (latency, (int) juce::jack_port_get_total_latency (client, (jack_port_t*) inputPorts [i]));

        return latency;
    }

    String inputId, outputId;

private:
    void process (const int numSamples)
    {
        int numActiveInChans = 0, numActiveOutChans = 0;

        for (int i = 0; i < totalNumberOfInputChannels; ++i)
        {
            if (activeInputChannels[i])
                if (jack_default_audio_sample_t* in
                        = (jack_default_audio_sample_t*) juce::jack_port_get_buffer ((jack_port_t*) inputPorts.getUnchecked(i), numSamples))
                    inChans [numActiveInChans++] = (float*) in;
        }

        for (int i = 0; i < totalNumberOfOutputChannels; ++i)
        {
            if (activeOutputChannels[i])
                if (jack_default_audio_sample_t* out
                        = (jack_default_audio_sample_t*) juce::jack_port_get_buffer ((jack_port_t*) outputPorts.getUnchecked(i), numSamples))
                    outChans [numActiveOutChans++] = (float*) out;
        }

        const ScopedLock sl (callbackLock);

        if (callback != nullptr)
        {
            if ((numActiveInChans + numActiveOutChans) > 0)
                callback->audioDeviceIOCallback (const_cast<const float**> (inChans.getData()), numActiveInChans,
                                                 outChans, numActiveOutChans, numSamples);
        }
        else
        {
            for (int i = 0; i < numActiveOutChans; ++i)
                zeromem (outChans[i], sizeof (float) * numSamples);
        }
    }

    static int processCallback (jack_nframes_t nframes, void* callbackArgument)
    {
        if (callbackArgument != nullptr)
            ((JackAudioIODevice*) callbackArgument)->process (nframes);

        return 0;
    }

    static int xrunCallback (void* callbackArgument)
    {
        if (callbackArgument != nullptr)
            ((JackAudioIODevice*) callbackArgument)->xruns++;

        return 0;
    }

    void updateActivePorts()
    {
        BigInteger newOutputChannels, newInputChannels;

        for (int i = 0; i < outputPorts.size(); ++i)
            if (juce::jack_port_connected ((jack_port_t*) outputPorts.getUnchecked(i)))
                newOutputChannels.setBit (i);

        for (int i = 0; i < inputPorts.size(); ++i)
            if (juce::jack_port_connected ((jack_port_t*) inputPorts.getUnchecked(i)))
                newInputChannels.setBit (i);

        if (newOutputChannels != activeOutputChannels
             || newInputChannels != activeInputChannels)
        {
            AudioIODeviceCallback* const oldCallback = callback;

            stop();

            activeOutputChannels = newOutputChannels;
            activeInputChannels  = newInputChannels;

            if (oldCallback != nullptr)
                start (oldCallback);

            sendDeviceChangedCallback();
        }
    }

    static void portConnectCallback (jack_port_id_t, jack_port_id_t, int, void* arg)
    {
        if (JackAudioIODevice* device = static_cast<JackAudioIODevice*> (arg))
            device->updateActivePorts();
    }

    static void threadInitCallback (void* /* callbackArgument */)
    {
        JUCE_JACK_LOG ("JackAudioIODevice::initialise");
    }

    static void shutdownCallback (void* callbackArgument)
    {
        JUCE_JACK_LOG ("JackAudioIODevice::shutdown");

        if (JackAudioIODevice* device = (JackAudioIODevice*) callbackArgument)
        {
            device->client = nullptr;
            device->close();
        }
    }

    static void errorCallback (const char* msg)
    {
        JUCE_JACK_LOG ("JackAudioIODevice::errorCallback " + String (msg));
    }

    static void sendDeviceChangedCallback();

    bool deviceIsOpen;
    jack_client_t* client;
    String lastError;
    AudioIODeviceCallback* callback;
    CriticalSection callbackLock;

    HeapBlock<float*> inChans, outChans;
    int totalNumberOfInputChannels;
    int totalNumberOfOutputChannels;
    Array<void*> inputPorts, outputPorts;
    BigInteger activeInputChannels, activeOutputChannels;

    int xruns;
};


//==============================================================================
class JackAudioIODeviceType  : public AudioIODeviceType
{
public:
    JackAudioIODeviceType()
        : AudioIODeviceType ("JACK"),
          hasScanned (false)
    {
        activeDeviceTypes.add (this);
    }

    ~JackAudioIODeviceType()
    {
        activeDeviceTypes.removeFirstMatchingValue (this);
    }

    void scanForDevices()
    {
        hasScanned = true;
        inputNames.clear();
        inputIds.clear();
        outputNames.clear();
        outputIds.clear();

        if (juce_libjackHandle == nullptr)  juce_libjackHandle = dlopen ("libjack.so.0", RTLD_LAZY);
        if (juce_libjackHandle == nullptr)  juce_libjackHandle = dlopen ("libjack.so",   RTLD_LAZY);
        if (juce_libjackHandle == nullptr)  return;

        jack_status_t status;

        // open a dummy client
        if (jack_client_t* const client = juce::jack_client_open ("JuceJackDummy", JackNoStartServer, &status))
        {
            // scan for output devices
            for (JackPortIterator i (client, false); i.next();)
            {
                if (i.clientName != (JUCE_JACK_CLIENT_NAME) && ! inputNames.contains (i.clientName))
                {
                    inputNames.add (i.clientName);
                    inputIds.add (i.ports [i.index]);
                }
            }

            // scan for input devices
            for (JackPortIterator i (client, true); i.next();)
            {
                if (i.clientName != (JUCE_JACK_CLIENT_NAME) && ! outputNames.contains (i.clientName))
                {
                    outputNames.add (i.clientName);
                    outputIds.add (i.ports [i.index]);
                }
            }

            juce::jack_client_close (client);
        }
        else
        {
            JUCE_JACK_LOG_STATUS (status);
        }
    }

    StringArray getDeviceNames (bool wantInputNames) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this
        return wantInputNames ? inputNames : outputNames;
    }

    int getDefaultDeviceIndex (bool /* forInput */) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this
        return 0;
    }

    bool hasSeparateInputsAndOutputs() const    { return true; }

    int getIndexOfDevice (AudioIODevice* device, bool asInput) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        if (JackAudioIODevice* d = dynamic_cast<JackAudioIODevice*> (device))
            return asInput ? inputIds.indexOf (d->inputId)
                           : outputIds.indexOf (d->outputId);

        return -1;
    }

    AudioIODevice* createDevice (const String& outputDeviceName,
                                 const String& inputDeviceName)
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        const int inputIndex = inputNames.indexOf (inputDeviceName);
        const int outputIndex = outputNames.indexOf (outputDeviceName);

        if (inputIndex >= 0 || outputIndex >= 0)
            return new JackAudioIODevice (outputIndex >= 0 ? outputDeviceName
                                                           : inputDeviceName,
                                          inputIds [inputIndex],
                                          outputIds [outputIndex]);

        return nullptr;
    }

    void portConnectionChange()    { callDeviceChangeListeners(); }

private:
    StringArray inputNames, outputNames, inputIds, outputIds;
    bool hasScanned;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (JackAudioIODeviceType)
};

void JackAudioIODevice::sendDeviceChangedCallback()
{
    for (int i = activeDeviceTypes.size(); --i >= 0;)
        if (JackAudioIODeviceType* d = activeDeviceTypes[i])
            d->portConnectionChange();
}

//==============================================================================
AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_JACK()
{
    return new JackAudioIODeviceType();
}

} // namespace juce
