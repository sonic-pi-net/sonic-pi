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

#ifndef DRV_QUERYDEVICEINTERFACE
 #define DRV_RESERVED                  0x0800
 #define DRV_QUERYDEVICEINTERFACE     (DRV_RESERVED + 12)
 #define DRV_QUERYDEVICEINTERFACESIZE (DRV_RESERVED + 13)
#endif

namespace juce
{

struct MidiServiceType
{
    struct InputWrapper
    {
        virtual ~InputWrapper() {}

        virtual String getDeviceIdentifier() = 0;
        virtual String getDeviceName() = 0;

        virtual void start() = 0;
        virtual void stop() = 0;
    };

    struct OutputWrapper
    {
        virtual ~OutputWrapper() {}

        virtual String getDeviceIdentifier() = 0;
        virtual String getDeviceName() = 0;

        virtual void sendMessageNow (const MidiMessage&) = 0;
    };

    MidiServiceType() {}
    virtual ~MidiServiceType() {}

    virtual Array<MidiDeviceInfo> getAvailableDevices (bool) = 0;
    virtual MidiDeviceInfo getDefaultDevice (bool) = 0;

    virtual InputWrapper*  createInputWrapper  (MidiInput&, const String&, MidiInputCallback&) = 0;
    virtual OutputWrapper* createOutputWrapper (const String&) = 0;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiServiceType)
};

//==============================================================================
struct Win32MidiService  : public MidiServiceType,
                           private Timer
{
    Win32MidiService() {}

    Array<MidiDeviceInfo> getAvailableDevices (bool isInput) override
    {
        return isInput ? Win32InputWrapper::getAvailableDevices()
                       : Win32OutputWrapper::getAvailableDevices();
    }

    MidiDeviceInfo getDefaultDevice (bool isInput) override
    {
        return isInput ? Win32InputWrapper::getDefaultDevice()
                       : Win32OutputWrapper::getDefaultDevice();
    }

    InputWrapper* createInputWrapper (MidiInput& input, const String& deviceIdentifier, MidiInputCallback& callback) override
    {
        return new Win32InputWrapper (*this, input, deviceIdentifier, callback);
    }

    OutputWrapper* createOutputWrapper (const String& deviceIdentifier) override
    {
        return new Win32OutputWrapper (*this, deviceIdentifier);
    }

private:
    struct Win32InputWrapper;

    //==============================================================================
    struct MidiInCollector  : public ReferenceCountedObject
    {
        MidiInCollector (Win32MidiService& s, MidiDeviceInfo d)
            : deviceInfo (d), midiService (s)
        {
        }

        ~MidiInCollector()
        {
            stop();

            if (deviceHandle != 0)
            {
                for (int count = 5; --count >= 0;)
                {
                    if (midiInClose (deviceHandle) == MMSYSERR_NOERROR)
                        break;

                    Sleep (20);
                }
            }
        }

        using Ptr = ReferenceCountedObjectPtr<MidiInCollector>;

        void addClient (Win32InputWrapper* c)
        {
            const ScopedLock sl (clientLock);
            jassert (! clients.contains (c));
            clients.add (c);
        }

        void removeClient (Win32InputWrapper* c)
        {
            const ScopedLock sl (clientLock);
            clients.removeFirstMatchingValue (c);
            startOrStop();
            midiService.asyncCheckForUnusedCollectors();
        }

        void handleMessage (const uint8* bytes, uint32 timeStamp)
        {
            if (bytes[0] >= 0x80 && isStarted.load())
            {
                {
                    auto len = MidiMessage::getMessageLengthFromFirstByte (bytes[0]);
                    auto time = convertTimeStamp (timeStamp);
                    const ScopedLock sl (clientLock);

                    for (auto* c : clients)
                        c->pushMidiData (bytes, len, time);
                }

                writeFinishedBlocks();
            }
        }

        void handleSysEx (MIDIHDR* hdr, uint32 timeStamp)
        {
            if (isStarted.load() && hdr->dwBytesRecorded > 0)
            {
                {
                    auto time = convertTimeStamp (timeStamp);
                    const ScopedLock sl (clientLock);

                    for (auto* c : clients)
                        c->pushMidiData (hdr->lpData, (int) hdr->dwBytesRecorded, time);
                }

                writeFinishedBlocks();
            }
        }

        void startOrStop()
        {
            const ScopedLock sl (clientLock);

            if (countRunningClients() == 0)
                stop();
            else
                start();
        }

        void start()
        {
            if (deviceHandle != 0 && ! isStarted.load())
            {
                activeMidiCollectors.addIfNotAlreadyThere (this);

                for (int i = 0; i < (int) numHeaders; ++i)
                {
                    headers[i].prepare (deviceHandle);
                    headers[i].write (deviceHandle);
                }

                startTime = Time::getMillisecondCounterHiRes();
                auto res = midiInStart (deviceHandle);

                if (res == MMSYSERR_NOERROR)
                    isStarted = true;
                else
                    unprepareAllHeaders();
            }
        }

        void stop()
        {
            if (isStarted.load())
            {
                isStarted = false;
                midiInReset (deviceHandle);
                midiInStop (deviceHandle);
                activeMidiCollectors.removeFirstMatchingValue (this);
                unprepareAllHeaders();
            }
        }

        static void CALLBACK midiInCallback (HMIDIIN, UINT uMsg, DWORD_PTR dwInstance,
                                             DWORD_PTR midiMessage, DWORD_PTR timeStamp)
        {
            auto* collector = reinterpret_cast<MidiInCollector*> (dwInstance);

            // This is primarily a check for the collector being a dangling
            // pointer, as the callback can sometimes be delayed
            if (activeMidiCollectors.contains (collector))
            {
                if (uMsg == MIM_DATA)
                    collector->handleMessage ((const uint8*) &midiMessage, (uint32) timeStamp);
                else if (uMsg == MIM_LONGDATA)
                    collector->handleSysEx ((MIDIHDR*) midiMessage, (uint32) timeStamp);
            }
        }

        MidiDeviceInfo deviceInfo;
        HMIDIIN deviceHandle = 0;

    private:
        Win32MidiService& midiService;
        CriticalSection clientLock;
        Array<Win32InputWrapper*> clients;
        std::atomic<bool> isStarted { false };
        double startTime = 0;

        // This static array is used to prevent occasional callbacks to objects that are
        // in the process of being deleted
        static Array<MidiInCollector*, CriticalSection> activeMidiCollectors;

        int countRunningClients() const
        {
            int num = 0;

            for (auto* c : clients)
                if (c->started)
                    ++num;

            return num;
        }

        struct MidiHeader
        {
            MidiHeader() {}

            void prepare (HMIDIIN device)
            {
                zerostruct (hdr);
                hdr.lpData = data;
                hdr.dwBufferLength = (DWORD) numElementsInArray (data);

                midiInPrepareHeader (device, &hdr, sizeof (hdr));
            }

            void unprepare (HMIDIIN device)
            {
                if ((hdr.dwFlags & WHDR_DONE) != 0)
                {
                    int c = 10;
                    while (--c >= 0 && midiInUnprepareHeader (device, &hdr, sizeof (hdr)) == MIDIERR_STILLPLAYING)
                        Thread::sleep (20);

                    jassert (c >= 0);
                }
            }

            void write (HMIDIIN device)
            {
                hdr.dwBytesRecorded = 0;
                midiInAddBuffer (device, &hdr, sizeof (hdr));
            }

            void writeIfFinished (HMIDIIN device)
            {
                if ((hdr.dwFlags & WHDR_DONE) != 0)
                    write (device);
            }

            MIDIHDR hdr;
            char data[256];

            JUCE_DECLARE_NON_COPYABLE (MidiHeader)
        };

        enum { numHeaders = 32 };
        MidiHeader headers[numHeaders];

        void writeFinishedBlocks()
        {
            for (int i = 0; i < (int) numHeaders; ++i)
                headers[i].writeIfFinished (deviceHandle);
        }

        void unprepareAllHeaders()
        {
            for (int i = 0; i < (int) numHeaders; ++i)
                headers[i].unprepare (deviceHandle);
        }

        double convertTimeStamp (uint32 timeStamp)
        {
            auto t = startTime + timeStamp;
            auto now = Time::getMillisecondCounterHiRes();

            if (t > now)
            {
                if (t > now + 2.0)
                    startTime -= 1.0;

                t = now;
            }

            return t * 0.001;
        }

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiInCollector)
    };

    //==============================================================================
    template<class WrapperType>
    struct Win32MidiDeviceQuery
    {
        static Array<MidiDeviceInfo> getAvailableDevices()
        {
            StringArray deviceNames, deviceIDs;
            auto deviceCaps = WrapperType::getDeviceCaps();

            for (int i = 0; i < deviceCaps.size(); ++i)
            {
                deviceNames.add (deviceCaps[i].szPname);

                auto identifier = getInterfaceIDForDevice ((UINT) i);

                if (identifier.isNotEmpty())
                    deviceIDs.add (identifier);
                else
                    deviceIDs.add (deviceNames[i]);
            }

            deviceNames.appendNumbersToDuplicates (false, false, CharPointer_UTF8 ("-"), CharPointer_UTF8 (""));
            deviceIDs  .appendNumbersToDuplicates (false, false, CharPointer_UTF8 ("-"), CharPointer_UTF8 (""));

            Array<MidiDeviceInfo> devices;

            for (int i = 0; i < deviceNames.size(); ++i)
                devices.add ({ deviceNames[i], deviceIDs[i] });

            return devices;
        }

    private:
        static String getInterfaceIDForDevice (UINT id)
        {
            ULONG size = 0;

            if (WrapperType::sendMidiMessage ((UINT_PTR) id, DRV_QUERYDEVICEINTERFACESIZE, (DWORD_PTR) &size, 0) == MMSYSERR_NOERROR)
            {
                WCHAR interfaceName[512] = {};

                if (isPositiveAndBelow (size, sizeof (interfaceName))
                    && WrapperType::sendMidiMessage ((UINT_PTR) id, DRV_QUERYDEVICEINTERFACE,
                                                     (DWORD_PTR) interfaceName, sizeof (interfaceName)) == MMSYSERR_NOERROR)
                {
                    return interfaceName;
                }
            }

            return {};
        }
    };

    struct Win32InputWrapper  : public InputWrapper,
                                public Win32MidiDeviceQuery<Win32InputWrapper>
    {
        Win32InputWrapper (Win32MidiService& parentService, MidiInput& midiInput, const String& deviceIdentifier, MidiInputCallback& c)
            : input (midiInput), callback (c)
        {
            collector = getOrCreateCollector (parentService, deviceIdentifier);
            collector->addClient (this);
        }

        ~Win32InputWrapper()
        {
            collector->removeClient (this);
        }

        static MidiInCollector::Ptr getOrCreateCollector (Win32MidiService& parentService, const String& deviceIdentifier)
        {
            UINT deviceID = MIDI_MAPPER;
            String deviceName;
            auto devices = getAvailableDevices();

            for (int i = 0; i < devices.size(); ++i)
            {
                auto d = devices.getUnchecked (i);

                if (d.identifier == deviceIdentifier)
                {
                    deviceID = i;
                    deviceName = d.name;
                    break;
                }
            }

            const ScopedLock sl (parentService.activeCollectorLock);

            for (auto& c : parentService.activeCollectors)
                if (c->deviceInfo.identifier == deviceIdentifier)
                    return c;

            MidiInCollector::Ptr c (new MidiInCollector (parentService, { deviceName, deviceIdentifier }));

            HMIDIIN h;
            auto err = midiInOpen (&h, deviceID,
                                   (DWORD_PTR) &MidiInCollector::midiInCallback,
                                   (DWORD_PTR) (MidiInCollector*) c.get(),
                                   CALLBACK_FUNCTION);

            if (err != MMSYSERR_NOERROR)
                throw std::runtime_error ("Failed to create Windows input device wrapper");

            c->deviceHandle = h;
            parentService.activeCollectors.add (c);
            return c;
        }

        static DWORD sendMidiMessage (UINT_PTR deviceID, UINT msg, DWORD_PTR arg1, DWORD_PTR arg2)
        {
            return midiInMessage ((HMIDIIN) deviceID, msg, arg1, arg2);
        }

        static Array<MIDIINCAPS> getDeviceCaps()
        {
            Array<MIDIINCAPS> devices;

            for (UINT i = 0; i < midiInGetNumDevs(); ++i)
            {
                MIDIINCAPS mc = {};

                if (midiInGetDevCaps (i, &mc, sizeof (mc)) == MMSYSERR_NOERROR)
                    devices.add (mc);
            }

            return devices;
        }

        static MidiDeviceInfo getDefaultDevice()  { return getAvailableDevices().getFirst(); }

        void start() override   { started = true;  concatenator.reset(); collector->startOrStop(); }
        void stop() override    { started = false; collector->startOrStop(); concatenator.reset(); }

        String getDeviceIdentifier() override   { return collector->deviceInfo.identifier; }
        String getDeviceName() override         { return collector->deviceInfo.name; }

        void pushMidiData (const void* inputData, int numBytes, double time)
        {
            concatenator.pushMidiData (inputData, numBytes, time, &input, callback);
        }

        MidiInput& input;
        MidiInputCallback& callback;
        MidiDataConcatenator concatenator { 4096 };
        MidiInCollector::Ptr collector;
        bool started = false;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Win32InputWrapper)
    };

    //==============================================================================
    struct MidiOutHandle    : public ReferenceCountedObject
    {
        using Ptr = ReferenceCountedObjectPtr<MidiOutHandle>;

        MidiOutHandle (Win32MidiService& parent, MidiDeviceInfo d, HMIDIOUT h)
            : owner (parent), deviceInfo (d), handle (h)
        {
            owner.activeOutputHandles.add (this);
        }

        ~MidiOutHandle()
        {
            if (handle != nullptr)
                midiOutClose (handle);

            owner.activeOutputHandles.removeFirstMatchingValue (this);
        }

        Win32MidiService& owner;
        MidiDeviceInfo deviceInfo;
        HMIDIOUT handle;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiOutHandle)
    };

    //==============================================================================
    struct Win32OutputWrapper  : public OutputWrapper,
                                 public Win32MidiDeviceQuery<Win32OutputWrapper>
    {
        Win32OutputWrapper (Win32MidiService& p, const String& deviceIdentifier)
            : parent (p)
        {
            auto devices = getAvailableDevices();
            UINT deviceID = MIDI_MAPPER;
            String deviceName;

            for (int i = 0; i < devices.size(); ++i)
            {
                auto d = devices.getUnchecked (i);

                if (d.identifier == deviceIdentifier)
                {
                    deviceID = i;
                    deviceName = d.name;
                    break;
                }
            }

            if (deviceID == MIDI_MAPPER)
            {
                // use the microsoft sw synth as a default - best not to allow deviceID
                // to be MIDI_MAPPER, or else device sharing breaks
                for (int i = 0; i < devices.size(); ++i)
                    if (devices[i].name.containsIgnoreCase ("microsoft"))
                        deviceID = (UINT) i;
            }

            for (int i = parent.activeOutputHandles.size(); --i >= 0;)
            {
                auto* activeHandle = parent.activeOutputHandles.getUnchecked (i);

                if (activeHandle->deviceInfo.identifier == deviceIdentifier)
                {
                    han = activeHandle;
                    return;
                }
            }

            for (int i = 4; --i >= 0;)
            {
                HMIDIOUT h = 0;
                auto res = midiOutOpen (&h, deviceID, 0, 0, CALLBACK_NULL);

                if (res == MMSYSERR_NOERROR)
                {
                    han = new MidiOutHandle (parent, { deviceName, deviceIdentifier }, h);
                    return;
                }

                if (res == MMSYSERR_ALLOCATED)
                    Sleep (100);
                else
                    break;
            }

            throw std::runtime_error ("Failed to create Windows output device wrapper");
        }

        void sendMessageNow (const MidiMessage& message) override
        {
            if (message.getRawDataSize() > 3 || message.isSysEx())
            {
                MIDIHDR h = {};

                h.lpData = (char*) message.getRawData();
                h.dwBytesRecorded = h.dwBufferLength  = (DWORD) message.getRawDataSize();

                if (midiOutPrepareHeader (han->handle, &h, sizeof (MIDIHDR)) == MMSYSERR_NOERROR)
                {
                    auto res = midiOutLongMsg (han->handle, &h, sizeof (MIDIHDR));

                    if (res == MMSYSERR_NOERROR)
                    {
                        while ((h.dwFlags & MHDR_DONE) == 0)
                            Sleep (1);

                        int count = 500; // 1 sec timeout

                        while (--count >= 0)
                        {
                            res = midiOutUnprepareHeader (han->handle, &h, sizeof (MIDIHDR));

                            if (res == MIDIERR_STILLPLAYING)
                                Sleep (2);
                            else
                                break;
                        }
                    }
                }
            }
            else
            {
                for (int i = 0; i < 50; ++i)
                {
                    if (midiOutShortMsg (han->handle, *(unsigned int*) message.getRawData()) != MIDIERR_NOTREADY)
                        break;

                    Sleep (1);
                }
            }
        }

        static DWORD sendMidiMessage (UINT_PTR deviceID, UINT msg, DWORD_PTR arg1, DWORD_PTR arg2)
        {
            return midiOutMessage ((HMIDIOUT) deviceID, msg, arg1, arg2);
        }

        static Array<MIDIOUTCAPS> getDeviceCaps()
        {
            Array<MIDIOUTCAPS> devices;

            for (UINT i = 0; i < midiOutGetNumDevs(); ++i)
            {
                MIDIOUTCAPS mc = {};

                if (midiOutGetDevCaps (i, &mc, sizeof (mc)) == MMSYSERR_NOERROR)
                    devices.add (mc);
            }

            return devices;
        }

        static MidiDeviceInfo getDefaultDevice()
        {
            auto defaultIndex = []()
            {
                auto deviceCaps = getDeviceCaps();

                for (int i = 0; i < deviceCaps.size(); ++i)
                    if ((deviceCaps[i].wTechnology & MOD_MAPPER) != 0)
                        return i;

                return 0;
            }();

            return getAvailableDevices()[defaultIndex];
        }

        String getDeviceIdentifier() override   { return han->deviceInfo.identifier; }
        String getDeviceName() override         { return han->deviceInfo.name; }

        Win32MidiService& parent;
        MidiOutHandle::Ptr han;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Win32OutputWrapper)
    };

    //==============================================================================
    void asyncCheckForUnusedCollectors()
    {
        startTimer (10);
    }

    void timerCallback() override
    {
        stopTimer();

        const ScopedLock sl (activeCollectorLock);

        for (int i = activeCollectors.size(); --i >= 0;)
            if (activeCollectors.getObjectPointer(i)->getReferenceCount() == 1)
                activeCollectors.remove (i);
    }

    CriticalSection activeCollectorLock;
    ReferenceCountedArray<MidiInCollector> activeCollectors;
    Array<MidiOutHandle*> activeOutputHandles;
};

Array<Win32MidiService::MidiInCollector*, CriticalSection> Win32MidiService::MidiInCollector::activeMidiCollectors;

//==============================================================================
//==============================================================================
#if JUCE_USE_WINRT_MIDI

#ifndef JUCE_FORCE_WINRT_MIDI
 #define JUCE_FORCE_WINRT_MIDI 0
#endif

#ifndef JUCE_WINRT_MIDI_LOGGING
 #define JUCE_WINRT_MIDI_LOGGING 0
#endif

#if JUCE_WINRT_MIDI_LOGGING
 #define JUCE_WINRT_MIDI_LOG(x)  DBG(x)
#else
 #define JUCE_WINRT_MIDI_LOG(x)
#endif

using namespace Microsoft::WRL;

using namespace ABI::Windows::Foundation;
using namespace ABI::Windows::Foundation::Collections;
using namespace ABI::Windows::Devices::Midi;
using namespace ABI::Windows::Devices::Enumeration;
using namespace ABI::Windows::Storage::Streams;

//==============================================================================
struct WinRTMidiService  : public MidiServiceType
{
public:
    //==============================================================================
    WinRTMidiService()
    {
        auto* wrtWrapper = WinRTWrapper::getInstance();

        if (! wrtWrapper->isInitialised())
            throw std::runtime_error ("Failed to initialise the WinRT wrapper");

        midiInFactory = wrtWrapper->getWRLFactory<IMidiInPortStatics> (&RuntimeClass_Windows_Devices_Midi_MidiInPort[0]);

        if (midiInFactory == nullptr)
            throw std::runtime_error ("Failed to create midi in factory");

        midiOutFactory = wrtWrapper->getWRLFactory<IMidiOutPortStatics> (&RuntimeClass_Windows_Devices_Midi_MidiOutPort[0]);

        if (midiOutFactory == nullptr)
            throw std::runtime_error ("Failed to create midi out factory");

        // The WinRT BLE MIDI API doesn't provide callbacks when devices become disconnected,
        // but it does require a disconnection via the API before a device will reconnect again.
        // We can monitor the BLE connection state of paired devices to get callbacks when
        // connections are broken.
        bleDeviceWatcher.reset (new BLEDeviceWatcher());

        if (! bleDeviceWatcher->start())
            throw std::runtime_error ("Failed to start the BLE device watcher");

        inputDeviceWatcher.reset (new MidiIODeviceWatcher<IMidiInPortStatics> (midiInFactory));

        if (! inputDeviceWatcher->start())
            throw std::runtime_error ("Failed to start the midi input device watcher");

        outputDeviceWatcher.reset (new MidiIODeviceWatcher<IMidiOutPortStatics> (midiOutFactory));

        if (! outputDeviceWatcher->start())
            throw std::runtime_error ("Failed to start the midi output device watcher");
    }

    Array<MidiDeviceInfo> getAvailableDevices (bool isInput) override
    {
        return isInput ? inputDeviceWatcher ->getAvailableDevices()
                       : outputDeviceWatcher->getAvailableDevices();
    }

    MidiDeviceInfo getDefaultDevice (bool isInput) override
    {
        return isInput ? inputDeviceWatcher ->getDefaultDevice()
                       : outputDeviceWatcher->getDefaultDevice();
    }

    InputWrapper* createInputWrapper (MidiInput& input, const String& deviceIdentifier, MidiInputCallback& callback) override
    {
        return new WinRTInputWrapper (*this, input, deviceIdentifier, callback);
    }

    OutputWrapper* createOutputWrapper (const String& deviceIdentifier) override
    {
        return new WinRTOutputWrapper (*this, deviceIdentifier);
    }

private:
    //==============================================================================
    class DeviceCallbackHandler
    {
    public:
        virtual ~DeviceCallbackHandler() {};

        virtual HRESULT addDevice (IDeviceInformation*) = 0;
        virtual HRESULT removeDevice (IDeviceInformationUpdate*) = 0;
        virtual HRESULT updateDevice (IDeviceInformationUpdate*) = 0;

        bool attach (HSTRING deviceSelector, DeviceInformationKind infoKind)
        {
            auto* wrtWrapper = WinRTWrapper::getInstanceWithoutCreating();

            if (wrtWrapper == nullptr)
            {
                JUCE_WINRT_MIDI_LOG ("Failed to get the WinRTWrapper singleton!");
                return false;
            }

            auto deviceInfoFactory = wrtWrapper->getWRLFactory<IDeviceInformationStatics2> (&RuntimeClass_Windows_Devices_Enumeration_DeviceInformation[0]);

            if (deviceInfoFactory == nullptr)
                return false;

            // A quick way of getting an IVector<HSTRING>...
            auto requestedProperties = [wrtWrapper]
            {
                auto devicePicker = wrtWrapper->activateInstance<IDevicePicker> (&RuntimeClass_Windows_Devices_Enumeration_DevicePicker[0],
                                                                                 __uuidof (IDevicePicker));
                jassert (devicePicker != nullptr);

                IVector<HSTRING>* result;
                auto hr = devicePicker->get_RequestedProperties (&result);
                jassert (SUCCEEDED (hr));

                hr = result->Clear();
                jassert (SUCCEEDED (hr));

                return result;
            }();

            StringArray propertyKeys ("System.Devices.ContainerId",
                                      "System.Devices.Aep.ContainerId",
                                      "System.Devices.Aep.IsConnected");

            for (auto& key : propertyKeys)
            {
                WinRTWrapper::ScopedHString hstr (key);
                auto hr = requestedProperties->Append (hstr.get());

                if (FAILED (hr))
                {
                    jassertfalse;
                    return false;
                }
            }

            WinRTWrapper::ComPtr<IIterable<HSTRING>> iter;
            auto hr = requestedProperties->QueryInterface (__uuidof (IIterable<HSTRING>), (void**) iter.resetAndGetPointerAddress());

            if (FAILED (hr))
            {
                jassertfalse;
                return false;
            }

            hr = deviceInfoFactory->CreateWatcherWithKindAqsFilterAndAdditionalProperties (deviceSelector, iter, infoKind,
                                                                                           watcher.resetAndGetPointerAddress());

            if (FAILED (hr))
            {
                jassertfalse;
                return false;
            }

            enumerationThread.startThread();

            return true;
        };

        void detach()
        {
            enumerationThread.stopThread (2000);

            if (watcher == nullptr)
                return;

            auto hr = watcher->Stop();
            jassert (SUCCEEDED (hr));

            if (deviceAddedToken.value != 0)
            {
                hr = watcher->remove_Added (deviceAddedToken);
                jassert (SUCCEEDED (hr));
                deviceAddedToken.value = 0;
            }

            if (deviceUpdatedToken.value != 0)
            {
                hr = watcher->remove_Updated (deviceUpdatedToken);
                jassert (SUCCEEDED (hr));
                deviceUpdatedToken.value = 0;
            }

            if (deviceRemovedToken.value != 0)
            {
                hr = watcher->remove_Removed (deviceRemovedToken);
                jassert (SUCCEEDED (hr));
                deviceRemovedToken.value = 0;
            }

            watcher = nullptr;
        }

        template<typename InfoType>
        IInspectable* getValueFromDeviceInfo (String key, InfoType* info)
        {
            __FIMapView_2_HSTRING_IInspectable* properties;
            info->get_Properties (&properties);

            boolean found = false;
            WinRTWrapper::ScopedHString keyHstr (key);
            auto hr = properties->HasKey (keyHstr.get(), &found);

            if (FAILED (hr))
            {
                jassertfalse;
                return nullptr;
            }

            if (! found)
                return nullptr;

            IInspectable* inspectable;
            hr = properties->Lookup (keyHstr.get(), &inspectable);

            if (FAILED (hr))
            {
                jassertfalse;
                return nullptr;
            }

            return inspectable;
        }

        String getGUIDFromInspectable (IInspectable& inspectable)
        {
            WinRTWrapper::ComPtr<IReference<GUID>> guidRef;
            auto hr = inspectable.QueryInterface (__uuidof (IReference<GUID>),
                                                  (void**) guidRef.resetAndGetPointerAddress());

            if (FAILED (hr))
            {
                jassertfalse;
                return {};
            }

            GUID result;
            hr = guidRef->get_Value (&result);

            if (FAILED (hr))
            {
                jassertfalse;
                return {};
            }

            OLECHAR* resultString;
            StringFromCLSID (result, &resultString);

            return resultString;
        }

        bool getBoolFromInspectable (IInspectable& inspectable)
        {
            WinRTWrapper::ComPtr<IReference<bool>> boolRef;
            auto hr = inspectable.QueryInterface (__uuidof (IReference<bool>),
                                                  (void**) boolRef.resetAndGetPointerAddress());

            if (FAILED (hr))
            {
                jassertfalse;
                return false;
            }

            boolean result;
            hr = boolRef->get_Value (&result);

            if (FAILED (hr))
            {
                jassertfalse;
                return false;
            }

            return result;
        }

    private:
        //==============================================================================
        struct DeviceEnumerationThread   : public Thread
        {
            DeviceEnumerationThread (DeviceCallbackHandler& h,
                                     WinRTWrapper::ComPtr<IDeviceWatcher>& w,
                                     EventRegistrationToken& added,
                                     EventRegistrationToken& removed,
                                     EventRegistrationToken& updated)
                    : Thread ("WinRT Device Enumeration Thread"), handler (h), watcher (w),
                      deviceAddedToken (added), deviceRemovedToken (removed), deviceUpdatedToken (updated)
            {}

            void run() override
            {
                auto handlerPtr = std::addressof (handler);

                watcher->add_Added (
                    Callback<ITypedEventHandler<DeviceWatcher*, DeviceInformation*>> (
                        [handlerPtr] (IDeviceWatcher*, IDeviceInformation* info) { return handlerPtr->addDevice (info); }
                    ).Get(),
                    &deviceAddedToken);

                watcher->add_Removed (
                    Callback<ITypedEventHandler<DeviceWatcher*, DeviceInformationUpdate*>> (
                        [handlerPtr] (IDeviceWatcher*, IDeviceInformationUpdate* infoUpdate) { return handlerPtr->removeDevice (infoUpdate); }
                    ).Get(),
                    &deviceRemovedToken);

                watcher->add_Updated (
                    Callback<ITypedEventHandler<DeviceWatcher*, DeviceInformationUpdate*>> (
                        [handlerPtr] (IDeviceWatcher*, IDeviceInformationUpdate* infoUpdate) { return handlerPtr->updateDevice (infoUpdate); }
                    ).Get(),
                    &deviceUpdatedToken);

                watcher->Start();
            }

            DeviceCallbackHandler& handler;
            WinRTWrapper::ComPtr<IDeviceWatcher>& watcher;
            EventRegistrationToken& deviceAddedToken, deviceRemovedToken, deviceUpdatedToken;
        };

        //==============================================================================
        WinRTWrapper::ComPtr<IDeviceWatcher> watcher;

        EventRegistrationToken deviceAddedToken   { 0 },
                               deviceRemovedToken { 0 },
                               deviceUpdatedToken { 0 };

        DeviceEnumerationThread enumerationThread { *this, watcher,
                                                    deviceAddedToken,
                                                    deviceRemovedToken,
                                                    deviceUpdatedToken };
    };

    //==============================================================================
    struct BLEDeviceWatcher final   : private DeviceCallbackHandler
    {
        struct DeviceInfo
        {
            String containerID;
            bool isConnected = false;
        };

        BLEDeviceWatcher() = default;

        ~BLEDeviceWatcher()
        {
            detach();
        }

        //==============================================================================
        HRESULT addDevice (IDeviceInformation* addedDeviceInfo) override
        {
            HSTRING deviceIDHst;
            auto hr = addedDeviceInfo->get_Id (&deviceIDHst);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to query added BLE device ID!");
                return S_OK;
            }

            auto* wrtWrapper = WinRTWrapper::getInstanceWithoutCreating();

            if (wrtWrapper == nullptr)
            {
                JUCE_WINRT_MIDI_LOG ("Failed to get the WinRTWrapper singleton!");
                return false;
            }

            auto deviceID = wrtWrapper->hStringToString (deviceIDHst);
            JUCE_WINRT_MIDI_LOG ("Detected paired BLE device: " << deviceID);

            if (auto* containerIDValue = getValueFromDeviceInfo ("System.Devices.Aep.ContainerId", addedDeviceInfo))
            {
                auto containerID = getGUIDFromInspectable (*containerIDValue);

                if (containerID.isNotEmpty())
                {
                    DeviceInfo info = { containerID };

                    if (auto* connectedValue = getValueFromDeviceInfo ("System.Devices.Aep.IsConnected", addedDeviceInfo))
                        info.isConnected = getBoolFromInspectable (*connectedValue);

                    JUCE_WINRT_MIDI_LOG ("Adding BLE device: " << deviceID << " " << info.containerID
                                         << " " << (info.isConnected ? "connected" : "disconnected"));
                    devices.set (deviceID, info);

                    return S_OK;
                }
            }

            JUCE_WINRT_MIDI_LOG ("Failed to get a container ID for BLE device: " << deviceID);
            return S_OK;
        }

        HRESULT removeDevice (IDeviceInformationUpdate* removedDeviceInfo) override
        {
            HSTRING removedDeviceIdHstr;
            auto hr = removedDeviceInfo->get_Id (&removedDeviceIdHstr);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to query removed BLE device ID!");
                return S_OK;
            }

            auto* wrtWrapper = WinRTWrapper::getInstanceWithoutCreating();

            if (wrtWrapper == nullptr)
            {
                JUCE_WINRT_MIDI_LOG ("Failed to get the WinRTWrapper singleton!");
                return false;
            }

            auto removedDeviceId = wrtWrapper->hStringToString (removedDeviceIdHstr);

            JUCE_WINRT_MIDI_LOG ("Removing BLE device: " << removedDeviceId);

            {
                const ScopedLock lock (deviceChanges);

                if (devices.contains (removedDeviceId))
                {
                    auto& info = devices.getReference (removedDeviceId);
                    listeners.call ([&info] (Listener& l) { l.bleDeviceDisconnected (info.containerID); });
                    devices.remove (removedDeviceId);
                    JUCE_WINRT_MIDI_LOG ("Removed BLE device: " << removedDeviceId);
                }
            }

            return S_OK;
        }

        HRESULT updateDevice (IDeviceInformationUpdate* updatedDeviceInfo) override
        {
            HSTRING updatedDeviceIdHstr;
            auto hr = updatedDeviceInfo->get_Id (&updatedDeviceIdHstr);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to query updated BLE device ID!");
                return S_OK;
            }

            auto* wrtWrapper = WinRTWrapper::getInstanceWithoutCreating();

            if (wrtWrapper == nullptr)
            {
                JUCE_WINRT_MIDI_LOG ("Failed to get the WinRTWrapper singleton!");
                return false;
            }

            auto updatedDeviceId = wrtWrapper->hStringToString (updatedDeviceIdHstr);

            JUCE_WINRT_MIDI_LOG ("Updating BLE device: " << updatedDeviceId);

            if (auto* connectedValue = getValueFromDeviceInfo ("System.Devices.Aep.IsConnected", updatedDeviceInfo))
            {
                auto isConnected = getBoolFromInspectable (*connectedValue);

                {
                    const ScopedLock lock (deviceChanges);

                    if (! devices.contains (updatedDeviceId))
                        return S_OK;

                    auto& info = devices.getReference (updatedDeviceId);

                    if (info.isConnected && ! isConnected)
                    {
                        JUCE_WINRT_MIDI_LOG ("BLE device connection status change: " << updatedDeviceId << " " << info.containerID << " " << (isConnected ? "connected" : "disconnected"));
                        listeners.call ([&info] (Listener& l) { l.bleDeviceDisconnected (info.containerID); });
                    }

                    info.isConnected = isConnected;
                }
            }

            return S_OK;
        }

        //==============================================================================
        bool start()
        {
            WinRTWrapper::ScopedHString deviceSelector ("System.Devices.Aep.ProtocolId:=\"{bb7bb05e-5972-42b5-94fc-76eaa7084d49}\""
                                                        " AND System.Devices.Aep.IsPaired:=System.StructuredQueryType.Boolean#True");
            return attach (deviceSelector.get(), DeviceInformationKind::DeviceInformationKind_AssociationEndpoint);
        }

        //==============================================================================
        struct Listener
        {
            virtual ~Listener() {};
            virtual void bleDeviceAdded (const String& containerID) = 0;
            virtual void bleDeviceDisconnected (const String& containerID) = 0;
        };

        void addListener (Listener* l)
        {
            listeners.add (l);
        }

        void removeListener (Listener* l)
        {
            listeners.remove (l);
        }

        //==============================================================================
        ListenerList<Listener> listeners;
        HashMap<String, DeviceInfo> devices;
        CriticalSection deviceChanges;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BLEDeviceWatcher);
    };

    //==============================================================================
    struct WinRTMIDIDeviceInfo
    {
        String deviceID, containerID, name;
        bool isDefault = false;
    };

    //==============================================================================
    template <typename COMFactoryType>
    struct MidiIODeviceWatcher final   : private DeviceCallbackHandler
    {
        MidiIODeviceWatcher (WinRTWrapper::ComPtr<COMFactoryType>& comFactory)
            : factory (comFactory)
        {
        }

        ~MidiIODeviceWatcher()
        {
            detach();
        }

        HRESULT addDevice (IDeviceInformation* addedDeviceInfo) override
        {
            WinRTMIDIDeviceInfo info;

            HSTRING deviceID;
            auto hr = addedDeviceInfo->get_Id (&deviceID);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to query added MIDI device ID!");
                return S_OK;
            }

            auto* wrtWrapper = WinRTWrapper::getInstanceWithoutCreating();

            if (wrtWrapper == nullptr)
            {
                JUCE_WINRT_MIDI_LOG ("Failed to get the WinRTWrapper singleton!");
                return false;
            }

            info.deviceID = wrtWrapper->hStringToString (deviceID);

            JUCE_WINRT_MIDI_LOG ("Detected MIDI device: " << info.deviceID);

            boolean isEnabled = false;
            hr = addedDeviceInfo->get_IsEnabled (&isEnabled);

            if (FAILED (hr) || ! isEnabled)
            {
                JUCE_WINRT_MIDI_LOG ("MIDI device not enabled: " << info.deviceID);
                return S_OK;
            }

            // We use the container ID to match a MIDI device with a generic BLE device, if possible
            if (auto* containerIDValue = getValueFromDeviceInfo ("System.Devices.ContainerId", addedDeviceInfo))
                info.containerID = getGUIDFromInspectable (*containerIDValue);

            HSTRING name;
            hr = addedDeviceInfo->get_Name (&name);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to query detected MIDI device name for " << info.deviceID);
                return S_OK;
            }

            info.name = wrtWrapper->hStringToString (name);

            boolean isDefault = false;
            hr = addedDeviceInfo->get_IsDefault (&isDefault);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to query detected MIDI device defaultness for " << info.deviceID << " " << info.name);
                return S_OK;
            }

            info.isDefault = isDefault;

            JUCE_WINRT_MIDI_LOG ("Adding MIDI device: " << info.deviceID << " " << info.containerID << " " << info.name);

            {
                const ScopedLock lock (deviceChanges);
                connectedDevices.add (info);
            }

            return S_OK;
        }

        HRESULT removeDevice (IDeviceInformationUpdate* removedDeviceInfo) override
        {
            HSTRING removedDeviceIdHstr;
            auto hr = removedDeviceInfo->get_Id (&removedDeviceIdHstr);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to query removed MIDI device ID!");
                return S_OK;
            }

            auto* wrtWrapper = WinRTWrapper::getInstanceWithoutCreating();

            if (wrtWrapper == nullptr)
            {
                JUCE_WINRT_MIDI_LOG ("Failed to get the WinRTWrapper singleton!");
                return false;
            }

            auto removedDeviceId = wrtWrapper->hStringToString (removedDeviceIdHstr);

            JUCE_WINRT_MIDI_LOG ("Removing MIDI device: " << removedDeviceId);

            {
                const ScopedLock lock (deviceChanges);

                for (int i = 0; i < connectedDevices.size(); ++i)
                {
                    if (connectedDevices[i].deviceID == removedDeviceId)
                    {
                        connectedDevices.remove (i);
                        JUCE_WINRT_MIDI_LOG ("Removed MIDI device: " << removedDeviceId);
                        break;
                    }
                }
            }

            return S_OK;
        }

        // This is never called
        HRESULT updateDevice (IDeviceInformationUpdate*) override   { return S_OK; }

        bool start()
        {
            HSTRING deviceSelector;
            auto hr = factory->GetDeviceSelector (&deviceSelector);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to get MIDI device selector!");
                return false;
            }

            return attach (deviceSelector, DeviceInformationKind::DeviceInformationKind_DeviceInterface);
        }

        Array<MidiDeviceInfo> getAvailableDevices()
        {
            {
                const ScopedLock lock (deviceChanges);
                lastQueriedConnectedDevices = connectedDevices;
            }

            StringArray deviceNames, deviceIDs;

            for (auto info : lastQueriedConnectedDevices.get())
            {
                deviceNames.add (info.name);
                deviceIDs  .add (info.containerID);
            }

            deviceNames.appendNumbersToDuplicates (false, false, CharPointer_UTF8 ("-"), CharPointer_UTF8 (""));
            deviceIDs  .appendNumbersToDuplicates (false, false, CharPointer_UTF8 ("-"), CharPointer_UTF8 (""));

            Array<MidiDeviceInfo> devices;

            for (int i = 0; i < deviceNames.size(); ++i)
                devices.add ({ deviceNames[i], deviceIDs[i] });

            return devices;
        }

        MidiDeviceInfo getDefaultDevice()
        {
            auto& lastDevices = lastQueriedConnectedDevices.get();

            for (auto& d : lastDevices)
                if (d.isDefault)
                    return { d.name, d.containerID };

            return {};
        }

        WinRTMIDIDeviceInfo getWinRTDeviceInfoForDevice (const String& deviceIdentifier)
        {
            auto devices = getAvailableDevices();

            for (int i = 0; i < devices.size(); ++i)
                if (devices.getUnchecked (i).identifier == deviceIdentifier)
                    return lastQueriedConnectedDevices.get()[i];

            return {};
        }

        WinRTWrapper::ComPtr<COMFactoryType>& factory;

        Array<WinRTMIDIDeviceInfo> connectedDevices;
        CriticalSection deviceChanges;
        ThreadLocalValue<Array<WinRTMIDIDeviceInfo>> lastQueriedConnectedDevices;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiIODeviceWatcher);
    };

    //==============================================================================
    template <typename COMFactoryType, typename COMInterfaceType, typename COMType>
    struct OpenMidiPortThread  : public Thread
    {
        OpenMidiPortThread (String threadName, String midiDeviceID,
                            WinRTWrapper::ComPtr<COMFactoryType>& comFactory,
                            WinRTWrapper::ComPtr<COMInterfaceType>& comPort)
            : Thread (threadName),
              deviceID (midiDeviceID),
              factory (comFactory),
              port (comPort)
        {
        }

        ~OpenMidiPortThread()
        {
            stopThread (2000);
        }

        void run() override
        {
            WinRTWrapper::ScopedHString hDeviceId (deviceID);
            WinRTWrapper::ComPtr<IAsyncOperation<COMType*>> asyncOp;
            auto hr = factory->FromIdAsync (hDeviceId.get(), asyncOp.resetAndGetPointerAddress());

            if (FAILED (hr))
                return;

            hr = asyncOp->put_Completed (Callback<IAsyncOperationCompletedHandler<COMType*>> (
                [this] (IAsyncOperation<COMType*>* asyncOpPtr, AsyncStatus)
                {
                    if (asyncOpPtr == nullptr)
                        return E_ABORT;

                    auto hr = asyncOpPtr->GetResults (port.resetAndGetPointerAddress());

                    if (FAILED (hr))
                        return hr;

                    portOpened.signal();
                    return S_OK;
                }
            ).Get());

            // We need to use a timeout here, rather than waiting indefinitely, as the
            // WinRT API can occasionally hang!
            portOpened.wait (2000);
        }

        const String deviceID;
        WinRTWrapper::ComPtr<COMFactoryType>& factory;
        WinRTWrapper::ComPtr<COMInterfaceType>& port;
        WaitableEvent portOpened { true };
    };

    //==============================================================================
    template <typename MIDIIOStaticsType, typename MIDIPort>
    class WinRTIOWrapper   : private BLEDeviceWatcher::Listener
    {
    public:
        WinRTIOWrapper (BLEDeviceWatcher& bleWatcher,
                        MidiIODeviceWatcher<MIDIIOStaticsType>& midiDeviceWatcher,
                        const String& deviceIdentifier)
            : bleDeviceWatcher (bleWatcher)
        {
            {
                const ScopedLock lock (midiDeviceWatcher.deviceChanges);
                deviceInfo = midiDeviceWatcher.getWinRTDeviceInfoForDevice (deviceIdentifier);
            }

            if (deviceInfo.deviceID.isEmpty())
                throw std::runtime_error ("Invalid device index");

            JUCE_WINRT_MIDI_LOG ("Creating JUCE MIDI IO: " << deviceInfo.deviceID);

            if (deviceInfo.containerID.isNotEmpty())
            {
                bleDeviceWatcher.addListener (this);

                const ScopedLock lock (bleDeviceWatcher.deviceChanges);

                HashMap<String, BLEDeviceWatcher::DeviceInfo>::Iterator iter (bleDeviceWatcher.devices);

                while (iter.next())
                {
                    if (iter.getValue().containerID == deviceInfo.containerID)
                    {
                        isBLEDevice = true;
                        break;
                    }
                }
            }
        }

        virtual ~WinRTIOWrapper()
        {
            bleDeviceWatcher.removeListener (this);

            disconnect();
        }

        //==============================================================================
        virtual void disconnect()
        {
            if (midiPort != nullptr)
            {
                if (isBLEDevice)
                    midiPort->Release();
            }

            midiPort = nullptr;
        }

    private:
        //==============================================================================
        void bleDeviceAdded (const String& containerID) override
        {
            if (containerID == deviceInfo.containerID)
                isBLEDevice = true;
        }

        void bleDeviceDisconnected (const String& containerID) override
        {
            if (containerID == deviceInfo.containerID)
            {
                JUCE_WINRT_MIDI_LOG ("Disconnecting MIDI port from BLE disconnection: " << deviceInfo.deviceID
                                     << " " << deviceInfo.containerID << " " << deviceInfo.name);
                disconnect();
            }
        }

    protected:
        //==============================================================================
        BLEDeviceWatcher& bleDeviceWatcher;
        WinRTMIDIDeviceInfo deviceInfo;
        bool isBLEDevice = false;
        WinRTWrapper::ComPtr<MIDIPort> midiPort;
    };

    //==============================================================================
    struct WinRTInputWrapper final  : public InputWrapper,
                                      private WinRTIOWrapper<IMidiInPortStatics, IMidiInPort>

    {
        WinRTInputWrapper (WinRTMidiService& service, MidiInput& input, const String& deviceIdentifier, MidiInputCallback& cb)
            : WinRTIOWrapper <IMidiInPortStatics, IMidiInPort> (*service.bleDeviceWatcher, *service.inputDeviceWatcher, deviceIdentifier),
              inputDevice (input),
              callback (cb)
        {
            OpenMidiPortThread<IMidiInPortStatics, IMidiInPort, MidiInPort> portThread ("Open WinRT MIDI input port",
                                                                                        deviceInfo.deviceID,
                                                                                        service.midiInFactory,
                                                                                        midiPort);
            portThread.startThread();
            portThread.waitForThreadToExit (-1);

            if (midiPort == nullptr)
            {
                JUCE_WINRT_MIDI_LOG ("Timed out waiting for midi input port creation");
                return;
            }

            startTime = Time::getMillisecondCounterHiRes();

            auto hr = midiPort->add_MessageReceived (
                Callback<ITypedEventHandler<MidiInPort*, MidiMessageReceivedEventArgs*>> (
                    [this] (IMidiInPort*, IMidiMessageReceivedEventArgs* args) { return midiInMessageReceived (args); }
                ).Get(),
                &midiInMessageToken);

            if (FAILED (hr))
            {
                JUCE_WINRT_MIDI_LOG ("Failed to set MIDI input callback");
                jassertfalse;
            }
        }

        ~WinRTInputWrapper()
        {
            disconnect();
        }

        //==============================================================================
        void start() override
        {
            if (! isStarted)
            {
                concatenator.reset();
                isStarted = true;
            }
        }

        void stop() override
        {
            if (isStarted)
            {
                isStarted = false;
                concatenator.reset();
            }
        }

        String getDeviceIdentifier() override    { return deviceInfo.containerID; }
        String getDeviceName() override          { return deviceInfo.name; }

        //==============================================================================
        void disconnect() override
        {
            stop();

            if (midiPort != nullptr && midiInMessageToken.value != 0)
                midiPort->remove_MessageReceived (midiInMessageToken);

            WinRTIOWrapper<IMidiInPortStatics, IMidiInPort>::disconnect();
        }

        //==============================================================================
        HRESULT midiInMessageReceived (IMidiMessageReceivedEventArgs* args)
        {
            if (! isStarted)
                return S_OK;

            WinRTWrapper::ComPtr<IMidiMessage> message;
            auto hr = args->get_Message (message.resetAndGetPointerAddress());

            if (FAILED (hr))
                return hr;

            WinRTWrapper::ComPtr<IBuffer> buffer;
            hr = message->get_RawData (buffer.resetAndGetPointerAddress());

            if (FAILED (hr))
                return hr;

            WinRTWrapper::ComPtr<Windows::Storage::Streams::IBufferByteAccess> bufferByteAccess;
            hr = buffer->QueryInterface (bufferByteAccess.resetAndGetPointerAddress());

            if (FAILED (hr))
                return hr;

            uint8_t* bufferData = nullptr;
            hr = bufferByteAccess->Buffer (&bufferData);

            if (FAILED (hr))
                return hr;

            uint32_t numBytes = 0;
            hr = buffer->get_Length (&numBytes);

            if (FAILED (hr))
                return hr;

            ABI::Windows::Foundation::TimeSpan timespan;
            hr = message->get_Timestamp (&timespan);

            if (FAILED (hr))
                return hr;

            concatenator.pushMidiData (bufferData, numBytes,
                                       convertTimeStamp (timespan.Duration),
                                       &inputDevice, callback);
            return S_OK;
        }

        double convertTimeStamp (int64 timestamp)
        {
            auto millisecondsSinceStart = static_cast<double> (timestamp) / 10000.0;
            auto t = startTime + millisecondsSinceStart;
            auto now = Time::getMillisecondCounterHiRes();

            if (t > now)
            {
                if (t > now + 2.0)
                    startTime -= 1.0;

                t = now;
            }

            return t * 0.001;
        }

        //==============================================================================
        MidiInput& inputDevice;
        MidiInputCallback& callback;

        MidiDataConcatenator concatenator { 4096 };
        EventRegistrationToken midiInMessageToken { 0 };

        double startTime = 0;
        bool isStarted = false;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WinRTInputWrapper);
    };

    //==============================================================================
    struct WinRTOutputWrapper final  : public OutputWrapper,
                                       private WinRTIOWrapper <IMidiOutPortStatics, IMidiOutPort>
    {
        WinRTOutputWrapper (WinRTMidiService& service, const String& deviceIdentifier)
            : WinRTIOWrapper <IMidiOutPortStatics, IMidiOutPort> (*service.bleDeviceWatcher, *service.outputDeviceWatcher, deviceIdentifier)
        {
            OpenMidiPortThread<IMidiOutPortStatics, IMidiOutPort, IMidiOutPort> portThread ("Open WinRT MIDI output port",
                                                                                            deviceInfo.deviceID,
                                                                                            service.midiOutFactory,
                                                                                            midiPort);
            portThread.startThread();
            portThread.waitForThreadToExit (-1);

            if (midiPort == nullptr)
                throw std::runtime_error ("Timed out waiting for midi output port creation");

            auto* wrtWrapper = WinRTWrapper::getInstanceWithoutCreating();

            if (wrtWrapper == nullptr)
                throw std::runtime_error ("Failed to get the WinRTWrapper singleton!");

            auto bufferFactory = wrtWrapper->getWRLFactory<IBufferFactory> (&RuntimeClass_Windows_Storage_Streams_Buffer[0]);

            if (bufferFactory == nullptr)
                throw std::runtime_error ("Failed to create output buffer factory");

            auto hr = bufferFactory->Create (static_cast<UINT32> (65536), buffer.resetAndGetPointerAddress());

            if (FAILED (hr))
                throw std::runtime_error ("Failed to create output buffer");

            hr = buffer->QueryInterface (bufferByteAccess.resetAndGetPointerAddress());

            if (FAILED (hr))
                throw std::runtime_error ("Failed to get buffer byte access");

            hr = bufferByteAccess->Buffer (&bufferData);

            if (FAILED (hr))
                throw std::runtime_error ("Failed to get buffer data pointer");
        }

        //==============================================================================
        void sendMessageNow (const MidiMessage& message) override
        {
            if (midiPort == nullptr)
                return;

            auto numBytes = message.getRawDataSize();
            auto hr = buffer->put_Length (numBytes);

            if (FAILED (hr))
            {
                jassertfalse;
                return;
            }

            memcpy_s (bufferData, numBytes, message.getRawData(), numBytes);
            midiPort->SendBuffer (buffer);
        }

        String getDeviceIdentifier() override    { return deviceInfo.containerID; }
        String getDeviceName() override          { return deviceInfo.name; }

        //==============================================================================
        WinRTWrapper::ComPtr<IBuffer> buffer;
        WinRTWrapper::ComPtr<Windows::Storage::Streams::IBufferByteAccess> bufferByteAccess;
        uint8_t* bufferData = nullptr;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WinRTOutputWrapper);
    };

    WinRTWrapper::ComPtr<IMidiInPortStatics>  midiInFactory;
    WinRTWrapper::ComPtr<IMidiOutPortStatics> midiOutFactory;

    std::unique_ptr<MidiIODeviceWatcher<IMidiInPortStatics>>  inputDeviceWatcher;
    std::unique_ptr<MidiIODeviceWatcher<IMidiOutPortStatics>> outputDeviceWatcher;
    std::unique_ptr<BLEDeviceWatcher> bleDeviceWatcher;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WinRTMidiService)
};

#endif   // JUCE_USE_WINRT_MIDI

//==============================================================================
//==============================================================================
#if ! JUCE_MINGW
 extern RTL_OSVERSIONINFOW getWindowsVersionInfo();
#endif

struct MidiService :  public DeletedAtShutdown
{
    MidiService()
    {
      #if JUCE_USE_WINRT_MIDI && ! JUCE_MINGW
       #if ! JUCE_FORCE_WINRT_MIDI
        auto windowsVersionInfo = getWindowsVersionInfo();
        if (windowsVersionInfo.dwMajorVersion >= 10 && windowsVersionInfo.dwBuildNumber >= 17763)
       #endif
        {
            try
            {
                internal.reset (new WinRTMidiService());
                return;
            }
            catch (std::runtime_error&) {}
        }
      #endif

        internal.reset (new Win32MidiService());
    }

    ~MidiService()
    {
        clearSingletonInstance();
    }

    static MidiServiceType& getService()
    {
        jassert (getInstance()->internal != nullptr);
        return *getInstance()->internal.get();
    }

    JUCE_DECLARE_SINGLETON (MidiService, false)

private:
    std::unique_ptr<MidiServiceType> internal;
};

JUCE_IMPLEMENT_SINGLETON (MidiService)

//==============================================================================
static int findDefaultDeviceIndex (const Array<MidiDeviceInfo>& available, const MidiDeviceInfo& defaultDevice)
{
    for (int i = 0; i < available.size(); ++i)
        if (available.getUnchecked (i) == defaultDevice)
            return i;

    return 0;
}

Array<MidiDeviceInfo> MidiInput::getAvailableDevices()
{
    return MidiService::getService().getAvailableDevices (true);
}

MidiDeviceInfo MidiInput::getDefaultDevice()
{
    return MidiService::getService().getDefaultDevice (true);
}

std::unique_ptr<MidiInput> MidiInput::openDevice (const String& deviceIdentifier, MidiInputCallback* callback)
{
    if (deviceIdentifier.isEmpty() || callback == nullptr)
        return {};

    std::unique_ptr<MidiInput> in (new MidiInput ({}, deviceIdentifier));
    std::unique_ptr<MidiServiceType::InputWrapper> wrapper;

    try
    {
        wrapper.reset (MidiService::getService().createInputWrapper (*in, deviceIdentifier, *callback));
    }
    catch (std::runtime_error&)
    {
        return {};
    }

    in->setName (wrapper->getDeviceName());
    in->internal = wrapper.release();

    return in;
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
    return findDefaultDeviceIndex (getAvailableDevices(), getDefaultDevice());
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
    delete static_cast<MidiServiceType::InputWrapper*> (internal);
}

void MidiInput::start()   { static_cast<MidiServiceType::InputWrapper*> (internal)->start(); }
void MidiInput::stop()    { static_cast<MidiServiceType::InputWrapper*> (internal)->stop(); }

//==============================================================================
Array<MidiDeviceInfo> MidiOutput::getAvailableDevices()
{
    return MidiService::getService().getAvailableDevices (false);
}

MidiDeviceInfo MidiOutput::getDefaultDevice()
{
    return MidiService::getService().getDefaultDevice (false);
}

std::unique_ptr<MidiOutput> MidiOutput::openDevice (const String& deviceIdentifier)
{
    if (deviceIdentifier.isEmpty())
        return {};

    std::unique_ptr<MidiServiceType::OutputWrapper> wrapper;

    try
    {
        wrapper.reset (MidiService::getService().createOutputWrapper (deviceIdentifier));
    }
    catch (std::runtime_error&)
    {
        return {};
    }

    std::unique_ptr<MidiOutput> out;
    out.reset (new MidiOutput (wrapper->getDeviceName(), deviceIdentifier));

    out->internal = wrapper.release();

    return out;
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
    return findDefaultDeviceIndex (getAvailableDevices(), getDefaultDevice());
}

std::unique_ptr<MidiOutput> MidiOutput::openDevice (int index)
{
    return openDevice (getAvailableDevices()[index].identifier);
}

MidiOutput::~MidiOutput()
{
    stopBackgroundThread();
    delete static_cast<MidiServiceType::OutputWrapper*> (internal);
}

void MidiOutput::sendMessageNow (const MidiMessage& message)
{
    static_cast<MidiServiceType::OutputWrapper*> (internal)->sendMessageNow (message);
}

} // namespace juce
