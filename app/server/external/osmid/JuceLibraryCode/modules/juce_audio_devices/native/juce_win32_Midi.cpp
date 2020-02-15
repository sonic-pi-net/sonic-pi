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

struct MidiServiceType
{
    struct InputWrapper
    {
        virtual ~InputWrapper() {}

        virtual String getDeviceName() = 0;
        virtual void start() = 0;
        virtual void stop() = 0;
    };

    struct OutputWrapper
    {
        virtual ~OutputWrapper() {}

        virtual String getDeviceName() = 0;
        virtual void sendMessageNow (const MidiMessage&) = 0;
    };

    MidiServiceType() {}
    virtual ~MidiServiceType() {}

    virtual StringArray getDevices (bool) = 0;
    virtual int getDefaultDeviceIndex (bool) = 0;

    virtual InputWrapper* createInputWrapper (MidiInput*, int, MidiInputCallback*) = 0;
    virtual OutputWrapper* createOutputWrapper (int) = 0;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiServiceType)
};

//==============================================================================
class WindowsMidiService  : public MidiServiceType
{
private:
    struct WindowsInputWrapper  : public InputWrapper
    {
        struct MidiInCollector
        {
            MidiInCollector (WindowsMidiService& s,
                             MidiInput* const inputDevice,
                             MidiInputCallback& cb)
                : midiService (s),
                  input (inputDevice),
                  callback (cb)
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

            void handleMessage (const uint8* bytes, const uint32 timeStamp)
            {
                if (bytes[0] >= 0x80 && isStarted)
                {
                    concatenator.pushMidiData (bytes,
                                               MidiMessage::getMessageLengthFromFirstByte (bytes[0]),
                                               convertTimeStamp (timeStamp),
                                               input,
                                               callback);
                    writeFinishedBlocks();
                }
            }

            void handleSysEx (MIDIHDR* const hdr, const uint32 timeStamp)
            {
                if (isStarted && hdr->dwBytesRecorded > 0)
                {
                    concatenator.pushMidiData (hdr->lpData, (int) hdr->dwBytesRecorded,
                                               convertTimeStamp (timeStamp), input, callback);
                    writeFinishedBlocks();
                }
            }

            void start()
            {
                if (deviceHandle != 0 && ! isStarted)
                {
                    midiService.activeMidiCollectors.addIfNotAlreadyThere (this);

                    for (int i = 0; i < (int) numHeaders; ++i)
                    {
                        headers[i].prepare (deviceHandle);
                        headers[i].write (deviceHandle);
                    }

                    startTime = Time::getMillisecondCounterHiRes();
                    MMRESULT res = midiInStart (deviceHandle);

                    if (res == MMSYSERR_NOERROR)
                    {
                        concatenator.reset();
                        isStarted = true;
                    }
                    else
                    {
                        unprepareAllHeaders();
                    }
                }
            }

            void stop()
            {
                if (isStarted)
                {
                    isStarted = false;
                    midiInReset (deviceHandle);
                    midiInStop (deviceHandle);
                    midiService.activeMidiCollectors.removeFirstMatchingValue (this);
                    unprepareAllHeaders();
                    concatenator.reset();
                }
            }

            static void CALLBACK midiInCallback (HMIDIIN, UINT uMsg, DWORD_PTR dwInstance,
                                                 DWORD_PTR midiMessage, DWORD_PTR timeStamp)
            {
                auto* collector = reinterpret_cast<MidiInCollector*> (dwInstance);

                if (collector->midiService.activeMidiCollectors.contains (collector))
                {
                    if (uMsg == MIM_DATA)
                        collector->handleMessage ((const uint8*) &midiMessage, (uint32) timeStamp);
                    else if (uMsg == MIM_LONGDATA)
                        collector->handleSysEx ((MIDIHDR*) midiMessage, (uint32) timeStamp);
                }
            }

            HMIDIIN deviceHandle = 0;

        private:
            WindowsMidiService& midiService;
            MidiInput* input;
            MidiInputCallback& callback;
            MidiDataConcatenator concatenator { 4096 };
            bool volatile isStarted = false;
            double startTime = 0;

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
                char data [256];

                JUCE_DECLARE_NON_COPYABLE (MidiHeader)
            };

            enum { numHeaders = 32 };
            MidiHeader headers [numHeaders];

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
        WindowsInputWrapper (WindowsMidiService& parentService,
                             MidiInput* const input,
                             const int index,
                             MidiInputCallback* const callback)
        {
            auto names = getDevices();
            UINT deviceId = MIDI_MAPPER;

            if (isPositiveAndBelow (index, names.size()))
            {
                deviceName = names[index];
                deviceId = index;
            }

            collector = new MidiInCollector (parentService, input, *callback);

            HMIDIIN h;
            MMRESULT err = midiInOpen (&h, deviceId,
                                       (DWORD_PTR) &MidiInCollector::midiInCallback,
                                       (DWORD_PTR) (MidiInCollector*) collector.get(),
                                       CALLBACK_FUNCTION);

            if (err != MMSYSERR_NOERROR)
                throw std::runtime_error ("Failed to create Windows input device wrapper");

            collector->deviceHandle = h;
        }

        ~WindowsInputWrapper() {}

        static StringArray getDevices()
        {
            StringArray s;
            const UINT num = midiInGetNumDevs();

            for (UINT i = 0; i < num; ++i)
            {
                MIDIINCAPS mc = { 0 };

                if (midiInGetDevCaps (i, &mc, sizeof (mc)) == MMSYSERR_NOERROR)
                    s.add (String (mc.szPname, (size_t) numElementsInArray (mc.szPname)));
            }

            s.appendNumbersToDuplicates (false, false, CharPointer_UTF8 ("-"), CharPointer_UTF8 (""));
            return s;
        }

        static int getDefaultDeviceIndex()
        {
            return 0;
        }

        void start() override   { collector->start(); }
        void stop() override    { collector->stop(); }

        String getDeviceName() override
        {
            return deviceName;
        }

        String deviceName;
        ScopedPointer<MidiInCollector> collector;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WindowsInputWrapper)
    };

    //==============================================================================
    struct WindowsOutputWrapper  : public OutputWrapper
    {
        struct MidiOutHandle
        {
            int refCount;
            UINT deviceId;
            HMIDIOUT handle;

            JUCE_LEAK_DETECTOR (MidiOutHandle)
        };

        WindowsOutputWrapper (WindowsMidiService& p, int index) : parent (p)
        {
            auto names = getDevices();
            UINT deviceId = MIDI_MAPPER;

            if (isPositiveAndBelow (index, names.size()))
            {
                deviceName = names[index];
                deviceId = index;
            }

            if (deviceId == MIDI_MAPPER)
            {
                // use the microsoft sw synth as a default - best not to allow deviceId
                // to be MIDI_MAPPER, or else device sharing breaks
                for (int i = 0; i < names.size(); ++i)
                    if (names[i].containsIgnoreCase ("microsoft"))
                        deviceId = (UINT) i;
            }

            for (int i = parent.activeOutputHandles.size(); --i >= 0;)
            {
                auto* activeHandle = parent.activeOutputHandles.getUnchecked (i);

                if (activeHandle->deviceId == deviceId)
                {
                    activeHandle->refCount++;
                    han = activeHandle;
                    return;
                }
            }

            for (int i = 4; --i >= 0;)
            {
                HMIDIOUT h = 0;
                MMRESULT res = midiOutOpen (&h, deviceId, 0, 0, CALLBACK_NULL);

                if (res == MMSYSERR_NOERROR)
                {
                    han = new MidiOutHandle();
                    han->deviceId = deviceId;
                    han->refCount = 1;
                    han->handle = h;
                    parent.activeOutputHandles.add (han);
                    return;
                }

                if (res == MMSYSERR_ALLOCATED)
                    Sleep (100);
                else
                    break;
            }

            throw std::runtime_error ("Failed to create Windows output device wrapper");
        }

        ~WindowsOutputWrapper()
        {
            if (parent.activeOutputHandles.contains (han.get()) && --(han->refCount) == 0)
            {
                midiOutClose (han->handle);
                parent.activeOutputHandles.removeFirstMatchingValue (han.get());
            }
        }

        void sendMessageNow (const MidiMessage& message) override
        {
            if (message.getRawDataSize() > 3 || message.isSysEx())
            {
                MIDIHDR h = { 0 };

                h.lpData = (char*) message.getRawData();
                h.dwBytesRecorded = h.dwBufferLength  = (DWORD) message.getRawDataSize();

                if (midiOutPrepareHeader (han->handle, &h, sizeof (MIDIHDR)) == MMSYSERR_NOERROR)
                {
                    MMRESULT res = midiOutLongMsg (han->handle, &h, sizeof (MIDIHDR));

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

        static Array<MIDIOUTCAPS> getDeviceCaps()
        {
            Array<MIDIOUTCAPS> devices;
            const UINT num = midiOutGetNumDevs();

            for (UINT i = 0; i < num; ++i)
            {
                MIDIOUTCAPS mc = { 0 };

                if (midiOutGetDevCaps (i, &mc, sizeof (mc)) == MMSYSERR_NOERROR)
                    devices.add (mc);
            }

            return devices;
        }

        static StringArray getDevices()
        {
            StringArray s;

            for (auto& mc : getDeviceCaps())
                s.add (String (mc.szPname, (size_t) numElementsInArray (mc.szPname)));

            s.appendNumbersToDuplicates (false, false, CharPointer_UTF8 ("-"), CharPointer_UTF8 (""));
            return s;
        }

        static int getDefaultDeviceIndex()
        {
            int n = 0;

            for (auto& mc : getDeviceCaps())
            {
                if ((mc.wTechnology & MOD_MAPPER) != 0)
                    return n;

                ++n;
            }

            return 0;
        }

        String getDeviceName() override
        {
            return deviceName;
        }

        WindowsMidiService& parent;
        String deviceName;

        ScopedPointer<MidiOutHandle> han;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WindowsOutputWrapper)
    };

public:
    WindowsMidiService() {}

    StringArray getDevices (bool isInput) override
    {
        return isInput ? WindowsInputWrapper::getDevices()
                       : WindowsOutputWrapper::getDevices();
    }

    int getDefaultDeviceIndex (bool isInput) override
    {
        return isInput ? WindowsInputWrapper::getDefaultDeviceIndex()
                       : WindowsOutputWrapper::getDefaultDeviceIndex();
    }

    InputWrapper* createInputWrapper (MidiInput* input, int index, MidiInputCallback* callback) override
    {
        return new WindowsInputWrapper (*this, input, index, callback);
    }

    OutputWrapper* createOutputWrapper (int index) override
    {
        return new WindowsOutputWrapper (*this, index);
    }

private:
    Array<WindowsInputWrapper::MidiInCollector*, CriticalSection> activeMidiCollectors;
    Array<WindowsOutputWrapper::MidiOutHandle*> activeOutputHandles;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WindowsMidiService)
};

//==============================================================================
#if JUCE_USE_WINRT_MIDI

using namespace Microsoft::WRL;

using namespace ABI::Windows::Foundation;
using namespace ABI::Windows::Devices::Midi;
using namespace ABI::Windows::Devices::Enumeration;
using namespace ABI::Windows::Storage::Streams;

class WinRTMidiService  : public MidiServiceType
{
private:
    template <typename COMFactoryType>
    struct MidiIODeviceWatcher
    {
        struct DeviceInfo
        {
            String name;
            String id;
            bool isDefault = false;
        };

        MidiIODeviceWatcher (ComSmartPtr<COMFactoryType>& comFactory)
            : factory (comFactory)
        {
        }

        ~MidiIODeviceWatcher()
        {
            stop();
        }

        bool start()
        {
            HSTRING deviceSelector;
            HRESULT hr = factory->GetDeviceSelector (&deviceSelector);
            if (FAILED (hr))
                return false;

            auto deviceInformationFactory = WinRTWrapper::getInstance()->getWRLFactory<IDeviceInformationStatics> (&RuntimeClass_Windows_Devices_Enumeration_DeviceInformation[0]);
            if (deviceInformationFactory == nullptr)
                return false;

            hr = deviceInformationFactory->CreateWatcherAqsFilter (deviceSelector, watcher.resetAndGetPointerAddress());
            if (FAILED (hr))
                return false;

            class DeviceEnumerationThread  : public Thread
            {
            public:
                DeviceEnumerationThread (String threadName, MidiIODeviceWatcher<COMFactoryType>& p)
                    : Thread (threadName), parent (p)
                {}

                void run() override
                {
                    auto parentPtr = &parent;

                    parent.watcher->add_Added (
                        Callback<ITypedEventHandler<DeviceWatcher*, DeviceInformation*>> (
                            [parentPtr](IDeviceWatcher*, IDeviceInformation* info) { return parentPtr->addDevice (info); }
                        ).Get(),
                        &parent.deviceAddedToken);

                    parent.watcher->add_Removed (
                        Callback<ITypedEventHandler<DeviceWatcher*, DeviceInformationUpdate*>> (
                            [parentPtr](IDeviceWatcher*, IDeviceInformationUpdate* info) { return parentPtr->removeDevice (info); }
                        ).Get(),
                        &parent.deviceRemovedToken);

                    EventRegistrationToken deviceEnumerationCompletedToken { 0 };
                    parent.watcher->add_EnumerationCompleted (
                        Callback<ITypedEventHandler<DeviceWatcher*, IInspectable*>> (
                            [this](IDeviceWatcher*, IInspectable*) { enumerationCompleted.signal(); return S_OK; }
                        ).Get(),
                        &deviceEnumerationCompletedToken);

                    parent.watcher->Start();
                    enumerationCompleted.wait();

                    if (deviceEnumerationCompletedToken.value != 0)
                        parent.watcher->remove_EnumerationCompleted (deviceEnumerationCompletedToken);
                }

            private:
                MidiIODeviceWatcher<COMFactoryType>& parent;
                WaitableEvent enumerationCompleted;
            };

            DeviceEnumerationThread enumerationThread ("WinRT Device Enumeration Thread", *this);
            enumerationThread.startThread();
            enumerationThread.waitForThreadToExit (4000);

            return true;
        }

        bool stop()
        {
            if (watcher == nullptr)
                return true;

            if (deviceAddedToken.value != 0)
            {
                HRESULT hr = watcher->remove_Added (deviceAddedToken);
                if (FAILED (hr))
                    return false;

                deviceAddedToken.value = 0;
            }

            if (deviceRemovedToken.value != 0)
            {
                HRESULT hr = watcher->remove_Removed (deviceRemovedToken);
                if (FAILED (hr))
                    return false;

                deviceRemovedToken.value = 0;
            }

            HRESULT hr = watcher->Stop();
            if (FAILED (hr))
                return false;

            watcher = nullptr;
            return true;
        }

        HRESULT addDevice (IDeviceInformation* addedDeviceInfo)
        {
            boolean isEnabled;
            HRESULT hr = addedDeviceInfo->get_IsEnabled (&isEnabled);
            if (FAILED (hr))
                return S_OK;

            if (! isEnabled)
                return S_OK;

            const ScopedLock lock (deviceChanges);

            DeviceInfo info;

            HSTRING name;
            hr = addedDeviceInfo->get_Name (&name);
            if (FAILED (hr))
                return S_OK;

            info.name = WinRTWrapper::getInstance()->hStringToString (name);

            HSTRING id;
            hr = addedDeviceInfo->get_Id (&id);
            if (FAILED (hr))
                return S_OK;

            info.id = WinRTWrapper::getInstance()->hStringToString (id);

            boolean isDefault;
            hr = addedDeviceInfo->get_IsDefault (&isDefault);
            if (FAILED (hr))
                return S_OK;

            info.isDefault = isDefault != 0;

            connectedDevices.add (info);

            return S_OK;
        }

        HRESULT removeDevice (IDeviceInformationUpdate* removedDeviceInfo)
        {
            const ScopedLock lock (deviceChanges);

            HSTRING removedDeviceIdHstr;
            removedDeviceInfo->get_Id (&removedDeviceIdHstr);
            String removedDeviceId = WinRTWrapper::getInstance()->hStringToString (removedDeviceIdHstr);

            for (int i = 0; i < connectedDevices.size(); ++i)
            {
                if (connectedDevices[i].id == removedDeviceId)
                {
                    connectedDevices.remove (i);
                    break;
                }
            }

            return S_OK;
        }

        StringArray getDevices()
        {
            {
                const ScopedLock lock (deviceChanges);
                lastQueriedConnectedDevices = connectedDevices;
            }

            StringArray result;
            for (auto info : lastQueriedConnectedDevices.get())
                result.add (info.name);

            return result;
        }

        int getDefaultDeviceIndex()
        {
            auto& lastDevices = lastQueriedConnectedDevices.get();
            for (int i = 0; i < lastDevices.size(); ++i)
                if (lastDevices[i].isDefault)
                    return i;

            return 0;
        }

        String getDeviceNameFromIndex (const int index)
        {
            if (isPositiveAndBelow (index, lastQueriedConnectedDevices.get().size()))
                return lastQueriedConnectedDevices.get()[index].name;

            return {};
        }

        String getDeviceID (const String name)
        {
            const ScopedLock lock (deviceChanges);

            for (auto info : connectedDevices)
                if (info.name == name)
                    return info.id;

            return {};
        }

        ComSmartPtr<COMFactoryType>& factory;

        EventRegistrationToken deviceAddedToken   { 0 },
                               deviceRemovedToken { 0 };

        ComSmartPtr<IDeviceWatcher> watcher;

        Array<DeviceInfo> connectedDevices;
        CriticalSection deviceChanges;
        ThreadLocalValue<Array<DeviceInfo>> lastQueriedConnectedDevices;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiIODeviceWatcher);
    };

    template <typename COMFactoryType, typename COMInterfaceType, typename COMType>
    class OpenMidiPortThread  : public Thread
    {
    public:
        OpenMidiPortThread (String threadName,
                            String midiDeviceId,
                            ComSmartPtr<COMFactoryType>& comFactory,
                            ComSmartPtr<COMInterfaceType>& comPort)
            : Thread (threadName),
              deviceId (midiDeviceId),
              factory (comFactory),
              port (comPort)
        {
        }

        ~OpenMidiPortThread()
        {
        }

        void run() override
        {
            WinRTWrapper::ScopedHString hDeviceId (deviceId);
            ComSmartPtr<IAsyncOperation<COMType*>> asyncOp;
            HRESULT hr = factory->FromIdAsync (hDeviceId.get(), asyncOp.resetAndGetPointerAddress());
            if (FAILED (hr))
                return;

            hr = asyncOp->put_Completed (Callback<IAsyncOperationCompletedHandler<COMType*>> (
                [this] (IAsyncOperation<COMType*>* asyncOpPtr, AsyncStatus)
                {
                    if (asyncOpPtr == nullptr)
                        return E_ABORT;

                    HRESULT hr = asyncOpPtr->GetResults (port.resetAndGetPointerAddress());
                    if (FAILED (hr))
                        return hr;

                    portOpened.signal();
                    return S_OK;
                }
            ).Get());

            // When using Bluetooth the asynchronous port opening operation will occasionally
            // hang, so we use a timeout. We will be able to remove this when Microsoft
            // improves the Bluetooth MIDI stack.
            portOpened.wait (2000);
        }

        const String deviceId;
        ComSmartPtr<COMFactoryType>& factory;
        ComSmartPtr<COMInterfaceType>& port;

        WaitableEvent portOpened { true };
    };

    struct WinRTInputWrapper  : public InputWrapper
    {
        WinRTInputWrapper (WinRTMidiService& service,
                           MidiInput* const input,
                           const int index,
                           MidiInputCallback& cb)
            : inputDevice (input),
              callback (cb),
              concatenator (4096)
        {
            const ScopedLock lock (service.inputDeviceWatcher->deviceChanges);

            deviceName = service.inputDeviceWatcher->getDeviceNameFromIndex (index);
            if (deviceName.isEmpty())
                throw std::runtime_error ("Invalid device index");

            const auto deviceID = service.inputDeviceWatcher->getDeviceID (deviceName);
            if (deviceID.isEmpty())
                throw std::runtime_error ("Device unavailable");

            OpenMidiPortThread<IMidiInPortStatics, IMidiInPort, MidiInPort> portThread ("Open WinRT MIDI input port",
                                                                                        deviceID,
                                                                                        service.midiInFactory,
                                                                                        midiInPort);
            portThread.startThread();
            portThread.waitForThreadToExit (-1);
            if (midiInPort == nullptr)
                throw std::runtime_error ("Timed out waiting for midi input port creation");

            startTime = Time::getMillisecondCounterHiRes();

            HRESULT hr = midiInPort->add_MessageReceived (
                Callback<ITypedEventHandler<MidiInPort*, MidiMessageReceivedEventArgs*>> (
                    [this] (IMidiInPort*, IMidiMessageReceivedEventArgs* args) { return midiInMessageReceived (args); }
                ).Get(),
                &midiInMessageToken);
            if (FAILED (hr))
                throw std::runtime_error ("Failed to set midi input callback");
        }

        ~WinRTInputWrapper()
        {
            if (midiInMessageToken.value != 0)
                midiInPort->remove_MessageReceived (midiInMessageToken);

            midiInPort = nullptr;
        }

        void start() override
        {
            if (!isStarted)
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

        String getDeviceName() override
        {
            return deviceName;
        }

        HRESULT midiInMessageReceived (IMidiMessageReceivedEventArgs* args)
        {
            if (! isStarted)
                return S_OK;

            ComSmartPtr<IMidiMessage> message;
            HRESULT hr = args->get_Message (message.resetAndGetPointerAddress());
            if (FAILED (hr))
                return hr;

            ComSmartPtr<IBuffer> buffer;
            hr = message->get_RawData (buffer.resetAndGetPointerAddress());
            if (FAILED (hr))
                return hr;

            ComSmartPtr<Windows::Storage::Streams::IBufferByteAccess> bufferByteAccess;
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

            concatenator.pushMidiData (bufferData,
                                       numBytes,
                                       convertTimeStamp (timespan.Duration),
                                       inputDevice,
                                       callback);

            return S_OK;
        }

        double convertTimeStamp (int64 timestamp)
        {
            const auto millisecondsSinceStart = static_cast<double> (timestamp) / 10000.0;
            double t = startTime + millisecondsSinceStart;

            const double now = Time::getMillisecondCounterHiRes();
            if (t > now)
            {
                if (t > now + 2.0)
                    startTime -= 1.0;

                t = now;
            }

            return t * 0.001;
        }

        MidiInput* inputDevice;
        MidiInputCallback& callback;
        String deviceName;
        MidiDataConcatenator concatenator;
        ComSmartPtr<IMidiInPort> midiInPort;
        EventRegistrationToken midiInMessageToken { 0 };

        double startTime = 0;
        bool isStarted = false;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WinRTInputWrapper);
    };

    struct WinRTOutputWrapper  : public OutputWrapper
    {
        WinRTOutputWrapper (WinRTMidiService& service, const int index)
        {
            const ScopedLock lock (service.outputDeviceWatcher->deviceChanges);

            deviceName = service.outputDeviceWatcher->getDeviceNameFromIndex (index);
            if (deviceName.isEmpty())
                throw std::runtime_error ("Invalid device index");

            const auto deviceID = service.outputDeviceWatcher->getDeviceID (deviceName);
            if (deviceID.isEmpty())
                throw std::runtime_error ("Device unavailable");

            OpenMidiPortThread<IMidiOutPortStatics, IMidiOutPort, IMidiOutPort> portThread ("Open WinRT MIDI output port",
                                                                                            deviceID,
                                                                                            service.midiOutFactory,
                                                                                            midiOutPort);
            portThread.startThread();
            portThread.waitForThreadToExit (-1);
            if (midiOutPort == nullptr)
                throw std::runtime_error ("Timed out waiting for midi output port creation");

            auto bufferFactory = WinRTWrapper::getInstance()->getWRLFactory<IBufferFactory> (&RuntimeClass_Windows_Storage_Streams_Buffer[0]);
            if (bufferFactory == nullptr)
                throw std::runtime_error ("Failed to create output buffer factory");

            HRESULT hr = bufferFactory->Create (static_cast<UINT32> (65536), buffer.resetAndGetPointerAddress());
            if (FAILED (hr))
                throw std::runtime_error ("Failed to create output buffer");

            hr = buffer->QueryInterface (bufferByteAccess.resetAndGetPointerAddress());
            if (FAILED (hr))
                throw std::runtime_error ("Failed to get buffer byte access");

            hr = bufferByteAccess->Buffer (&bufferData);
            if (FAILED (hr))
                throw std::runtime_error ("Failed to get buffer data pointer");
        }

        ~WinRTOutputWrapper() {}

        void sendMessageNow (const MidiMessage& message) override
        {
            const UINT32 numBytes = message.getRawDataSize();
            HRESULT hr = buffer->put_Length (numBytes);
            if (FAILED (hr))
                jassertfalse;

            memcpy_s (bufferData, numBytes, message.getRawData(), numBytes);

            midiOutPort->SendBuffer (buffer);
        }

        String getDeviceName() override
        {
            return deviceName;
        }

        String deviceName;
        ComSmartPtr<IMidiOutPort> midiOutPort;
        ComSmartPtr<IBuffer> buffer;
        ComSmartPtr<Windows::Storage::Streams::IBufferByteAccess> bufferByteAccess;
        uint8_t* bufferData = nullptr;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WinRTOutputWrapper);
    };

public:
    WinRTMidiService()
    {
        if (! WinRTWrapper::getInstance()->isInitialised())
            throw std::runtime_error ("Failed to initialise the WinRT wrapper");

        midiInFactory = WinRTWrapper::getInstance()->getWRLFactory<IMidiInPortStatics> (&RuntimeClass_Windows_Devices_Midi_MidiInPort[0]);
        if (midiInFactory == nullptr)
            throw std::runtime_error ("Failed to create midi in factory");

        midiOutFactory = WinRTWrapper::getInstance()->getWRLFactory<IMidiOutPortStatics> (&RuntimeClass_Windows_Devices_Midi_MidiOutPort[0]);
        if (midiOutFactory == nullptr)
            throw std::runtime_error ("Failed to create midi out factory");

        inputDeviceWatcher  = new MidiIODeviceWatcher<IMidiInPortStatics>  (midiInFactory);
        if (! inputDeviceWatcher->start())
            throw std::runtime_error ("Failed to start midi input device watcher");

        outputDeviceWatcher = new MidiIODeviceWatcher<IMidiOutPortStatics> (midiOutFactory);
        if (! outputDeviceWatcher->start())
            throw std::runtime_error ("Failed to start midi output device watcher");
    }

    ~WinRTMidiService()
    {
    }

    StringArray getDevices (bool isInput) override
    {
        return isInput ? inputDeviceWatcher ->getDevices()
                       : outputDeviceWatcher->getDevices();
    }

    int getDefaultDeviceIndex (bool isInput) override
    {
        return isInput ? inputDeviceWatcher ->getDefaultDeviceIndex()
                       : outputDeviceWatcher->getDefaultDeviceIndex();
    }

    InputWrapper* createInputWrapper (MidiInput* input, int index, MidiInputCallback* callback) override
    {
        return new WinRTInputWrapper (*this, input, index, *callback);
    }

    OutputWrapper* createOutputWrapper (int index) override
    {
        return new WinRTOutputWrapper (*this, index);
    }

    ComSmartPtr<IMidiInPortStatics>  midiInFactory;
    ComSmartPtr<IMidiOutPortStatics> midiOutFactory;

    ScopedPointer<MidiIODeviceWatcher<IMidiInPortStatics>>  inputDeviceWatcher;
    ScopedPointer<MidiIODeviceWatcher<IMidiOutPortStatics>> outputDeviceWatcher;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WinRTMidiService)
};

#endif   // JUCE_USE_WINRT_MIDI

//==============================================================================
class MidiService :  public DeletedAtShutdown
{
public:
    ~MidiService();

    MidiServiceType* getService();

    juce_DeclareSingleton (MidiService, false)

private:
    MidiService();

    ScopedPointer<MidiServiceType> internal;
};

juce_ImplementSingleton (MidiService)

MidiService::~MidiService()
{
    clearSingletonInstance();
}

MidiServiceType* MidiService::getService()
{
    return internal.get();
}

MidiService::MidiService()
{
   #if JUCE_USE_WINRT_MIDI
    try
    {
        internal = new WinRTMidiService();
        return;
    }
    catch (std::runtime_error&)
    {
    }
   #endif

    internal = new WindowsMidiService();
}

//==============================================================================
StringArray MidiInput::getDevices()
{
    return MidiService::getInstance()->getService()->getDevices (true);
}

int MidiInput::getDefaultDeviceIndex()
{
    return MidiService::getInstance()->getService()->getDefaultDeviceIndex (true);
}

MidiInput::MidiInput (const String& deviceName)
    : name (deviceName)
{
}

MidiInput* MidiInput::openDevice (const int index, MidiInputCallback* const callback)
{
    if (callback == nullptr)
        return nullptr;

    ScopedPointer<MidiInput> in (new MidiInput (String()));
    ScopedPointer<MidiServiceType::InputWrapper> wrapper;

    try
    {
        wrapper = MidiService::getInstance()->getService()->createInputWrapper (in, index, callback);
    }
    catch (std::runtime_error&)
    {
        return nullptr;
    }

    in->setName (wrapper->getDeviceName());
    in->internal = wrapper.release();
    return in.release();
}

MidiInput::~MidiInput()
{
    delete static_cast<MidiServiceType::InputWrapper*> (internal);
}

void MidiInput::start()   { static_cast<MidiServiceType::InputWrapper*> (internal)->start(); }
void MidiInput::stop()    { static_cast<MidiServiceType::InputWrapper*> (internal)->stop(); }

//==============================================================================
StringArray MidiOutput::getDevices()
{
    return MidiService::getInstance()->getService()->getDevices (false);
}

int MidiOutput::getDefaultDeviceIndex()
{
    return MidiService::getInstance()->getService()->getDefaultDeviceIndex (false);
}

MidiOutput* MidiOutput::openDevice (const int index)
{
    ScopedPointer<MidiServiceType::OutputWrapper> wrapper;

    try
    {
        wrapper = MidiService::getInstance()->getService()->createOutputWrapper (index);
    }
    catch (std::runtime_error&)
    {
        return nullptr;
    }

    ScopedPointer<MidiOutput> out (new MidiOutput (wrapper->getDeviceName()));
    out->internal = wrapper.release();
    return out.release();
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
