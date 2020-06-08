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

extern HWND juce_messageWindowHandle;

#if JUCE_MODULE_AVAILABLE_juce_audio_plugin_client && JucePlugin_Build_Unity
 bool juce_isRunningInUnity();
#endif

#if JUCE_MODULE_AVAILABLE_juce_gui_extra
 LRESULT juce_offerEventToActiveXControl (::MSG&);
#endif

using CheckEventBlockedByModalComps = bool (*)(const MSG&);
CheckEventBlockedByModalComps isEventBlockedByModalComps = nullptr;

using SettingChangeCallbackFunc = void (*)(void);
SettingChangeCallbackFunc settingChangeCallback = nullptr;

//==============================================================================
class InternalMessageQueue
{
public:
    InternalMessageQueue()
    {
        messageWindow = std::make_unique<HiddenMessageWindow> (messageWindowName, (WNDPROC) messageWndProc);
        juce_messageWindowHandle = messageWindow->getHWND();
    }

    ~InternalMessageQueue()
    {
        juce_messageWindowHandle = 0;
        clearSingletonInstance();
    }

    JUCE_DECLARE_SINGLETON (InternalMessageQueue, false)

    //==============================================================================
    void broadcastMessage (const String& message)
    {
        auto localCopy = message;

        Array<HWND> windows;
        EnumWindows (&broadcastEnumWindowProc, (LPARAM) &windows);

        for (int i = windows.size(); --i >= 0;)
        {
            COPYDATASTRUCT data;
            data.dwData = broadcastMessageMagicNumber;
            data.cbData = (localCopy.length() + 1) * sizeof (CharPointer_UTF32::CharType);
            data.lpData = (void*) localCopy.toUTF32().getAddress();

            DWORD_PTR result;
            SendMessageTimeout (windows.getUnchecked (i), WM_COPYDATA,
                                (WPARAM) juce_messageWindowHandle,
                                (LPARAM) &data,
                                SMTO_BLOCK | SMTO_ABORTIFHUNG, 8000, &result);
        }
    }

    void postMessage (MessageManager::MessageBase* message)
    {
        bool shouldTriggerMessageQueueDispatch = false;

        {
            const ScopedLock sl (lock);

            shouldTriggerMessageQueueDispatch = messageQueue.isEmpty();
            messageQueue.add (message);
        }

        if (! shouldTriggerMessageQueueDispatch)
            return;

       #if JUCE_MODULE_AVAILABLE_juce_audio_plugin_client && JucePlugin_Build_Unity
        if (juce_isRunningInUnity())
        {
            SendNotifyMessage (juce_messageWindowHandle, customMessageID, 0, 0);
            return;
        }
        #endif

        PostMessage (juce_messageWindowHandle, customMessageID, 0, 0);
    }

    bool dispatchNextMessage (bool returnIfNoPendingMessages)
    {
        MSG m;

        if (returnIfNoPendingMessages && ! PeekMessage (&m, (HWND) 0, 0, 0, PM_NOREMOVE))
            return false;

        if (GetMessage (&m, (HWND) 0, 0, 0) >= 0)
        {
           #if JUCE_MODULE_AVAILABLE_juce_gui_extra
            if (juce_offerEventToActiveXControl (m) != S_FALSE)
                return true;
           #endif

            if (m.message == customMessageID && m.hwnd == juce_messageWindowHandle)
            {
                dispatchMessages();
            }
            else if (m.message == WM_QUIT)
            {
                if (auto* app = JUCEApplicationBase::getInstance())
                    app->systemRequestedQuit();
            }
            else if (isEventBlockedByModalComps == nullptr || ! isEventBlockedByModalComps (m))
            {
                if ((m.message == WM_LBUTTONDOWN || m.message == WM_RBUTTONDOWN)
                      && ! JuceWindowIdentifier::isJUCEWindow (m.hwnd))
                {
                    // if it's someone else's window being clicked on, and the focus is
                    // currently on a juce window, pass the kb focus over..
                    auto currentFocus = GetFocus();

                    if (currentFocus == 0 || JuceWindowIdentifier::isJUCEWindow (currentFocus))
                        SetFocus (m.hwnd);
                }

                TranslateMessage (&m);
                DispatchMessage (&m);
            }
        }

        return true;
    }

private:
    //==============================================================================
    static LRESULT CALLBACK messageWndProc (HWND h, UINT message, WPARAM wParam, LPARAM lParam) noexcept
    {
        if (h == juce_messageWindowHandle)
        {
            if (message == customMessageID)
            {
                if (auto* queue = InternalMessageQueue::getInstanceWithoutCreating())
                    queue->dispatchMessages();

                return 0;
            }

            if (message == WM_COPYDATA)
            {
                handleBroadcastMessage (reinterpret_cast<const COPYDATASTRUCT*> (lParam));
                return 0;
            }

            if (message == WM_SETTINGCHANGE)
                if (settingChangeCallback != nullptr)
                    settingChangeCallback();
        }

        return DefWindowProc (h, message, wParam, lParam);
    }

    static BOOL CALLBACK broadcastEnumWindowProc (HWND hwnd, LPARAM lParam)
    {
        if (hwnd != juce_messageWindowHandle)
        {
            TCHAR windowName[64] = { 0 }; // no need to read longer strings than this
            GetWindowText (hwnd, windowName, 63);

            if (String (windowName) == messageWindowName)
                reinterpret_cast<Array<HWND>*> (lParam)->add (hwnd);
        }

        return TRUE;
    }

    static void dispatchMessage (MessageManager::MessageBase* message)
    {
        JUCE_TRY
        {
            message->messageCallback();
        }
        JUCE_CATCH_EXCEPTION

        message->decReferenceCount();
    }

    static void handleBroadcastMessage (const COPYDATASTRUCT* data)
    {
        if (data != nullptr && data->dwData == broadcastMessageMagicNumber)
        {
            struct BroadcastMessage  : public CallbackMessage
            {
                BroadcastMessage (CharPointer_UTF32 text, size_t length) : message (text, length) {}
                void messageCallback() override { MessageManager::getInstance()->deliverBroadcastMessage (message); }

                String message;
            };

            (new BroadcastMessage (CharPointer_UTF32 ((const CharPointer_UTF32::CharType*) data->lpData),
                                   data->cbData / sizeof (CharPointer_UTF32::CharType)))
                ->post();
        }
    }

    void dispatchMessages()
    {
        ReferenceCountedArray<MessageManager::MessageBase> messagesToDispatch;

        {
            const ScopedLock sl (lock);

            if (messageQueue.isEmpty())
                return;

            messagesToDispatch.swapWith (messageQueue);
        }

        for (int i = 0; i < messagesToDispatch.size(); ++i)
        {
            auto message = messagesToDispatch.getUnchecked (i);
            message->incReferenceCount();
            dispatchMessage (message.get());
        }
    }

    //==============================================================================
    static constexpr unsigned int customMessageID = WM_USER + 123;
    static constexpr unsigned int broadcastMessageMagicNumber = 0xc403;
    static const TCHAR messageWindowName[];

    std::unique_ptr<HiddenMessageWindow> messageWindow;

    CriticalSection lock;
    ReferenceCountedArray<MessageManager::MessageBase> messageQueue;

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (InternalMessageQueue)
};

JUCE_IMPLEMENT_SINGLETON (InternalMessageQueue)

const TCHAR InternalMessageQueue::messageWindowName[] = _T("JUCEWindow");

//==============================================================================
bool MessageManager::dispatchNextMessageOnSystemQueue (bool returnIfNoPendingMessages)
{
    if (auto* queue = InternalMessageQueue::getInstanceWithoutCreating())
        return queue->dispatchNextMessage (returnIfNoPendingMessages);

    return false;
}

bool MessageManager::postMessageToSystemQueue (MessageManager::MessageBase* const message)
{
    if (auto* queue = InternalMessageQueue::getInstanceWithoutCreating())
    {
        queue->postMessage (message);
        return true;
    }

    return false;
}

void MessageManager::broadcastMessage (const String& value)
{
    if (auto* queue = InternalMessageQueue::getInstanceWithoutCreating())
        queue->broadcastMessage (value);
}

//==============================================================================
void MessageManager::doPlatformSpecificInitialisation()
{
    OleInitialize (0);
    InternalMessageQueue::getInstance();
}

void MessageManager::doPlatformSpecificShutdown()
{
    InternalMessageQueue::deleteInstance();
    OleUninitialize();
}

//==============================================================================
struct MountedVolumeListChangeDetector::Pimpl   : private DeviceChangeDetector
{
    Pimpl (MountedVolumeListChangeDetector& d) : DeviceChangeDetector (L"MountedVolumeList"), owner (d)
    {
        File::findFileSystemRoots (lastVolumeList);
    }

    void systemDeviceChanged() override
    {
        Array<File> newList;
        File::findFileSystemRoots (newList);

        if (lastVolumeList != newList)
        {
            lastVolumeList = newList;
            owner.mountedVolumeListChanged();
        }
    }

    MountedVolumeListChangeDetector& owner;
    Array<File> lastVolumeList;
};

MountedVolumeListChangeDetector::MountedVolumeListChangeDetector()  { pimpl.reset (new Pimpl (*this)); }
MountedVolumeListChangeDetector::~MountedVolumeListChangeDetector() {}

} // namespace juce
