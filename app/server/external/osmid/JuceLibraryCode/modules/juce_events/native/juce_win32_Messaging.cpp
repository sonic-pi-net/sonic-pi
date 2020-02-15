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

typedef bool (*CheckEventBlockedByModalComps) (const MSG&);
CheckEventBlockedByModalComps isEventBlockedByModalComps = nullptr;

//==============================================================================
namespace WindowsMessageHelpers
{
    const unsigned int customMessageID = WM_USER + 123;
    const unsigned int broadcastMessageMagicNumber = 0xc403;

    const TCHAR messageWindowName[] = _T("JUCEWindow");
    ScopedPointer<HiddenMessageWindow> messageWindow;

    void dispatchMessageFromLParam (LPARAM lParam)
    {
        if (MessageManager::MessageBase* message = reinterpret_cast<MessageManager::MessageBase*> (lParam))
        {
            JUCE_TRY
            {
                message->messageCallback();
            }
            JUCE_CATCH_EXCEPTION

            message->decReferenceCount();
        }
    }

    BOOL CALLBACK broadcastEnumWindowProc (HWND hwnd, LPARAM lParam)
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

    void handleBroadcastMessage (const COPYDATASTRUCT* const data)
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

    //==============================================================================
    LRESULT CALLBACK messageWndProc (HWND h, const UINT message, const WPARAM wParam, const LPARAM lParam) noexcept
    {
        if (h == juce_messageWindowHandle)
        {
            if (message == customMessageID)
            {
                // (These are trapped early in our dispatch loop, but must also be checked
                // here in case some 3rd-party code is running the dispatch loop).
                dispatchMessageFromLParam (lParam);
                return 0;
            }

            if (message == WM_COPYDATA)
            {
                handleBroadcastMessage (reinterpret_cast<const COPYDATASTRUCT*> (lParam));
                return 0;
            }
        }

        return DefWindowProc (h, message, wParam, lParam);
    }
}

#if JUCE_MODULE_AVAILABLE_juce_gui_extra
LRESULT juce_offerEventToActiveXControl (::MSG&);
#endif

//==============================================================================
bool MessageManager::dispatchNextMessageOnSystemQueue (const bool returnIfNoPendingMessages)
{
    using namespace WindowsMessageHelpers;
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
            dispatchMessageFromLParam (m.lParam);
        }
        else if (m.message == WM_QUIT)
        {
            if (JUCEApplicationBase* const app = JUCEApplicationBase::getInstance())
                app->systemRequestedQuit();
        }
        else if (isEventBlockedByModalComps == nullptr || ! isEventBlockedByModalComps (m))
        {
            if ((m.message == WM_LBUTTONDOWN || m.message == WM_RBUTTONDOWN)
                  && ! JuceWindowIdentifier::isJUCEWindow (m.hwnd))
            {
                // if it's someone else's window being clicked on, and the focus is
                // currently on a juce window, pass the kb focus over..
                HWND currentFocus = GetFocus();

                if (currentFocus == 0 || JuceWindowIdentifier::isJUCEWindow (currentFocus))
                    SetFocus (m.hwnd);
            }

            TranslateMessage (&m);
            DispatchMessage (&m);
        }
    }

    return true;
}

bool MessageManager::postMessageToSystemQueue (MessageManager::MessageBase* const message)
{
    message->incReferenceCount();
    return PostMessage (juce_messageWindowHandle, WindowsMessageHelpers::customMessageID, 0, (LPARAM) message) != 0;
}

void MessageManager::broadcastMessage (const String& value)
{
    const String localCopy (value);

    Array<HWND> windows;
    EnumWindows (&WindowsMessageHelpers::broadcastEnumWindowProc, (LPARAM) &windows);

    for (int i = windows.size(); --i >= 0;)
    {
        COPYDATASTRUCT data;
        data.dwData = WindowsMessageHelpers::broadcastMessageMagicNumber;
        data.cbData = (localCopy.length() + 1) * sizeof (CharPointer_UTF32::CharType);
        data.lpData = (void*) localCopy.toUTF32().getAddress();

        DWORD_PTR result;
        SendMessageTimeout (windows.getUnchecked (i), WM_COPYDATA,
                            (WPARAM) juce_messageWindowHandle,
                            (LPARAM) &data,
                            SMTO_BLOCK | SMTO_ABORTIFHUNG, 8000, &result);
    }
}

//==============================================================================
void MessageManager::doPlatformSpecificInitialisation()
{
    OleInitialize (0);

    using namespace WindowsMessageHelpers;
    messageWindow = new HiddenMessageWindow (messageWindowName, (WNDPROC) messageWndProc);
    juce_messageWindowHandle = messageWindow->getHWND();
}

void MessageManager::doPlatformSpecificShutdown()
{
    WindowsMessageHelpers::messageWindow = nullptr;

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

MountedVolumeListChangeDetector::MountedVolumeListChangeDetector()  { pimpl = new Pimpl (*this); }
MountedVolumeListChangeDetector::~MountedVolumeListChangeDetector() {}

} // namespace juce
