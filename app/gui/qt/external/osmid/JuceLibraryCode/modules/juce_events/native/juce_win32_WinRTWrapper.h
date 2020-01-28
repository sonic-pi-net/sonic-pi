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

class WinRTWrapper :   public DeletedAtShutdown
{
public:
    juce_DeclareSingleton (WinRTWrapper, true)

    class ScopedHString
    {
    public:
        ScopedHString (String str)
        {
            if (WinRTWrapper::getInstance()->isInitialised())
                WinRTWrapper::getInstance()->createHString (str.toWideCharPointer(),
                                                            static_cast<uint32_t> (str.length()),
                                                            &hstr);
        }

        ~ScopedHString()
        {
            if (WinRTWrapper::getInstance()->isInitialised() && hstr != nullptr)
                WinRTWrapper::getInstance()->deleteHString (hstr);
        }

        HSTRING get() const noexcept
        {
            return hstr;
        }

    private:
        HSTRING hstr = nullptr;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScopedHString)
    };

    ~WinRTWrapper()
    {
        if (winRTHandle != nullptr)
            ::FreeLibrary (winRTHandle);
    }

    String hStringToString (HSTRING hstr)
    {
        if (isInitialised())
            if (const wchar_t* str = getHStringRawBuffer (hstr, nullptr))
                return String (str);

        return {};
    }

    bool isInitialised() const noexcept
    {
        return initialised;
    }

    template <class ComClass>
    ComSmartPtr<ComClass> getWRLFactory (const wchar_t* runtimeClassID)
    {
        ComSmartPtr<ComClass> comPtr;

        if (isInitialised())
        {
            ScopedHString classID (runtimeClassID);
            if (classID.get() != nullptr)
                roGetActivationFactory (classID.get(), __uuidof (ComClass), (void**) comPtr.resetAndGetPointerAddress());
        }

        return comPtr;
    }

private:
    WinRTWrapper()
    {
        winRTHandle = ::LoadLibraryA ("api-ms-win-core-winrt-l1-1-0");
        if (winRTHandle == nullptr)
            return;

        roInitialize           = (RoInitializeFuncPtr)              ::GetProcAddress (winRTHandle, "RoInitialize");
        createHString          = (WindowsCreateStringFuncPtr)       ::GetProcAddress (winRTHandle, "WindowsCreateString");
        deleteHString          = (WindowsDeleteStringFuncPtr)       ::GetProcAddress (winRTHandle, "WindowsDeleteString");
        getHStringRawBuffer    = (WindowsGetStringRawBufferFuncPtr) ::GetProcAddress (winRTHandle, "WindowsGetStringRawBuffer");
        roGetActivationFactory = (RoGetActivationFactoryFuncPtr)    ::GetProcAddress (winRTHandle, "RoGetActivationFactory");

        if (roInitialize == nullptr || createHString == nullptr || deleteHString == nullptr
         || getHStringRawBuffer == nullptr || roGetActivationFactory == nullptr)
            return;

        HRESULT status = roInitialize (1);
        initialised = ! (status != S_OK && status != S_FALSE && status != 0x80010106L);
    }

    HMODULE winRTHandle = nullptr;
    bool initialised = false;

    typedef HRESULT (WINAPI* RoInitializeFuncPtr) (int);
    typedef HRESULT (WINAPI* WindowsCreateStringFuncPtr) (LPCWSTR, UINT32, HSTRING*);
    typedef HRESULT (WINAPI* WindowsDeleteStringFuncPtr) (HSTRING);
    typedef PCWSTR  (WINAPI* WindowsGetStringRawBufferFuncPtr) (HSTRING, UINT32*);
    typedef HRESULT (WINAPI* RoGetActivationFactoryFuncPtr) (HSTRING, REFIID, void**);

    RoInitializeFuncPtr roInitialize = nullptr;
    WindowsCreateStringFuncPtr createHString = nullptr;
    WindowsDeleteStringFuncPtr deleteHString = nullptr;
    WindowsGetStringRawBufferFuncPtr getHStringRawBuffer = nullptr;
    RoGetActivationFactoryFuncPtr roGetActivationFactory = nullptr;
};

} // namespace juce
