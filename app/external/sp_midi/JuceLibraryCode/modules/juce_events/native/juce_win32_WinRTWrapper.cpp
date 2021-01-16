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

WinRTWrapper::ScopedHString::ScopedHString (String str)
{
    if (WinRTWrapper::getInstance()->isInitialised())
        WinRTWrapper::getInstance()->createHString (str.toWideCharPointer(),
                                                    static_cast<uint32_t> (str.length()),
                                                    &hstr);
}

WinRTWrapper::ScopedHString::~ScopedHString()
{
    if (WinRTWrapper::getInstance()->isInitialised() && hstr != nullptr)
        WinRTWrapper::getInstance()->deleteHString (hstr);
}

WinRTWrapper::~WinRTWrapper()
{
    if (winRTHandle != nullptr)
        ::FreeLibrary (winRTHandle);

    clearSingletonInstance();
}

String WinRTWrapper::hStringToString (HSTRING hstr)
{
    if (isInitialised())
        if (const wchar_t* str = getHStringRawBuffer (hstr, nullptr))
            return String (str);

    return {};
}

WinRTWrapper::WinRTWrapper()
{
    winRTHandle = ::LoadLibraryA ("api-ms-win-core-winrt-l1-1-0");

    if (winRTHandle == nullptr)
        return;

    roInitialize           = (RoInitializeFuncPtr)              ::GetProcAddress (winRTHandle, "RoInitialize");
    createHString          = (WindowsCreateStringFuncPtr)       ::GetProcAddress (winRTHandle, "WindowsCreateString");
    deleteHString          = (WindowsDeleteStringFuncPtr)       ::GetProcAddress (winRTHandle, "WindowsDeleteString");
    getHStringRawBuffer    = (WindowsGetStringRawBufferFuncPtr) ::GetProcAddress (winRTHandle, "WindowsGetStringRawBuffer");
    roActivateInstance     = (RoActivateInstanceFuncPtr)        ::GetProcAddress (winRTHandle, "RoActivateInstance");
    roGetActivationFactory = (RoGetActivationFactoryFuncPtr)    ::GetProcAddress (winRTHandle, "RoGetActivationFactory");

    if (roInitialize == nullptr || createHString == nullptr || deleteHString == nullptr
        || getHStringRawBuffer == nullptr || roActivateInstance == nullptr || roGetActivationFactory == nullptr)
        return;

    HRESULT status = roInitialize (1);
    initialised = ! (status != S_OK && status != S_FALSE && status != 0x80010106L);
}

JUCE_IMPLEMENT_SINGLETON (WinRTWrapper)

}
