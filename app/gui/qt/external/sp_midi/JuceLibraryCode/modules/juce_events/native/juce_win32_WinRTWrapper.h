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
    class ScopedHString
    {
    public:
        ScopedHString (String);

        ~ScopedHString();

        HSTRING get() const noexcept          { return hstr; }

    private:
        HSTRING hstr = nullptr;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScopedHString)
    };

    template <class ComClass>
    class ComPtr
    {
    public:
        ComPtr() noexcept {}
        ComPtr (ComClass* obj) : p (obj) { if (p) p->AddRef(); }
        ComPtr (const ComPtr& other) : p (other.p) { if (p) p->AddRef(); }
        ~ComPtr() { release(); }

        operator ComClass*()   const noexcept { return p; }
        ComClass& operator*()  const noexcept { return *p; }
        ComClass* operator->() const noexcept { return p; }

        ComPtr& operator= (ComClass* const newP)
        {
            if (newP != nullptr)
                newP->AddRef();

            release();
            p = newP;
            return *this;
        }

        ComPtr& operator= (const ComPtr& newP) { return operator= (newP.p); }

        ComClass** resetAndGetPointerAddress()
        {
            release();
            p = nullptr;
            return &p;
        }

    private:
        ComClass* p = nullptr;

        void release() { if (p != nullptr) p->Release(); }

        ComClass** operator&() noexcept; // private to avoid it being used accidentally
    };

    JUCE_DECLARE_SINGLETON (WinRTWrapper, true)

    ~WinRTWrapper();

    String hStringToString (HSTRING);

    bool isInitialised() const noexcept       { return initialised; }

    template <class ComClass>
    ComPtr<ComClass> activateInstance (const wchar_t* runtimeClassID, REFCLSID classUUID)
    {
        ComPtr<ComClass> result;

        if (isInitialised())
        {
            ComPtr<IInspectable> inspectable;
            ScopedHString runtimeClass (runtimeClassID);
            auto hr = roActivateInstance (runtimeClass.get(), inspectable.resetAndGetPointerAddress());

            if (SUCCEEDED (hr))
                inspectable->QueryInterface (classUUID, (void**) result.resetAndGetPointerAddress());
        }

        return result;
    }

    template <class ComClass>
    ComPtr<ComClass> getWRLFactory (const wchar_t* runtimeClassID)
    {
        ComPtr<ComClass> comPtr;

        if (isInitialised())
        {
            ScopedHString classID (runtimeClassID);

            if (classID.get() != nullptr)
                roGetActivationFactory (classID.get(), __uuidof (ComClass), (void**) comPtr.resetAndGetPointerAddress());
        }

        return comPtr;
    }

private:
    WinRTWrapper();

    HMODULE winRTHandle = nullptr;
    bool initialised = false;

    typedef HRESULT (WINAPI* RoInitializeFuncPtr) (int);
    typedef HRESULT (WINAPI* WindowsCreateStringFuncPtr) (LPCWSTR, UINT32, HSTRING*);
    typedef HRESULT (WINAPI* WindowsDeleteStringFuncPtr) (HSTRING);
    typedef PCWSTR  (WINAPI* WindowsGetStringRawBufferFuncPtr) (HSTRING, UINT32*);
    typedef HRESULT (WINAPI* RoActivateInstanceFuncPtr) (HSTRING, IInspectable**);
    typedef HRESULT (WINAPI* RoGetActivationFactoryFuncPtr) (HSTRING, REFIID, void**);

    RoInitializeFuncPtr roInitialize = nullptr;
    WindowsCreateStringFuncPtr createHString = nullptr;
    WindowsDeleteStringFuncPtr deleteHString = nullptr;
    WindowsGetStringRawBufferFuncPtr getHStringRawBuffer = nullptr;
    RoActivateInstanceFuncPtr roActivateInstance = nullptr;
    RoGetActivationFactoryFuncPtr roGetActivationFactory = nullptr;
};

} // namespace juce
