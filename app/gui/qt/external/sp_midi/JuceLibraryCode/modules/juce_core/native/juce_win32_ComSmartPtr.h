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

#if JUCE_MINGW || (! (defined (_MSC_VER) || defined (__uuidof)))
 #ifdef __uuidof
  #undef __uuidof
 #endif

 template<typename Type> struct UUIDGetter { static CLSID get() { jassertfalse; return {}; } };
 #define __uuidof(x)  UUIDGetter<x>::get()

 template <>
 struct UUIDGetter<::IUnknown>
 {
     static CLSID get()     { return { 0, 0, 0, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } }; }
 };

 #define JUCE_DECLARE_UUID_GETTER(name, uuid) \
    template<> struct UUIDGetter<name> { static CLSID get()  { return uuidFromString (uuid); } };

 #define JUCE_COMCLASS(name, guid) \
    struct name; \
    JUCE_DECLARE_UUID_GETTER (name, guid) \
    struct name

#else
 #define JUCE_DECLARE_UUID_GETTER(name, uuid)
 #define JUCE_COMCLASS(name, guid)       struct __declspec (uuid (guid)) name
#endif

inline GUID uuidFromString (const char* s) noexcept
{
    uint32 ints[4] = {};

    for (uint32 digitIndex = 0; digitIndex < 32;)
    {
        auto c = *s++;
        uint32 digit;

        if (c >= '0' && c <= '9')       digit = c - '0';
        else if (c >= 'a' && c <= 'f')  digit = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')  digit = c - 'A' + 10;
        else if (c == '-')              continue;
        else break;

        ints[digitIndex / 8] |= (digit << 4 * (7 - (digitIndex & 7)));
        ++digitIndex;
    }

    return { ints[0],
             (uint16) (ints[1] >> 16),
             (uint16) ints[1],
             { (uint8) (ints[2] >> 24), (uint8) (ints[2] >> 16), (uint8) (ints[2] >> 8), (uint8) ints[2],
               (uint8) (ints[3] >> 24), (uint8) (ints[3] >> 16), (uint8) (ints[3] >> 8), (uint8) ints[3] }};
}

//==============================================================================
/** A simple COM smart pointer.

    @tags{Core}
*/
template <class ComClass>
class ComSmartPtr
{
public:
    ComSmartPtr() noexcept {}
    ComSmartPtr (ComClass* obj) : p (obj)                   { if (p) p->AddRef(); }
    ComSmartPtr (const ComSmartPtr& other) : p (other.p)    { if (p) p->AddRef(); }
    ~ComSmartPtr()                                          { release(); }

    operator ComClass*() const noexcept     { return p; }
    ComClass& operator*() const noexcept    { return *p; }
    ComClass* operator->() const noexcept   { return p; }

    ComSmartPtr& operator= (ComClass* const newP)
    {
        if (newP != nullptr)  newP->AddRef();
        release();
        p = newP;
        return *this;
    }

    ComSmartPtr& operator= (const ComSmartPtr& newP)  { return operator= (newP.p); }

    // Releases and nullifies this pointer and returns its address
    ComClass** resetAndGetPointerAddress()
    {
        release();
        p = nullptr;
        return &p;
    }

    HRESULT CoCreateInstance (REFCLSID classUUID, DWORD dwClsContext = CLSCTX_INPROC_SERVER)
    {
        auto hr = ::CoCreateInstance (classUUID, 0, dwClsContext, __uuidof (ComClass), (void**) resetAndGetPointerAddress());
        jassert (hr != CO_E_NOTINITIALIZED); // You haven't called CoInitialize for the current thread!
        return hr;
    }

    template <class OtherComClass>
    HRESULT QueryInterface (REFCLSID classUUID, ComSmartPtr<OtherComClass>& destObject) const
    {
        if (p == nullptr)
            return E_POINTER;

        return p->QueryInterface (classUUID, (void**) destObject.resetAndGetPointerAddress());
    }

    template <class OtherComClass>
    HRESULT QueryInterface (ComSmartPtr<OtherComClass>& destObject) const
    {
        return this->QueryInterface (__uuidof (OtherComClass), destObject);
    }

private:
    ComClass* p = nullptr;

    void release()  { if (p != nullptr) p->Release(); }

    ComClass** operator&() noexcept; // private to avoid it being used accidentally
};

//==============================================================================
#define JUCE_COMRESULT  HRESULT __stdcall

//==============================================================================
template <class ComClass>
class ComBaseClassHelperBase   : public ComClass
{
public:
    ComBaseClassHelperBase (unsigned int initialRefCount)  : refCount (initialRefCount) {}
    virtual ~ComBaseClassHelperBase() {}

    ULONG __stdcall AddRef()    { return ++refCount; }
    ULONG __stdcall Release()   { auto r = --refCount; if (r == 0) delete this; return r; }

protected:
    ULONG refCount;

    JUCE_COMRESULT QueryInterface (REFIID refId, void** result)
    {
        if (refId == __uuidof (IUnknown))
            return castToType<IUnknown> (result);

        *result = nullptr;
        return E_NOINTERFACE;
    }

    template <class Type>
    JUCE_COMRESULT castToType (void** result)
    {
        this->AddRef(); *result = dynamic_cast<Type*> (this); return S_OK;
    }
};

/** Handy base class for writing COM objects, providing ref-counting and a basic QueryInterface method.

    @tags{Core}
*/
template <class ComClass>
class ComBaseClassHelper   : public ComBaseClassHelperBase<ComClass>
{
public:
    ComBaseClassHelper (unsigned int initialRefCount = 1) : ComBaseClassHelperBase<ComClass> (initialRefCount) {}
    ~ComBaseClassHelper() {}

    JUCE_COMRESULT QueryInterface (REFIID refId, void** result)
    {
        if (refId == __uuidof (ComClass))
            return this->template castToType<ComClass> (result);

        return ComBaseClassHelperBase<ComClass>::QueryInterface (refId, result);
    }
};

} // namespace juce
