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

#ifndef DOXYGEN

namespace juce
{

//==============================================================================
/**
    This class is deprecated. You should use std::unique_ptr instead.
*/
template <class ObjectType>
class ScopedPointer
{
public:
    //==============================================================================
    // ScopedPointer is deprecated! You should use std::unique_ptr instead.
    JUCE_DEPRECATED_ATTRIBUTE inline ScopedPointer() = default;

    // ScopedPointer is deprecated! You should use std::unique_ptr instead.
    JUCE_DEPRECATED_ATTRIBUTE inline ScopedPointer (decltype (nullptr)) noexcept {}

    // ScopedPointer is deprecated! You should use std::unique_ptr instead.
    JUCE_DEPRECATED_ATTRIBUTE inline ScopedPointer (ObjectType* objectToTakePossessionOf) noexcept
        : object (objectToTakePossessionOf)
    {
    }

    // ScopedPointer is deprecated! You should use std::unique_ptr instead.
    ScopedPointer (ScopedPointer& objectToTransferFrom) noexcept
        : object (objectToTransferFrom.release())
    {
    }

    // ScopedPointer is deprecated! You should use std::unique_ptr instead.
    JUCE_DEPRECATED_ATTRIBUTE inline ~ScopedPointer()         { reset(); }

    ScopedPointer& operator= (ScopedPointer& objectToTransferFrom)
    {
        if (this != objectToTransferFrom.getAddress())
        {
            // Two ScopedPointers should never be able to refer to the same object - if
            // this happens, you must have done something dodgy!
            jassert (object == nullptr || object != objectToTransferFrom.object);
            reset (objectToTransferFrom.release());
        }

        return *this;
    }

    ScopedPointer& operator= (ObjectType* newObjectToTakePossessionOf)
    {
        reset (newObjectToTakePossessionOf);
        return *this;
    }

    ScopedPointer (ScopedPointer&& other) noexcept  : object (other.object)
    {
        other.object = nullptr;
    }

    ScopedPointer& operator= (ScopedPointer&& other) noexcept
    {
        reset (other.release());
        return *this;
    }

    //==============================================================================
    inline operator ObjectType*() const noexcept                                    { return object; }
    inline ObjectType* get() const noexcept                                         { return object; }
    inline ObjectType& operator*() const noexcept                                   { return *object; }
    inline ObjectType* operator->() const noexcept                                  { return object; }

    void reset()
    {
        auto* oldObject = object;
        object = {};
        ContainerDeletePolicy<ObjectType>::destroy (oldObject);
    }

    void reset (ObjectType* newObject)
    {
        if (object != newObject)
        {
            auto* oldObject = object;
            object = newObject;
            ContainerDeletePolicy<ObjectType>::destroy (oldObject);
        }
        else
        {
            // You're trying to reset this ScopedPointer to itself! This will work here as ScopedPointer does an equality check
            // but be aware that std::unique_ptr won't do this and you could end up with some nasty, subtle bugs!
            jassert (newObject == nullptr);
        }
    }

    void reset (ScopedPointer& newObject)
    {
        reset (newObject.release());
    }

    ObjectType* release() noexcept  { auto* o = object; object = {}; return o; }

    //==============================================================================
    void swapWith (ScopedPointer<ObjectType>& other) noexcept
    {
        // Two ScopedPointers should never be able to refer to the same object - if
        // this happens, you must have done something dodgy!
        jassert (object != other.object || this == other.getAddress() || object == nullptr);

        std::swap (object, other.object);
    }

    inline ObjectType* createCopy() const { return createCopyIfNotNull (object); }

private:
    //==============================================================================
    ObjectType* object = nullptr;

    const ScopedPointer* getAddress() const noexcept  { return this; } // Used internally to avoid the & operator

   #if ! JUCE_MSVC  // (MSVC can't deal with multiple copy constructors)
    ScopedPointer (const ScopedPointer&) = delete;
    ScopedPointer& operator= (const ScopedPointer&) = delete;
   #endif
};

//==============================================================================
template <typename ObjectType1, typename ObjectType2>
bool operator== (ObjectType1* pointer1, const ScopedPointer<ObjectType2>& pointer2) noexcept
{
    return pointer1 == pointer2.get();
}

template <typename ObjectType1, typename ObjectType2>
bool operator!= (ObjectType1* pointer1, const ScopedPointer<ObjectType2>& pointer2) noexcept
{
    return pointer1 != pointer2.get();
}

template <typename ObjectType1, typename ObjectType2>
bool operator== (const ScopedPointer<ObjectType1>& pointer1, ObjectType2* pointer2) noexcept
{
    return pointer1.get() == pointer2;
}

template <typename ObjectType1, typename ObjectType2>
bool operator!= (const ScopedPointer<ObjectType1>& pointer1, ObjectType2* pointer2) noexcept
{
    return pointer1.get() != pointer2;
}

template <typename ObjectType1, typename ObjectType2>
bool operator== (const ScopedPointer<ObjectType1>& pointer1, const ScopedPointer<ObjectType2>& pointer2) noexcept
{
    return pointer1.get() == pointer2.get();
}

template <typename ObjectType1, typename ObjectType2>
bool operator!= (const ScopedPointer<ObjectType1>& pointer1, const ScopedPointer<ObjectType2>& pointer2) noexcept
{
    return pointer1.get() != pointer2.get();
}

template <class ObjectType>
bool operator== (decltype (nullptr), const ScopedPointer<ObjectType>& pointer) noexcept
{
    return pointer.get() == nullptr;
}

template <class ObjectType>
bool operator!= (decltype (nullptr), const ScopedPointer<ObjectType>& pointer) noexcept
{
    return pointer.get() != nullptr;
}

template <class ObjectType>
bool operator== (const ScopedPointer<ObjectType>& pointer, decltype (nullptr)) noexcept
{
    return pointer.get() == nullptr;
}

template <class ObjectType>
bool operator!= (const ScopedPointer<ObjectType>& pointer, decltype (nullptr)) noexcept
{
    return pointer.get() != nullptr;
}

//==============================================================================
// NB: This is just here to prevent any silly attempts to call deleteAndZero() on a ScopedPointer.
template <typename Type>
void deleteAndZero (ScopedPointer<Type>&)  { static_assert (sizeof (Type) == 12345,
                                                            "Attempt to call deleteAndZero() on a ScopedPointer"); }

} // namespace juce

#endif
