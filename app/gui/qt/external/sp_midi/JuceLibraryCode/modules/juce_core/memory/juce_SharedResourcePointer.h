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

//==============================================================================
/**
    A smart-pointer that automatically creates and manages the lifetime of a
    shared static instance of a class.

    The SharedObjectType template type indicates the class to use for the shared
    object - the only requirements on this class are that it must have a public
    default constructor and destructor.

    The SharedResourcePointer offers a pattern that differs from using a singleton or
    static instance of an object, because it uses reference-counting to make sure that
    the underlying shared object is automatically created/destroyed according to the
    number of SharedResourcePointer objects that exist. When the last one is deleted,
    the underlying object is also immediately destroyed. This allows you to use scoping
    to manage the lifetime of a shared resource.

    Note: The construction/deletion of the shared object must not involve any
    code that makes recursive calls to a SharedResourcePointer, or you'll cause
    a deadlock.

    Example:
    @code
    // An example of a class that contains the shared data you want to use.
    struct MySharedData
    {
        // There's no need to ever create an instance of this class directly yourself,
        // but it does need a public constructor that does the initialisation.
        MySharedData()
        {
            sharedStuff = generateHeavyweightStuff();
        }

        Array<SomeKindOfData> sharedStuff;
    };

    struct DataUserClass
    {
        DataUserClass()
        {
            // Multiple instances of the DataUserClass will all have the same
            // shared common instance of MySharedData referenced by their sharedData
            // member variables.
            useSharedStuff (sharedData->sharedStuff);
        }

        // By keeping this pointer as a member variable, the shared resource
        // is guaranteed to be available for as long as the DataUserClass object.
        SharedResourcePointer<MySharedData> sharedData;
    };

    @endcode

    @tags{Core}
*/
template <typename SharedObjectType>
class SharedResourcePointer
{
public:
    /** Creates an instance of the shared object.
        If other SharedResourcePointer objects for this type already exist, then
        this one will simply point to the same shared object that they are already
        using. Otherwise, if this is the first SharedResourcePointer to be created,
        then a shared object will be created automatically.
    */
    SharedResourcePointer()
    {
        initialise();
    }

    SharedResourcePointer (const SharedResourcePointer&)
    {
        initialise();
    }

    /** Destructor.
        If no other SharedResourcePointer objects exist, this will also delete
        the shared object to which it refers.
    */
    ~SharedResourcePointer()
    {
        auto& holder = getSharedObjectHolder();
        const SpinLock::ScopedLockType sl (holder.lock);

        if (--(holder.refCount) == 0)
            holder.sharedInstance = nullptr;
    }

    /** Returns the shared object. */
    operator SharedObjectType*() const noexcept         { return sharedObject; }

    /** Returns the shared object. */
    SharedObjectType& get() const noexcept              { return *sharedObject; }

    /** Returns the object that this pointer references.
        The pointer returned may be a nullptr, of course.
    */
    SharedObjectType& getObject() const noexcept        { return *sharedObject; }

    /** Returns the shared object. */
    SharedObjectType* operator->() const noexcept       { return sharedObject; }

    /** Returns the number of SharedResourcePointers that are currently holding the shared object. */
    int getReferenceCount() const noexcept              { return getSharedObjectHolder().refCount; }

private:
    struct SharedObjectHolder
    {
        SpinLock lock;
        std::unique_ptr<SharedObjectType> sharedInstance;
        int refCount;
    };

    static SharedObjectHolder& getSharedObjectHolder() noexcept
    {
        static void* holder [(sizeof (SharedObjectHolder) + sizeof(void*) - 1) / sizeof(void*)] = { nullptr };
        return *reinterpret_cast<SharedObjectHolder*> (holder);
    }

    SharedObjectType* sharedObject;

    void initialise()
    {
        auto& holder = getSharedObjectHolder();
        const SpinLock::ScopedLockType sl (holder.lock);

        if (++(holder.refCount) == 1)
            holder.sharedInstance.reset (new SharedObjectType());

        sharedObject = holder.sharedInstance.get();
    }

    // There's no need to assign to a SharedResourcePointer because every
    // instance of the class is exactly the same!
    SharedResourcePointer& operator= (const SharedResourcePointer&) = delete;

    JUCE_LEAK_DETECTOR (SharedResourcePointer)
};

} // namespace juce
