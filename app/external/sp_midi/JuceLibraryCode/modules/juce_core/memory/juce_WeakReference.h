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

//==============================================================================
/**
    This class acts as a pointer which will automatically become null if the object
    to which it points is deleted.

    To accomplish this, the source object needs to cooperate by performing a couple of simple tasks.
    It must embed a WeakReference::Master object, which stores a shared pointer object, and must clear
    this master pointer in its destructor.

    Note that WeakReference is not designed to be thread-safe, so if you're accessing it from
    different threads, you'll need to do your own locking around all uses of the pointer and
    the object it refers to.

    E.g.
    @code
    class MyObject
    {
    public:
        MyObject() {}

        ~MyObject()
        {
            // This will zero all the references - you need to call this in your destructor.
            masterReference.clear();
        }

    private:
        // You need to embed a variable of this type, with the name "masterReference" inside your object. If the
        // variable is not public, you should make your class a friend of WeakReference<MyObject> so that the
        // WeakReference class can access it.
        WeakReference<MyObject>::Master masterReference;
        friend class WeakReference<MyObject>;
    };

    OR: just use the handy JUCE_DECLARE_WEAK_REFERENCEABLE macro to do all this for you.

    // Here's an example of using a pointer..

    auto* n = new MyObject();
    WeakReference<MyObject> myObjectRef = n;

    auto pointer1 = myObjectRef.get();  // returns a valid pointer to 'n'
    delete n;
    auto pointer2 = myObjectRef.get();  // now returns nullptr
    @endcode

    @see WeakReference::Master

    @tags{Core}
*/
template <class ObjectType, class ReferenceCountingType = ReferenceCountedObject>
class WeakReference
{
public:
    /** Creates a null WeakReference. */
    inline WeakReference() = default;

    /** Creates a WeakReference that points at the given object. */
    WeakReference (ObjectType* object)  : holder (getRef (object)) {}

    /** Creates a copy of another WeakReference. */
    WeakReference (const WeakReference& other) noexcept         : holder (other.holder) {}

    /** Move constructor */
    WeakReference (WeakReference&& other) noexcept              : holder (std::move (other.holder)) {}

    /** Copies another pointer to this one. */
    WeakReference& operator= (const WeakReference& other)       { holder = other.holder; return *this; }

    /** Copies another pointer to this one. */
    WeakReference& operator= (ObjectType* newObject)            { holder = getRef (newObject); return *this; }

    /** Move assignment operator */
    WeakReference& operator= (WeakReference&& other) noexcept   { holder = std::move (other.holder); return *this; }

    /** Returns the object that this pointer refers to, or null if the object no longer exists. */
    ObjectType* get() const noexcept                            { return holder != nullptr ? holder->get() : nullptr; }

    /** Returns the object that this pointer refers to, or null if the object no longer exists. */
    operator ObjectType*() const noexcept                       { return get(); }

    /** Returns the object that this pointer refers to, or null if the object no longer exists. */
    ObjectType* operator->() const noexcept                     { return get(); }

    /** This returns true if this reference has been pointing at an object, but that object has
        since been deleted.

        If this reference was only ever pointing at a null pointer, this will return false. Using
        operator=() to make this refer to a different object will reset this flag to match the status
        of the reference from which you're copying.
    */
    bool wasObjectDeleted() const noexcept                      { return holder != nullptr && holder->get() == nullptr; }

    bool operator== (ObjectType* object) const noexcept         { return get() == object; }
    bool operator!= (ObjectType* object) const noexcept         { return get() != object; }

    //==============================================================================
    /** This class is used internally by the WeakReference class - don't use it directly
        in your code!
        @see WeakReference
    */
    class SharedPointer   : public ReferenceCountingType
    {
    public:
        explicit SharedPointer (ObjectType* obj) noexcept : owner (obj) {}

        inline ObjectType* get() const noexcept     { return owner; }
        void clearPointer() noexcept                { owner = nullptr; }

    private:
        ObjectType* owner;

        JUCE_DECLARE_NON_COPYABLE (SharedPointer)
    };

    using SharedRef = ReferenceCountedObjectPtr<SharedPointer>;

    //==============================================================================
    /**
        This class is embedded inside an object to which you want to attach WeakReference pointers.
        See the WeakReference class notes for an example of how to use this class.
        @see WeakReference
    */
    class Master
    {
    public:
        Master() = default;

        ~Master() noexcept
        {
            // You must remember to call clear() in your source object's destructor! See the notes
            // for the WeakReference class for an example of how to do this.
            jassert (sharedPointer == nullptr || sharedPointer->get() == nullptr);
        }

        /** The first call to this method will create an internal object that is shared by all weak
            references to the object.
        */
        SharedRef getSharedPointer (ObjectType* object)
        {
            if (sharedPointer == nullptr)
            {
                sharedPointer = *new SharedPointer (object);
            }
            else
            {
                // You're trying to create a weak reference to an object that has already been deleted!!
                jassert (sharedPointer->get() != nullptr);
            }

            return sharedPointer;
        }

        /** The object that owns this master pointer should call this before it gets destroyed,
            to zero all the references to this object that may be out there. See the WeakReference
            class notes for an example of how to do this.
        */
        void clear() noexcept
        {
            if (sharedPointer != nullptr)
                sharedPointer->clearPointer();
        }

        /** Returns the number of WeakReferences that are out there pointing to this object. */
        int getNumActiveWeakReferences() const noexcept
        {
            return sharedPointer == nullptr ? 0 : (sharedPointer->getReferenceCount() - 1);
        }

    private:
        SharedRef sharedPointer;

        JUCE_DECLARE_NON_COPYABLE (Master)
    };

private:
    SharedRef holder;

    static SharedRef getRef (ObjectType* o)
    {
        if (o != nullptr)
            return o->masterReference.getSharedPointer (o);

        return {};
    }
};


//==============================================================================
/**
     Macro to easily allow a class to be made weak-referenceable.
     This can be inserted in a class definition to add the requisite weak-ref boilerplate to that class.
     e.g.

     @code
     class MyObject
     {
     public:
         MyObject();
         ~MyObject();

     private:
         JUCE_DECLARE_WEAK_REFERENCEABLE (MyObject)
     };
     @endcode

     @see WeakReference, WeakReference::Master
*/
#define JUCE_DECLARE_WEAK_REFERENCEABLE(Class) \
    struct WeakRefMaster  : public juce::WeakReference<Class>::Master { ~WeakRefMaster() { this->clear(); } }; \
    WeakRefMaster masterReference; \
    friend class juce::WeakReference<Class>; \


} // namespace juce
