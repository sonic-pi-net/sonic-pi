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
    A base class which provides methods for reference-counting.

    To add reference-counting to a class, derive it from this class, and
    use the ReferenceCountedObjectPtr class to point to it.

    e.g. @code
    class MyClass : public ReferenceCountedObject
    {
        void foo();

        // This is a neat way of declaring a typedef for a pointer class,
        // rather than typing out the full templated name each time..
        typedef ReferenceCountedObjectPtr<MyClass> Ptr;
    };

    MyClass::Ptr p = new MyClass();
    MyClass::Ptr p2 = p;
    p = nullptr;
    p2->foo();
    @endcode

    Once a new ReferenceCountedObject has been assigned to a pointer, be
    careful not to delete the object manually.

    This class uses an Atomic<int> value to hold the reference count, so that it
    the pointers can be passed between threads safely. For a faster but non-thread-safe
    version, use SingleThreadedReferenceCountedObject instead.

    @see ReferenceCountedObjectPtr, ReferenceCountedArray, SingleThreadedReferenceCountedObject
*/
class JUCE_API  ReferenceCountedObject
{
public:
    //==============================================================================
    /** Increments the object's reference count.

        This is done automatically by the smart pointer, but is public just
        in case it's needed for nefarious purposes.
    */
    void incReferenceCount() noexcept
    {
        ++refCount;
    }

    /** Decreases the object's reference count.
        If the count gets to zero, the object will be deleted.
    */
    void decReferenceCount() noexcept
    {
        jassert (getReferenceCount() > 0);

        if (--refCount == 0)
            delete this;
    }

    /** Decreases the object's reference count.
        If the count gets to zero, the object will not be deleted, but this method
        will return true, allowing the caller to take care of deletion.
    */
    bool decReferenceCountWithoutDeleting() noexcept
    {
        jassert (getReferenceCount() > 0);
        return --refCount == 0;
    }

    /** Returns the object's current reference count. */
    int getReferenceCount() const noexcept       { return refCount.get(); }


protected:
    //==============================================================================
    /** Creates the reference-counted object (with an initial ref count of zero). */
    ReferenceCountedObject() {}

    /** Copying from another object does not affect this one's reference-count. */
    ReferenceCountedObject (const ReferenceCountedObject&) noexcept {}
    /** Copying from another object does not affect this one's reference-count. */
    ReferenceCountedObject (ReferenceCountedObject&&) noexcept {}
    /** Copying from another object does not affect this one's reference-count. */
    ReferenceCountedObject& operator= (const ReferenceCountedObject&) noexcept  { return *this; }
    /** Copying from another object does not affect this one's reference-count. */
    ReferenceCountedObject& operator= (ReferenceCountedObject&&) noexcept       { return *this; }

    /** Destructor. */
    virtual ~ReferenceCountedObject()
    {
        // it's dangerous to delete an object that's still referenced by something else!
        jassert (getReferenceCount() == 0);
    }

    /** Resets the reference count to zero without deleting the object.
        You should probably never need to use this!
    */
    void resetReferenceCount() noexcept
    {
        refCount = 0;
    }

private:
    //==============================================================================
    Atomic<int> refCount { 0 };
    friend struct ContainerDeletePolicy<ReferenceCountedObject>;
};


//==============================================================================
/**
    Adds reference-counting to an object.

    This is effectively a version of the ReferenceCountedObject class, but which
    uses a non-atomic counter, and so is not thread-safe (but which will be more
    efficient).
    For more details on how to use it, see the ReferenceCountedObject class notes.

    @see ReferenceCountedObject, ReferenceCountedObjectPtr, ReferenceCountedArray
*/
class JUCE_API  SingleThreadedReferenceCountedObject
{
public:
    //==============================================================================
    /** Increments the object's reference count.

        This is done automatically by the smart pointer, but is public just
        in case it's needed for nefarious purposes.
    */
    void incReferenceCount() noexcept
    {
        ++refCount;
    }

    /** Decreases the object's reference count.
        If the count gets to zero, the object will be deleted.
    */
    void decReferenceCount() noexcept
    {
        jassert (getReferenceCount() > 0);

        if (--refCount == 0)
            delete this;
    }

    /** Decreases the object's reference count.
        If the count gets to zero, the object will not be deleted, but this method
        will return true, allowing the caller to take care of deletion.
    */
    bool decReferenceCountWithoutDeleting() noexcept
    {
        jassert (getReferenceCount() > 0);
        return --refCount == 0;
    }

    /** Returns the object's current reference count. */
    int getReferenceCount() const noexcept       { return refCount; }


protected:
    //==============================================================================
    /** Creates the reference-counted object (with an initial ref count of zero). */
    SingleThreadedReferenceCountedObject() {}

    /** Copying from another object does not affect this one's reference-count. */
    SingleThreadedReferenceCountedObject (const SingleThreadedReferenceCountedObject&) {}
    /** Copying from another object does not affect this one's reference-count. */
    SingleThreadedReferenceCountedObject (SingleThreadedReferenceCountedObject&&) {}
    /** Copying from another object does not affect this one's reference-count. */
    SingleThreadedReferenceCountedObject& operator= (const SingleThreadedReferenceCountedObject&) { return *this; }
    /** Copying from another object does not affect this one's reference-count. */
    SingleThreadedReferenceCountedObject& operator= (SingleThreadedReferenceCountedObject&&) { return *this; }

    /** Destructor. */
    virtual ~SingleThreadedReferenceCountedObject()
    {
        // it's dangerous to delete an object that's still referenced by something else!
        jassert (getReferenceCount() == 0);
    }

private:
    //==============================================================================
    int refCount = 0;
    friend struct ContainerDeletePolicy<ReferenceCountedObject>;
};


//==============================================================================
/**
    A smart-pointer class which points to a reference-counted object.

    The template parameter specifies the class of the object you want to point to - the easiest
    way to make a class reference-countable is to simply make it inherit from ReferenceCountedObject
    or SingleThreadedReferenceCountedObject, but if you need to, you can roll your own reference-countable
    class by implementing a set of methods called incReferenceCount(), decReferenceCount(), and
    decReferenceCountWithoutDeleting(). See ReferenceCountedObject for examples of how these methods
    should behave.

    When using this class, you'll probably want to create a typedef to abbreviate the full
    templated name - e.g.
    @code
    struct MyClass  : public ReferenceCountedObject
    {
        typedef ReferenceCountedObjectPtr<MyClass> Ptr;
        ...
    }
    @endcode

    @see ReferenceCountedObject, ReferenceCountedObjectArray
*/
template <class ReferenceCountedObjectClass>
class ReferenceCountedObjectPtr
{
public:
    /** The class being referenced by this pointer. */
    typedef ReferenceCountedObjectClass ReferencedType;

    //==============================================================================
    /** Creates a pointer to a null object. */
    ReferenceCountedObjectPtr() noexcept {}

    /** Creates a pointer to a null object. */
    ReferenceCountedObjectPtr (decltype (nullptr)) noexcept {}

    /** Creates a pointer to an object.
        This will increment the object's reference-count.
    */
    ReferenceCountedObjectPtr (ReferencedType* refCountedObject) noexcept
        : referencedObject (refCountedObject)
    {
        incIfNotNull (refCountedObject);
    }

    /** Copies another pointer.
        This will increment the object's reference-count.
    */
    ReferenceCountedObjectPtr (const ReferenceCountedObjectPtr& other) noexcept
        : referencedObject (other.referencedObject)
    {
        incIfNotNull (referencedObject);
    }

    /** Copies another pointer.
        This will increment the object's reference-count (if it is non-null).
    */
    template <typename Convertible>
    ReferenceCountedObjectPtr (const ReferenceCountedObjectPtr<Convertible>& other) noexcept
        : referencedObject (static_cast<ReferencedType*> (other.get()))
    {
        incIfNotNull (referencedObject);
    }

    /** Changes this pointer to point at a different object.
        The reference count of the old object is decremented, and it might be
        deleted if it hits zero. The new object's count is incremented.
    */
    ReferenceCountedObjectPtr& operator= (const ReferenceCountedObjectPtr& other)
    {
        return operator= (other.referencedObject);
    }

    /** Changes this pointer to point at a different object.
        The reference count of the old object is decremented, and it might be
        deleted if it hits zero. The new object's count is incremented.
    */
    template <typename Convertible>
    ReferenceCountedObjectPtr& operator= (const ReferenceCountedObjectPtr<Convertible>& other)
    {
        return operator= (static_cast<ReferencedType*> (other.get()));
    }

    /** Changes this pointer to point at a different object.

        The reference count of the old object is decremented, and it might be
        deleted if it hits zero. The new object's count is incremented.
    */
    ReferenceCountedObjectPtr& operator= (ReferencedType* const newObject)
    {
        if (referencedObject != newObject)
        {
            incIfNotNull (newObject);
            auto* oldObject = referencedObject;
            referencedObject = newObject;
            decIfNotNull (oldObject);
        }

        return *this;
    }

    /** Takes-over the object from another pointer. */
    ReferenceCountedObjectPtr (ReferenceCountedObjectPtr&& other) noexcept
        : referencedObject (other.referencedObject)
    {
        other.referencedObject = nullptr;
    }

    /** Takes-over the object from another pointer. */
    ReferenceCountedObjectPtr& operator= (ReferenceCountedObjectPtr&& other)
    {
        std::swap (referencedObject, other.referencedObject);
        return *this;
    }

    /** Destructor.
        This will decrement the object's reference-count, which will cause the
        object to be deleted when the ref-count hits zero.
    */
    ~ReferenceCountedObjectPtr()
    {
        decIfNotNull (referencedObject);
    }

    //==============================================================================
    /** Returns the object that this pointer references.
        The pointer returned may be null, of course.
    */
    operator ReferencedType*() const noexcept       { return referencedObject; }

    /** Returns the object that this pointer references.
        The pointer returned may be null, of course.
    */
    ReferencedType* get() const noexcept            { return referencedObject; }

    /** Returns the object that this pointer references.
        The pointer returned may be null, of course.
    */
    ReferencedType* getObject() const noexcept      { return referencedObject; }

    // the -> operator is called on the referenced object
    ReferencedType* operator->() const noexcept
    {
        jassert (referencedObject != nullptr); // null pointer method call!
        return referencedObject;
    }

private:
    //==============================================================================
    ReferencedType* referencedObject = nullptr;

    static void incIfNotNull (ReferencedType* o) noexcept
    {
        if (o != nullptr)
            o->incReferenceCount();
    }

    static void decIfNotNull (ReferencedType* o) noexcept
    {
        if (o != nullptr && o->decReferenceCountWithoutDeleting())
            ContainerDeletePolicy<ReferencedType>::destroy (o);
    }
};


//==============================================================================
/** Compares two ReferenceCountedObjectPtrs. */
template <typename ReferenceCountedObjectClass>
bool operator== (const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object1, ReferenceCountedObjectClass* const object2) noexcept
{
    return object1.get() == object2;
}

/** Compares two ReferenceCountedObjectPtrs. */
template <typename ReferenceCountedObjectClass>
bool operator== (const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object1, const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object2) noexcept
{
    return object1.get() == object2.get();
}

/** Compares two ReferenceCountedObjectPtrs. */
template <typename ReferenceCountedObjectClass>
bool operator== (ReferenceCountedObjectClass* object1, const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object2) noexcept
{
    return object1 == object2.get();
}

/** Compares two ReferenceCountedObjectPtrs. */
template <typename ReferenceCountedObjectClass>
bool operator!= (const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object1, const ReferenceCountedObjectClass* object2) noexcept
{
    return object1.get() != object2;
}

/** Compares two ReferenceCountedObjectPtrs. */
template <typename ReferenceCountedObjectClass>
bool operator!= (const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object1, const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object2) noexcept
{
    return object1.get() != object2.get();
}

/** Compares two ReferenceCountedObjectPtrs. */
template <typename ReferenceCountedObjectClass>
bool operator!= (ReferenceCountedObjectClass* object1, const ReferenceCountedObjectPtr<ReferenceCountedObjectClass>& object2) noexcept
{
    return object1 != object2.get();
}

} // namespace juce
