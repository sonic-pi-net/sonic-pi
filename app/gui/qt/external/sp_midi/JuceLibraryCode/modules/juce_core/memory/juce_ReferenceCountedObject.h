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
        using Ptr = ReferenceCountedObjectPtr<MyClass>;
    };

    MyClass::Ptr p = new MyClass();
    MyClass::Ptr p2 = p;
    p = nullptr;
    p2->foo();
    @endcode

    Once a new ReferenceCountedObject has been assigned to a pointer, be
    careful not to delete the object manually.

    This class uses an Atomic<int> value to hold the reference count, so
    the reference count can be updated on multiple threads. Note that
    whilst it's thread-safe to create and delete a ReferenceCountedObjectPtr
    to a ReferenceCountedObject shared between threads, it's not thread-safe
    to modify or swap the ReferenceCountedObject.

    For a faster but non-thread-safe version, use SingleThreadedReferenceCountedObject
    instead.

    @see ReferenceCountedObjectPtr, ReferenceCountedArray, SingleThreadedReferenceCountedObject

    @tags{Core}
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
    ReferenceCountedObject() = default;

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

    @tags{Core}
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
    SingleThreadedReferenceCountedObject() = default;

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
        using Ptr = ReferenceCountedObjectPtr<MyClass>;
        ...
    }
    @endcode

    @see ReferenceCountedObject, ReferenceCountedObjectArray

    @tags{Core}
*/
template <class ObjectType>
class ReferenceCountedObjectPtr
{
public:
    /** The class being referenced by this pointer. */
    using ReferencedType = ObjectType;

    //==============================================================================
    /** Creates a pointer to a null object. */
    ReferenceCountedObjectPtr() = default;

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

    /** Creates a pointer to an object.
        This will increment the object's reference-count.
    */
    ReferenceCountedObjectPtr (ReferencedType& refCountedObject) noexcept
        : referencedObject (&refCountedObject)
    {
        refCountedObject.incReferenceCount();
    }

    /** Copies another pointer.
        This will increment the object's reference-count.
    */
    ReferenceCountedObjectPtr (const ReferenceCountedObjectPtr& other) noexcept
        : referencedObject (other.referencedObject)
    {
        incIfNotNull (referencedObject);
    }

    /** Takes-over the object from another pointer. */
    ReferenceCountedObjectPtr (ReferenceCountedObjectPtr&& other) noexcept
        : referencedObject (other.referencedObject)
    {
        other.referencedObject = nullptr;
    }

    /** Copies another pointer.
        This will increment the object's reference-count (if it is non-null).
    */
    template <typename Convertible>
    ReferenceCountedObjectPtr (const ReferenceCountedObjectPtr<Convertible>& other) noexcept
        : referencedObject (other.get())
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
        return operator= (other.get());
    }

    /** Changes this pointer to point at a different object.

        The reference count of the old object is decremented, and it might be
        deleted if it hits zero. The new object's count is incremented.
    */
    ReferenceCountedObjectPtr& operator= (ReferencedType* newObject)
    {
        if (newObject != nullptr)
            return operator= (*newObject);

        reset();
        return *this;
    }

    /** Changes this pointer to point at a different object.

        The reference count of the old object is decremented, and it might be
        deleted if it hits zero. The new object's count is incremented.
    */
    ReferenceCountedObjectPtr& operator= (ReferencedType& newObject)
    {
        if (referencedObject != &newObject)
        {
            newObject.incReferenceCount();
            auto* oldObject = referencedObject;
            referencedObject = &newObject;
            decIfNotNull (oldObject);
        }

        return *this;
    }

    /** Resets this pointer to a null pointer. */
    ReferenceCountedObjectPtr& operator= (decltype (nullptr))
    {
        reset();
        return *this;
    }

    /** Takes-over the object from another pointer. */
    ReferenceCountedObjectPtr& operator= (ReferenceCountedObjectPtr&& other) noexcept
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
    ReferencedType* get() const noexcept                    { return referencedObject; }

    /** Resets this object to a null pointer. */
    void reset() noexcept
    {
        auto oldObject = referencedObject;  // need to null the pointer before deleting the object
        referencedObject = nullptr;         // in case this ptr is itself deleted as a side-effect
        decIfNotNull (oldObject);           // of the destructor
    }

    // the -> operator is called on the referenced object
    ReferencedType* operator->() const noexcept
    {
        jassert (referencedObject != nullptr); // null pointer method call!
        return referencedObject;
    }

    /** Dereferences the object that this pointer references.
        The pointer returned may be null, of course.
    */
    ReferencedType& operator*() const noexcept              { jassert (referencedObject != nullptr); return *referencedObject; }

    /** Checks whether this pointer is null */
    bool operator== (decltype (nullptr)) const noexcept     { return referencedObject == nullptr; }
    /** Checks whether this pointer is null */
    bool operator!= (decltype (nullptr)) const noexcept     { return referencedObject != nullptr; }

    /** Compares two ReferenceCountedObjectPtrs. */
    bool operator== (const ObjectType* other) const noexcept                 { return referencedObject == other; }
    /** Compares two ReferenceCountedObjectPtrs. */
    bool operator== (const ReferenceCountedObjectPtr& other) const noexcept  { return referencedObject == other.get(); }
    /** Compares two ReferenceCountedObjectPtrs. */
    bool operator!= (const ObjectType* other) const noexcept                 { return referencedObject != other; }
    /** Compares two ReferenceCountedObjectPtrs. */
    bool operator!= (const ReferenceCountedObjectPtr& other) const noexcept  { return referencedObject != other.get(); }

   #if JUCE_STRICT_REFCOUNTEDPOINTER
    /** Checks whether this pointer is null */
    explicit operator bool() const noexcept                 { return referencedObject != nullptr; }

   #else
    /** Returns the object that this pointer references.
        The pointer returned may be null, of course.
        Note that this methods allows the compiler to be very lenient with what it allows you to do
        with the pointer, it's safer to disable this by setting JUCE_STRICT_REFCOUNTEDPOINTER=1, which
        increased type safety and can prevent some common slip-ups.
    */
    operator ReferencedType*() const noexcept               { return referencedObject; }
   #endif


    // This old method is deprecated in favour of the shorter and more standard get() method.
    JUCE_DEPRECATED_WITH_BODY (ReferencedType* getObject() const, { return get(); })

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
template <typename Type>
bool operator== (const Type* object1, const ReferenceCountedObjectPtr<Type>& object2) noexcept
{
    return object1 == object2.get();
}

/** Compares two ReferenceCountedObjectPtrs. */
template <typename Type>
bool operator!= (const Type* object1, const ReferenceCountedObjectPtr<Type>& object2) noexcept
{
    return object1 != object2.get();
}

} // namespace juce
