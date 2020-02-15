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
    Holds a list of objects derived from ReferenceCountedObject, or which implement basic
    reference-count handling methods.

    The template parameter specifies the class of the object you want to point to - the easiest
    way to make a class reference-countable is to simply make it inherit from ReferenceCountedObject
    or SingleThreadedReferenceCountedObject, but if you need to, you can roll your own reference-countable
    class by implementing a set of methods called incReferenceCount(), decReferenceCount(), and
    decReferenceCountWithoutDeleting(). See ReferenceCountedObject for examples of how these methods
    should behave.

    A ReferenceCountedArray holds objects derived from ReferenceCountedObject,
    and takes care of incrementing and decrementing their ref counts when they
    are added and removed from the array.

    To make all the array's methods thread-safe, pass in "CriticalSection" as the templated
    TypeOfCriticalSectionToUse parameter, instead of the default DummyCriticalSection.

    @see Array, OwnedArray, StringArray
*/
template <class ObjectClass, class TypeOfCriticalSectionToUse = DummyCriticalSection>
class ReferenceCountedArray
{
public:
    typedef ReferenceCountedObjectPtr<ObjectClass> ObjectClassPtr;

    //==============================================================================
    /** Creates an empty array.
        @see ReferenceCountedObject, Array, OwnedArray
    */
    ReferenceCountedArray() noexcept
    {
    }

    /** Creates a copy of another array */
    ReferenceCountedArray (const ReferenceCountedArray& other) noexcept
    {
        const ScopedLockType lock (other.getLock());
        numUsed = other.numUsed;
        data.setAllocatedSize (numUsed);
        memcpy (data.elements, other.getRawDataPointer(), (size_t) numUsed * sizeof (ObjectClass*));

        for (auto* o : *this)
            if (o != nullptr)
                o->incReferenceCount();
    }

    /** Moves from another array */
    ReferenceCountedArray (ReferenceCountedArray&& other) noexcept
        : data (static_cast<ArrayAllocationBase<ObjectClass*, TypeOfCriticalSectionToUse>&&> (other.data)),
          numUsed (other.numUsed)
    {
        other.numUsed = 0;
    }

    /** Creates a copy of another array */
    template <class OtherObjectClass, class OtherCriticalSection>
    ReferenceCountedArray (const ReferenceCountedArray<OtherObjectClass, OtherCriticalSection>& other) noexcept
    {
        const typename ReferenceCountedArray<OtherObjectClass, OtherCriticalSection>::ScopedLockType lock (other.getLock());
        numUsed = other.size();
        data.setAllocatedSize (numUsed);
        memcpy (data.elements, other.getRawDataPointer(), (size_t) numUsed * sizeof (ObjectClass*));

        for (auto* o : *this)
            if (o != nullptr)
                o->incReferenceCount();
    }

    /** Copies another array into this one.
        Any existing objects in this array will first be released.
    */
    ReferenceCountedArray& operator= (const ReferenceCountedArray& other) noexcept
    {
        releaseAllObjects();
        auto otherCopy = other;
        swapWith (otherCopy);
        return *this;
    }

    /** Copies another array into this one.
        Any existing objects in this array will first be released.
    */
    template <class OtherObjectClass>
    ReferenceCountedArray& operator= (const ReferenceCountedArray<OtherObjectClass, TypeOfCriticalSectionToUse>& other) noexcept
    {
        auto otherCopy = other;
        swapWith (otherCopy);
        return *this;
    }

    /** Moves from another array */
    ReferenceCountedArray& operator= (ReferenceCountedArray&& other) noexcept
    {
        releaseAllObjects();
        data = static_cast<ArrayAllocationBase<ObjectClass*, TypeOfCriticalSectionToUse>&&> (other.data);
        numUsed = other.numUsed;
        other.numUsed = 0;
        return *this;
    }

    /** Destructor.
        Any objects in the array will be released, and may be deleted if not referenced from elsewhere.
    */
    ~ReferenceCountedArray()
    {
        releaseAllObjects();
    }

    //==============================================================================
    /** Removes all objects from the array.
        Any objects in the array that whose reference counts drop to zero will be deleted.
    */
    void clear()
    {
        const ScopedLockType lock (getLock());
        releaseAllObjects();
        data.setAllocatedSize (0);
    }

    /** Removes all objects from the array without freeing the array's allocated storage.
        Any objects in the array that whose reference counts drop to zero will be deleted.
        @see clear
    */
    void clearQuick()
    {
        const ScopedLockType lock (getLock());
        releaseAllObjects();
    }

    /** Returns the current number of objects in the array. */
    inline int size() const noexcept
    {
        return numUsed;
    }

    /** Returns true if the array is empty, false otherwise. */
    inline bool isEmpty() const noexcept
    {
        return numUsed == 0;
    }

    /** Returns a pointer to the object at this index in the array.

        If the index is out-of-range, this will return a null pointer, (and
        it could be null anyway, because it's ok for the array to hold null
        pointers as well as objects).

        @see getUnchecked
    */
    inline ObjectClassPtr operator[] (int index) const noexcept
    {
        return getObjectPointer (index);
    }

    /** Returns a pointer to the object at this index in the array, without checking
        whether the index is in-range.

        This is a faster and less safe version of operator[] which doesn't check the index passed in, so
        it can be used when you're sure the index is always going to be legal.
    */
    inline ObjectClassPtr getUnchecked (int index) const noexcept
    {
        return getObjectPointerUnchecked (index);
    }

    /** Returns a raw pointer to the object at this index in the array.

        If the index is out-of-range, this will return a null pointer, (and
        it could be null anyway, because it's ok for the array to hold null
        pointers as well as objects).

        @see getUnchecked
    */
    inline ObjectClass* getObjectPointer (int index) const noexcept
    {
        const ScopedLockType lock (getLock());

        if (isPositiveAndBelow (index, numUsed))
        {
            jassert (data.elements != nullptr);
            return data.elements[index];
        }

        return {};
    }

    /** Returns a raw pointer to the object at this index in the array, without checking
        whether the index is in-range.
    */
    inline ObjectClass* getObjectPointerUnchecked (const int index) const noexcept
    {
        const ScopedLockType lock (getLock());
        jassert (isPositiveAndBelow (index, numUsed) && data.elements != nullptr);
        return data.elements[index];
    }

    /** Returns a pointer to the first object in the array.

        This will return a null pointer if the array's empty.
        @see getLast
    */
    inline ObjectClassPtr getFirst() const noexcept
    {
        const ScopedLockType lock (getLock());

        if (numUsed > 0)
        {
            jassert (data.elements != nullptr);
            return data.elements[0];
        }

        return {};
    }

    /** Returns a pointer to the last object in the array.

        This will return a null pointer if the array's empty.
        @see getFirst
    */
    inline ObjectClassPtr getLast() const noexcept
    {
        const ScopedLockType lock (getLock());

        if (numUsed > 0)
        {
            jassert (data.elements != nullptr);
            return data.elements[numUsed - 1];
        }

        return {};
    }

    /** Returns a pointer to the actual array data.
        This pointer will only be valid until the next time a non-const method
        is called on the array.
    */
    inline ObjectClass** getRawDataPointer() const noexcept
    {
        return data.elements;
    }

    //==============================================================================
    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ObjectClass** begin() const noexcept
    {
        return data.elements;
    }

    /** Returns a pointer to the element which follows the last element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ObjectClass** end() const noexcept
    {
        return data.elements + numUsed;
    }

    //==============================================================================
    /** Finds the index of the first occurrence of an object in the array.

        @param objectToLookFor    the object to look for
        @returns                  the index at which the object was found, or -1 if it's not found
    */
    int indexOf (const ObjectClass* objectToLookFor) const noexcept
    {
        const ScopedLockType lock (getLock());
        auto** e = data.elements.get();
        auto** endPointer = e + numUsed;

        while (e != endPointer)
        {
            if (objectToLookFor == *e)
                return static_cast<int> (e - data.elements.get());

            ++e;
        }

        return -1;
    }

    /** Returns true if the array contains a specified object.

        @param objectToLookFor      the object to look for
        @returns                    true if the object is in the array
    */
    bool contains (const ObjectClass* objectToLookFor) const noexcept
    {
        const ScopedLockType lock (getLock());
        auto** e = data.elements.get();
        auto** endPointer = e + numUsed;

        while (e != endPointer)
        {
            if (objectToLookFor == *e)
                return true;

            ++e;
        }

        return false;
    }

    /** Appends a new object to the end of the array.

        This will increase the new object's reference count.

        @param newObject       the new object to add to the array
        @see set, insert, addIfNotAlreadyThere, addSorted, addArray
    */
    ObjectClass* add (ObjectClass* newObject) noexcept
    {
        const ScopedLockType lock (getLock());
        data.ensureAllocatedSize (numUsed + 1);
        jassert (data.elements != nullptr);
        data.elements[numUsed++] = newObject;

        if (newObject != nullptr)
            newObject->incReferenceCount();

        return newObject;
    }

    /** Inserts a new object into the array at the given index.

        If the index is less than 0 or greater than the size of the array, the
        element will be added to the end of the array.
        Otherwise, it will be inserted into the array, moving all the later elements
        along to make room.

        This will increase the new object's reference count.

        @param indexToInsertAt      the index at which the new element should be inserted
        @param newObject            the new object to add to the array
        @see add, addSorted, addIfNotAlreadyThere, set
    */
    ObjectClass* insert (int indexToInsertAt, ObjectClass* newObject) noexcept
    {
        if (indexToInsertAt < 0)
            return add (newObject);

        const ScopedLockType lock (getLock());

        if (indexToInsertAt > numUsed)
            indexToInsertAt = numUsed;

        data.ensureAllocatedSize (numUsed + 1);
        jassert (data.elements != nullptr);

        auto** e = data.elements + indexToInsertAt;
        auto numToMove = numUsed - indexToInsertAt;

        if (numToMove > 0)
            memmove (e + 1, e, sizeof (ObjectClass*) * (size_t) numToMove);

        *e = newObject;

        if (newObject != nullptr)
            newObject->incReferenceCount();

        ++numUsed;
        return newObject;
    }

    /** Appends a new object at the end of the array as long as the array doesn't
        already contain it.

        If the array already contains a matching object, nothing will be done.

        @param newObject   the new object to add to the array
        @returns           true if the object has been added, false otherwise
    */
    bool addIfNotAlreadyThere (ObjectClass* newObject) noexcept
    {
        const ScopedLockType lock (getLock());

        if (contains (newObject))
            return false;

        add (newObject);
        return true;
    }

    /** Replaces an object in the array with a different one.

        If the index is less than zero, this method does nothing.
        If the index is beyond the end of the array, the new object is added to the end of the array.

        The object being added has its reference count increased, and if it's replacing
        another object, then that one has its reference count decreased, and may be deleted.

        @param indexToChange        the index whose value you want to change
        @param newObject            the new value to set for this index.
        @see add, insert, remove
    */
    void set (int indexToChange, ObjectClass* newObject)
    {
        if (indexToChange >= 0)
        {
            const ScopedLockType lock (getLock());

            if (newObject != nullptr)
                newObject->incReferenceCount();

            if (indexToChange < numUsed)
            {
                releaseObject (data.elements[indexToChange]);
                data.elements[indexToChange] = newObject;
            }
            else
            {
                data.ensureAllocatedSize (numUsed + 1);
                jassert (data.elements != nullptr);
                data.elements[numUsed++] = newObject;
            }
        }
    }

    /** Adds elements from another array to the end of this array.

        @param arrayToAddFrom       the array from which to copy the elements
        @param startIndex           the first element of the other array to start copying from
        @param numElementsToAdd     how many elements to add from the other array. If this
                                    value is negative or greater than the number of available elements,
                                    all available elements will be copied.
        @see add
    */
    void addArray (const ReferenceCountedArray& arrayToAddFrom,
                   int startIndex = 0,
                   int numElementsToAdd = -1) noexcept
    {
        const ScopedLockType lock1 (arrayToAddFrom.getLock());

        {
            const ScopedLockType lock2 (getLock());

            if (startIndex < 0)
            {
                jassertfalse;
                startIndex = 0;
            }

            if (numElementsToAdd < 0 || startIndex + numElementsToAdd > arrayToAddFrom.size())
                numElementsToAdd = arrayToAddFrom.size() - startIndex;

            if (numElementsToAdd > 0)
            {
                data.ensureAllocatedSize (numUsed + numElementsToAdd);

                while (--numElementsToAdd >= 0)
                    add (arrayToAddFrom.getUnchecked (startIndex++));
            }
        }
    }

    /** Inserts a new object into the array assuming that the array is sorted.

        This will use a comparator to find the position at which the new object
        should go. If the array isn't sorted, the behaviour of this
        method will be unpredictable.

        @param comparator   the comparator object to use to compare the elements - see the
                            sort() method for details about this object's form
        @param newObject    the new object to insert to the array
        @returns the index at which the new object was added
        @see add, sort
    */
    template <class ElementComparator>
    int addSorted (ElementComparator& comparator, ObjectClass* newObject) noexcept
    {
        const ScopedLockType lock (getLock());
        auto index = findInsertIndexInSortedArray (comparator, data.elements.get(), newObject, 0, numUsed);
        insert (index, newObject);
        return index;
    }

    /** Inserts or replaces an object in the array, assuming it is sorted.

        This is similar to addSorted, but if a matching element already exists, then it will be
        replaced by the new one, rather than the new one being added as well.
    */
    template <class ElementComparator>
    void addOrReplaceSorted (ElementComparator& comparator, ObjectClass* newObject) noexcept
    {
        const ScopedLockType lock (getLock());
        auto index = findInsertIndexInSortedArray (comparator, data.elements.get(), newObject, 0, numUsed);

        if (index > 0 && comparator.compareElements (newObject, data.elements[index - 1]) == 0)
            set (index - 1, newObject); // replace an existing object that matches
        else
            insert (index, newObject);  // no match, so insert the new one
    }

    /** Finds the index of an object in the array, assuming that the array is sorted.

        This will use a comparator to do a binary-chop to find the index of the given
        element, if it exists. If the array isn't sorted, the behaviour of this
        method will be unpredictable.

        @param comparator           the comparator to use to compare the elements - see the sort()
                                    method for details about the form this object should take
        @param objectToLookFor      the object to search for
        @returns                    the index of the element, or -1 if it's not found
        @see addSorted, sort
    */
    template <class ElementComparator>
    int indexOfSorted (ElementComparator& comparator,
                       const ObjectClass* objectToLookFor) const noexcept
    {
        ignoreUnused (comparator);
        const ScopedLockType lock (getLock());
        int s = 0, e = numUsed;

        while (s < e)
        {
            if (comparator.compareElements (objectToLookFor, data.elements[s]) == 0)
                return s;

            auto halfway = (s + e) / 2;

            if (halfway == s)
                break;

            if (comparator.compareElements (objectToLookFor, data.elements[halfway]) >= 0)
                s = halfway;
            else
                e = halfway;
        }

        return -1;
    }

    //==============================================================================
    /** Removes an object from the array.

        This will remove the object at a given index and move back all the
        subsequent objects to close the gap.

        If the index passed in is out-of-range, nothing will happen.

        The object that is removed will have its reference count decreased,
        and may be deleted if not referenced from elsewhere.

        @param indexToRemove    the index of the element to remove
        @see removeObject, removeRange
    */
    void remove (int indexToRemove)
    {
        const ScopedLockType lock (getLock());

        if (isPositiveAndBelow (indexToRemove, numUsed))
        {
            auto** e = data.elements + indexToRemove;
            releaseObject (*e);
            --numUsed;
            auto numberToShift = numUsed - indexToRemove;

            if (numberToShift > 0)
                memmove (e, e + 1, sizeof (ObjectClass*) * (size_t) numberToShift);

            if ((numUsed << 1) < data.numAllocated)
                minimiseStorageOverheads();
        }
    }

    /** Removes and returns an object from the array.

        This will remove the object at a given index and return it, moving back all
        the subsequent objects to close the gap. If the index passed in is out-of-range,
        nothing will happen and a null pointer will be returned.

        @param indexToRemove    the index of the element to remove
        @see remove, removeObject, removeRange
    */
    ObjectClassPtr removeAndReturn (int indexToRemove)
    {
        ObjectClassPtr removedItem;
        const ScopedLockType lock (getLock());

        if (isPositiveAndBelow (indexToRemove, numUsed))
        {
            auto** e = data.elements + indexToRemove;
            removedItem = *e;
            releaseObject (*e);
            --numUsed;
            auto numberToShift = numUsed - indexToRemove;

            if (numberToShift > 0)
                memmove (e, e + 1, sizeof (ObjectClass*) * (size_t) numberToShift);

            if ((numUsed << 1) < data.numAllocated)
                minimiseStorageOverheads();
        }

        return removedItem;
    }

    /** Removes the first occurrence of a specified object from the array.

        If the item isn't found, no action is taken. If it is found, it is
        removed and has its reference count decreased.

        @param objectToRemove   the object to try to remove
        @see remove, removeRange
    */
    void removeObject (ObjectClass* objectToRemove)
    {
        const ScopedLockType lock (getLock());
        remove (indexOf (objectToRemove));
    }

    /** Removes a range of objects from the array.

        This will remove a set of objects, starting from the given index,
        and move any subsequent elements down to close the gap.

        If the range extends beyond the bounds of the array, it will
        be safely clipped to the size of the array.

        The objects that are removed will have their reference counts decreased,
        and may be deleted if not referenced from elsewhere.

        @param startIndex       the index of the first object to remove
        @param numberToRemove   how many objects should be removed
        @see remove, removeObject
    */
    void removeRange (int startIndex,
                      int numberToRemove)
    {
        const ScopedLockType lock (getLock());

        auto start    = jlimit (0, numUsed, startIndex);
        auto endIndex = jlimit (0, numUsed, startIndex + numberToRemove);

        if (endIndex > start)
        {
            for (int i = start; i < endIndex; ++i)
            {
                releaseObject (data.elements[i]);
                data.elements[i] = nullptr; // (in case one of the destructors accesses this array and hits a dangling pointer)
            }

            auto rangeSize = endIndex - start;
            auto** e = data.elements + start;
            int i = numUsed - endIndex;
            numUsed -= rangeSize;

            while (--i >= 0)
            {
                *e = e[rangeSize];
                ++e;
            }

            if ((numUsed << 1) < data.numAllocated)
                minimiseStorageOverheads();
        }
    }

    /** Removes the last n objects from the array.

        The objects that are removed will have their reference counts decreased,
        and may be deleted if not referenced from elsewhere.

        @param howManyToRemove   how many objects to remove from the end of the array
        @see remove, removeObject, removeRange
    */
    void removeLast (int howManyToRemove = 1)
    {
        const ScopedLockType lock (getLock());

        if (howManyToRemove > numUsed)
            howManyToRemove = numUsed;

        while (--howManyToRemove >= 0)
            remove (numUsed - 1);
    }

    /** Swaps a pair of objects in the array.

        If either of the indexes passed in is out-of-range, nothing will happen,
        otherwise the two objects at these positions will be exchanged.
    */
    void swap (int index1, int index2) noexcept
    {
        const ScopedLockType lock (getLock());

        if (isPositiveAndBelow (index1, numUsed)
             && isPositiveAndBelow (index2, numUsed))
        {
            std::swap (data.elements[index1],
                       data.elements[index2]);
        }
    }

    /** Moves one of the objects to a different position.

        This will move the object to a specified index, shuffling along
        any intervening elements as required.

        So for example, if you have the array { 0, 1, 2, 3, 4, 5 } then calling
        move (2, 4) would result in { 0, 1, 3, 4, 2, 5 }.

        @param currentIndex     the index of the object to be moved. If this isn't a
                                valid index, then nothing will be done
        @param newIndex         the index at which you'd like this object to end up. If this
                                is less than zero, it will be moved to the end of the array
    */
    void move (int currentIndex, int newIndex) noexcept
    {
        if (currentIndex != newIndex)
        {
            const ScopedLockType lock (getLock());

            if (isPositiveAndBelow (currentIndex, numUsed))
            {
                if (! isPositiveAndBelow (newIndex, numUsed))
                    newIndex = numUsed - 1;

                auto* value = data.elements[currentIndex];

                if (newIndex > currentIndex)
                {
                    memmove (data.elements + currentIndex,
                             data.elements + currentIndex + 1,
                             sizeof (ObjectClass*) * (size_t) (newIndex - currentIndex));
                }
                else
                {
                    memmove (data.elements + newIndex + 1,
                             data.elements + newIndex,
                             sizeof (ObjectClass*) * (size_t) (currentIndex - newIndex));
                }

                data.elements[newIndex] = value;
            }
        }
    }

    //==============================================================================
    /** This swaps the contents of this array with those of another array.

        If you need to exchange two arrays, this is vastly quicker than using copy-by-value
        because it just swaps their internal pointers.
    */
    template <class OtherArrayType>
    void swapWith (OtherArrayType& otherArray) noexcept
    {
        const ScopedLockType lock1 (getLock());
        const typename OtherArrayType::ScopedLockType lock2 (otherArray.getLock());
        data.swapWith (otherArray.data);
        std::swap (numUsed, otherArray.numUsed);
    }

    //==============================================================================
    /** Compares this array to another one.

        @returns true only if the other array contains the same objects in the same order
    */
    bool operator== (const ReferenceCountedArray& other) const noexcept
    {
        const ScopedLockType lock2 (other.getLock());
        const ScopedLockType lock1 (getLock());

        if (numUsed != other.numUsed)
            return false;

        for (int i = numUsed; --i >= 0;)
            if (data.elements[i] != other.data.elements[i])
                return false;

        return true;
    }

    /** Compares this array to another one.

        @see operator==
    */
    bool operator!= (const ReferenceCountedArray<ObjectClass, TypeOfCriticalSectionToUse>& other) const noexcept
    {
        return ! operator== (other);
    }

    //==============================================================================
    /** Sorts the elements in the array.

        This will use a comparator object to sort the elements into order. The object
        passed must have a method of the form:
        @code
        int compareElements (ElementType first, ElementType second);
        @endcode

        ..and this method must return:
          - a value of < 0 if the first comes before the second
          - a value of 0 if the two objects are equivalent
          - a value of > 0 if the second comes before the first

        To improve performance, the compareElements() method can be declared as static or const.

        @param comparator   the comparator to use for comparing elements.
        @param retainOrderOfEquivalentItems     if this is true, then items
                            which the comparator says are equivalent will be
                            kept in the order in which they currently appear
                            in the array. This is slower to perform, but may
                            be important in some cases. If it's false, a faster
                            algorithm is used, but equivalent elements may be
                            rearranged.

        @see sortArray
    */
    template <class ElementComparator>
    void sort (ElementComparator& comparator,
               bool retainOrderOfEquivalentItems = false) const noexcept
    {
        ignoreUnused (comparator); // if you pass in an object with a static compareElements() method, this
                                   // avoids getting warning messages about the parameter being unused

        const ScopedLockType lock (getLock());
        sortArray (comparator, data.elements.get(), 0, size() - 1, retainOrderOfEquivalentItems);
    }

    //==============================================================================
    /** Reduces the amount of storage being used by the array.

        Arrays typically allocate slightly more storage than they need, and after
        removing elements, they may have quite a lot of unused space allocated.
        This method will reduce the amount of allocated storage to a minimum.
    */
    void minimiseStorageOverheads() noexcept
    {
        const ScopedLockType lock (getLock());
        data.shrinkToNoMoreThan (numUsed);
    }

    /** Increases the array's internal storage to hold a minimum number of elements.

        Calling this before adding a large known number of elements means that
        the array won't have to keep dynamically resizing itself as the elements
        are added, and it'll therefore be more efficient.
    */
    void ensureStorageAllocated (const int minNumElements)
    {
        const ScopedLockType lock (getLock());
        data.ensureAllocatedSize (minNumElements);
    }

    //==============================================================================
    /** Returns the CriticalSection that locks this array.
        To lock, you can call getLock().enter() and getLock().exit(), or preferably use
        an object of ScopedLockType as an RAII lock for it.
    */
    inline const TypeOfCriticalSectionToUse& getLock() const noexcept      { return data; }

    /** Returns the type of scoped lock to use for locking this array */
    typedef typename TypeOfCriticalSectionToUse::ScopedLockType ScopedLockType;


    //==============================================================================
   #ifndef DOXYGEN
    // Note that the swapWithArray method has been replaced by a more flexible templated version,
    // and renamed "swapWith" to be more consistent with the names used in other classes.
    JUCE_DEPRECATED_WITH_BODY (void swapWithArray (ReferenceCountedArray& other) noexcept, { swapWith (other); })
   #endif

private:
    //==============================================================================
    ArrayAllocationBase<ObjectClass*, TypeOfCriticalSectionToUse> data;
    int numUsed = 0;

    void releaseAllObjects()
    {
        while (numUsed > 0)
            releaseObject (data.elements[--numUsed]);

        jassert (numUsed == 0);
    }

    static void releaseObject (ObjectClass* o)
    {
        if (o != nullptr && o->decReferenceCountWithoutDeleting())
            ContainerDeletePolicy<ObjectClass>::destroy (o);
    }
};

} // namespace juce
