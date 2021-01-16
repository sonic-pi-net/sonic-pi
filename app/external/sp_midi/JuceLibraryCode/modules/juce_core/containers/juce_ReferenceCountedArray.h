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

    @tags{Core}
*/
template <class ObjectClass, class TypeOfCriticalSectionToUse = DummyCriticalSection>
class ReferenceCountedArray
{
public:
    using ObjectClassPtr = ReferenceCountedObjectPtr<ObjectClass>;

    //==============================================================================
    /** Creates an empty array.
        @see ReferenceCountedObject, Array, OwnedArray
    */
    ReferenceCountedArray() = default;

    /** Creates a copy of another array */
    ReferenceCountedArray (const ReferenceCountedArray& other) noexcept
    {
        const ScopedLockType lock (other.getLock());
        values.addArray (other.begin(), other.size());

        for (auto* o : *this)
            if (o != nullptr)
                o->incReferenceCount();
    }

    /** Moves from another array */
    ReferenceCountedArray (ReferenceCountedArray&& other) noexcept
        : values (std::move (other.values))
    {
    }

    /** Creates a copy of another array */
    template <class OtherObjectClass, class OtherCriticalSection>
    ReferenceCountedArray (const ReferenceCountedArray<OtherObjectClass, OtherCriticalSection>& other) noexcept
    {
        const typename ReferenceCountedArray<OtherObjectClass, OtherCriticalSection>::ScopedLockType lock (other.getLock());
        values.addArray (other.begin(), other.size());

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
        values = std::move (other.values);
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
        Any objects in the array whose reference counts drop to zero will be deleted.
    */
    void clear()
    {
        const ScopedLockType lock (getLock());
        clearQuick();
        values.setAllocatedSize (0);
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
        return values.size();
    }

    /** Returns true if the array is empty, false otherwise. */
    inline bool isEmpty() const noexcept
    {
        return size() == 0;
    }

    /** Returns a pointer to the object at this index in the array.

        If the index is out-of-range, this will return a null pointer, (and
        it could be null anyway, because it's ok for the array to hold null
        pointers as well as objects).

        @see getUnchecked
    */
    inline ObjectClassPtr operator[] (int index) const noexcept
    {
        return ObjectClassPtr (getObjectPointer (index));
    }

    /** Returns a pointer to the object at this index in the array, without checking
        whether the index is in-range.

        This is a faster and less safe version of operator[] which doesn't check the index passed in, so
        it can be used when you're sure the index is always going to be legal.
    */
    inline ObjectClassPtr getUnchecked (int index) const noexcept
    {
        return ObjectClassPtr (getObjectPointerUnchecked (index));
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
        return values.getValueWithDefault (index);
    }

    /** Returns a raw pointer to the object at this index in the array, without checking
        whether the index is in-range.
    */
    inline ObjectClass* getObjectPointerUnchecked (int index) const noexcept
    {
        const ScopedLockType lock (getLock());
        return values[index];
    }

    /** Returns a pointer to the first object in the array.

        This will return a null pointer if the array's empty.
        @see getLast
    */
    inline ObjectClassPtr getFirst() const noexcept
    {
        const ScopedLockType lock (getLock());
        return values.getFirst();
    }

    /** Returns a pointer to the last object in the array.

        This will return a null pointer if the array's empty.
        @see getFirst
    */
    inline ObjectClassPtr getLast() const noexcept
    {
        const ScopedLockType lock (getLock());
        return values.getLast();
    }

    /** Returns a pointer to the actual array data.
        This pointer will only be valid until the next time a non-const method
        is called on the array.
    */
    inline ObjectClass** getRawDataPointer() const noexcept
    {
        return values.begin();
    }

    //==============================================================================
    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ObjectClass** begin() noexcept
    {
        return values.begin();
    }

    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ObjectClass* const* begin() const noexcept
    {
        return values.begin();
    }

    /** Returns a pointer to the element which follows the last element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ObjectClass** end() noexcept
    {
        return values.end();
    }

    /** Returns a pointer to the element which follows the last element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ObjectClass* const* end() const noexcept
    {
        return values.end();
    }

    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with the standard C++ containers.
    */
    inline ObjectClass** data() noexcept
    {
        return begin();
    }

    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with the standard C++ containers.
    */
    inline ObjectClass* const* data() const noexcept
    {
        return begin();
    }

    //==============================================================================
    /** Finds the index of the first occurrence of an object in the array.

        @param objectToLookFor    the object to look for
        @returns                  the index at which the object was found, or -1 if it's not found
    */
    int indexOf (const ObjectClass* objectToLookFor) const noexcept
    {
        const ScopedLockType lock (getLock());
        auto* e = values.begin();
        auto* endPointer = values.end();

        while (e != endPointer)
        {
            if (objectToLookFor == *e)
                return static_cast<int> (e - values.begin());

            ++e;
        }

        return -1;
    }

    /** Finds the index of the first occurrence of an object in the array.

        @param objectToLookFor    the object to look for
        @returns                  the index at which the object was found, or -1 if it's not found
    */
    int indexOf (const ObjectClassPtr& objectToLookFor) const noexcept      { return indexOf (objectToLookFor.get()); }

    /** Returns true if the array contains a specified object.

        @param objectToLookFor      the object to look for
        @returns                    true if the object is in the array
    */
    bool contains (const ObjectClass* objectToLookFor) const noexcept
    {
        const ScopedLockType lock (getLock());
        auto* e = values.begin();
        auto* endPointer = values.end();

        while (e != endPointer)
        {
            if (objectToLookFor == *e)
                return true;

            ++e;
        }

        return false;
    }

    /** Returns true if the array contains a specified object.

        @param objectToLookFor      the object to look for
        @returns                    true if the object is in the array
    */
    bool contains (const ObjectClassPtr& objectToLookFor) const noexcept    { return contains (objectToLookFor.get()); }

    /** Appends a new object to the end of the array.

        This will increase the new object's reference count.

        @param newObject       the new object to add to the array
        @see set, insert, addIfNotAlreadyThere, addSorted, addArray
    */
    ObjectClass* add (ObjectClass* newObject)
    {
        const ScopedLockType lock (getLock());
        values.add (newObject);

        if (newObject != nullptr)
            newObject->incReferenceCount();

        return newObject;
    }

    /** Appends a new object to the end of the array.

        This will increase the new object's reference count.

        @param newObject       the new object to add to the array
        @see set, insert, addIfNotAlreadyThere, addSorted, addArray
    */
    ObjectClass* add (const ObjectClassPtr& newObject)          { return add (newObject.get()); }

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
    ObjectClass* insert (int indexToInsertAt, ObjectClass* newObject)
    {
        values.insert (indexToInsertAt, newObject, 1);

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
    ObjectClass* insert (int indexToInsertAt, const ObjectClassPtr& newObject)      { return insert (indexToInsertAt, newObject.get()); }

    /** Appends a new object at the end of the array as long as the array doesn't
        already contain it.

        If the array already contains a matching object, nothing will be done.

        @param newObject   the new object to add to the array
        @returns           true if the object has been added, false otherwise
    */
    bool addIfNotAlreadyThere (ObjectClass* newObject)
    {
        const ScopedLockType lock (getLock());

        if (contains (newObject))
            return false;

        add (newObject);
        return true;
    }

    /** Appends a new object at the end of the array as long as the array doesn't
        already contain it.

        If the array already contains a matching object, nothing will be done.

        @param newObject   the new object to add to the array
        @returns           true if the object has been added, false otherwise
    */
    bool addIfNotAlreadyThere (const ObjectClassPtr& newObject)         { return addIfNotAlreadyThere (newObject.get()); }

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

            if (indexToChange < values.size())
            {
                auto* e = values[indexToChange];
                values[indexToChange] = newObject;
                releaseObject (e);
            }
            else
            {
                values.add (newObject);
            }
        }
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
    void set (int indexToChange, const ObjectClassPtr& newObject)       { set (indexToChange, newObject.get()); }

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

            auto numElementsAdded = values.addArray (arrayToAddFrom.values, startIndex, numElementsToAdd);
            auto** e = values.end();

            for (int i = 0; i < numElementsAdded; ++i)
                (*(--e))->incReferenceCount();
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
        auto index = findInsertIndexInSortedArray (comparator, values.begin(), newObject, 0, values.size());
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
        auto index = findInsertIndexInSortedArray (comparator, values.begin(), newObject, 0, values.size());

        if (index > 0 && comparator.compareElements (newObject, values[index - 1]) == 0)
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
        int s = 0, e = values.size();

        while (s < e)
        {
            if (comparator.compareElements (objectToLookFor, values[s]) == 0)
                return s;

            auto halfway = (s + e) / 2;

            if (halfway == s)
                break;

            if (comparator.compareElements (objectToLookFor, values[halfway]) >= 0)
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

        if (isPositiveAndBelow (indexToRemove, values.size()))
        {
            auto* e = *(values.begin() + indexToRemove);
            values.removeElements (indexToRemove, 1);
            releaseObject (e);

            if ((values.size() << 1) < values.capacity())
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

        if (isPositiveAndBelow (indexToRemove, values.size()))
        {
            auto* e = *(values.begin() + indexToRemove);
            removedItem = e;
            values.removeElements (indexToRemove, 1);
            releaseObject (e);

            if ((values.size() << 1) < values.capacity())
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

    /** Removes the first occurrence of a specified object from the array.

        If the item isn't found, no action is taken. If it is found, it is
        removed and has its reference count decreased.

        @param objectToRemove   the object to try to remove
        @see remove, removeRange
    */
    void removeObject (const ObjectClassPtr& objectToRemove)    { removeObject (objectToRemove.get()); }

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
        startIndex    = jlimit (0, values.size(), startIndex);
        auto endIndex = jlimit (0, values.size(), startIndex + numberToRemove);
        numberToRemove = endIndex - startIndex;

        if (numberToRemove > 0)
        {
            Array<ObjectClass*> objectsToRemove;
            objectsToRemove.addArray (values.begin() + startIndex, numberToRemove);

            values.removeElements (startIndex, numberToRemove);

            for (auto& o : objectsToRemove)
                releaseObject (o);

            if ((values.size() << 1) < values.capacity())
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

        if (howManyToRemove > values.size())
            howManyToRemove = values.size();

        while (--howManyToRemove >= 0)
            remove (values.size() - 1);
    }

    /** Swaps a pair of objects in the array.

        If either of the indexes passed in is out-of-range, nothing will happen,
        otherwise the two objects at these positions will be exchanged.
    */
    void swap (int index1, int index2) noexcept
    {
        const ScopedLockType lock (getLock());

        if (isPositiveAndBelow (index1, values.size())
         && isPositiveAndBelow (index2, values.size()))
        {
            std::swap (values[index1], values[index2]);
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
            values.move (currentIndex, newIndex);
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
        values.swapWith (otherArray.values);
    }

    //==============================================================================
    /** Compares this array to another one.

        @returns true only if the other array contains the same objects in the same order
    */
    bool operator== (const ReferenceCountedArray& other) const noexcept
    {
        const ScopedLockType lock2 (other.getLock());
        const ScopedLockType lock1 (getLock());
        return values == other.values;
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
               bool retainOrderOfEquivalentItems = false) noexcept
    {
        // If you pass in an object with a static compareElements() method, this
        // avoids getting warning messages about the parameter being unused
        ignoreUnused (comparator);

        const ScopedLockType lock (getLock());
        sortArray (comparator, values.begin(), 0, values.size() - 1, retainOrderOfEquivalentItems);
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
        values.shrinkToNoMoreThan (values.size());
    }

    /** Increases the array's internal storage to hold a minimum number of elements.

        Calling this before adding a large known number of elements means that
        the array won't have to keep dynamically resizing itself as the elements
        are added, and it'll therefore be more efficient.
    */
    void ensureStorageAllocated (const int minNumElements)
    {
        const ScopedLockType lock (getLock());
        values.ensureAllocatedSize (minNumElements);
    }

    //==============================================================================
    /** Returns the CriticalSection that locks this array.
        To lock, you can call getLock().enter() and getLock().exit(), or preferably use
        an object of ScopedLockType as an RAII lock for it.
    */
    inline const TypeOfCriticalSectionToUse& getLock() const noexcept      { return values; }

    /** Returns the type of scoped lock to use for locking this array */
    using ScopedLockType = typename TypeOfCriticalSectionToUse::ScopedLockType;

    //==============================================================================
   #ifndef DOXYGEN
    // Note that the swapWithArray method has been replaced by a more flexible templated version,
    // and renamed "swapWith" to be more consistent with the names used in other classes.
    JUCE_DEPRECATED_WITH_BODY (void swapWithArray (ReferenceCountedArray& other) noexcept, { swapWith (other); })
   #endif

private:
    //==============================================================================
    ArrayBase<ObjectClass*, TypeOfCriticalSectionToUse> values;

    void releaseAllObjects()
    {
        auto i = values.size();

        while (--i >= 0)
        {
            auto* e = values[i];
            values.removeElements (i, 1);
            releaseObject (e);
        }
    }

    static void releaseObject (ObjectClass* o)
    {
        if (o != nullptr && o->decReferenceCountWithoutDeleting())
            ContainerDeletePolicy<ObjectClass>::destroy (o);
    }
};

} // namespace juce
