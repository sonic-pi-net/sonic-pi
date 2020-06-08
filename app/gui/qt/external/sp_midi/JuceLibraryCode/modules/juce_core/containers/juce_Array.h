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
    Holds a resizable array of primitive or copy-by-value objects.

    Examples of arrays are: Array<int>, Array<Rectangle> or Array<MyClass*>

    The Array class can be used to hold simple, non-polymorphic objects as well as primitive types - to
    do so, the class must fulfill these requirements:
    - it must have a copy constructor and assignment operator
    - it must be able to be relocated in memory by a memcpy without this causing any problems - so
      objects whose functionality relies on external pointers or references to themselves can not be used.

    You can of course have an array of pointers to any kind of object, e.g. Array<MyClass*>, but if
    you do this, the array doesn't take any ownership of the objects - see the OwnedArray class or the
    ReferenceCountedArray class for more powerful ways of holding lists of objects.

    For holding lists of strings, you can use Array\<String\>, but it's usually better to use the
    specialised class StringArray, which provides more useful functions.

    To make all the array's methods thread-safe, pass in "CriticalSection" as the templated
    TypeOfCriticalSectionToUse parameter, instead of the default DummyCriticalSection.

    @see OwnedArray, ReferenceCountedArray, StringArray, CriticalSection

    @tags{Core}
*/
template <typename ElementType,
          typename TypeOfCriticalSectionToUse = DummyCriticalSection,
          int minimumAllocatedSize = 0>
class Array
{
private:
    using ParameterType = typename TypeHelpers::ParameterType<ElementType>::type;

public:
    //==============================================================================
    /** Creates an empty array. */
    Array() = default;

    /** Creates a copy of another array.
        @param other    the array to copy
    */
    Array (const Array& other)
    {
        const ScopedLockType lock (other.getLock());
        values.addArray (other.values.begin(), other.values.size());
    }

    Array (Array&& other) noexcept
        : values (std::move (other.values))
    {
    }

    /** Initalises from a null-terminated raw array of values.
        @param data   the data to copy from
    */
    template <typename TypeToCreateFrom>
    explicit Array (const TypeToCreateFrom* data)
    {
        while (*values != TypeToCreateFrom())
            add (*data++);
    }

    /** Initalises from a raw array of values.
        @param data         the data to copy from
        @param numValues    the number of values in the array
    */
    template <typename TypeToCreateFrom>
    Array (const TypeToCreateFrom* data, int numValues)
    {
        values.addArray (data, numValues);
    }

    /** Initalises an Array of size 1 containing a single element. */
    Array (const ElementType& singleElementToAdd)
    {
        add (singleElementToAdd);
    }

    /** Initalises an Array of size 1 containing a single element. */
    Array (ElementType&& singleElementToAdd)
    {
        add (std::move (singleElementToAdd));
    }

    /** Initalises an Array from a list of items. */
    template <typename... OtherElements>
    Array (const ElementType& firstNewElement, OtherElements... otherElements)
    {
        values.add (firstNewElement, otherElements...);
    }

    /** Initalises an Array from a list of items. */
    template <typename... OtherElements>
    Array (ElementType&& firstNewElement, OtherElements... otherElements)
    {
        values.add (std::move (firstNewElement), otherElements...);
    }

    template <typename TypeToCreateFrom>
    Array (const std::initializer_list<TypeToCreateFrom>& items)
    {
        addArray (items);
    }

    /** Destructor. */
    ~Array() = default;

    /** Copies another array.
        @param other    the array to copy
    */
    Array& operator= (const Array& other)
    {
        if (this != &other)
        {
            auto otherCopy (other);
            swapWith (otherCopy);
        }

        return *this;
    }

    Array& operator= (Array&& other) noexcept
    {
        const ScopedLockType lock (getLock());
        values = std::move (other.values);
        return *this;
    }

    //==============================================================================
    /** Compares this array to another one.
        Two arrays are considered equal if they both contain the same set of
        elements, in the same order.
        @param other    the other array to compare with
    */
    template <class OtherArrayType>
    bool operator== (const OtherArrayType& other) const
    {
        const ScopedLockType lock (getLock());
        const typename OtherArrayType::ScopedLockType lock2 (other.getLock());
        return values == other;
    }

    /** Compares this array to another one.
        Two arrays are considered equal if they both contain the same set of
        elements, in the same order.
        @param other    the other array to compare with
    */
    template <class OtherArrayType>
    bool operator!= (const OtherArrayType& other) const
    {
        return ! operator== (other);
    }

    //==============================================================================
    /** Removes all elements from the array.
        This will remove all the elements, and free any storage that the array is
        using. To clear the array without freeing the storage, use the clearQuick()
        method instead.

        @see clearQuick
    */
    void clear()
    {
        const ScopedLockType lock (getLock());
        clearQuick();
        values.setAllocatedSize (0);
    }

    /** Removes all elements from the array without freeing the array's allocated storage.
        @see clear
    */
    void clearQuick()
    {
        const ScopedLockType lock (getLock());
        values.clear();
    }

    /** Fills the Array with the provided value. */
    void fill (const ParameterType& newValue) noexcept
    {
        const ScopedLockType lock (getLock());

        for (auto& e : *this)
            e = newValue;
    }

    //==============================================================================
    /** Returns the current number of elements in the array. */
    inline int size() const noexcept
    {
        const ScopedLockType lock (getLock());
        return values.size();
    }

    /** Returns true if the array is empty, false otherwise. */
    inline bool isEmpty() const noexcept
    {
        return size() == 0;
    }

    /** Returns one of the elements in the array.
        If the index passed in is beyond the range of valid elements, this
        will return a default value.

        If you're certain that the index will always be a valid element, you
        can call getUnchecked() instead, which is faster.

        @param index    the index of the element being requested (0 is the first element in the array)
        @see getUnchecked, getFirst, getLast
    */
    ElementType operator[] (int index) const
    {
        const ScopedLockType lock (getLock());
        return values.getValueWithDefault (index);
    }

    /** Returns one of the elements in the array, without checking the index passed in.

        Unlike the operator[] method, this will try to return an element without
        checking that the index is within the bounds of the array, so should only
        be used when you're confident that it will always be a valid index.

        @param index    the index of the element being requested (0 is the first element in the array)
        @see operator[], getFirst, getLast
    */
    inline ElementType getUnchecked (int index) const
    {
        const ScopedLockType lock (getLock());
        return values[index];
    }

    /** Returns a direct reference to one of the elements in the array, without checking the index passed in.

        This is like getUnchecked, but returns a direct reference to the element, so that
        you can alter it directly. Obviously this can be dangerous, so only use it when
        absolutely necessary.

        @param index    the index of the element being requested (0 is the first element in the array)
        @see operator[], getFirst, getLast
    */
    inline ElementType& getReference (int index) noexcept
    {
        const ScopedLockType lock (getLock());
        return values[index];
    }

    /** Returns a direct reference to one of the elements in the array, without checking the index passed in.

        This is like getUnchecked, but returns a direct reference to the element. Obviously
        this can be dangerous, so only use it when absolutely necessary.

        @param index    the index of the element being requested (0 is the first element in the array)
        @see operator[], getFirst, getLast
    */
    inline const ElementType& getReference (int index) const noexcept
    {
        const ScopedLockType lock (getLock());
        return values[index];
    }

    /** Returns the first element in the array, or a default value if the array is empty.
        @see operator[], getUnchecked, getLast
    */
    inline ElementType getFirst() const noexcept
    {
        const ScopedLockType lock (getLock());
        return values.getFirst();
    }

    /** Returns the last element in the array, or a default value if the array is empty.

        @see operator[], getUnchecked, getFirst
    */
    inline ElementType getLast() const noexcept
    {
        const ScopedLockType lock (getLock());
        return values.getLast();
    }

    /** Returns a pointer to the actual array data.
        This pointer will only be valid until the next time a non-const method
        is called on the array.
    */
    inline ElementType* getRawDataPointer() noexcept
    {
        return values.begin();
    }

    /** Returns a pointer to the actual array data.
        This pointer will only be valid until the next time a non-const method
        is called on the array.
    */
    inline const ElementType* getRawDataPointer() const noexcept
    {
        return values.begin();
    }

    //==============================================================================
    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ElementType* begin() noexcept
    {
        return values.begin();
    }

    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline const ElementType* begin() const noexcept
    {
        return values.begin();
    }

    /** Returns a pointer to the element which follows the last element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline ElementType* end() noexcept
    {
        return values.end();
    }

    /** Returns a pointer to the element which follows the last element in the array.
        This method is provided for compatibility with standard C++ iteration mechanisms.
    */
    inline const ElementType* end() const noexcept
    {
        return values.end();
    }

    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with the standard C++ containers.
    */
    inline ElementType* data() noexcept
    {
        return begin();
    }

    /** Returns a pointer to the first element in the array.
        This method is provided for compatibility with the standard C++ containers.
    */
    inline const ElementType* data() const noexcept
    {
        return begin();
    }

    //==============================================================================
    /** Finds the index of the first element which matches the value passed in.

        This will search the array for the given object, and return the index
        of its first occurrence. If the object isn't found, the method will return -1.

        @param elementToLookFor   the value or object to look for
        @returns                  the index of the object, or -1 if it's not found
    */
    int indexOf (ParameterType elementToLookFor) const
    {
        const ScopedLockType lock (getLock());
        auto e = values.begin();
        auto endPtr = values.end();

        for (; e != endPtr; ++e)
            if (elementToLookFor == *e)
                return static_cast<int> (e - values.begin());

        return -1;
    }

    /** Returns true if the array contains at least one occurrence of an object.

        @param elementToLookFor     the value or object to look for
        @returns                    true if the item is found
    */
    bool contains (ParameterType elementToLookFor) const
    {
        const ScopedLockType lock (getLock());
        auto e = values.begin();
        auto endPtr = values.end();

        for (; e != endPtr; ++e)
            if (elementToLookFor == *e)
                return true;

        return false;
    }

    //==============================================================================
    /** Appends a new element at the end of the array.
        @param newElement       the new object to add to the array
        @see set, insert, addIfNotAlreadyThere, addSorted, addUsingDefaultSort, addArray
    */
    void add (const ElementType& newElement)
    {
        const ScopedLockType lock (getLock());
        values.add (newElement);
    }

    /** Appends a new element at the end of the array.
        @param newElement       the new object to add to the array
        @see set, insert, addIfNotAlreadyThere, addSorted, addUsingDefaultSort, addArray
    */
    void add (ElementType&& newElement)
    {
        const ScopedLockType lock (getLock());
        values.add (std::move (newElement));
    }

    /** Appends multiple new elements at the end of the array. */
    template <typename... OtherElements>
    void add (const ElementType& firstNewElement, OtherElements... otherElements)
    {
        const ScopedLockType lock (getLock());
        values.add (firstNewElement, otherElements...);
    }

    /** Appends multiple new elements at the end of the array. */
    template <typename... OtherElements>
    void add (ElementType&& firstNewElement, OtherElements... otherElements)
    {
        const ScopedLockType lock (getLock());
        values.add (std::move (firstNewElement), otherElements...);
    }

    /** Inserts a new element into the array at a given position.

        If the index is less than 0 or greater than the size of the array, the
        element will be added to the end of the array.
        Otherwise, it will be inserted into the array, moving all the later elements
        along to make room.

        @param indexToInsertAt    the index at which the new element should be
                                  inserted (pass in -1 to add it to the end)
        @param newElement         the new object to add to the array
        @see add, addSorted, addUsingDefaultSort, set
    */
    void insert (int indexToInsertAt, ParameterType newElement)
    {
        const ScopedLockType lock (getLock());
        values.insert (indexToInsertAt, newElement, 1);
    }

    /** Inserts multiple copies of an element into the array at a given position.

        If the index is less than 0 or greater than the size of the array, the
        element will be added to the end of the array.
        Otherwise, it will be inserted into the array, moving all the later elements
        along to make room.

        @param indexToInsertAt    the index at which the new element should be inserted
        @param newElement         the new object to add to the array
        @param numberOfTimesToInsertIt  how many copies of the value to insert
        @see insert, add, addSorted, set
    */
    void insertMultiple (int indexToInsertAt, ParameterType newElement,
                         int numberOfTimesToInsertIt)
    {
        if (numberOfTimesToInsertIt > 0)
        {
            const ScopedLockType lock (getLock());
            values.insert (indexToInsertAt, newElement, numberOfTimesToInsertIt);
        }
    }

    /** Inserts an array of values into this array at a given position.

        If the index is less than 0 or greater than the size of the array, the
        new elements will be added to the end of the array.
        Otherwise, they will be inserted into the array, moving all the later elements
        along to make room.

        @param indexToInsertAt      the index at which the first new element should be inserted
        @param newElements          the new values to add to the array
        @param numberOfElements     how many items are in the array
        @see insert, add, addSorted, set
    */
    void insertArray (int indexToInsertAt,
                      const ElementType* newElements,
                      int numberOfElements)
    {
        if (numberOfElements > 0)
        {
            const ScopedLockType lock (getLock());
            values.insertArray (indexToInsertAt, newElements, numberOfElements);
        }
    }

    /** Appends a new element at the end of the array as long as the array doesn't
        already contain it.

        If the array already contains an element that matches the one passed in, nothing
        will be done.

        @param newElement   the new object to add to the array
        @return             true if the element was added to the array; false otherwise.
    */
    bool addIfNotAlreadyThere (ParameterType newElement)
    {
        const ScopedLockType lock (getLock());

        if (contains (newElement))
            return false;

        add (newElement);
        return true;
    }

    /** Replaces an element with a new value.

        If the index is less than zero, this method does nothing.
        If the index is beyond the end of the array, the item is added to the end of the array.

        @param indexToChange    the index whose value you want to change
        @param newValue         the new value to set for this index.
        @see add, insert
    */
    void set (int indexToChange, ParameterType newValue)
    {
        if (indexToChange >= 0)
        {
            const ScopedLockType lock (getLock());

            if (indexToChange < values.size())
                values[indexToChange] = newValue;
            else
                values.add (newValue);
        }
        else
        {
            jassertfalse;
        }
    }

    /** Replaces an element with a new value without doing any bounds-checking.

        This just sets a value directly in the array's internal storage, so you'd
        better make sure it's in range!

        @param indexToChange    the index whose value you want to change
        @param newValue         the new value to set for this index.
        @see set, getUnchecked
    */
    void setUnchecked (int indexToChange, ParameterType newValue)
    {
        const ScopedLockType lock (getLock());
        jassert (isPositiveAndBelow (indexToChange, values.size()));
        values[indexToChange] = newValue;
    }

    /** Adds elements from an array to the end of this array.

        @param elementsToAdd        an array of some kind of object from which elements
                                    can be constructed.
        @param numElementsToAdd     how many elements are in this other array
        @see add
    */
    template <typename Type>
    void addArray (const Type* elementsToAdd, int numElementsToAdd)
    {
        const ScopedLockType lock (getLock());

        if (numElementsToAdd > 0)
            values.addArray (elementsToAdd, numElementsToAdd);
    }

    template <typename TypeToCreateFrom>
    void addArray (const std::initializer_list<TypeToCreateFrom>& items)
    {
        const ScopedLockType lock (getLock());
        values.addArray (items);
    }

    /** Adds elements from a null-terminated array of pointers to the end of this array.

        @param elementsToAdd    an array of pointers to some kind of object from which elements
                                can be constructed. This array must be terminated by a nullptr
        @see addArray
    */
    template <typename Type>
    void addNullTerminatedArray (const Type* const* elementsToAdd)
    {
        int num = 0;

        for (auto e = elementsToAdd; *e != nullptr; ++e)
            ++num;

        addArray (elementsToAdd, num);
    }

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

    /** Adds elements from another array to the end of this array.

        @param arrayToAddFrom       the array from which to copy the elements
        @see add
    */
    template <class OtherArrayType>
    void addArray (const OtherArrayType& arrayToAddFrom)
    {
        const typename OtherArrayType::ScopedLockType lock1 (arrayToAddFrom.getLock());
        const ScopedLockType lock2 (getLock());

        values.addArray (arrayToAddFrom);
    }

    /** Adds elements from another array to the end of this array.

        @param arrayToAddFrom       the array from which to copy the elements
        @param startIndex           the first element of the other array to start copying from
        @param numElementsToAdd     how many elements to add from the other array. If this
                                    value is negative or greater than the number of available elements,
                                    all available elements will be copied.
        @see add
    */
    template <class OtherArrayType>
    typename std::enable_if<! std::is_pointer<OtherArrayType>::value, void>::type
    addArray (const OtherArrayType& arrayToAddFrom,
              int startIndex,
              int numElementsToAdd = -1)
    {
        const typename OtherArrayType::ScopedLockType lock1 (arrayToAddFrom.getLock());
        const ScopedLockType lock2 (getLock());

        values.addArray (arrayToAddFrom, startIndex, numElementsToAdd);
    }

    /** This will enlarge or shrink the array to the given number of elements, by adding
        or removing items from its end.

        If the array is smaller than the given target size, empty elements will be appended
        until its size is as specified. If its size is larger than the target, items will be
        removed from its end to shorten it.
    */
    void resize (int targetNumItems)
    {
        jassert (targetNumItems >= 0);
        auto numToAdd = targetNumItems - values.size();

        if (numToAdd > 0)
            insertMultiple (values.size(), ElementType(), numToAdd);
        else if (numToAdd < 0)
            removeRange (targetNumItems, -numToAdd);
    }

    /** Inserts a new element into the array, assuming that the array is sorted.

        This will use a comparator to find the position at which the new element
        should go. If the array isn't sorted, the behaviour of this
        method will be unpredictable.

        @param comparator   the comparator to use to compare the elements - see the sort()
                            method for details about the form this object should take
        @param newElement   the new element to insert to the array
        @returns the index at which the new item was added
        @see addUsingDefaultSort, add, sort
    */
    template <class ElementComparator>
    int addSorted (ElementComparator& comparator, ParameterType newElement)
    {
        const ScopedLockType lock (getLock());
        auto index = findInsertIndexInSortedArray (comparator, values.begin(), newElement, 0, values.size());
        insert (index, newElement);
        return index;
    }

    /** Inserts a new element into the array, assuming that the array is sorted.

        This will use the DefaultElementComparator class for sorting, so your ElementType
        must be suitable for use with that class. If the array isn't sorted, the behaviour of this
        method will be unpredictable.

        @param newElement   the new element to insert to the array
        @see addSorted, sort
    */
    void addUsingDefaultSort (ParameterType newElement)
    {
        DefaultElementComparator <ElementType> comparator;
        addSorted (comparator, newElement);
    }

    /** Finds the index of an element in the array, assuming that the array is sorted.

        This will use a comparator to do a binary-chop to find the index of the given
        element, if it exists. If the array isn't sorted, the behaviour of this
        method will be unpredictable.

        @param comparator           the comparator to use to compare the elements - see the sort()
                                    method for details about the form this object should take
        @param elementToLookFor     the element to search for
        @returns                    the index of the element, or -1 if it's not found
        @see addSorted, sort
    */
    template <typename ElementComparator, typename TargetValueType>
    int indexOfSorted (ElementComparator& comparator, TargetValueType elementToLookFor) const
    {
        ignoreUnused (comparator); // if you pass in an object with a static compareElements() method, this
                                   // avoids getting warning messages about the parameter being unused

        const ScopedLockType lock (getLock());

        for (int s = 0, e = values.size();;)
        {
            if (s >= e)
                return -1;

            if (comparator.compareElements (elementToLookFor, values[s]) == 0)
                return s;

            auto halfway = (s + e) / 2;

            if (halfway == s)
                return -1;

            if (comparator.compareElements (elementToLookFor, values[halfway]) >= 0)
                s = halfway;
            else
                e = halfway;
        }
    }

    //==============================================================================
    /** Removes an element from the array.

        This will remove the element at a given index, and move back
        all the subsequent elements to close the gap.
        If the index passed in is out-of-range, nothing will happen.

        @param indexToRemove    the index of the element to remove
        @see removeAndReturn, removeFirstMatchingValue, removeAllInstancesOf, removeRange
    */
    void remove (int indexToRemove)
    {
        const ScopedLockType lock (getLock());

        if (isPositiveAndBelow (indexToRemove, values.size()))
            removeInternal (indexToRemove);
    }

    /** Removes an element from the array.

        This will remove the element at a given index, and move back
        all the subsequent elements to close the gap.
        If the index passed in is out-of-range, nothing will happen.

        @param indexToRemove    the index of the element to remove
        @returns                the element that has been removed
        @see removeFirstMatchingValue, removeAllInstancesOf, removeRange
    */
    ElementType removeAndReturn (int indexToRemove)
    {
        const ScopedLockType lock (getLock());

        if (isPositiveAndBelow (indexToRemove, values.size()))
        {
            ElementType removed (values[indexToRemove]);
            removeInternal (indexToRemove);
            return removed;
        }

        return ElementType();
    }

    /** Removes an element from the array.

        This will remove the element pointed to by the given iterator,
        and move back all the subsequent elements to close the gap.
        If the iterator passed in does not point to an element within the
        array, behaviour is undefined.

        @param elementToRemove  a pointer to the element to remove
        @see removeFirstMatchingValue, removeAllInstancesOf, removeRange, removeIf
    */
    void remove (const ElementType* elementToRemove)
    {
        jassert (elementToRemove != nullptr);
        const ScopedLockType lock (getLock());

        jassert (values.begin() != nullptr);
        auto indexToRemove = (int) (elementToRemove - values.begin());

        if (! isPositiveAndBelow (indexToRemove, values.size()))
        {
            jassertfalse;
            return;
        }

        removeInternal (indexToRemove);
    }

    /** Removes an item from the array.

        This will remove the first occurrence of the given element from the array.
        If the item isn't found, no action is taken.

        @param valueToRemove   the object to try to remove
        @see remove, removeRange, removeIf
    */
    void removeFirstMatchingValue (ParameterType valueToRemove)
    {
        const ScopedLockType lock (getLock());
        auto* e = values.begin();

        for (int i = 0; i < values.size(); ++i)
        {
            if (valueToRemove == e[i])
            {
                removeInternal (i);
                break;
            }
        }
    }

    /** Removes items from the array.

        This will remove all occurrences of the given element from the array.
        If no such items are found, no action is taken.

        @param valueToRemove   the object to try to remove
        @return how many objects were removed.
        @see remove, removeRange, removeIf
    */
    int removeAllInstancesOf (ParameterType valueToRemove)
    {
        int numRemoved = 0;
        const ScopedLockType lock (getLock());

        for (int i = values.size(); --i >= 0;)
        {
            if (valueToRemove == values[i])
            {
                removeInternal (i);
                ++numRemoved;
            }
        }

        return numRemoved;
    }

    /** Removes items from the array.

        This will remove all objects from the array that match a condition.
        If no such items are found, no action is taken.

        @param predicate   the condition when to remove an item. Must be a callable
                           type that takes an ElementType and returns a bool

        @return how many objects were removed.
        @see remove, removeRange, removeAllInstancesOf
    */
    template <typename PredicateType>
    int removeIf (PredicateType&& predicate)
    {
        int numRemoved = 0;
        const ScopedLockType lock (getLock());

        for (int i = values.size(); --i >= 0;)
        {
            if (predicate (values[i]))
            {
                removeInternal (i);
                ++numRemoved;
            }
        }

        return numRemoved;
    }

    /** Removes a range of elements from the array.

        This will remove a set of elements, starting from the given index,
        and move subsequent elements down to close the gap.

        If the range extends beyond the bounds of the array, it will
        be safely clipped to the size of the array.

        @param startIndex       the index of the first element to remove
        @param numberToRemove   how many elements should be removed
        @see remove, removeFirstMatchingValue, removeAllInstancesOf, removeIf
    */
    void removeRange (int startIndex, int numberToRemove)
    {
        const ScopedLockType lock (getLock());

        auto endIndex = jlimit (0, values.size(), startIndex + numberToRemove);
        startIndex    = jlimit (0, values.size(), startIndex);
        numberToRemove = endIndex - startIndex;

        if (numberToRemove > 0)
        {
            values.removeElements (startIndex, numberToRemove);
            minimiseStorageAfterRemoval();
        }
    }

    /** Removes the last n elements from the array.

        @param howManyToRemove   how many elements to remove from the end of the array
        @see remove, removeFirstMatchingValue, removeAllInstancesOf, removeRange
    */
    void removeLast (int howManyToRemove = 1)
    {
        jassert (howManyToRemove >= 0);

        if (howManyToRemove > 0)
        {
            const ScopedLockType lock (getLock());

            if (howManyToRemove > values.size())
                howManyToRemove = values.size();

            values.removeElements (values.size() - howManyToRemove, howManyToRemove);
            minimiseStorageAfterRemoval();
        }
    }

    /** Removes any elements which are also in another array.

        @param otherArray   the other array in which to look for elements to remove
        @see removeValuesNotIn, remove, removeFirstMatchingValue, removeAllInstancesOf, removeRange
    */
    template <class OtherArrayType>
    void removeValuesIn (const OtherArrayType& otherArray)
    {
        const typename OtherArrayType::ScopedLockType lock1 (otherArray.getLock());
        const ScopedLockType lock2 (getLock());

        if (this == &otherArray)
        {
            clear();
        }
        else
        {
            if (otherArray.size() > 0)
            {
                for (int i = values.size(); --i >= 0;)
                    if (otherArray.contains (values[i]))
                        removeInternal (i);
            }
        }
    }

    /** Removes any elements which are not found in another array.

        Only elements which occur in this other array will be retained.

        @param otherArray    the array in which to look for elements NOT to remove
        @see removeValuesIn, remove, removeFirstMatchingValue, removeAllInstancesOf, removeRange
    */
    template <class OtherArrayType>
    void removeValuesNotIn (const OtherArrayType& otherArray)
    {
        const typename OtherArrayType::ScopedLockType lock1 (otherArray.getLock());
        const ScopedLockType lock2 (getLock());

        if (this != &otherArray)
        {
            if (otherArray.size() <= 0)
            {
                clear();
            }
            else
            {
                for (int i = values.size(); --i >= 0;)
                    if (! otherArray.contains (values[i]))
                        removeInternal (i);
            }
        }
    }

    /** Swaps over two elements in the array.

        This swaps over the elements found at the two indexes passed in.
        If either index is out-of-range, this method will do nothing.

        @param index1   index of one of the elements to swap
        @param index2   index of the other element to swap
    */
    void swap (int index1, int index2)
    {
        const ScopedLockType lock (getLock());
        values.swap (index1, index2);
    }

    /** Moves one of the values to a different position.

        This will move the value to a specified index, shuffling along
        any intervening elements as required.

        So for example, if you have the array { 0, 1, 2, 3, 4, 5 } then calling
        move (2, 4) would result in { 0, 1, 3, 4, 2, 5 }.

        @param currentIndex     the index of the value to be moved. If this isn't a
                                valid index, then nothing will be done
        @param newIndex         the index at which you'd like this value to end up. If this
                                is less than zero, the value will be moved to the end
                                of the array
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
    /** Reduces the amount of storage being used by the array.

        Arrays typically allocate slightly more storage than they need, and after
        removing elements, they may have quite a lot of unused space allocated.
        This method will reduce the amount of allocated storage to a minimum.
    */
    void minimiseStorageOverheads()
    {
        const ScopedLockType lock (getLock());
        values.shrinkToNoMoreThan (values.size());
    }

    /** Increases the array's internal storage to hold a minimum number of elements.

        Calling this before adding a large known number of elements means that
        the array won't have to keep dynamically resizing itself as the elements
        are added, and it'll therefore be more efficient.
    */
    void ensureStorageAllocated (int minNumElements)
    {
        const ScopedLockType lock (getLock());
        values.ensureAllocatedSize (minNumElements);
    }

    //==============================================================================
    /** Sorts the array using a default comparison operation.
        If the type of your elements isn't supported by the DefaultElementComparator class
        then you may need to use the other version of sort, which takes a custom comparator.
    */
    void sort()
    {
        DefaultElementComparator<ElementType> comparator;
        sort (comparator);
    }

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

        @see addSorted, indexOfSorted, sortArray
    */
    template <class ElementComparator>
    void sort (ElementComparator& comparator,
               bool retainOrderOfEquivalentItems = false)
    {
        const ScopedLockType lock (getLock());
        ignoreUnused (comparator); // if you pass in an object with a static compareElements() method, this
                                   // avoids getting warning messages about the parameter being unused
        sortArray (comparator, values.begin(), 0, size() - 1, retainOrderOfEquivalentItems);
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
    JUCE_DEPRECATED_WITH_BODY (void swapWithArray (Array& other) noexcept, { swapWith (other); })
   #endif

private:
    //==============================================================================
    ArrayBase<ElementType, TypeOfCriticalSectionToUse> values;

    void removeInternal (int indexToRemove)
    {
        values.removeElements (indexToRemove, 1);
        minimiseStorageAfterRemoval();
    }

    void minimiseStorageAfterRemoval()
    {
        if (values.capacity() > jmax (minimumAllocatedSize, values.size() * 2))
            values.shrinkToNoMoreThan (jmax (values.size(), jmax (minimumAllocatedSize, 64 / (int) sizeof (ElementType))));
    }
};

} // namespace juce
