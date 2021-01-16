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

/**
    A basic object container.

    This class isn't really for public use - it's used by the other
    array classes, but might come in handy for some purposes.

    It inherits from a critical section class to allow the arrays to use
    the "empty base class optimisation" pattern to reduce their footprint.

    @see Array, OwnedArray, ReferenceCountedArray

    @tags{Core}
*/
template <class ElementType, class TypeOfCriticalSectionToUse>
class ArrayBase  : public TypeOfCriticalSectionToUse
{
private:
    using ParameterType = typename TypeHelpers::ParameterType<ElementType>::type;

    template <class OtherElementType, class OtherCriticalSection>
    using AllowConversion = typename std::enable_if<! std::is_same<std::tuple<ElementType, TypeOfCriticalSectionToUse>,
                                                                   std::tuple<OtherElementType, OtherCriticalSection>>::value>::type;

public:
    //==============================================================================
    ArrayBase() = default;

    ~ArrayBase()
    {
        clear();
    }

    ArrayBase (ArrayBase&& other) noexcept
        : elements (std::move (other.elements)),
          numAllocated (other.numAllocated),
          numUsed (other.numUsed)
    {
        other.numAllocated = 0;
        other.numUsed = 0;
    }

    ArrayBase& operator= (ArrayBase&& other) noexcept
    {
        if (this != &other)
        {
            auto tmp (std::move (other));
            swapWith (tmp);
        }

        return *this;
    }

    /** Converting move constructor.
        Only enabled when the other array has a different type to this one.
        If you see a compile error here, it's probably because you're attempting a conversion that
        HeapBlock won't allow.
    */
    template <class OtherElementType,
              class OtherCriticalSection,
              typename = AllowConversion<OtherElementType, OtherCriticalSection>>
    ArrayBase (ArrayBase<OtherElementType, OtherCriticalSection>&& other) noexcept
        : elements (std::move (other.elements)),
          numAllocated (other.numAllocated),
          numUsed (other.numUsed)
    {
        other.numAllocated = 0;
        other.numUsed = 0;
    }

    /** Converting move assignment operator.
        Only enabled when the other array has a different type to this one.
        If you see a compile error here, it's probably because you're attempting a conversion that
        HeapBlock won't allow.
    */
    template <class OtherElementType,
              class OtherCriticalSection,
              typename = AllowConversion<OtherElementType, OtherCriticalSection>>
    ArrayBase& operator= (ArrayBase<OtherElementType, OtherCriticalSection>&& other) noexcept
    {
        // No need to worry about assignment to *this, because 'other' must be of a different type.
        elements = std::move (other.elements);
        numAllocated = other.numAllocated;
        numUsed = other.numUsed;

        other.numAllocated = 0;
        other.numUsed = 0;

        return *this;
    }

    //==============================================================================
    template <class OtherArrayType>
    bool operator== (const OtherArrayType& other) const noexcept
    {
        if (size() != (int) other.size())
            return false;

        auto* e = begin();

        for (auto& o : other)
            if (! (*e++ == o))
                return false;

        return true;
    }

    template <class OtherArrayType>
    bool operator!= (const OtherArrayType& other) const noexcept
    {
        return ! operator== (other);
    }

    //==============================================================================
    inline ElementType& operator[] (const int index) noexcept
    {
        jassert (elements != nullptr);
        jassert (isPositiveAndBelow (index, numUsed));
        return elements[index];
    }

    inline const ElementType& operator[] (const int index) const noexcept
    {
        jassert (elements != nullptr);
        jassert (isPositiveAndBelow (index, numUsed));
        return elements[index];
    }

    inline ElementType getValueWithDefault (const int index) const noexcept
    {
        return isPositiveAndBelow (index, numUsed) ? elements[index] : ElementType();
    }

    inline ElementType getFirst() const noexcept
    {
        return numUsed > 0 ? elements[0] : ElementType();
    }

    inline ElementType getLast() const noexcept
    {
        return numUsed > 0 ? elements[numUsed - 1] : ElementType();
    }

    //==============================================================================
    inline ElementType* begin() noexcept
    {
        return elements;
    }

    inline const ElementType* begin() const noexcept
    {
        return elements;
    }

    inline ElementType* end() noexcept
    {
        return elements + numUsed;
    }

    inline const ElementType* end() const noexcept
    {
        return elements + numUsed;
    }

    inline ElementType* data() noexcept
    {
        return elements;
    }

    inline const ElementType* data() const noexcept
    {
        return elements;
    }

    inline int size() const noexcept
    {
        return numUsed;
    }

    inline int capacity() const noexcept
    {
        return numAllocated;
    }

    //==============================================================================
    void setAllocatedSize (int numElements)
    {
        jassert (numElements >= numUsed);

        if (numAllocated != numElements)
        {
            if (numElements > 0)
                setAllocatedSizeInternal (numElements);
            else
                elements.free();
        }

        numAllocated = numElements;
    }

    void ensureAllocatedSize (int minNumElements)
    {
        if (minNumElements > numAllocated)
            setAllocatedSize ((minNumElements + minNumElements / 2 + 8) & ~7);

        jassert (numAllocated <= 0 || elements != nullptr);
    }

    void shrinkToNoMoreThan (int maxNumElements)
    {
        if (maxNumElements < numAllocated)
            setAllocatedSize (maxNumElements);
    }

    void clear()
    {
        for (int i = 0; i < numUsed; ++i)
            elements[i].~ElementType();

        numUsed = 0;
    }

    //==============================================================================
    void swapWith (ArrayBase& other) noexcept
    {
        elements.swapWith (other.elements);
        std::swap (numAllocated, other.numAllocated);
        std::swap (numUsed,      other.numUsed);
    }

    //==============================================================================
    void add (const ElementType& newElement)
    {
        checkSourceIsNotAMember (&newElement);
        ensureAllocatedSize (numUsed + 1);
        addAssumingCapacityIsReady (newElement);
    }

    void add (ElementType&& newElement)
    {
        checkSourceIsNotAMember (&newElement);
        ensureAllocatedSize (numUsed + 1);
        addAssumingCapacityIsReady (std::move (newElement));
    }

    template <typename... OtherElements>
    void add (const ElementType& firstNewElement, OtherElements... otherElements)
    {
        checkSourceIsNotAMember (&firstNewElement);
        ensureAllocatedSize (numUsed + 1 + (int) sizeof... (otherElements));
        addAssumingCapacityIsReady (firstNewElement, otherElements...);
    }

    template <typename... OtherElements>
    void add (ElementType&& firstNewElement, OtherElements... otherElements)
    {
        checkSourceIsNotAMember (&firstNewElement);
        ensureAllocatedSize (numUsed + 1 + (int) sizeof... (otherElements));
        addAssumingCapacityIsReady (std::move (firstNewElement), otherElements...);
    }

    //==============================================================================
    template <typename Type>
    void addArray (const Type* elementsToAdd, int numElementsToAdd)
    {
        ensureAllocatedSize (numUsed + numElementsToAdd);
        addArrayInternal (elementsToAdd, numElementsToAdd);
        numUsed += numElementsToAdd;
    }

    template <typename TypeToCreateFrom>
    void addArray (const std::initializer_list<TypeToCreateFrom>& items)
    {
        ensureAllocatedSize (numUsed + (int) items.size());

        for (auto& item : items)
            new (elements + numUsed++) ElementType (item);
    }

    template <class OtherArrayType>
    void addArray (const OtherArrayType& arrayToAddFrom)
    {
        jassert ((const void*) this != (const void*) &arrayToAddFrom); // can't add from our own elements!
        ensureAllocatedSize (numUsed + (int) arrayToAddFrom.size());

        for (auto& e : arrayToAddFrom)
            addAssumingCapacityIsReady (e);
    }

    template <class OtherArrayType>
    typename std::enable_if<! std::is_pointer<OtherArrayType>::value, int>::type
    addArray (const OtherArrayType& arrayToAddFrom,
              int startIndex, int numElementsToAdd = -1)
    {
        jassert ((const void*) this != (const void*) &arrayToAddFrom); // can't add from our own elements!

        if (startIndex < 0)
        {
            jassertfalse;
            startIndex = 0;
        }

        if (numElementsToAdd < 0 || startIndex + numElementsToAdd > (int) arrayToAddFrom.size())
            numElementsToAdd = (int) arrayToAddFrom.size() - startIndex;

        addArray (arrayToAddFrom.data() + startIndex, numElementsToAdd);

        return numElementsToAdd;
    }

    //==============================================================================
    void insert (int indexToInsertAt, ParameterType newElement, int numberOfTimesToInsertIt)
    {
        checkSourceIsNotAMember (&newElement);
        auto* space = createInsertSpace (indexToInsertAt, numberOfTimesToInsertIt);

        for (int i = 0; i < numberOfTimesToInsertIt; ++i)
            new (space++) ElementType (newElement);

        numUsed += numberOfTimesToInsertIt;
    }

    void insertArray (int indexToInsertAt, const ElementType* newElements, int numberOfElements)
    {
        auto* space = createInsertSpace (indexToInsertAt, numberOfElements);

        for (int i = 0; i < numberOfElements; ++i)
            new (space++) ElementType (*(newElements++));

        numUsed += numberOfElements;
    }

    //==============================================================================
    void removeElements (int indexToRemoveAt, int numElementsToRemove)
    {
        jassert (indexToRemoveAt >= 0);
        jassert (numElementsToRemove >= 0);
        jassert (indexToRemoveAt + numElementsToRemove <= numUsed);

        if (numElementsToRemove > 0)
        {
            removeElementsInternal (indexToRemoveAt, numElementsToRemove);
            numUsed -= numElementsToRemove;
        }
    }

    //==============================================================================
    void swap (int index1, int index2)
    {
        if (isPositiveAndBelow (index1, numUsed)
         && isPositiveAndBelow (index2, numUsed))
        {
            std::swap (elements[index1],
                       elements[index2]);
        }
    }

    //==============================================================================
    void move (int currentIndex, int newIndex) noexcept
    {
        if (isPositiveAndBelow (currentIndex, numUsed))
        {
            if (! isPositiveAndBelow (newIndex, numUsed))
                newIndex = numUsed - 1;

            moveInternal (currentIndex, newIndex);
        }
    }

private:
    //==============================================================================
    template <typename T>
   #if defined(__GNUC__) && __GNUC__ < 5 && ! defined(__clang__)
    using IsTriviallyCopyable = std::is_scalar<T>;
   #else
    using IsTriviallyCopyable = std::is_trivially_copyable<T>;
   #endif

    template <typename T>
    using TriviallyCopyableVoid = typename std::enable_if<IsTriviallyCopyable<T>::value, void>::type;

    template <typename T>
    using NonTriviallyCopyableVoid = typename std::enable_if<! IsTriviallyCopyable<T>::value, void>::type;

    //==============================================================================
    template <typename T = ElementType>
    TriviallyCopyableVoid<T> addArrayInternal (const ElementType* otherElements, int numElements)
    {
        memcpy (elements + numUsed, otherElements, (size_t) numElements * sizeof (ElementType));
    }

    template <typename Type, typename T = ElementType>
    TriviallyCopyableVoid<T> addArrayInternal (const Type* otherElements, int numElements)
    {
        auto* start = elements + numUsed;

        while (--numElements >= 0)
            new (start++) ElementType (*(otherElements++));
    }

    template <typename Type, typename T = ElementType>
    NonTriviallyCopyableVoid<T> addArrayInternal (const Type* otherElements, int numElements)
    {
        auto* start = elements + numUsed;

        while (--numElements >= 0)
            new (start++) ElementType (*(otherElements++));
    }

    //==============================================================================
    template <typename T = ElementType>
    TriviallyCopyableVoid<T> setAllocatedSizeInternal (int numElements)
    {
        elements.realloc ((size_t) numElements);
    }

    template <typename T = ElementType>
    NonTriviallyCopyableVoid<T> setAllocatedSizeInternal (int numElements)
    {
        HeapBlock<ElementType> newElements (numElements);

        for (int i = 0; i < numUsed; ++i)
        {
            new (newElements + i) ElementType (std::move (elements[i]));
            elements[i].~ElementType();
        }

        elements = std::move (newElements);
    }

    //==============================================================================
    ElementType* createInsertSpace (int indexToInsertAt, int numElements)
    {
        ensureAllocatedSize (numUsed + numElements);

        if (! isPositiveAndBelow (indexToInsertAt, numUsed))
            return elements + numUsed;

        createInsertSpaceInternal (indexToInsertAt, numElements);

        return elements + indexToInsertAt;
    }

    template <typename T = ElementType>
    TriviallyCopyableVoid<T> createInsertSpaceInternal (int indexToInsertAt, int numElements)
    {
        auto* start = elements + indexToInsertAt;
        auto numElementsToShift = numUsed - indexToInsertAt;
        memmove (start + numElements, start, (size_t) numElementsToShift * sizeof (ElementType));
    }

    template <typename T = ElementType>
    NonTriviallyCopyableVoid<T> createInsertSpaceInternal (int indexToInsertAt, int numElements)
    {
        auto* end = elements + numUsed;
        auto* newEnd = end + numElements;
        auto numElementsToShift = numUsed - indexToInsertAt;

        for (int i = 0; i < numElementsToShift; ++i)
        {
            new (--newEnd) ElementType (std::move (*(--end)));
            end->~ElementType();
        }
    }

    //==============================================================================
    template <typename T = ElementType>
    TriviallyCopyableVoid<T> removeElementsInternal (int indexToRemoveAt, int numElementsToRemove)
    {
        auto* start = elements + indexToRemoveAt;
        auto numElementsToShift = numUsed - (indexToRemoveAt + numElementsToRemove);
        memmove (start, start + numElementsToRemove, (size_t) numElementsToShift * sizeof (ElementType));
    }

    template <typename T = ElementType>
    NonTriviallyCopyableVoid<T> removeElementsInternal (int indexToRemoveAt, int numElementsToRemove)
    {
        auto numElementsToShift = numUsed - (indexToRemoveAt + numElementsToRemove);
        auto* destination = elements + indexToRemoveAt;
        auto* source = destination + numElementsToRemove;

        for (int i = 0; i < numElementsToShift; ++i)
            moveAssignElement (destination++, std::move (*(source++)));

        for (int i = 0; i < numElementsToRemove; ++i)
            (destination++)->~ElementType();
    }

    //==============================================================================
    template <typename T = ElementType>
    TriviallyCopyableVoid<T> moveInternal (int currentIndex, int newIndex) noexcept
    {
        char tempCopy[sizeof (ElementType)];
        memcpy (tempCopy, elements + currentIndex, sizeof (ElementType));

        if (newIndex > currentIndex)
        {
            memmove (elements + currentIndex,
                     elements + currentIndex + 1,
                     (size_t) (newIndex - currentIndex) * sizeof (ElementType));
        }
        else
        {
            memmove (elements + newIndex + 1,
                     elements + newIndex,
                     (size_t) (currentIndex - newIndex) * sizeof (ElementType));
        }

        memcpy (elements + newIndex, tempCopy, sizeof (ElementType));
    }

    template <typename T = ElementType>
    NonTriviallyCopyableVoid<T> moveInternal (int currentIndex, int newIndex) noexcept
    {
        auto* e = elements + currentIndex;
        ElementType tempCopy (std::move (*e));
        auto delta = newIndex - currentIndex;

        if (delta > 0)
        {
            for (int i = 0; i < delta; ++i)
            {
                moveAssignElement (e, std::move (*(e + 1)));
                ++e;
            }
        }
        else
        {
            for (int i = 0; i < -delta; ++i)
            {
                moveAssignElement (e, std::move (*(e - 1)));
                --e;
            }
        }

        moveAssignElement (e, std::move (tempCopy));
    }

    //==============================================================================
    void addAssumingCapacityIsReady (const ElementType& element)   { new (elements + numUsed++) ElementType (element); }
    void addAssumingCapacityIsReady (ElementType&& element)        { new (elements + numUsed++) ElementType (std::move (element)); }

    template <typename... OtherElements>
    void addAssumingCapacityIsReady (const ElementType& firstNewElement, OtherElements... otherElements)
    {
        addAssumingCapacityIsReady (firstNewElement);
        addAssumingCapacityIsReady (otherElements...);
    }

    template <typename... OtherElements>
    void addAssumingCapacityIsReady (ElementType&& firstNewElement, OtherElements... otherElements)
    {
        addAssumingCapacityIsReady (std::move (firstNewElement));
        addAssumingCapacityIsReady (otherElements...);
    }

    //==============================================================================
    template <typename T = ElementType>
    typename std::enable_if<std::is_move_assignable<T>::value, void>::type
    moveAssignElement (ElementType* destination, ElementType&& source)
    {
        *destination = std::move (source);
    }

    template <typename T = ElementType>
    typename std::enable_if<! std::is_move_assignable<T>::value, void>::type
    moveAssignElement (ElementType* destination, ElementType&& source)
    {
        destination->~ElementType();
        new (destination) ElementType (std::move (source));
    }

    void checkSourceIsNotAMember (const ElementType* element)
    {
        // when you pass a reference to an existing element into a method like add() which
        // may need to reallocate the array to make more space, the incoming reference may
        // be deleted indirectly during the reallocation operation! To work around this,
        // make a local copy of the item you're trying to add (and maybe use std::move to
        // move it into the add() method to avoid any extra overhead)
        jassert (element < begin() || element >= end());
        ignoreUnused (element);
    }

    //==============================================================================
    HeapBlock<ElementType> elements;
    int numAllocated = 0, numUsed = 0;

    template <class OtherElementType, class OtherCriticalSection>
    friend class ArrayBase;

    JUCE_DECLARE_NON_COPYABLE (ArrayBase)
};

} // namespace juce
