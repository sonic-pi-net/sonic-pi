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

#ifndef DOXYGEN

/** This is an internal helper class which converts a juce ElementComparator style
    class (using a "compareElements" method) into a class that's compatible with
    std::sort (i.e. using an operator() to compare the elements)

    @tags{Core}
*/
template <typename ElementComparator>
struct SortFunctionConverter
{
    SortFunctionConverter (ElementComparator& e) : comparator (e) {}

    template <typename Type>
    bool operator() (Type a, Type b)  { return comparator.compareElements (a, b) < 0; }

private:
    ElementComparator& comparator;
    SortFunctionConverter& operator= (const SortFunctionConverter&) = delete;
};

#endif


//==============================================================================
/**
    Sorts a range of elements in an array.

    The comparator object that is passed-in must define a public method with the following
    signature:
    @code
    int compareElements (ElementType first, ElementType second);
    @endcode

    ..and this method must return:
      - a value of < 0 if the first comes before the second
      - a value of 0 if the two objects are equivalent
      - a value of > 0 if the second comes before the first

    To improve performance, the compareElements() method can be declared as static or const.

    @param comparator       an object which defines a compareElements() method
    @param array            the array to sort
    @param firstElement     the index of the first element of the range to be sorted
    @param lastElement      the index of the last element in the range that needs
                            sorting (this is inclusive)
    @param retainOrderOfEquivalentItems     if true, the order of items that the
                            comparator deems the same will be maintained - this will be
                            a slower algorithm than if they are allowed to be moved around.

    @see sortArrayRetainingOrder
*/
template <class ElementType, class ElementComparator>
static void sortArray (ElementComparator& comparator,
                       ElementType* const array,
                       int firstElement,
                       int lastElement,
                       const bool retainOrderOfEquivalentItems)
{
    jassert (firstElement >= 0);

    if (lastElement > firstElement)
    {
        SortFunctionConverter<ElementComparator> converter (comparator);

        if (retainOrderOfEquivalentItems)
            std::stable_sort (array + firstElement, array + lastElement + 1, converter);
        else
            std::sort        (array + firstElement, array + lastElement + 1, converter);
    }
}


//==============================================================================
/**
    Searches a sorted array of elements, looking for the index at which a specified value
    should be inserted for it to be in the correct order.

    The comparator object that is passed-in must define a public method with the following
    signature:
    @code
    int compareElements (ElementType first, ElementType second);
    @endcode

    ..and this method must return:
      - a value of < 0 if the first comes before the second
      - a value of 0 if the two objects are equivalent
      - a value of > 0 if the second comes before the first

    To improve performance, the compareElements() method can be declared as static or const.

    @param comparator       an object which defines a compareElements() method
    @param array            the array to search
    @param newElement       the value that is going to be inserted
    @param firstElement     the index of the first element to search
    @param lastElement      the index of the last element in the range (this is non-inclusive)
*/
template <class ElementType, class ElementComparator>
static int findInsertIndexInSortedArray (ElementComparator& comparator,
                                         ElementType* const array,
                                         const ElementType newElement,
                                         int firstElement,
                                         int lastElement)
{
    jassert (firstElement <= lastElement);

    ignoreUnused (comparator); // if you pass in an object with a static compareElements() method, this
                               // avoids getting warning messages about the parameter being unused

    while (firstElement < lastElement)
    {
        if (comparator.compareElements (newElement, array [firstElement]) == 0)
        {
            ++firstElement;
            break;
        }
        else
        {
            const int halfway = (firstElement + lastElement) >> 1;

            if (halfway == firstElement)
            {
                if (comparator.compareElements (newElement, array [halfway]) >= 0)
                    ++firstElement;

                break;
            }
            else if (comparator.compareElements (newElement, array [halfway]) >= 0)
            {
                firstElement = halfway;
            }
            else
            {
                lastElement = halfway;
            }
        }
    }

    return firstElement;
}

//==============================================================================
/**
    A simple ElementComparator class that can be used to sort an array of
    objects that support the '<' operator.

    This will work for primitive types and objects that implement operator<().

    Example: @code
    Array<int> myArray;
    DefaultElementComparator<int> sorter;
    myArray.sort (sorter);
    @endcode

    @see ElementComparator

    @tags{Core}
*/
template <class ElementType>
class DefaultElementComparator
{
private:
    using ParameterType = typename TypeHelpers::ParameterType<ElementType>::type;

public:
    static int compareElements (ParameterType first, ParameterType second)
    {
        return (first < second) ? -1 : ((second < first) ? 1 : 0);
    }
};

} // namespace juce
