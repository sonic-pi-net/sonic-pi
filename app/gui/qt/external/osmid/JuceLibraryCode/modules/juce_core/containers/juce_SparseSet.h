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
    Holds a set of primitive values, storing them as a set of ranges.

    This container acts like an array, but can efficiently hold large contiguous
    ranges of values. It's quite a specialised class, mostly useful for things
    like keeping the set of selected rows in a listbox.

    The type used as a template parameter must be an integer type, such as int, short,
    int64, etc.
*/
template <class Type>
class SparseSet
{
public:
    //==============================================================================
    /** Creates a new empty set. */
    SparseSet()
    {
    }

    /** Creates a copy of another SparseSet. */
    SparseSet (const SparseSet<Type>& other)
        : values (other.values)
    {
    }

    //==============================================================================
    /** Clears the set. */
    void clear()
    {
        values.clear();
    }

    /** Checks whether the set is empty.

        This is much quicker than using (size() == 0).
    */
    bool isEmpty() const noexcept
    {
        return values.size() == 0;
    }

    /** Returns the number of values in the set.

        Because of the way the data is stored, this method can take longer if there
        are a lot of items in the set. Use isEmpty() for a quick test of whether there
        are any items.
    */
    Type size() const
    {
        Type total (0);

        for (int i = 0; i < values.size(); i += 2)
            total += values.getUnchecked (i + 1) - values.getUnchecked (i);

        return total;
    }

    /** Returns one of the values in the set.

        @param index    the index of the value to retrieve, in the range 0 to (size() - 1).
        @returns        the value at this index, or 0 if it's out-of-range
    */
    Type operator[] (Type index) const
    {
        for (int i = 0; i < values.size(); i += 2)
        {
            const Type start (values.getUnchecked (i));
            const Type len (values.getUnchecked (i + 1) - start);

            if (index < len)
                return start + index;

            index -= len;
        }

        return Type();
    }

    /** Checks whether a particular value is in the set. */
    bool contains (const Type valueToLookFor) const
    {
        for (int i = 0; i < values.size(); ++i)
            if (valueToLookFor < values.getUnchecked(i))
                return (i & 1) != 0;

        return false;
    }

    //==============================================================================
    /** Returns the number of contiguous blocks of values.
        @see getRange
    */
    int getNumRanges() const noexcept
    {
        return values.size() >> 1;
    }

    /** Returns one of the contiguous ranges of values stored.
        @param rangeIndex   the index of the range to look up, between 0
                            and (getNumRanges() - 1)
        @see getTotalRange
    */
    const Range<Type> getRange (const int rangeIndex) const
    {
        if (isPositiveAndBelow (rangeIndex, getNumRanges()))
            return Range<Type> (values.getUnchecked (rangeIndex << 1),
                                values.getUnchecked ((rangeIndex << 1) + 1));

        return Range<Type>();
    }

    /** Returns the range between the lowest and highest values in the set.
        @see getRange
    */
    Range<Type> getTotalRange() const
    {
        if (values.size() > 0)
        {
            jassert ((values.size() & 1) == 0);
            return Range<Type> (values.getUnchecked (0),
                                values.getUnchecked (values.size() - 1));
        }

        return Range<Type>();
    }

    //==============================================================================
    /** Adds a range of contiguous values to the set.
        e.g. addRange (Range \<int\> (10, 14)) will add (10, 11, 12, 13) to the set.
    */
    void addRange (const Range<Type> range)
    {
        jassert (range.getLength() >= 0);
        if (range.getLength() > 0)
        {
            removeRange (range);

            values.addUsingDefaultSort (range.getStart());
            values.addUsingDefaultSort (range.getEnd());

            simplify();
        }
    }

    /** Removes a range of values from the set.
        e.g. removeRange (Range\<int\> (10, 14)) will remove (10, 11, 12, 13) from the set.
    */
    void removeRange (const Range<Type> rangeToRemove)
    {
        jassert (rangeToRemove.getLength() >= 0);

        if (rangeToRemove.getLength() > 0
             && values.size() > 0
             && rangeToRemove.getStart() < values.getUnchecked (values.size() - 1)
             && values.getUnchecked(0) < rangeToRemove.getEnd())
        {
            const bool onAtStart = contains (rangeToRemove.getStart() - 1);
            const Type lastValue (jmin (rangeToRemove.getEnd(), values.getLast()));
            const bool onAtEnd = contains (lastValue);

            for (int i = values.size(); --i >= 0;)
            {
                if (values.getUnchecked(i) <= lastValue)
                {
                    while (values.getUnchecked(i) >= rangeToRemove.getStart())
                    {
                        values.remove (i);

                        if (--i < 0)
                            break;
                    }

                    break;
                }
            }

            if (onAtStart)   values.addUsingDefaultSort (rangeToRemove.getStart());
            if (onAtEnd)     values.addUsingDefaultSort (lastValue);

            simplify();
        }
    }

    /** Does an XOR of the values in a given range. */
    void invertRange (const Range<Type> range)
    {
        SparseSet newItems;
        newItems.addRange (range);

        for (int i = getNumRanges(); --i >= 0;)
            newItems.removeRange (getRange (i));

        removeRange (range);

        for (int i = newItems.getNumRanges(); --i >= 0;)
            addRange (newItems.getRange(i));
    }

    /** Checks whether any part of a given range overlaps any part of this set. */
    bool overlapsRange (const Range<Type> range)
    {
        if (range.getLength() > 0)
        {
            for (int i = getNumRanges(); --i >= 0;)
            {
                if (values.getUnchecked ((i << 1) + 1) <= range.getStart())
                    return false;

                if (values.getUnchecked (i << 1) < range.getEnd())
                    return true;
            }
        }

        return false;
    }

    /** Checks whether the whole of a given range is contained within this one. */
    bool containsRange (const Range<Type> range)
    {
        if (range.getLength() > 0)
        {
            for (int i = getNumRanges(); --i >= 0;)
            {
                if (values.getUnchecked ((i << 1) + 1) <= range.getStart())
                    return false;

                if (values.getUnchecked (i << 1) <= range.getStart()
                     && range.getEnd() <= values.getUnchecked ((i << 1) + 1))
                    return true;
            }
        }

        return false;
    }

    //==============================================================================
    bool operator== (const SparseSet<Type>& other) noexcept
    {
        return values == other.values;
    }

    bool operator!= (const SparseSet<Type>& other) noexcept
    {
        return values != other.values;
    }

private:
    //==============================================================================
    // alternating start/end values of ranges of values that are present.
    Array<Type, DummyCriticalSection> values;

    void simplify()
    {
        jassert ((values.size() & 1) == 0);

        for (int i = values.size(); --i > 0;)
            if (values.getUnchecked(i) == values.getUnchecked (i - 1))
                values.removeRange (--i, 2);
    }
};

} // namespace juce
