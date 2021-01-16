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
    Holds a set of primitive values, storing them as a set of ranges.

    This container acts like an array, but can efficiently hold large contiguous
    ranges of values. It's quite a specialised class, mostly useful for things
    like keeping the set of selected rows in a listbox.

    The type used as a template parameter must be an integer type, such as int, short,
    int64, etc.

    @tags{Core}
*/
template <class Type>
class SparseSet
{
public:
    //==============================================================================
    SparseSet() = default;

    SparseSet (const SparseSet&) = default;
    SparseSet& operator= (const SparseSet&) = default;

    SparseSet (SparseSet&& other) noexcept : ranges (std::move (other.ranges)) {}
    SparseSet& operator= (SparseSet&& other) noexcept { ranges = std::move (other.ranges); return *this; }

    //==============================================================================
    /** Clears the set. */
    void clear()                                { ranges.clear(); }

    /** Checks whether the set is empty.
        This is much quicker than using (size() == 0).
    */
    bool isEmpty() const noexcept               { return ranges.isEmpty(); }

    /** Returns the number of values in the set.

        Because of the way the data is stored, this method can take longer if there
        are a lot of items in the set. Use isEmpty() for a quick test of whether there
        are any items.
    */
    Type size() const noexcept
    {
        Type total = {};

        for (auto& r : ranges)
            total += r.getLength();

        return total;
    }

    /** Returns one of the values in the set.

        @param index    the index of the value to retrieve, in the range 0 to (size() - 1).
        @returns        the value at this index, or 0 if it's out-of-range
    */
    Type operator[] (Type index) const noexcept
    {
        Type total = {};

        for (auto& r : ranges)
        {
            auto end = total + r.getLength();

            if (index < end)
                return r.getStart() + (index - total);

            total = end;
        }

        return {};
    }

    /** Checks whether a particular value is in the set. */
    bool contains (Type valueToLookFor) const noexcept
    {
        for (auto& r : ranges)
        {
            if (r.getStart() > valueToLookFor)
                break;

            if (r.getEnd() > valueToLookFor)
                return true;
        }

        return false;
    }

    //==============================================================================
    /** Returns the number of contiguous blocks of values.
        @see getRange
    */
    int getNumRanges() const noexcept                           { return ranges.size(); }

    /** Returns one of the contiguous ranges of values stored.
        @param rangeIndex   the index of the range to look up, between 0
                            and (getNumRanges() - 1)
        @see getTotalRange
    */
    Range<Type> getRange (int rangeIndex) const noexcept        { return ranges[rangeIndex]; }

    /** Returns the range between the lowest and highest values in the set.
        @see getRange
    */
    Range<Type> getTotalRange() const noexcept
    {
        if (ranges.isEmpty())
            return {};

        return { ranges.getFirst().getStart(),
                 ranges.getLast().getEnd() };
    }

    //==============================================================================
    /** Adds a range of contiguous values to the set.
        e.g. addRange (Range \<int\> (10, 14)) will add (10, 11, 12, 13) to the set.
    */
    void addRange (Range<Type> range)
    {
        if (! range.isEmpty())
        {
            removeRange (range);
            ranges.add (range);
            std::sort (ranges.begin(), ranges.end(),
                       [] (Range<Type> a, Range<Type> b) { return a.getStart() < b.getStart(); });
            simplify();
        }
    }

    /** Removes a range of values from the set.
        e.g. removeRange (Range\<int\> (10, 14)) will remove (10, 11, 12, 13) from the set.
    */
    void removeRange (Range<Type> rangeToRemove)
    {
        if (getTotalRange().intersects (rangeToRemove) && ! rangeToRemove.isEmpty())
        {
            for (int i = ranges.size(); --i >= 0;)
            {
                auto& r = ranges.getReference(i);

                if (r.getEnd() <= rangeToRemove.getStart())
                    break;

                if (r.getStart() >= rangeToRemove.getEnd())
                    continue;

                if (rangeToRemove.contains (r))
                {
                    ranges.remove (i);
                }
                else if (r.contains (rangeToRemove))
                {
                    auto r1 = r.withEnd (rangeToRemove.getStart());
                    auto r2 = r.withStart (rangeToRemove.getEnd());

                    // this should be covered in if (rangeToRemove.contains (r))
                    jassert (! r1.isEmpty() || ! r2.isEmpty());

                    r = r1;

                    if (r.isEmpty())
                        r = r2;

                    if (! r1.isEmpty() && ! r2.isEmpty())
                        ranges.insert (i + 1, r2);
                }
                else if (rangeToRemove.getEnd() > r.getEnd())
                {
                    r.setEnd (rangeToRemove.getStart());
                }
                else
                {
                    r.setStart (rangeToRemove.getEnd());
                }
            }
        }
    }

    /** Does an XOR of the values in a given range. */
    void invertRange (Range<Type> range)
    {
        SparseSet newItems;
        newItems.addRange (range);

        for (auto& r : ranges)
            newItems.removeRange (r);

        removeRange (range);

        for (auto& r : newItems.ranges)
            addRange (r);
    }

    /** Checks whether any part of a given range overlaps any part of this set. */
    bool overlapsRange (Range<Type> range) const noexcept
    {
        if (! range.isEmpty())
            for (auto& r : ranges)
                if (r.intersects (range))
                    return true;

        return false;
    }

    /** Checks whether the whole of a given range is contained within this one. */
    bool containsRange (Range<Type> range) const noexcept
    {
        if (! range.isEmpty())
            for (auto& r : ranges)
                if (r.contains (range))
                    return true;

        return false;
    }

    /** Returns the set as a list of ranges, which you may want to iterate over. */
    const Array<Range<Type>>& getRanges() const noexcept        { return ranges; }

    //==============================================================================
    bool operator== (const SparseSet& other) const noexcept     { return ranges == other.ranges; }
    bool operator!= (const SparseSet& other) const noexcept     { return ranges != other.ranges; }

private:
    //==============================================================================
    Array<Range<Type>> ranges;

    void simplify()
    {
        for (int i = ranges.size(); --i > 0;)
        {
            auto& r1 = ranges.getReference (i - 1);
            auto& r2 = ranges.getReference (i);

            if (r1.getEnd() == r2.getStart())
            {
                r1.setEnd (r2.getEnd());
                ranges.remove (i);
            }
        }
    }
};

} // namespace juce
