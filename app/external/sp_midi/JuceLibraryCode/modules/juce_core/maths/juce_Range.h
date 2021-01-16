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
/** A general-purpose range object, that simply represents any linear range with
    a start and end point.

    Note that when checking whether values fall within the range, the start value is
    considered to be inclusive, and the end of the range exclusive.

    The templated parameter is expected to be a primitive integer or floating point
    type, though class types could also be used if they behave in a number-like way.

    @tags{Core}
*/
template <typename ValueType>
class Range
{
public:
    //==============================================================================
    /** Constructs an empty range. */
    constexpr Range() = default;

    /** Constructs a range with given start and end values. */
    constexpr Range (const ValueType startValue, const ValueType endValue) noexcept
        : start (startValue), end (jmax (startValue, endValue))
    {
    }

    /** Constructs a copy of another range. */
    constexpr Range (const Range&) = default;

    /** Copies another range object. */
    Range& operator= (const Range&) = default;

    /** Returns the range that lies between two positions (in either order). */
    constexpr static Range between (const ValueType position1, const ValueType position2) noexcept
    {
        return position1 < position2 ? Range (position1, position2)
                                     : Range (position2, position1);
    }

    /** Returns a range with a given start and length. */
    static Range withStartAndLength (const ValueType startValue, const ValueType length) noexcept
    {
        jassert (length >= ValueType());
        return Range (startValue, startValue + length);
    }

    /** Returns a range with the specified start position and a length of zero. */
    constexpr static Range emptyRange (const ValueType start) noexcept
    {
        return Range (start, start);
    }

    //==============================================================================
    /** Returns the start of the range. */
    constexpr inline ValueType getStart() const noexcept          { return start; }

    /** Returns the length of the range. */
    constexpr inline ValueType getLength() const noexcept         { return end - start; }

    /** Returns the end of the range. */
    constexpr inline ValueType getEnd() const noexcept            { return end; }

    /** Returns true if the range has a length of zero. */
    constexpr inline bool isEmpty() const noexcept                { return start == end; }

    //==============================================================================
    /** Changes the start position of the range, leaving the end position unchanged.
        If the new start position is higher than the current end of the range, the end point
        will be pushed along to equal it, leaving an empty range at the new position.
    */
    void setStart (const ValueType newStart) noexcept
    {
        start = newStart;
        if (end < newStart)
            end = newStart;
    }

    /** Returns a range with the same end as this one, but a different start.
        If the new start position is higher than the current end of the range, the end point
        will be pushed along to equal it, returning an empty range at the new position.
    */
    constexpr Range withStart (const ValueType newStart) const noexcept
    {
        return Range (newStart, jmax (newStart, end));
    }

    /** Returns a range with the same length as this one, but moved to have the given start position. */
    constexpr Range movedToStartAt (const ValueType newStart) const noexcept
    {
        return Range (newStart, end + (newStart - start));
    }

    /** Changes the end position of the range, leaving the start unchanged.
        If the new end position is below the current start of the range, the start point
        will be pushed back to equal the new end point.
    */
    void setEnd (const ValueType newEnd) noexcept
    {
        end = newEnd;
        if (newEnd < start)
            start = newEnd;
    }

    /** Returns a range with the same start position as this one, but a different end.
        If the new end position is below the current start of the range, the start point
        will be pushed back to equal the new end point.
    */
    constexpr Range withEnd (const ValueType newEnd) const noexcept
    {
        return Range (jmin (start, newEnd), newEnd);
    }

    /** Returns a range with the same length as this one, but moved to have the given end position. */
    constexpr Range movedToEndAt (const ValueType newEnd) const noexcept
    {
        return Range (start + (newEnd - end), newEnd);
    }

    /** Changes the length of the range.
        Lengths less than zero are treated as zero.
    */
    void setLength (const ValueType newLength) noexcept
    {
        end = start + jmax (ValueType(), newLength);
    }

    /** Returns a range with the same start as this one, but a different length.
        Lengths less than zero are treated as zero.
    */
    constexpr Range withLength (const ValueType newLength) const noexcept
    {
        return Range (start, start + newLength);
    }

    /** Returns a range which has its start moved down and its end moved up by the
        given amount.
        @returns The returned range will be (start - amount, end + amount)
    */
    constexpr Range expanded (ValueType amount) const noexcept
    {
        return Range (start - amount, end + amount);
    }

    //==============================================================================
    /** Adds an amount to the start and end of the range. */
    inline Range operator+= (const ValueType amountToAdd) noexcept
    {
        start += amountToAdd;
        end += amountToAdd;
        return *this;
    }

    /** Subtracts an amount from the start and end of the range. */
    inline Range operator-= (const ValueType amountToSubtract) noexcept
    {
        start -= amountToSubtract;
        end -= amountToSubtract;
        return *this;
    }

    /** Returns a range that is equal to this one with an amount added to its
        start and end.
    */
    constexpr Range operator+ (const ValueType amountToAdd) const noexcept
    {
        return Range (start + amountToAdd, end + amountToAdd);
    }

    /** Returns a range that is equal to this one with the specified amount
        subtracted from its start and end. */
    constexpr Range operator- (const ValueType amountToSubtract) const noexcept
    {
        return Range (start - amountToSubtract, end - amountToSubtract);
    }

    constexpr bool operator== (Range other) const noexcept     { return start == other.start && end == other.end; }
    constexpr bool operator!= (Range other) const noexcept     { return start != other.start || end != other.end; }

    //==============================================================================
    /** Returns true if the given position lies inside this range.
        When making this comparison, the start value is considered to be inclusive,
        and the end of the range exclusive.
    */
    constexpr bool contains (const ValueType position) const noexcept
    {
        return start <= position && position < end;
    }

    /** Returns the nearest value to the one supplied, which lies within the range. */
    ValueType clipValue (const ValueType value) const noexcept
    {
        return jlimit (start, end, value);
    }

    /** Returns true if the given range lies entirely inside this range. */
    constexpr bool contains (Range other) const noexcept
    {
        return start <= other.start && end >= other.end;
    }

    /** Returns true if the given range intersects this one. */
    constexpr bool intersects (Range other) const noexcept
    {
        return other.start < end && start < other.end;
    }

    /** Returns the range that is the intersection of the two ranges, or an empty range
        with an undefined start position if they don't overlap. */
    constexpr Range getIntersectionWith (Range other) const noexcept
    {
        return Range (jmax (start, other.start),
                      jmin (end, other.end));
    }

    /** Returns the smallest range that contains both this one and the other one. */
    constexpr Range getUnionWith (Range other) const noexcept
    {
        return Range (jmin (start, other.start),
                      jmax (end, other.end));
    }

    /** Returns the smallest range that contains both this one and the given value. */
    constexpr Range getUnionWith (const ValueType valueToInclude) const noexcept
    {
        return Range (jmin (valueToInclude, start),
                      jmax (valueToInclude, end));
    }

    /** Returns a given range, after moving it forwards or backwards to fit it
        within this range.

        If the supplied range has a greater length than this one, the return value
        will be this range.

        Otherwise, if the supplied range is smaller than this one, the return value
        will be the new range, shifted forwards or backwards so that it doesn't extend
        beyond this one, but keeping its original length.
    */
    Range constrainRange (Range rangeToConstrain) const noexcept
    {
        const ValueType otherLen = rangeToConstrain.getLength();
        return getLength() <= otherLen
                ? *this
                : rangeToConstrain.movedToStartAt (jlimit (start, end - otherLen, rangeToConstrain.getStart()));
    }

    /** Scans an array of values for its min and max, and returns these as a Range. */
    static Range findMinAndMax (const ValueType* values, int numValues) noexcept
    {
        if (numValues <= 0)
            return Range();

        const ValueType first (*values++);
        Range r (first, first);

        while (--numValues > 0) // (> 0 rather than >= 0 because we've already taken the first sample)
        {
            const ValueType v (*values++);

            if (r.end < v)    r.end = v;
            if (v < r.start)  r.start = v;
        }

        return r;
    }

private:
    //==============================================================================
    ValueType start{}, end{};
};

} // namespace juce
