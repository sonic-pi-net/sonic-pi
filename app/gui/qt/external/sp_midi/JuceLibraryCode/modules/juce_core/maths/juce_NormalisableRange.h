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
    Represents a mapping between an arbitrary range of values and a
    normalised 0->1 range.

    The properties of the mapping also include an optional snapping interval
    and skew-factor.

    @see Range

    @tags{Core}
*/
template <typename ValueType>
class NormalisableRange
{
public:
    /** Creates a continuous range that performs a dummy mapping. */
    NormalisableRange() = default;

    NormalisableRange (const NormalisableRange&) = default;
    NormalisableRange& operator= (const NormalisableRange&) = default;
    NormalisableRange (NormalisableRange&&) = default;
    NormalisableRange& operator= (NormalisableRange&&) = default;

    /** Creates a NormalisableRange with a given range, interval and skew factor. */
    NormalisableRange (ValueType rangeStart,
                       ValueType rangeEnd,
                       ValueType intervalValue,
                       ValueType skewFactor,
                       bool useSymmetricSkew = false) noexcept
        : start (rangeStart), end (rangeEnd), interval (intervalValue),
          skew (skewFactor), symmetricSkew (useSymmetricSkew)
    {
        checkInvariants();
    }

    /** Creates a NormalisableRange with a given range, continuous interval, but a dummy skew-factor. */
    NormalisableRange (ValueType rangeStart,
                       ValueType rangeEnd) noexcept
        : start (rangeStart), end (rangeEnd)
    {
        checkInvariants();
    }

    /** Creates a NormalisableRange with a given range and interval, but a dummy skew-factor. */
    NormalisableRange (ValueType rangeStart,
                       ValueType rangeEnd,
                       ValueType intervalValue) noexcept
        : start (rangeStart), end (rangeEnd), interval (intervalValue)
    {
        checkInvariants();
    }

    /** Creates a NormalisableRange with a given range, continuous interval, but a dummy skew-factor. */
    NormalisableRange (Range<ValueType> range) noexcept
        : NormalisableRange (range.getStart(), range.getEnd())
    {
    }

    /** Creates a NormalisableRange with a given range and interval, but a dummy skew-factor. */
    NormalisableRange (Range<ValueType> range, ValueType intervalValue) noexcept
        : NormalisableRange (range.getStart(), range.getEnd(), intervalValue)
    {
    }

    /** A function object which can remap a value in some way based on the start and end of a range. */
    using ValueRemapFunction = std::function<ValueType(ValueType rangeStart,
                                                       ValueType rangeEnd,
                                                       ValueType valueToRemap)>;

    /** Creates a NormalisableRange with a given range and an injective mapping function.

        @param rangeStart           The minimum value in the range.
        @param rangeEnd             The maximum value in the range.
        @param convertFrom0To1Func  A function which uses the current start and end of this NormalisableRange
                                    and produces a mapped value from a normalised value.
        @param convertTo0To1Func    A function which uses the current start and end of this NormalisableRange
                                    and produces a normalised value from a mapped value.
        @param snapToLegalValueFunc A function which uses the current start and end of this NormalisableRange
                                    to take a mapped value and snap it to the nearest legal value.
    */
    NormalisableRange (ValueType rangeStart,
                       ValueType rangeEnd,
                       ValueRemapFunction convertFrom0To1Func,
                       ValueRemapFunction convertTo0To1Func,
                       ValueRemapFunction snapToLegalValueFunc = {}) noexcept
        : start (rangeStart),
          end   (rangeEnd),
          convertFrom0To1Function  (std::move (convertFrom0To1Func)),
          convertTo0To1Function    (std::move (convertTo0To1Func)),
          snapToLegalValueFunction (std::move (snapToLegalValueFunc))
    {
        checkInvariants();
    }

    /** Uses the properties of this mapping to convert a non-normalised value to
        its 0->1 representation.
    */
    ValueType convertTo0to1 (ValueType v) const noexcept
    {
        if (convertTo0To1Function != nullptr)
            return clampTo0To1 (convertTo0To1Function (start, end, v));

        auto proportion = clampTo0To1 ((v - start) / (end - start));

        if (skew == static_cast<ValueType> (1))
            return proportion;

        if (! symmetricSkew)
            return std::pow (proportion, skew);

        auto distanceFromMiddle = static_cast<ValueType> (2) * proportion - static_cast<ValueType> (1);

        return (static_cast<ValueType> (1) + std::pow (std::abs (distanceFromMiddle), skew)
                                           * (distanceFromMiddle < ValueType() ? static_cast<ValueType> (-1)
                                                                               : static_cast<ValueType> (1)))
               / static_cast<ValueType> (2);
    }

    /** Uses the properties of this mapping to convert a normalised 0->1 value to
        its full-range representation.
    */
    ValueType convertFrom0to1 (ValueType proportion) const noexcept
    {
        proportion = clampTo0To1 (proportion);

        if (convertFrom0To1Function != nullptr)
            return convertFrom0To1Function (start, end, proportion);

        if (! symmetricSkew)
        {
            if (skew != static_cast<ValueType> (1) && proportion > ValueType())
                proportion = std::exp (std::log (proportion) / skew);

            return start + (end - start) * proportion;
        }

        auto distanceFromMiddle = static_cast<ValueType> (2) * proportion - static_cast<ValueType> (1);

        if (skew != static_cast<ValueType> (1) && distanceFromMiddle != static_cast<ValueType> (0))
            distanceFromMiddle = std::exp (std::log (std::abs (distanceFromMiddle)) / skew)
                                 * (distanceFromMiddle < ValueType() ? static_cast<ValueType> (-1)
                                                                     : static_cast<ValueType> (1));

        return start + (end - start) / static_cast<ValueType> (2) * (static_cast<ValueType> (1) + distanceFromMiddle);
    }

    /** Takes a non-normalised value and snaps it based on either the interval property of
        this NormalisableRange or the lambda function supplied to the constructor.
    */
    ValueType snapToLegalValue (ValueType v) const noexcept
    {
        if (snapToLegalValueFunction != nullptr)
            return snapToLegalValueFunction (start, end, v);

        if (interval > ValueType())
            v = start + interval * std::floor ((v - start) / interval + static_cast<ValueType> (0.5));

        return (v <= start || end <= start) ? start : (v >= end ? end : v);
    }

    /** Returns the extent of the normalisable range. */
    Range<ValueType> getRange() const noexcept          { return { start, end }; }

    /** Given a value which is between the start and end points, this sets the skew
        such that convertFrom0to1 (0.5) will return this value.

        If you have used lambda functions for convertFrom0to1Func and convertFrom0to1Func in the
        constructor of this class then the skew value is ignored.

        @param centrePointValue  this must be greater than the start of the range and less than the end.
    */
    void setSkewForCentre (ValueType centrePointValue) noexcept
    {
        jassert (centrePointValue > start);
        jassert (centrePointValue < end);

        symmetricSkew = false;
        skew = std::log (static_cast<ValueType> (0.5)) / std::log ((centrePointValue - start) / (end - start));
        checkInvariants();
    }

    /** The minimum value of the non-normalised range. */
    ValueType start = 0;

    /** The maximum value of the non-normalised range. */
    ValueType end = 1;

    /** The snapping interval that should be used (for a non-normalised value). Use 0 for a
        continuous range.

        If you have used a lambda function for snapToLegalValueFunction in the constructor of
        this class then the interval is ignored.
    */
    ValueType interval = 0;

    /** An optional skew factor that alters the way values are distribute across the range.

        The skew factor lets you skew the mapping logarithmically so that larger or smaller
        values are given a larger proportion of the available space.

        A factor of 1.0 has no skewing effect at all. If the factor is < 1.0, the lower end
        of the range will fill more of the slider's length; if the factor is > 1.0, the upper
        end of the range will be expanded.

        If you have used lambda functions for convertFrom0to1Func and convertFrom0to1Func in the
        constructor of this class then the skew value is ignored.
    */
    ValueType skew = 1;

    /** If true, the skew factor applies from the middle of the slider to each of its ends. */
    bool symmetricSkew = false;

private:
    void checkInvariants() const
    {
        jassert (end > start);
        jassert (interval >= ValueType());
        jassert (skew > ValueType());
    }

    static ValueType clampTo0To1 (ValueType value)
    {
        auto clampedValue = jlimit (static_cast<ValueType> (0), static_cast<ValueType> (1), value);

        // If you hit this assertion then either your normalisation function is not working
        // correctly or your input is out of the expected bounds.
        jassert (clampedValue == value);

        return clampedValue;
    }

    ValueRemapFunction convertFrom0To1Function, convertTo0To1Function, snapToLegalValueFunction;
};

} // namespace juce
