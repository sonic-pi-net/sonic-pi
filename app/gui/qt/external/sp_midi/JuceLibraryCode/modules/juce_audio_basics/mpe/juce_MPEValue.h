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
    This class represents a single value for any of the MPE
    dimensions of control. It supports values with 7-bit or 14-bit resolutions
    (corresponding to 1 or 2 MIDI bytes, respectively). It also offers helper
    functions to query the value in a variety of representations that can be
    useful in an audio or MIDI context.

    @tags{Audio}
*/
class JUCE_API  MPEValue
{
public:
    //==============================================================================
    /** Default constructor.

        Constructs an MPEValue corresponding to the centre value.
    */
    MPEValue() noexcept;

    /** Constructs an MPEValue from an integer between 0 and 127
        (using 7-bit precision).
    */
    static MPEValue from7BitInt (int value) noexcept;

    /** Constructs an MPEValue from an integer between 0 and 16383
        (using 14-bit precision).
    */
    static MPEValue from14BitInt (int value) noexcept;

    /** Constructs an MPEValue corresponding to the centre value. */
    static MPEValue centreValue() noexcept;

    /** Constructs an MPEValue corresponding to the minimum value. */
    static MPEValue minValue() noexcept;

    /** Constructs an MPEValue corresponding to the maximum value. */
    static MPEValue maxValue() noexcept;

    /** Retrieves the current value as an integer between 0 and 127.

        Information will be lost if the value was initialised with a precision
        higher than 7-bit.
    */
    int as7BitInt() const noexcept;

    /** Retrieves the current value as an integer between 0 and 16383.

        Resolution will be lost if the value was initialised with a precision
        higher than 14-bit.
    */
    int as14BitInt() const noexcept;

    /** Retrieves the current value mapped to a float between -1.0f and 1.0f. */
    float asSignedFloat() const noexcept;

    /** Retrieves the current value mapped to a float between 0.0f and 1.0f. */
    float asUnsignedFloat() const noexcept;

    /** Returns true if two values are equal. */
    bool operator== (const MPEValue& other) const noexcept;

    /** Returns true if two values are not equal. */
    bool operator!= (const MPEValue& other) const noexcept;

private:
    //==============================================================================
    MPEValue (int normalisedValue);
    int normalisedValue = 8192;
};

} // namespace juce
