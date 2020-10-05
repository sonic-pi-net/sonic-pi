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
    Calculates and applies a sequence of changes to convert one text string into
    another.

    Once created, the TextDiff object contains an array of change objects, where
    each change can be either an insertion or a deletion. When applied in order
    to the original string, these changes will convert it to the target string.

    @tags{Core}
*/
class JUCE_API TextDiff
{
public:
    /** Creates a set of diffs for converting the original string into the target. */
    TextDiff (const String& original,
              const String& target);

    /** Applies this sequence of changes to the original string, producing the
        target string that was specified when generating them.

        Obviously it only makes sense to call this function with the string that
        was originally passed to the constructor. Any other input will produce an
        undefined result.
    */
    String appliedTo (String text) const;

    /** Describes a change, which can be either an insertion or deletion. */
    struct Change
    {
        String insertedText; /**< If this change is a deletion, this string will be empty; otherwise,
                                  it'll be the text that should be inserted at the index specified by start. */
        int start;    /**< Specifies the character index in a string at which text should be inserted or deleted. */
        int length;   /**< If this change is a deletion, this specifies the number of characters to delete. For an
                           insertion, this is the length of the new text being inserted. */

        /** Returns true if this change is a deletion, or false for an insertion. */
        bool isDeletion() const noexcept;

        /** Returns the result of applying this change to a string. */
        String appliedTo (const String& original) const noexcept;
    };

    /** The list of changes required to perform the transformation.
        Applying each of these, in order, to the original string will produce the target.
    */
    Array<Change> changes;
};

} // namespace juce
