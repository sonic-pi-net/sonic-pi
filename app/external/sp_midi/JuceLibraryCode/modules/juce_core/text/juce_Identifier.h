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
    Represents a string identifier, designed for accessing properties by name.

    Comparing two Identifier objects is very fast (an O(1) operation), but creating
    them can be slower than just using a String directly, so the optimal way to use them
    is to keep some static Identifier objects for the things you use often.

    @see NamedValueSet, ValueTree

    @tags{Core}
*/
class JUCE_API  Identifier  final
{
public:
    /** Creates a null identifier. */
    Identifier() noexcept;

    /** Creates an identifier with a specified name.
        Because this name may need to be used in contexts such as script variables or XML
        tags, it must only contain ascii letters and digits, or the underscore character.
    */
    Identifier (const char* name);

    /** Creates an identifier with a specified name.
        Because this name may need to be used in contexts such as script variables or XML
        tags, it must only contain ascii letters and digits, or the underscore character.
    */
    Identifier (const String& name);

    /** Creates an identifier with a specified name.
        Because this name may need to be used in contexts such as script variables or XML
        tags, it must only contain ascii letters and digits, or the underscore character.
    */
    Identifier (String::CharPointerType nameStart, String::CharPointerType nameEnd);

    /** Creates a copy of another identifier. */
    Identifier (const Identifier& other) noexcept;

    /** Creates a copy of another identifier. */
    Identifier& operator= (const Identifier& other) noexcept;

    /** Creates a copy of another identifier. */
    Identifier (Identifier&& other) noexcept;

    /** Creates a copy of another identifier. */
    Identifier& operator= (Identifier&& other) noexcept;

    /** Destructor */
    ~Identifier() noexcept;

    /** Compares two identifiers. This is a very fast operation. */
    inline bool operator== (const Identifier& other) const noexcept     { return name.getCharPointer() == other.name.getCharPointer(); }

    /** Compares two identifiers. This is a very fast operation. */
    inline bool operator!= (const Identifier& other) const noexcept     { return name.getCharPointer() != other.name.getCharPointer(); }

    /** Compares the identifier with a string. */
    inline bool operator== (StringRef other) const noexcept             { return name == other; }

    /** Compares the identifier with a string. */
    inline bool operator!= (StringRef other) const noexcept             { return name != other; }

    /** Compares the identifier with a string. */
    inline bool operator<  (StringRef other) const noexcept             { return name <  other; }

    /** Compares the identifier with a string. */
    inline bool operator<= (StringRef other) const noexcept             { return name <= other; }

    /** Compares the identifier with a string. */
    inline bool operator>  (StringRef other) const noexcept             { return name >  other; }

    /** Compares the identifier with a string. */
    inline bool operator>= (StringRef other) const noexcept             { return name >= other; }

    /** Returns this identifier as a string. */
    const String& toString() const noexcept                             { return name; }

    /** Returns this identifier's raw string pointer. */
    operator String::CharPointerType() const noexcept                   { return name.getCharPointer(); }

    /** Returns this identifier's raw string pointer. */
    String::CharPointerType getCharPointer() const noexcept             { return name.getCharPointer(); }

    /** Returns this identifier as a StringRef. */
    operator StringRef() const noexcept                                 { return name; }

    /** Returns true if this Identifier is not null */
    bool isValid() const noexcept                                       { return name.isNotEmpty(); }

    /** Returns true if this Identifier is null */
    bool isNull() const noexcept                                        { return name.isEmpty(); }

    /** A null identifier. */
    static Identifier null;

    /** Checks a given string for characters that might not be valid in an Identifier.
        Since Identifiers are used as a script variables and XML attributes, they should only contain
        alphanumeric characters, underscores, or the '-' and ':' characters.
    */
    static bool isValidIdentifier (const String& possibleIdentifier) noexcept;

private:
    String name;
};

} // namespace juce
