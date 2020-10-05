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
    Contains static methods for converting JSON-formatted text to and from var objects.

    The var class is structurally compatible with JSON-formatted data, so these
    functions allow you to parse JSON into a var object, and to convert a var
    object to JSON-formatted text.

    @see var

    @tags{Core}
*/
class JUCE_API  JSON
{
public:
    //==============================================================================
    /** Parses a string of JSON-formatted text, and returns a result code containing
        any parse errors.

        This will return the parsed structure in the parsedResult parameter, and will
        return a Result object to indicate whether parsing was successful, and if not,
        it will contain an error message.

        If you're not interested in the error message, you can use one of the other
        shortcut parse methods, which simply return a var() if the parsing fails.

        Note that this will only parse valid JSON, which means that the item given must
        be either an object or an array definition. If you want to also be able to parse
        any kind of primitive JSON object, use the fromString() method.
    */
    static Result parse (const String& text, var& parsedResult);

    /** Attempts to parse some JSON-formatted text, and returns the result as a var object.

        If the parsing fails, this simply returns var() - if you need to find out more
        detail about the parse error, use the alternative parse() method which returns a Result.

        Note that this will only parse valid JSON, which means that the item given must
        be either an object or an array definition. If you want to also be able to parse
        any kind of primitive JSON object, use the fromString() method.
    */
    static var parse (const String& text);

    /** Attempts to parse some JSON-formatted text from a file, and returns the result
        as a var object.

        Note that this is just a short-cut for reading the entire file into a string and
        parsing the result.

        If the parsing fails, this simply returns var() - if you need to find out more
        detail about the parse error, use the alternative parse() method which returns a Result.
    */
    static var parse (const File& file);

    /** Attempts to parse some JSON-formatted text from a stream, and returns the result
        as a var object.

        Note that this is just a short-cut for reading the entire stream into a string and
        parsing the result.

        If the parsing fails, this simply returns var() - if you need to find out more
        detail about the parse error, use the alternative parse() method which returns a Result.
    */
    static var parse (InputStream& input);

    //==============================================================================
    /** Returns a string which contains a JSON-formatted representation of the var object.
        If allOnOneLine is true, the result will be compacted into a single line of text
        with no carriage-returns. If false, it will be laid-out in a more human-readable format.
        The maximumDecimalPlaces parameter determines the precision of floating point numbers
        in scientific notation.
        @see writeToStream
    */
    static String toString (const var& objectToFormat,
                            bool allOnOneLine = false,
                            int maximumDecimalPlaces = 15);

    /** Parses a string that was created with the toString() method.
        This is slightly different to the parse() methods because they will reject primitive
        values and only accept array or object definitions, whereas this method will handle
        either.
    */
    static var fromString (StringRef);

    /** Writes a JSON-formatted representation of the var object to the given stream.
        If allOnOneLine is true, the result will be compacted into a single line of text
        with no carriage-returns. If false, it will be laid-out in a more human-readable format.
        The maximumDecimalPlaces parameter determines the precision of floating point numbers
        in scientific notation.
        @see toString
    */
    static void writeToStream (OutputStream& output,
                               const var& objectToFormat,
                               bool allOnOneLine = false,
                               int maximumDecimalPlaces = 15);

    /** Returns a version of a string with any extended characters escaped. */
    static String escapeString (StringRef);

    /** Parses a quoted string-literal in JSON format, returning the un-escaped result in the
        result parameter, and an error message in case the content was illegal.
        This advances the text parameter, leaving it positioned after the closing quote.
    */
    static Result parseQuotedString (String::CharPointerType& text, var& result);

private:
    //==============================================================================
    JSON() = delete; // This class can't be instantiated - just use its static methods.
};

} // namespace juce
