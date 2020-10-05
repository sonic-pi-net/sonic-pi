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
    Wraps a pointer to a null-terminated UTF-32 character string, and provides
    various methods to operate on the data.
    @see CharPointer_UTF8, CharPointer_UTF16

    @tags{Core}
*/
class CharPointer_UTF32  final
{
public:
    using CharType = juce_wchar;

    inline explicit CharPointer_UTF32 (const CharType* rawPointer) noexcept
        : data (const_cast<CharType*> (rawPointer))
    {
    }

    inline CharPointer_UTF32 (const CharPointer_UTF32& other) = default;

    inline CharPointer_UTF32 operator= (CharPointer_UTF32 other) noexcept
    {
        data = other.data;
        return *this;
    }

    inline CharPointer_UTF32 operator= (const CharType* text) noexcept
    {
        data = const_cast<CharType*> (text);
        return *this;
    }

    /** This is a pointer comparison, it doesn't compare the actual text. */
    inline bool operator== (CharPointer_UTF32 other) const noexcept     { return data == other.data; }
    inline bool operator!= (CharPointer_UTF32 other) const noexcept     { return data != other.data; }
    inline bool operator<= (CharPointer_UTF32 other) const noexcept     { return data <= other.data; }
    inline bool operator<  (CharPointer_UTF32 other) const noexcept     { return data <  other.data; }
    inline bool operator>= (CharPointer_UTF32 other) const noexcept     { return data >= other.data; }
    inline bool operator>  (CharPointer_UTF32 other) const noexcept     { return data >  other.data; }

    /** Returns the address that this pointer is pointing to. */
    inline CharType* getAddress() const noexcept        { return data; }

    /** Returns the address that this pointer is pointing to. */
    inline operator const CharType*() const noexcept    { return data; }

    /** Returns true if this pointer is pointing to a null character. */
    inline bool isEmpty() const noexcept                { return *data == 0; }

    /** Returns true if this pointer is not pointing to a null character. */
    inline bool isNotEmpty() const noexcept             { return *data != 0; }

    /** Returns the unicode character that this pointer is pointing to. */
    inline juce_wchar operator*() const noexcept        { return *data; }

    /** Moves this pointer along to the next character in the string. */
    inline CharPointer_UTF32 operator++() noexcept
    {
        ++data;
        return *this;
    }

    /** Moves this pointer to the previous character in the string. */
    inline CharPointer_UTF32 operator--() noexcept
    {
        --data;
        return *this;
    }

    /** Returns the character that this pointer is currently pointing to, and then
        advances the pointer to point to the next character. */
    inline juce_wchar getAndAdvance() noexcept  { return *data++; }

    /** Moves this pointer along to the next character in the string. */
    CharPointer_UTF32 operator++ (int) noexcept
    {
        auto temp (*this);
        ++data;
        return temp;
    }

    /** Moves this pointer forwards by the specified number of characters. */
    inline void operator+= (int numToSkip) noexcept
    {
        data += numToSkip;
    }

    inline void operator-= (int numToSkip) noexcept
    {
        data -= numToSkip;
    }

    /** Returns the character at a given character index from the start of the string. */
    inline juce_wchar& operator[] (int characterIndex) const noexcept
    {
        return data [characterIndex];
    }

    /** Returns a pointer which is moved forwards from this one by the specified number of characters. */
    CharPointer_UTF32 operator+ (int numToSkip) const noexcept
    {
        return CharPointer_UTF32 (data + numToSkip);
    }

    /** Returns a pointer which is moved backwards from this one by the specified number of characters. */
    CharPointer_UTF32 operator- (int numToSkip) const noexcept
    {
        return CharPointer_UTF32 (data - numToSkip);
    }

    /** Writes a unicode character to this string, and advances this pointer to point to the next position. */
    inline void write (juce_wchar charToWrite) noexcept
    {
        *data++ = charToWrite;
    }

    inline void replaceChar (juce_wchar newChar) noexcept
    {
        *data = newChar;
    }

    /** Writes a null character to this string (leaving the pointer's position unchanged). */
    inline void writeNull() const noexcept
    {
        *data = 0;
    }

    /** Returns the number of characters in this string. */
    size_t length() const noexcept
    {
       #if JUCE_NATIVE_WCHAR_IS_UTF32 && ! JUCE_ANDROID
        return wcslen (data);
       #else
        size_t n = 0;
        while (data[n] != 0)
            ++n;
        return n;
       #endif
    }

    /** Returns the number of characters in this string, or the given value, whichever is lower. */
    size_t lengthUpTo (size_t maxCharsToCount) const noexcept
    {
        return CharacterFunctions::lengthUpTo (*this, maxCharsToCount);
    }

    /** Returns the number of characters in this string, or up to the given end pointer, whichever is lower. */
    size_t lengthUpTo (CharPointer_UTF32 end) const noexcept
    {
        return CharacterFunctions::lengthUpTo (*this, end);
    }

    /** Returns the number of bytes that are used to represent this string.
        This includes the terminating null character.
    */
    size_t sizeInBytes() const noexcept
    {
        return sizeof (CharType) * (length() + 1);
    }

    /** Returns the number of bytes that would be needed to represent the given
        unicode character in this encoding format.
    */
    static size_t getBytesRequiredFor (juce_wchar) noexcept
    {
        return sizeof (CharType);
    }

    /** Returns the number of bytes that would be needed to represent the given
        string in this encoding format.
        The value returned does NOT include the terminating null character.
    */
    template <class CharPointer>
    static size_t getBytesRequiredFor (CharPointer text) noexcept
    {
        return sizeof (CharType) * text.length();
    }

    /** Returns a pointer to the null character that terminates this string. */
    CharPointer_UTF32 findTerminatingNull() const noexcept
    {
        return CharPointer_UTF32 (data + length());
    }

    /** Copies a source string to this pointer, advancing this pointer as it goes. */
    template <typename CharPointer>
    void writeAll (CharPointer src) noexcept
    {
        CharacterFunctions::copyAll (*this, src);
    }

    /** Copies a source string to this pointer, advancing this pointer as it goes. */
    void writeAll (CharPointer_UTF32 src) noexcept
    {
        auto* s = src.data;

        while ((*data = *s) != 0)
        {
            ++data;
            ++s;
        }
    }

    /** Copies a source string to this pointer, advancing this pointer as it goes.
        The maxDestBytes parameter specifies the maximum number of bytes that can be written
        to the destination buffer before stopping.
    */
    template <typename CharPointer>
    size_t writeWithDestByteLimit (CharPointer src, size_t maxDestBytes) noexcept
    {
        return CharacterFunctions::copyWithDestByteLimit (*this, src, maxDestBytes);
    }

    /** Copies a source string to this pointer, advancing this pointer as it goes.
        The maxChars parameter specifies the maximum number of characters that can be
        written to the destination buffer before stopping (including the terminating null).
    */
    template <typename CharPointer>
    void writeWithCharLimit (CharPointer src, int maxChars) noexcept
    {
        CharacterFunctions::copyWithCharLimit (*this, src, maxChars);
    }

    /** Compares this string with another one. */
    template <typename CharPointer>
    int compare (CharPointer other) const noexcept
    {
        return CharacterFunctions::compare (*this, other);
    }

   #if JUCE_NATIVE_WCHAR_IS_UTF32 && ! JUCE_ANDROID
    /** Compares this string with another one. */
    int compare (CharPointer_UTF32 other) const noexcept
    {
        return wcscmp (data, other.data);
    }
   #endif

    /** Compares this string with another one, up to a specified number of characters. */
    template <typename CharPointer>
    int compareUpTo (CharPointer other, int maxChars) const noexcept
    {
        return CharacterFunctions::compareUpTo (*this, other, maxChars);
    }

    /** Compares this string with another one. */
    template <typename CharPointer>
    int compareIgnoreCase (CharPointer other) const
    {
        return CharacterFunctions::compareIgnoreCase (*this, other);
    }

    /** Compares this string with another one, up to a specified number of characters. */
    template <typename CharPointer>
    int compareIgnoreCaseUpTo (CharPointer other, int maxChars) const noexcept
    {
        return CharacterFunctions::compareIgnoreCaseUpTo (*this, other, maxChars);
    }

    /** Returns the character index of a substring, or -1 if it isn't found. */
    template <typename CharPointer>
    int indexOf (CharPointer stringToFind) const noexcept
    {
        return CharacterFunctions::indexOf (*this, stringToFind);
    }

    /** Returns the character index of a unicode character, or -1 if it isn't found. */
    int indexOf (juce_wchar charToFind) const noexcept
    {
        int i = 0;

        while (data[i] != 0)
        {
            if (data[i] == charToFind)
                return i;

            ++i;
        }

        return -1;
    }

    /** Returns the character index of a unicode character, or -1 if it isn't found. */
    int indexOf (juce_wchar charToFind, bool ignoreCase) const noexcept
    {
        return ignoreCase ? CharacterFunctions::indexOfCharIgnoreCase (*this, charToFind)
                          : CharacterFunctions::indexOfChar (*this, charToFind);
    }

    /** Returns true if the first character of this string is whitespace. */
    bool isWhitespace() const                   { return CharacterFunctions::isWhitespace (*data) != 0; }
    /** Returns true if the first character of this string is a digit. */
    bool isDigit() const                        { return CharacterFunctions::isDigit (*data) != 0; }
    /** Returns true if the first character of this string is a letter. */
    bool isLetter() const                       { return CharacterFunctions::isLetter (*data) != 0; }
    /** Returns true if the first character of this string is a letter or digit. */
    bool isLetterOrDigit() const                { return CharacterFunctions::isLetterOrDigit (*data) != 0; }
    /** Returns true if the first character of this string is upper-case. */
    bool isUpperCase() const                    { return CharacterFunctions::isUpperCase (*data) != 0; }
    /** Returns true if the first character of this string is lower-case. */
    bool isLowerCase() const                    { return CharacterFunctions::isLowerCase (*data) != 0; }

    /** Returns an upper-case version of the first character of this string. */
    juce_wchar toUpperCase() const noexcept     { return CharacterFunctions::toUpperCase (*data); }
    /** Returns a lower-case version of the first character of this string. */
    juce_wchar toLowerCase() const noexcept     { return CharacterFunctions::toLowerCase (*data); }

    /** Parses this string as a 32-bit integer. */
    int getIntValue32() const noexcept          { return CharacterFunctions::getIntValue <int, CharPointer_UTF32> (*this); }
    /** Parses this string as a 64-bit integer. */
    int64 getIntValue64() const noexcept        { return CharacterFunctions::getIntValue <int64, CharPointer_UTF32> (*this); }

    /** Parses this string as a floating point double. */
    double getDoubleValue() const noexcept      { return CharacterFunctions::getDoubleValue (*this); }

    /** Returns the first non-whitespace character in the string. */
    CharPointer_UTF32 findEndOfWhitespace() const noexcept   { return CharacterFunctions::findEndOfWhitespace (*this); }

    /** Returns true if the given unicode character can be represented in this encoding. */
    static bool canRepresent (juce_wchar character) noexcept
    {
        return ((uint32) character) < (uint32) 0x10ffff;
    }

    /** Returns true if this data contains a valid string in this encoding. */
    static bool isValidString (const CharType* dataToTest, int maxBytesToRead)
    {
        maxBytesToRead /= (int) sizeof (CharType);

        while (--maxBytesToRead >= 0 && *dataToTest != 0)
            if (! canRepresent (*dataToTest++))
                return false;

        return true;
    }

    /** Atomically swaps this pointer for a new value, returning the previous value. */
    CharPointer_UTF32 atomicSwap (CharPointer_UTF32 newValue)
    {
        return CharPointer_UTF32 (reinterpret_cast<Atomic<CharType*>&> (data).exchange (newValue.data));
    }

private:
    CharType* data;
};

} // namespace juce
