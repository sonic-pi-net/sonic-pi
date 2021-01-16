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
    Wraps a pointer to a null-terminated UTF-16 character string, and provides
    various methods to operate on the data.
    @see CharPointer_UTF8, CharPointer_UTF32

    @tags{Core}
*/
class CharPointer_UTF16  final
{
public:
   #if JUCE_NATIVE_WCHAR_IS_UTF16
    using CharType = wchar_t;
   #else
    using CharType = int16;
   #endif

    inline explicit CharPointer_UTF16 (const CharType* rawPointer) noexcept
        : data (const_cast<CharType*> (rawPointer))
    {
    }

    inline CharPointer_UTF16 (const CharPointer_UTF16& other) = default;

    inline CharPointer_UTF16 operator= (CharPointer_UTF16 other) noexcept
    {
        data = other.data;
        return *this;
    }

    inline CharPointer_UTF16 operator= (const CharType* text) noexcept
    {
        data = const_cast<CharType*> (text);
        return *this;
    }

    /** This is a pointer comparison, it doesn't compare the actual text. */
    inline bool operator== (CharPointer_UTF16 other) const noexcept     { return data == other.data; }
    inline bool operator!= (CharPointer_UTF16 other) const noexcept     { return data != other.data; }
    inline bool operator<= (CharPointer_UTF16 other) const noexcept     { return data <= other.data; }
    inline bool operator<  (CharPointer_UTF16 other) const noexcept     { return data <  other.data; }
    inline bool operator>= (CharPointer_UTF16 other) const noexcept     { return data >= other.data; }
    inline bool operator>  (CharPointer_UTF16 other) const noexcept     { return data >  other.data; }

    /** Returns the address that this pointer is pointing to. */
    inline CharType* getAddress() const noexcept        { return data; }

    /** Returns the address that this pointer is pointing to. */
    inline operator const CharType*() const noexcept    { return data; }

    /** Returns true if this pointer is pointing to a null character. */
    inline bool isEmpty() const noexcept                { return *data == 0; }

    /** Returns true if this pointer is not pointing to a null character. */
    inline bool isNotEmpty() const noexcept             { return *data != 0; }

    /** Returns the unicode character that this pointer is pointing to. */
    juce_wchar operator*() const noexcept
    {
        auto n = (uint32) (uint16) *data;

        if (n >= 0xd800 && n <= 0xdfff && ((uint32) (uint16) data[1]) >= 0xdc00)
            n = 0x10000 + (((n - 0xd800) << 10) | (((uint32) (uint16) data[1]) - 0xdc00));

        return (juce_wchar) n;
    }

    /** Moves this pointer along to the next character in the string. */
    CharPointer_UTF16 operator++() noexcept
    {
        auto n = (uint32) (uint16) *data++;

        if (n >= 0xd800 && n <= 0xdfff && ((uint32) (uint16) *data) >= 0xdc00)
            ++data;

        return *this;
    }

    /** Moves this pointer back to the previous character in the string. */
    CharPointer_UTF16 operator--() noexcept
    {
        auto n = (uint32) (uint16) (*--data);

        if (n >= 0xdc00 && n <= 0xdfff)
            --data;

        return *this;
    }

    /** Returns the character that this pointer is currently pointing to, and then
        advances the pointer to point to the next character. */
    juce_wchar getAndAdvance() noexcept
    {
        auto n = (uint32) (uint16) *data++;

        if (n >= 0xd800 && n <= 0xdfff && ((uint32) (uint16) *data) >= 0xdc00)
            n = 0x10000 + ((((n - 0xd800) << 10) | (((uint32) (uint16) *data++) - 0xdc00)));

        return (juce_wchar) n;
    }

    /** Moves this pointer along to the next character in the string. */
    CharPointer_UTF16 operator++ (int) noexcept
    {
        auto temp (*this);
        ++*this;
        return temp;
    }

    /** Moves this pointer forwards by the specified number of characters. */
    void operator+= (int numToSkip) noexcept
    {
        if (numToSkip < 0)
        {
            while (++numToSkip <= 0)
                --*this;
        }
        else
        {
            while (--numToSkip >= 0)
                ++*this;
        }
    }

    /** Moves this pointer backwards by the specified number of characters. */
    void operator-= (int numToSkip) noexcept
    {
        operator+= (-numToSkip);
    }

    /** Returns the character at a given character index from the start of the string. */
    juce_wchar operator[] (int characterIndex) const noexcept
    {
        auto p (*this);
        p += characterIndex;
        return *p;
    }

    /** Returns a pointer which is moved forwards from this one by the specified number of characters. */
    CharPointer_UTF16 operator+ (int numToSkip) const noexcept
    {
        auto p (*this);
        p += numToSkip;
        return p;
    }

    /** Returns a pointer which is moved backwards from this one by the specified number of characters. */
    CharPointer_UTF16 operator- (int numToSkip) const noexcept
    {
        auto p (*this);
        p += -numToSkip;
        return p;
    }

    /** Writes a unicode character to this string, and advances this pointer to point to the next position. */
    void write (juce_wchar charToWrite) noexcept
    {
        if (charToWrite >= 0x10000)
        {
            charToWrite -= 0x10000;
            *data++ = (CharType) (0xd800 + (charToWrite >> 10));
            *data++ = (CharType) (0xdc00 + (charToWrite & 0x3ff));
        }
        else
        {
            *data++ = (CharType) charToWrite;
        }
    }

    /** Writes a null character to this string (leaving the pointer's position unchanged). */
    inline void writeNull() const noexcept
    {
        *data = 0;
    }

    /** Returns the number of characters in this string. */
    size_t length() const noexcept
    {
        auto* d = data;
        size_t count = 0;

        for (;;)
        {
            auto n = (uint32) (uint16) *d++;

            if (n >= 0xd800 && n <= 0xdfff)
            {
                if (*d++ == 0)
                    break;
            }
            else if (n == 0)
                break;

            ++count;
        }

        return count;
    }

    /** Returns the number of characters in this string, or the given value, whichever is lower. */
    size_t lengthUpTo (size_t maxCharsToCount) const noexcept
    {
        return CharacterFunctions::lengthUpTo (*this, maxCharsToCount);
    }

    /** Returns the number of characters in this string, or up to the given end pointer, whichever is lower. */
    size_t lengthUpTo (CharPointer_UTF16 end) const noexcept
    {
        return CharacterFunctions::lengthUpTo (*this, end);
    }

    /** Returns the number of bytes that are used to represent this string.
        This includes the terminating null character.
    */
    size_t sizeInBytes() const noexcept
    {
        return sizeof (CharType) * (findNullIndex (data) + 1);
    }

    /** Returns the number of bytes that would be needed to represent the given
        unicode character in this encoding format.
    */
    static size_t getBytesRequiredFor (juce_wchar charToWrite) noexcept
    {
        return (charToWrite >= 0x10000) ? (sizeof (CharType) * 2) : sizeof (CharType);
    }

    /** Returns the number of bytes that would be needed to represent the given
        string in this encoding format.
        The value returned does NOT include the terminating null character.
    */
    template <class CharPointer>
    static size_t getBytesRequiredFor (CharPointer text) noexcept
    {
        size_t count = 0;
        juce_wchar n;

        while ((n = text.getAndAdvance()) != 0)
            count += getBytesRequiredFor (n);

        return count;
    }

    /** Returns a pointer to the null character that terminates this string. */
    CharPointer_UTF16 findTerminatingNull() const noexcept
    {
        auto* t = data;

        while (*t != 0)
            ++t;

        return CharPointer_UTF16 (t);
    }

    /** Copies a source string to this pointer, advancing this pointer as it goes. */
    template <typename CharPointer>
    void writeAll (CharPointer src) noexcept
    {
        CharacterFunctions::copyAll (*this, src);
    }

    /** Copies a source string to this pointer, advancing this pointer as it goes. */
    void writeAll (CharPointer_UTF16 src) noexcept
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

    /** Compares this string with another one, up to a specified number of characters. */
    template <typename CharPointer>
    int compareUpTo (CharPointer other, int maxChars) const noexcept
    {
        return CharacterFunctions::compareUpTo (*this, other, maxChars);
    }

    /** Compares this string with another one. */
    template <typename CharPointer>
    int compareIgnoreCase (CharPointer other) const noexcept
    {
        return CharacterFunctions::compareIgnoreCase (*this, other);
    }

    /** Compares this string with another one, up to a specified number of characters. */
    template <typename CharPointer>
    int compareIgnoreCaseUpTo (CharPointer other, int maxChars) const noexcept
    {
        return CharacterFunctions::compareIgnoreCaseUpTo (*this, other, maxChars);
    }

   #if JUCE_MSVC && ! DOXYGEN
    int compareIgnoreCase (CharPointer_UTF16 other) const noexcept
    {
        return _wcsicmp (data, other.data);
    }

    int compareIgnoreCaseUpTo (CharPointer_UTF16 other, int maxChars) const noexcept
    {
        return _wcsnicmp (data, other.data, (size_t) maxChars);
    }

    int indexOf (CharPointer_UTF16 stringToFind) const noexcept
    {
        const CharType* const t = wcsstr (data, stringToFind.getAddress());
        return t == nullptr ? -1 : (int) (t - data);
    }
   #endif

    /** Returns the character index of a substring, or -1 if it isn't found. */
    template <typename CharPointer>
    int indexOf (CharPointer stringToFind) const noexcept
    {
        return CharacterFunctions::indexOf (*this, stringToFind);
    }

    /** Returns the character index of a unicode character, or -1 if it isn't found. */
    int indexOf (juce_wchar charToFind) const noexcept
    {
        return CharacterFunctions::indexOfChar (*this, charToFind);
    }

    /** Returns the character index of a unicode character, or -1 if it isn't found. */
    int indexOf (juce_wchar charToFind, bool ignoreCase) const noexcept
    {
        return ignoreCase ? CharacterFunctions::indexOfCharIgnoreCase (*this, charToFind)
                          : CharacterFunctions::indexOfChar (*this, charToFind);
    }

    /** Returns true if the first character of this string is whitespace. */
    bool isWhitespace() const noexcept          { return CharacterFunctions::isWhitespace (operator*()) != 0; }
    /** Returns true if the first character of this string is a digit. */
    bool isDigit() const noexcept               { return CharacterFunctions::isDigit (operator*()) != 0; }
    /** Returns true if the first character of this string is a letter. */
    bool isLetter() const noexcept              { return CharacterFunctions::isLetter (operator*()) != 0; }
    /** Returns true if the first character of this string is a letter or digit. */
    bool isLetterOrDigit() const noexcept       { return CharacterFunctions::isLetterOrDigit (operator*()) != 0; }
    /** Returns true if the first character of this string is upper-case. */
    bool isUpperCase() const noexcept           { return CharacterFunctions::isUpperCase (operator*()) != 0; }
    /** Returns true if the first character of this string is lower-case. */
    bool isLowerCase() const noexcept           { return CharacterFunctions::isLowerCase (operator*()) != 0; }

    /** Returns an upper-case version of the first character of this string. */
    juce_wchar toUpperCase() const noexcept     { return CharacterFunctions::toUpperCase (operator*()); }
    /** Returns a lower-case version of the first character of this string. */
    juce_wchar toLowerCase() const noexcept     { return CharacterFunctions::toLowerCase (operator*()); }

    /** Parses this string as a 32-bit integer. */
    int getIntValue32() const noexcept
    {
       #if JUCE_MSVC
        return _wtoi (data);
       #else
        return CharacterFunctions::getIntValue<int, CharPointer_UTF16> (*this);
       #endif
    }

    /** Parses this string as a 64-bit integer. */
    int64 getIntValue64() const noexcept
    {
       #if JUCE_MSVC
        return _wtoi64 (data);
       #else
        return CharacterFunctions::getIntValue<int64, CharPointer_UTF16> (*this);
       #endif
    }

    /** Parses this string as a floating point double. */
    double getDoubleValue() const noexcept                      { return CharacterFunctions::getDoubleValue (*this); }

    /** Returns the first non-whitespace character in the string. */
    CharPointer_UTF16 findEndOfWhitespace() const noexcept      { return CharacterFunctions::findEndOfWhitespace (*this); }

    /** Returns true if the given unicode character can be represented in this encoding. */
    static bool canRepresent (juce_wchar character) noexcept
    {
        auto n = (uint32) character;
        return n < 0x10ffff && (n < 0xd800 || n > 0xdfff);
    }

    /** Returns true if this data contains a valid string in this encoding. */
    static bool isValidString (const CharType* dataToTest, int maxBytesToRead)
    {
        maxBytesToRead /= (int) sizeof (CharType);

        while (--maxBytesToRead >= 0 && *dataToTest != 0)
        {
            auto n = (uint32) (uint16) *dataToTest++;

            if (n >= 0xd800)
            {
                if (n > 0x10ffff)
                    return false;

                if (n <= 0xdfff)
                {
                    if (n > 0xdc00)
                        return false;

                    auto nextChar = (uint32) (uint16) *dataToTest++;

                    if (nextChar < 0xdc00 || nextChar > 0xdfff)
                        return false;
                }
            }
        }

        return true;
    }

    /** Atomically swaps this pointer for a new value, returning the previous value. */
    CharPointer_UTF16 atomicSwap (CharPointer_UTF16 newValue)
    {
        return CharPointer_UTF16 (reinterpret_cast<Atomic<CharType*>&> (data).exchange (newValue.data));
    }

    /** These values are the byte-order-mark (BOM) values for a UTF-16 stream. */
    enum
    {
        byteOrderMarkBE1 = 0xfe,
        byteOrderMarkBE2 = 0xff,
        byteOrderMarkLE1 = 0xff,
        byteOrderMarkLE2 = 0xfe
    };

    /** Returns true if the first pair of bytes in this pointer are the UTF16 byte-order mark (big endian).
        The pointer must not be null, and must contain at least two valid bytes.
    */
    static bool isByteOrderMarkBigEndian (const void* possibleByteOrder) noexcept
    {
        jassert (possibleByteOrder != nullptr);
        auto c = static_cast<const uint8*> (possibleByteOrder);

        return c[0] == (uint8) byteOrderMarkBE1
            && c[1] == (uint8) byteOrderMarkBE2;
    }

    /** Returns true if the first pair of bytes in this pointer are the UTF16 byte-order mark (little endian).
        The pointer must not be null, and must contain at least two valid bytes.
    */
    static bool isByteOrderMarkLittleEndian (const void* possibleByteOrder) noexcept
    {
        jassert (possibleByteOrder != nullptr);
        auto c = static_cast<const uint8*> (possibleByteOrder);

        return c[0] == (uint8) byteOrderMarkLE1
            && c[1] == (uint8) byteOrderMarkLE2;
    }

private:
    CharType* data;

    static unsigned int findNullIndex (const CharType* t) noexcept
    {
        unsigned int n = 0;

        while (t[n] != 0)
            ++n;

        return n;
    }
};

} // namespace juce
