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
#if JUCE_WINDOWS && ! DOXYGEN
 #define JUCE_NATIVE_WCHAR_IS_UTF8      0
 #define JUCE_NATIVE_WCHAR_IS_UTF16     1
 #define JUCE_NATIVE_WCHAR_IS_UTF32     0
#else
 /** This macro will be set to 1 if the compiler's native wchar_t is an 8-bit type. */
 #define JUCE_NATIVE_WCHAR_IS_UTF8      0
 /** This macro will be set to 1 if the compiler's native wchar_t is a 16-bit type. */
 #define JUCE_NATIVE_WCHAR_IS_UTF16     0
 /** This macro will be set to 1 if the compiler's native wchar_t is a 32-bit type. */
 #define JUCE_NATIVE_WCHAR_IS_UTF32     1
#endif

#if JUCE_NATIVE_WCHAR_IS_UTF32 || DOXYGEN
 /** A platform-independent 32-bit unicode character type. */
 using juce_wchar = wchar_t;
#else
 using juce_wchar = uint32;
#endif

#ifndef DOXYGEN
 /** This macro is deprecated, but preserved for compatibility with old code. */
 #define JUCE_T(stringLiteral)   (L##stringLiteral)
#endif

#if JUCE_DEFINE_T_MACRO
 /** The 'T' macro is an alternative for using the "L" prefix in front of a string literal.

     This macro is deprecated, but available for compatibility with old code if you set
     JUCE_DEFINE_T_MACRO = 1. The fastest, most portable and best way to write your string
     literals is as standard char strings, using escaped utf-8 character sequences for extended
     characters, rather than trying to store them as wide-char strings.
 */
 #define T(stringLiteral)   JUCE_T(stringLiteral)
#endif

#if ! DOXYGEN

//==============================================================================
// GNU libstdc++ does not have std::make_unsigned
namespace internal
{
    template <typename Type> struct make_unsigned               { using type = Type; };
    template <> struct make_unsigned<signed char>               { using type = unsigned char; };
    template <> struct make_unsigned<char>                      { using type = unsigned char; };
    template <> struct make_unsigned<short>                     { using type = unsigned short; };
    template <> struct make_unsigned<int>                       { using type = unsigned int; };
    template <> struct make_unsigned<long>                      { using type = unsigned long; };
    template <> struct make_unsigned<long long>                 { using type = unsigned long long; };
}

#endif

//==============================================================================
/**
    A collection of functions for manipulating characters and character strings.

    Most of these methods are designed for internal use by the String and CharPointer
    classes, but some of them may be useful to call directly.

    @see String, CharPointer_UTF8, CharPointer_UTF16, CharPointer_UTF32

    @tags{Core}
*/
class JUCE_API  CharacterFunctions
{
public:
    //==============================================================================
    /** Converts a character to upper-case. */
    static juce_wchar toUpperCase (juce_wchar character) noexcept;
    /** Converts a character to lower-case. */
    static juce_wchar toLowerCase (juce_wchar character) noexcept;

    /** Checks whether a unicode character is upper-case. */
    static bool isUpperCase (juce_wchar character) noexcept;
    /** Checks whether a unicode character is lower-case. */
    static bool isLowerCase (juce_wchar character) noexcept;

    /** Checks whether a character is whitespace. */
    static bool isWhitespace (char character) noexcept;
    /** Checks whether a character is whitespace. */
    static bool isWhitespace (juce_wchar character) noexcept;

    /** Checks whether a character is a digit. */
    static bool isDigit (char character) noexcept;
    /** Checks whether a character is a digit. */
    static bool isDigit (juce_wchar character) noexcept;

    /** Checks whether a character is alphabetic. */
    static bool isLetter (char character) noexcept;
    /** Checks whether a character is alphabetic. */
    static bool isLetter (juce_wchar character) noexcept;

    /** Checks whether a character is alphabetic or numeric. */
    static bool isLetterOrDigit (char character) noexcept;
    /** Checks whether a character is alphabetic or numeric. */
    static bool isLetterOrDigit (juce_wchar character) noexcept;

    /** Checks whether a character is a printable character, i.e. alphabetic, numeric,
        a punctuation character or a space.
    */
    static bool isPrintable (char character) noexcept;

    /** Checks whether a character is a printable character, i.e. alphabetic, numeric,
        a punctuation character or a space.
    */
    static bool isPrintable (juce_wchar character) noexcept;

    /** Returns 0 to 16 for '0' to 'F", or -1 for characters that aren't a legal hex digit. */
    static int getHexDigitValue (juce_wchar digit) noexcept;

    /** Converts a byte of Windows 1252 codepage to unicode. */
    static juce_wchar getUnicodeCharFromWindows1252Codepage (uint8 windows1252Char) noexcept;

    //==============================================================================
    /** Parses a character string to read a floating-point number.
        Note that this will advance the pointer that is passed in, leaving it at
        the end of the number.
    */
    template <typename CharPointerType>
    static double readDoubleValue (CharPointerType& text) noexcept
    {
       #if JUCE_MINGW
        bool isNegative = false;
       #else
        JUCE_CONSTEXPR const int maxSignificantDigits = 17 + 1; // An additional digit for rounding
        JUCE_CONSTEXPR const int bufferSize = maxSignificantDigits + 7 + 1; // -.E-XXX and a trailing null-terminator
        char buffer[(size_t) bufferSize] = {};
        char* currentCharacter = &(buffer[0]);
       #endif

        text = text.findEndOfWhitespace();
        auto c = *text;

        switch (c)
        {
            case '-':
               #if JUCE_MINGW
                isNegative = true;
               #else
                *currentCharacter++ = '-';
               #endif
                // Fall-through..
            case '+':
                c = *++text;
        }

        switch (c)
        {
            case 'n':
            case 'N':
                if ((text[1] == 'a' || text[1] == 'A') && (text[2] == 'n' || text[2] == 'N'))
                    return std::numeric_limits<double>::quiet_NaN();
                break;

            case 'i':
            case 'I':
                if ((text[1] == 'n' || text[1] == 'N') && (text[2] == 'f' || text[2] == 'F'))
                    return std::numeric_limits<double>::infinity();
                break;
        }

       #if JUCE_MINGW
        // MinGW does not have access to the locale functions required for strtold, so we parse the doubles
        // ourselves. There are some edge cases where the least significant digit will be wrong!
        double result[3] = { 0 }, accumulator[2] = { 0 };
        int exponentAdjustment[2] = { 0 }, exponentAccumulator[2] = { -1, -1 };
        int exponent = 0, decPointIndex = 0, digit = 0;
        int lastDigit = 0, numSignificantDigits = 0;
        bool digitsFound = false;
        JUCE_CONSTEXPR const int maxSignificantDigits = 17 + 1;

        for (;;)
        {
            if (text.isDigit())
            {
                lastDigit = digit;
                digit = (int) text.getAndAdvance() - '0';
                digitsFound = true;

                if (decPointIndex != 0)
                    exponentAdjustment[1]++;

                if (numSignificantDigits == 0 && digit == 0)
                    continue;

                if (++numSignificantDigits > maxSignificantDigits)
                {
                    if (digit > 5)
                        ++accumulator [decPointIndex];
                    else if (digit == 5 && (lastDigit & 1) != 0)
                        ++accumulator [decPointIndex];

                    if (decPointIndex > 0)
                        exponentAdjustment[1]--;
                    else
                        exponentAdjustment[0]++;

                    while (text.isDigit())
                    {
                        ++text;
                        if (decPointIndex == 0)
                            exponentAdjustment[0]++;
                    }
                }
                else
                {
                    const auto maxAccumulatorValue = (double) ((std::numeric_limits<unsigned int>::max() - 9) / 10);
                    if (accumulator [decPointIndex] > maxAccumulatorValue)
                    {
                        result [decPointIndex] = mulexp10 (result [decPointIndex], exponentAccumulator [decPointIndex])
                                                 + accumulator [decPointIndex];
                        accumulator [decPointIndex] = 0;
                        exponentAccumulator [decPointIndex] = 0;
                    }

                    accumulator [decPointIndex] = accumulator[decPointIndex] * 10 + digit;
                    exponentAccumulator [decPointIndex]++;
                }
            }
            else if (decPointIndex == 0 && *text == '.')
            {
                ++text;
                decPointIndex = 1;

                if (numSignificantDigits > maxSignificantDigits)
                {
                    while (text.isDigit())
                        ++text;
                    break;
                }
            }
            else
            {
                break;
            }
        }

        result[0] = mulexp10 (result[0], exponentAccumulator[0]) + accumulator[0];

        if (decPointIndex != 0)
            result[1] = mulexp10 (result[1], exponentAccumulator[1]) + accumulator[1];

        c = *text;
        if ((c == 'e' || c == 'E') && digitsFound)
        {
            auto negativeExponent = false;

            switch (*++text)
            {
                case '-':   negativeExponent = true; // fall-through..
                case '+':   ++text;
            }

            while (text.isDigit())
                exponent = (exponent * 10) + ((int) text.getAndAdvance() - '0');

            if (negativeExponent)
                exponent = -exponent;
        }

        auto r = mulexp10 (result[0], exponent + exponentAdjustment[0]);
        if (decPointIndex != 0)
            r += mulexp10 (result[1], exponent - exponentAdjustment[1]);

        return isNegative ? -r : r;

       #else   // ! JUCE_MINGW

        int numSigFigs = 0;
        bool decimalPointFound = false;
        int extraExponent = 0;

        for (;;)
        {
            if (text.isDigit())
            {
                auto digit = (int) text.getAndAdvance() - '0';

                if (decimalPointFound)
                {
                    if (numSigFigs >= maxSignificantDigits)
                        continue;
                }
                else
                {
                    if (numSigFigs >= maxSignificantDigits)
                    {
                        ++extraExponent;
                        continue;
                    }

                    if (numSigFigs == 0 && digit == 0)
                        continue;
                }

                *currentCharacter++ = (char) ('0' + (char) digit);
                numSigFigs++;
            }
            else if ((! decimalPointFound) && *text == '.')
            {
                ++text;
                *currentCharacter++ = '.';
                decimalPointFound = true;
            }
            else
            {
                break;
            }
        }

        c = *text;

        auto writeExponentDigits = [](int exponent, char* destination)
        {
            auto exponentDivisor = 100;

            while (exponentDivisor > 1)
            {
                auto digit = exponent / exponentDivisor;
                *destination++ = (char) ('0' + (char) digit);
                exponent -= digit * exponentDivisor;
                exponentDivisor /= 10;
            }

            *destination++ = (char) ('0' + (char) exponent);
        };

        if ((c == 'e' || c == 'E') && numSigFigs > 0)
        {
            *currentCharacter++ = 'e';
            bool parsedExponentIsPositive = true;

            switch (*++text)
            {
                case '-':   parsedExponentIsPositive = false; // Fall-through..
                case '+':   ++text;
            }

            int exponent = 0;

            while (text.isDigit())
            {
                auto digit = (int) text.getAndAdvance() - '0';

                if (digit != 0 || exponent != 0)
                    exponent = (exponent * 10) + digit;
            }

            exponent = extraExponent + (parsedExponentIsPositive ? exponent : -exponent);

            if (exponent < 0)
                *currentCharacter++ = '-';

            exponent = std::abs (exponent);

            if (exponent > std::numeric_limits<double>::max_exponent10)
                return std::numeric_limits<double>::quiet_NaN();

            writeExponentDigits (exponent, currentCharacter);
        }
        else if (extraExponent > 0)
        {
            *currentCharacter++ = 'e';
            writeExponentDigits (extraExponent, currentCharacter);
        }

       #if JUCE_WINDOWS
        static _locale_t locale = _create_locale (LC_ALL, "C");
        return _strtod_l (&buffer[0], nullptr, locale);
       #else
        static locale_t locale = newlocale (LC_ALL_MASK, "C", nullptr);
        #if JUCE_ANDROID
        return (double) strtold_l (&buffer[0], nullptr, locale);
        #else
        return strtod_l (&buffer[0], nullptr, locale);
        #endif
       #endif

       #endif   // JUCE_MINGW
    }

    /** Parses a character string, to read a floating-point value. */
    template <typename CharPointerType>
    static double getDoubleValue (CharPointerType text) noexcept
    {
        return readDoubleValue (text);
    }

    //==============================================================================
    /** Parses a character string, to read an integer value. */
    template <typename IntType, typename CharPointerType>
    static IntType getIntValue (const CharPointerType text) noexcept
    {
        using UIntType = typename internal::make_unsigned<IntType>::type;

        UIntType v = 0;
        auto s = text.findEndOfWhitespace();
        const bool isNeg = *s == '-';

        if (isNeg)
            ++s;

        for (;;)
        {
            auto c = s.getAndAdvance();

            if (c >= '0' && c <= '9')
                v = v * 10 + (UIntType) (c - '0');
            else
                break;
        }

        return isNeg ? - (IntType) v : (IntType) v;
    }

    /** Parses a character string, to read a hexadecimal value. */
    template <typename ResultType>
    struct HexParser
    {
        template <typename CharPointerType>
        static ResultType parse (CharPointerType t) noexcept
        {
            ResultType result = 0;

            while (! t.isEmpty())
            {
                auto hexValue = CharacterFunctions::getHexDigitValue (t.getAndAdvance());

                if (hexValue >= 0)
                    result = (result << 4) | hexValue;
            }

            return result;
        }
    };

    //==============================================================================
    /** Counts the number of characters in a given string, stopping if the count exceeds
        a specified limit. */
    template <typename CharPointerType>
    static size_t lengthUpTo (CharPointerType text, const size_t maxCharsToCount) noexcept
    {
        size_t len = 0;

        while (len < maxCharsToCount && text.getAndAdvance() != 0)
            ++len;

        return len;
    }

    /** Counts the number of characters in a given string, stopping if the count exceeds
        a specified end-pointer. */
    template <typename CharPointerType>
    static size_t lengthUpTo (CharPointerType start, const CharPointerType end) noexcept
    {
        size_t len = 0;

        while (start < end && start.getAndAdvance() != 0)
            ++len;

        return len;
    }

    /** Copies null-terminated characters from one string to another. */
    template <typename DestCharPointerType, typename SrcCharPointerType>
    static void copyAll (DestCharPointerType& dest, SrcCharPointerType src) noexcept
    {
        while (auto c = src.getAndAdvance())
            dest.write (c);

        dest.writeNull();
    }

    /** Copies characters from one string to another, up to a null terminator
        or a given byte size limit. */
    template <typename DestCharPointerType, typename SrcCharPointerType>
    static size_t copyWithDestByteLimit (DestCharPointerType& dest, SrcCharPointerType src, size_t maxBytesToWrite) noexcept
    {
        auto startAddress = dest.getAddress();
        auto maxBytes = (ssize_t) maxBytesToWrite;
        maxBytes -= (ssize_t) sizeof (typename DestCharPointerType::CharType); // (allow for a terminating null)

        for (;;)
        {
            auto c = src.getAndAdvance();
            auto bytesNeeded = (ssize_t) DestCharPointerType::getBytesRequiredFor (c);
            maxBytes -= bytesNeeded;

            if (c == 0 || maxBytes < 0)
                break;

            dest.write (c);
        }

        dest.writeNull();

        return (size_t) getAddressDifference (dest.getAddress(), startAddress)
                 + sizeof (typename DestCharPointerType::CharType);
    }

    /** Copies characters from one string to another, up to a null terminator
        or a given maximum number of characters. */
    template <typename DestCharPointerType, typename SrcCharPointerType>
    static void copyWithCharLimit (DestCharPointerType& dest, SrcCharPointerType src, int maxChars) noexcept
    {
        while (--maxChars > 0)
        {
            auto c = src.getAndAdvance();

            if (c == 0)
                break;

            dest.write (c);
        }

        dest.writeNull();
    }

    /** Compares two characters. */
    static inline int compare (juce_wchar char1, juce_wchar char2) noexcept
    {
        if (auto diff = static_cast<int> (char1) - static_cast<int> (char2))
            return diff < 0 ? -1 : 1;

        return 0;
    }

    /** Compares two null-terminated character strings. */
    template <typename CharPointerType1, typename CharPointerType2>
    static int compare (CharPointerType1 s1, CharPointerType2 s2) noexcept
    {
        for (;;)
        {
            auto c1 = s1.getAndAdvance();

            if (auto diff = compare (c1, s2.getAndAdvance()))
                return diff;

            if (c1 == 0)
                break;
        }

        return 0;
    }

    /** Compares two null-terminated character strings, up to a given number of characters. */
    template <typename CharPointerType1, typename CharPointerType2>
    static int compareUpTo (CharPointerType1 s1, CharPointerType2 s2, int maxChars) noexcept
    {
        while (--maxChars >= 0)
        {
            auto c1 = s1.getAndAdvance();

            if (auto diff = compare (c1, s2.getAndAdvance()))
                return diff;

            if (c1 == 0)
                break;
        }

        return 0;
    }

    /** Compares two characters, using a case-independant match. */
    static inline int compareIgnoreCase (juce_wchar char1, juce_wchar char2) noexcept
    {
        return char1 != char2 ? compare (toUpperCase (char1), toUpperCase (char2)) : 0;
    }

    /** Compares two null-terminated character strings, using a case-independant match. */
    template <typename CharPointerType1, typename CharPointerType2>
    static int compareIgnoreCase (CharPointerType1 s1, CharPointerType2 s2) noexcept
    {
        for (;;)
        {
            auto c1 = s1.getAndAdvance();

            if (auto diff = compareIgnoreCase (c1, s2.getAndAdvance()))
                return diff;

            if (c1 == 0)
                break;
        }

        return 0;
    }

    /** Compares two null-terminated character strings, using a case-independent match. */
    template <typename CharPointerType1, typename CharPointerType2>
    static int compareIgnoreCaseUpTo (CharPointerType1 s1, CharPointerType2 s2, int maxChars) noexcept
    {
        while (--maxChars >= 0)
        {
            auto c1 = s1.getAndAdvance();

            if (auto diff = compareIgnoreCase (c1, s2.getAndAdvance()))
                return diff;

            if (c1 == 0)
                break;
        }

        return 0;
    }

    /** Finds the character index of a given substring in another string.
        Returns -1 if the substring is not found.
    */
    template <typename CharPointerType1, typename CharPointerType2>
    static int indexOf (CharPointerType1 textToSearch, const CharPointerType2 substringToLookFor) noexcept
    {
        int index = 0;
        auto substringLength = (int) substringToLookFor.length();

        for (;;)
        {
            if (textToSearch.compareUpTo (substringToLookFor, substringLength) == 0)
                return index;

            if (textToSearch.getAndAdvance() == 0)
                return -1;

            ++index;
        }
    }

    /** Returns a pointer to the first occurrence of a substring in a string.
        If the substring is not found, this will return a pointer to the string's
        null terminator.
    */
    template <typename CharPointerType1, typename CharPointerType2>
    static CharPointerType1 find (CharPointerType1 textToSearch, const CharPointerType2 substringToLookFor) noexcept
    {
        auto substringLength = (int) substringToLookFor.length();

        while (textToSearch.compareUpTo (substringToLookFor, substringLength) != 0
                 && ! textToSearch.isEmpty())
            ++textToSearch;

        return textToSearch;
    }

    /** Returns a pointer to the first occurrence of a substring in a string.
        If the substring is not found, this will return a pointer to the string's
        null terminator.
    */
    template <typename CharPointerType>
    static CharPointerType find (CharPointerType textToSearch, const juce_wchar charToLookFor) noexcept
    {
        for (;; ++textToSearch)
        {
            auto c = *textToSearch;

            if (c == charToLookFor || c == 0)
                break;
        }

        return textToSearch;
    }

    /** Finds the character index of a given substring in another string, using
        a case-independent match.
        Returns -1 if the substring is not found.
    */
    template <typename CharPointerType1, typename CharPointerType2>
    static int indexOfIgnoreCase (CharPointerType1 haystack, const CharPointerType2 needle) noexcept
    {
        int index = 0;
        auto needleLength = (int) needle.length();

        for (;;)
        {
            if (haystack.compareIgnoreCaseUpTo (needle, needleLength) == 0)
                return index;

            if (haystack.getAndAdvance() == 0)
                return -1;

            ++index;
        }
    }

    /** Finds the character index of a given character in another string.
        Returns -1 if the character is not found.
    */
    template <typename Type>
    static int indexOfChar (Type text, const juce_wchar charToFind) noexcept
    {
        int i = 0;

        while (! text.isEmpty())
        {
            if (text.getAndAdvance() == charToFind)
                return i;

            ++i;
        }

        return -1;
    }

    /** Finds the character index of a given character in another string, using
        a case-independent match.
        Returns -1 if the character is not found.
    */
    template <typename Type>
    static int indexOfCharIgnoreCase (Type text, juce_wchar charToFind) noexcept
    {
        charToFind = CharacterFunctions::toLowerCase (charToFind);
        int i = 0;

        while (! text.isEmpty())
        {
            if (text.toLowerCase() == charToFind)
                return i;

            ++text;
            ++i;
        }

        return -1;
    }

    /** Returns a pointer to the first non-whitespace character in a string.
        If the string contains only whitespace, this will return a pointer
        to its null terminator.
    */
    template <typename Type>
    static Type findEndOfWhitespace (Type text) noexcept
    {
        while (text.isWhitespace())
            ++text;

        return text;
    }

    /** Returns a pointer to the first character in the string which is found in
        the breakCharacters string.
    */
    template <typename Type, typename BreakType>
    static Type findEndOfToken (Type text, BreakType breakCharacters, Type quoteCharacters)
    {
        juce_wchar currentQuoteChar = 0;

        while (! text.isEmpty())
        {
            auto c = text.getAndAdvance();

            if (currentQuoteChar == 0 && breakCharacters.indexOf (c) >= 0)
            {
                --text;
                break;
            }

            if (quoteCharacters.indexOf (c) >= 0)
            {
                if (currentQuoteChar == 0)
                    currentQuoteChar = c;
                else if (currentQuoteChar == c)
                    currentQuoteChar = 0;
            }
        }

        return text;
    }

private:
    static double mulexp10 (double value, int exponent) noexcept;
};

} // namespace juce
