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

JUCE_BEGIN_IGNORE_WARNINGS_MSVC (4514 4996)

juce_wchar CharacterFunctions::toUpperCase (const juce_wchar character) noexcept
{
    return (juce_wchar) towupper ((wint_t) character);
}

juce_wchar CharacterFunctions::toLowerCase (const juce_wchar character) noexcept
{
    return (juce_wchar) towlower ((wint_t) character);
}

bool CharacterFunctions::isUpperCase (const juce_wchar character) noexcept
{
   #if JUCE_WINDOWS
    return iswupper ((wint_t) character) != 0;
   #else
    return toLowerCase (character) != character;
   #endif
}

bool CharacterFunctions::isLowerCase (const juce_wchar character) noexcept
{
   #if JUCE_WINDOWS
    return iswlower ((wint_t) character) != 0;
   #else
    return toUpperCase (character) != character;
   #endif
}

JUCE_END_IGNORE_WARNINGS_MSVC

//==============================================================================
bool CharacterFunctions::isWhitespace (const char character) noexcept
{
    return character == ' ' || (character <= 13 && character >= 9);
}

bool CharacterFunctions::isWhitespace (const juce_wchar character) noexcept
{
    return iswspace ((wint_t) character) != 0;
}

bool CharacterFunctions::isDigit (const char character) noexcept
{
    return (character >= '0' && character <= '9');
}

bool CharacterFunctions::isDigit (const juce_wchar character) noexcept
{
    return iswdigit ((wint_t) character) != 0;
}

bool CharacterFunctions::isLetter (const char character) noexcept
{
    return (character >= 'a' && character <= 'z')
        || (character >= 'A' && character <= 'Z');
}

bool CharacterFunctions::isLetter (const juce_wchar character) noexcept
{
    return iswalpha ((wint_t) character) != 0;
}

bool CharacterFunctions::isLetterOrDigit (const char character) noexcept
{
    return (character >= 'a' && character <= 'z')
        || (character >= 'A' && character <= 'Z')
        || (character >= '0' && character <= '9');
}

bool CharacterFunctions::isLetterOrDigit (const juce_wchar character) noexcept
{
    return iswalnum ((wint_t) character) != 0;
}

bool CharacterFunctions::isPrintable (const char character) noexcept
{
    return (character >= ' ' && character <= '~');
}

bool CharacterFunctions::isPrintable (const juce_wchar character) noexcept
{
    return iswprint ((wint_t) character) != 0;
}

int CharacterFunctions::getHexDigitValue (const juce_wchar digit) noexcept
{
    auto d = (unsigned int) (digit - '0');

    if (d < (unsigned int) 10)
        return (int) d;

    d += (unsigned int) ('0' - 'a');

    if (d < (unsigned int) 6)
        return (int) d + 10;

    d += (unsigned int) ('a' - 'A');

    if (d < (unsigned int) 6)
        return (int) d + 10;

    return -1;
}

double CharacterFunctions::mulexp10 (const double value, int exponent) noexcept
{
    if (exponent == 0)
        return value;

    if (value == 0.0)
        return 0;

    const bool negative = (exponent < 0);

    if (negative)
        exponent = -exponent;

    double result = 1.0, power = 10.0;

    for (int bit = 1; exponent != 0; bit <<= 1)
    {
        if ((exponent & bit) != 0)
        {
            exponent ^= bit;
            result *= power;

            if (exponent == 0)
                break;
        }

        power *= power;
    }

    return negative ? (value / result) : (value * result);
}

juce_wchar CharacterFunctions::getUnicodeCharFromWindows1252Codepage (const uint8 c) noexcept
{
    if (c < 0x80 || c >= 0xa0)
        return (juce_wchar) c;

    static const uint16 lookup[] = { 0x20AC, 0x0007, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021,
                                     0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0x0007, 0x017D, 0x0007,
                                     0x0007, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
                                     0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x0007, 0x017E, 0x0178 };

    return (juce_wchar) lookup[c - 0x80];
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

#define QUOTE(x) #x
#define STR(value) QUOTE(value)
#define ASYM_STRING_DOUBLE_PAIR(str, value) std::pair<String, double> (STR(str), value)
#define STRING_DOUBLE_PAIR(value) ASYM_STRING_DOUBLE_PAIR(value, value)
#define STRING_DOUBLE_PAIR_COMBOS(value) \
    STRING_DOUBLE_PAIR(value), \
    STRING_DOUBLE_PAIR(-value), \
    ASYM_STRING_DOUBLE_PAIR(+value, value), \
    ASYM_STRING_DOUBLE_PAIR(000000 ## value, value), \
    ASYM_STRING_DOUBLE_PAIR(+000 ## value, value), \
    ASYM_STRING_DOUBLE_PAIR(-0 ## value, -value)

class CharacterFunctionsTests  : public UnitTest
{
public:
    CharacterFunctionsTests()
        : UnitTest ("CharacterFunctions", UnitTestCategories::text)
    {}

    void runTest() override
    {
        beginTest ("readDoubleValue");

        static const std::pair<String, double> testValues[] =
        {
            // Integers
            STRING_DOUBLE_PAIR_COMBOS (0),
            STRING_DOUBLE_PAIR_COMBOS (3),
            STRING_DOUBLE_PAIR_COMBOS (4931),
            STRING_DOUBLE_PAIR_COMBOS (5000),
            STRING_DOUBLE_PAIR_COMBOS (9862097),

            // Floating point numbers
            STRING_DOUBLE_PAIR_COMBOS (7.000),
            STRING_DOUBLE_PAIR_COMBOS (0.2),
            STRING_DOUBLE_PAIR_COMBOS (.298630),
            STRING_DOUBLE_PAIR_COMBOS (1.118),
            STRING_DOUBLE_PAIR_COMBOS (0.9000),
            STRING_DOUBLE_PAIR_COMBOS (0.0000001),
            STRING_DOUBLE_PAIR_COMBOS (500.0000001),
            STRING_DOUBLE_PAIR_COMBOS (9862098.2398604),

            // Exponents
            STRING_DOUBLE_PAIR_COMBOS (0e0),
            STRING_DOUBLE_PAIR_COMBOS (0.e0),
            STRING_DOUBLE_PAIR_COMBOS (0.00000e0),
            STRING_DOUBLE_PAIR_COMBOS (.0e7),
            STRING_DOUBLE_PAIR_COMBOS (0e-5),
            STRING_DOUBLE_PAIR_COMBOS (2E0),
            STRING_DOUBLE_PAIR_COMBOS (4.E0),
            STRING_DOUBLE_PAIR_COMBOS (1.2000000E0),
            STRING_DOUBLE_PAIR_COMBOS (1.2000000E6),
            STRING_DOUBLE_PAIR_COMBOS (.398e3),
            STRING_DOUBLE_PAIR_COMBOS (10e10),
            STRING_DOUBLE_PAIR_COMBOS (1.4962e+2),
            STRING_DOUBLE_PAIR_COMBOS (3198693.0973e4),
            STRING_DOUBLE_PAIR_COMBOS (10973097.2087E-4),
            STRING_DOUBLE_PAIR_COMBOS (1.3986e00006),
            STRING_DOUBLE_PAIR_COMBOS (2087.3087e+00006),
            STRING_DOUBLE_PAIR_COMBOS (6.0872e-00006),

            // Too many sig figs. The parsing routine on MinGW gets the last
            // significant figure wrong.
            STRING_DOUBLE_PAIR_COMBOS (17654321098765432.9),
            STRING_DOUBLE_PAIR_COMBOS (183456789012345678.9),
            STRING_DOUBLE_PAIR_COMBOS (1934567890123456789.9),
            STRING_DOUBLE_PAIR_COMBOS (20345678901234567891.9),
            STRING_DOUBLE_PAIR_COMBOS (10000000000000000303786028427003666890752.000000),
            STRING_DOUBLE_PAIR_COMBOS (10000000000000000303786028427003666890752e3),
            STRING_DOUBLE_PAIR_COMBOS (10000000000000000303786028427003666890752e100),
            STRING_DOUBLE_PAIR_COMBOS (10000000000000000303786028427003666890752.000000e-5),
            STRING_DOUBLE_PAIR_COMBOS (10000000000000000303786028427003666890752.000000e-40),

            STRING_DOUBLE_PAIR_COMBOS (1.23456789012345678901234567890),
            STRING_DOUBLE_PAIR_COMBOS (1.23456789012345678901234567890e-111)

            // Limits. DBL_MAX may not exist on Linux.
           #if ! JUCE_LINUX
          , STRING_DOUBLE_PAIR (DBL_MAX),
            STRING_DOUBLE_PAIR (-DBL_MAX),
            STRING_DOUBLE_PAIR (DBL_MIN)
           #endif
        };

        for (auto trial : testValues)
        {
            auto charPtr = trial.first.getCharPointer();
            expectEquals (CharacterFunctions::readDoubleValue (charPtr), trial.second);
        }

        {
            String nans[] = { "NaN", "-nan", "+NAN", "1.0E1024", "-1.0E-999", "1.23456789012345678901234567890e123456789"};

            for (auto nan : nans)
            {
                auto charPtr = nan.getCharPointer();
                expect (std::isnan (CharacterFunctions::readDoubleValue (charPtr)));
            }
        }

        {
            String infs[] = { "Inf", "-inf",  "INF"};

            for (auto inf : infs)
            {
                auto charPtr = inf.getCharPointer();
                expect (std::isinf (CharacterFunctions::readDoubleValue (charPtr)));
            }
        }
    }
};

static CharacterFunctionsTests characterFunctionsTests;

#endif

} // namespace juce
