/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

namespace
{
    //==============================================================================
    template <typename CharPointerType>
    class OSCPatternMatcherImpl
    {
        typedef CharPointerType CharPtr;

    public:
        //==============================================================================
        static bool match (CharPtr pattern, CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            if (pattern == patternEnd)
                return matchTerminator (target, targetEnd);

            auto c = pattern.getAndAdvance();

            switch (c)
            {
                case '?':   return matchAnyChar (pattern, patternEnd, target, targetEnd);
                case '*':   return matchAnyOrNoChars (pattern, patternEnd, target, targetEnd);
                case '{':   return matchInsideStringSet (pattern, patternEnd, target, targetEnd);
                case '[':   return matchInsideCharSet (pattern, patternEnd, target, targetEnd);
                default:    return matchChar (c, pattern, patternEnd, target, targetEnd);
            }
        }

    private:
        //==============================================================================
        static bool matchTerminator (CharPtr target, CharPtr targetEnd)
        {
            return target == targetEnd;
        }

        //==============================================================================
        static bool matchChar (juce_wchar c, CharPtr pattern, CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            if (target == targetEnd || c != target.getAndAdvance())
                return false;

            return match (pattern, patternEnd, target, targetEnd);
        }

        //==============================================================================
        static bool matchAnyChar (CharPtr pattern, CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            if (target == targetEnd)
                return false;

            return match (pattern, patternEnd, ++target, targetEnd);
        }

        //==============================================================================
        static bool matchAnyOrNoChars (CharPtr pattern, CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            if (target == targetEnd)
                return pattern == patternEnd;

            if (match (pattern, patternEnd, target, targetEnd))
                return true;

            return matchAnyOrNoChars (pattern, patternEnd, ++target, targetEnd);
        }

        //==============================================================================
        static bool matchInsideStringSet (CharPtr pattern, CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            if (pattern == patternEnd)
                return false;

            // Note: in case this code is ever moved into the more generic CharPointerFunctions,
            // the next two lines probably will not compile as soon as this class is used with a
            // Char template type parameter that is not the same type as String::Char.
            StringArray set;
            String currentElement;

            while (pattern != patternEnd)
            {
                auto c = pattern.getAndAdvance();

                switch (c)
                {
                    case '}':
                        set.add (currentElement);
                        currentElement.clear();
                        return matchStringSet (set, pattern, patternEnd, target, targetEnd);

                    case ',':
                        set.add (currentElement);
                        currentElement.clear();
                        continue;

                    default:
                        currentElement += c;
                        continue;
                }
            }

            return false;
        }

        //==============================================================================
        static bool matchStringSet (const StringArray& set, CharPtr pattern,
                                    CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            if (set.size() == 0)
                return match (pattern, patternEnd, target, targetEnd);

            for (auto& str : set)
                if (str.getCharPointer().compareUpTo (target, str.length()) == 0)
                    if (match (pattern, patternEnd, target + str.length(), targetEnd))
                        return true;

            return false;
        }

        //==============================================================================
        static bool matchInsideCharSet (CharPtr pattern, CharPtr patternEnd,
                                        CharPtr target, CharPtr targetEnd)
        {
            if (pattern == patternEnd)
                return false;

            Array<juce_wchar> set;
            bool setIsNegated = false;

            while (pattern != patternEnd)
            {
                auto c = pattern.getAndAdvance();

                switch (c)
                {
                    case ']':
                        return matchCharSet (set, setIsNegated, pattern, patternEnd, target, targetEnd);

                    case '-':
                        if (! addCharRangeToSet (set, pattern, patternEnd, target, targetEnd))
                            return false;

                        break;

                    case '!':
                        if (set.size() == 0 && setIsNegated == false)
                        {
                            setIsNegated = true;
                            break;
                        }
                        // else = special case: fall through to default and treat '!' as a non-special character.

                    default:
                        set.add (c);
                        break;
                }
            }

            return false;
        }

        //==============================================================================
        static bool matchCharSet (const Array<juce_wchar>& set, bool setIsNegated,
                                  CharPtr pattern, CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            if (set.size() == 0)
                return match (pattern, patternEnd, target, targetEnd);

            if (target == targetEnd)
                return false;

            return setIsNegated ? matchCharSetNegated (set, pattern, patternEnd, target, targetEnd)
                                : matchCharSetNotNegated (set, pattern, patternEnd, target, targetEnd);
        }

        //==============================================================================
        static bool matchCharSetNegated (const Array<juce_wchar>& set, CharPtr pattern,
                                         CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            for (auto c : set)
                if (*target == c)
                    return false;

            return match (pattern, patternEnd, target + 1, targetEnd);
        }

        //==============================================================================
        static bool matchCharSetNotNegated (const Array<juce_wchar>& set, CharPtr pattern,
                                            CharPtr patternEnd, CharPtr target, CharPtr targetEnd)
        {
            for (auto c : set)
                if (*target == c)
                    if (match (pattern, patternEnd, target + 1, targetEnd))
                        return true;

            return false;
        }

        //==============================================================================
        static bool addCharRangeToSet (Array<juce_wchar>& set, CharPtr pattern,
                                       CharPtr /*patternEnd*/, CharPtr target, CharPtr targetEnd)
        {
            if (target == targetEnd)
                return false;

            auto rangeStart = set.getLast();
            auto rangeEnd = pattern.getAndAdvance();

            if (rangeEnd == ']')
            {
                set.add ('-');  // special case: '-' has no special meaning at the end.
                return true;
            }

            if (rangeEnd == ',' || rangeEnd == '{' || rangeEnd == '}' || set.size() == 0)
                return false;

            while (rangeEnd > rangeStart)
                set.add (++rangeStart);

            return true;
        }
    };

    //==============================================================================
    static bool matchOscPattern (const String& pattern, const String& target)
    {
        return OSCPatternMatcherImpl<String::CharPointerType>::match (pattern.getCharPointer(),
                                                                      pattern.getCharPointer().findTerminatingNull(),
                                                                      target.getCharPointer(),
                                                                      target.getCharPointer().findTerminatingNull());
    }

    //==============================================================================
    template <typename OSCAddressType> struct OSCAddressTokeniserTraits;
    template <> struct OSCAddressTokeniserTraits<OSCAddress>        { static const char* getDisallowedChars() { return " #*,?/[]{}"; } };
    template <> struct OSCAddressTokeniserTraits<OSCAddressPattern> { static const char* getDisallowedChars() { return " #/"; } };

    //==============================================================================
    template <typename OSCAddressType>
    struct OSCAddressTokeniser
    {
        typedef OSCAddressTokeniserTraits<OSCAddressType> Traits;

        //==============================================================================
        static bool isPrintableASCIIChar (juce_wchar c) noexcept
        {
            return c >= ' ' && c <= '~';
        }

        static bool isDisallowedChar (juce_wchar c) noexcept
        {
            return CharPointer_ASCII (Traits::getDisallowedChars()).indexOf (c, false) >= 0;
        }

        static bool containsOnlyAllowedPrintableASCIIChars (const String& string) noexcept
        {
            for (auto charPtr = string.getCharPointer(); ! charPtr.isEmpty();)
            {
                auto c = charPtr.getAndAdvance();

                if (! isPrintableASCIIChar (c) || isDisallowedChar (c))
                    return false;
            }

            return true;
        }

        //==============================================================================
        static StringArray tokenise (const String& address)
        {
            if (address.isEmpty())
                throw OSCFormatError ("OSC format error: address string cannot be empty.");

            if (! address.startsWithChar ('/'))
                throw OSCFormatError ("OSC format error: address string must start with a forward slash.");

            StringArray oscSymbols;
            oscSymbols.addTokens (address, "/", StringRef());
            oscSymbols.removeEmptyStrings (false);

            for (auto& token : oscSymbols)
                if (! containsOnlyAllowedPrintableASCIIChars (token))
                    throw OSCFormatError ("OSC format error: encountered characters not allowed in address string.");

            return oscSymbols;
        }
    };

}  // namespace

//==============================================================================
OSCAddress::OSCAddress (const String& address)
    : oscSymbols (OSCAddressTokeniser<OSCAddress>::tokenise (address)),
      asString (address.trimCharactersAtEnd ("/"))
{
}

OSCAddress::OSCAddress (const char* address)
    : oscSymbols (OSCAddressTokeniser<OSCAddress>::tokenise (String (address))),
      asString (String (address).trimCharactersAtEnd ("/"))
{
}

//==============================================================================
bool OSCAddress::operator== (const OSCAddress& other) const noexcept
{
    return asString == other.asString;
}

bool OSCAddress::operator!= (const OSCAddress& other) const noexcept
{
    return ! operator== (other);
}

//==============================================================================
String OSCAddress::toString() const noexcept
{
    return asString;
}

//==============================================================================
OSCAddressPattern::OSCAddressPattern (const String& address)
    : oscSymbols (OSCAddressTokeniser<OSCAddressPattern>::tokenise (address)),
      asString (address.trimCharactersAtEnd ("/")),
      wasInitialisedWithWildcards (asString.containsAnyOf ("*?{}[]"))

{
}

OSCAddressPattern::OSCAddressPattern (const char* address)
    : oscSymbols (OSCAddressTokeniser<OSCAddressPattern>::tokenise (String (address))),
      asString (String (address).trimCharactersAtEnd ("/")),
      wasInitialisedWithWildcards (asString.containsAnyOf ("*?{}[]"))
{
}

//==============================================================================
bool OSCAddressPattern::operator== (const OSCAddressPattern& other) const noexcept
{
    return asString == other.asString;
}

bool OSCAddressPattern::operator!= (const OSCAddressPattern& other) const noexcept
{
    return ! operator== (other);
}

//==============================================================================
bool OSCAddressPattern::matches (const OSCAddress& address) const noexcept
{
    if (! containsWildcards())
        return asString == address.asString;

    if (oscSymbols.size() != address.oscSymbols.size())
        return false;

    for (int i = 0; i < oscSymbols.size(); ++i)
        if (! matchOscPattern (oscSymbols[i], address.oscSymbols[i]))
            return false;

    return true;
}

//==============================================================================
String OSCAddressPattern::toString() const noexcept
{
    return asString;
}

//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class OSCAddressTests : public UnitTest
{
public:
    OSCAddressTests() : UnitTest ("OSCAddress class", "OSC") {}

    void runTest()
    {
        beginTest ("construction and parsing");
        {
            expectThrowsType (OSCAddress (""), OSCFormatError);
            expectThrowsType (OSCAddress ("noleadingslash"), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar "), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar#"), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar*"), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar,"), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar?"), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar["), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar]"), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar{"), OSCFormatError);
            expectThrowsType (OSCAddress ("/notallowedchar}andsomemorechars"), OSCFormatError);
            expectThrowsType (OSCAddress (String::fromUTF8 ("/nonasciicharacter\xc3\xbc""blabla")), OSCFormatError);
            expectThrowsType (OSCAddress ("/nonprintableasciicharacter\t"), OSCFormatError);

            expectDoesNotThrow (OSCAddress ("/"));
            expectDoesNotThrow (OSCAddress ("/a"));
            expectDoesNotThrow (OSCAddress ("/a/"));
            expectDoesNotThrow (OSCAddress ("/a/bcd/"));
            expectDoesNotThrow (OSCAddress ("/abcd/efgh/ijkLMNOPq/666r/s"));
            expectDoesNotThrow (OSCAddress ("/allowedprintablecharacters!$%&()+-.^_`|~"));
            expectDoesNotThrow (OSCAddress ("/additonalslashes//will///be////ignored"));
        }

        beginTest ("conversion to/from String");
        {
            OSCAddress address ("/this/is/a/very/long/address/");
            expectEquals (address.toString(), String ("/this/is/a/very/long/address"));
        }
    }
};

static OSCAddressTests OSCAddressUnitTests;

//==============================================================================

class OSCAddressPatternTests  : public UnitTest
{
public:
    OSCAddressPatternTests() : UnitTest ("OSCAddressPattern class", "OSC") {}

    void runTest()
    {
        beginTest ("construction and parsing");
        {
            expectThrowsType (OSCAddressPattern (""), OSCFormatError);
            expectThrowsType (OSCAddressPattern ("noleadingslash"), OSCFormatError);
            expectThrowsType (OSCAddressPattern ("/notallowedchar "), OSCFormatError);
            expectThrowsType (OSCAddressPattern ("/notallowedchar#andsomemorechars"), OSCFormatError);
            expectThrowsType (OSCAddressPattern (String::fromUTF8 ("/nonasciicharacter\xc3\xbc""blabla")), OSCFormatError);
            expectThrowsType (OSCAddressPattern ("/nonprintableasciicharacter\t"), OSCFormatError);

            expectDoesNotThrow (OSCAddressPattern ("/"));
            expectDoesNotThrow (OSCAddressPattern ("/a"));
            expectDoesNotThrow (OSCAddressPattern ("/a/"));
            expectDoesNotThrow (OSCAddressPattern ("/a/bcd/"));
            expectDoesNotThrow (OSCAddressPattern ("/abcd/efgh/ijkLMNOPq/666r/s"));
            expectDoesNotThrow (OSCAddressPattern ("/allowedprintablecharacters!$%&()+-.:;<=>@^_`|~"));
            expectDoesNotThrow (OSCAddressPattern ("/additonalslashes//will///be////ignored"));
        }

        beginTest ("construction and parsing - with wildcards");
        {
            expectDoesNotThrow (OSCAddressPattern ("/foo/b?r/"));
            expectDoesNotThrow (OSCAddressPattern ("/?????"));
            expectDoesNotThrow (OSCAddressPattern ("/foo/b*r"));
            expectDoesNotThrow (OSCAddressPattern ("/**"));
            expectDoesNotThrow (OSCAddressPattern ("/?/b/*c"));
        }

        beginTest ("construction and parsing - with match expressions");
        {
            expectDoesNotThrow (OSCAddressPattern ("/{}"));
            expectDoesNotThrow (OSCAddressPattern ("/{foo}"));
            expectDoesNotThrow (OSCAddressPattern ("/{foo,bar,baz}"));
            expectDoesNotThrow (OSCAddressPattern ("/[]"));
            expectDoesNotThrow (OSCAddressPattern ("/[abcde]"));
            expectDoesNotThrow (OSCAddressPattern ("/[a-e]"));
            expectDoesNotThrow (OSCAddressPattern ("/foo/[a-z]x{foo,bar}/*BAZ42/"));

            /* Note: if malformed expressions are used, e.g. "bracenotclosed{" or "{a-e}" or "[-foo]",
               this should not throw at construction time. Instead it should simply fail any pattern match later.
               So there is no need to test for those.
               The reason is that we do not actually parse the expressions now, but only during matching.
            */
        }

        beginTest ("equality comparison");
        {
            {
                OSCAddressPattern lhs ("/test/1");
                OSCAddressPattern rhs ("/test/1");
                expect (lhs == rhs);
                expect (! (lhs != rhs));
            }
            {
                OSCAddressPattern lhs ("/test/1");
                OSCAddressPattern rhs ("/test/1/");
                expect (lhs == rhs);
                expect (! (lhs != rhs));
            }
            {
                OSCAddressPattern lhs ("/test/1");
                OSCAddressPattern rhs ("/test/2");
                expect (! (lhs == rhs));
                expect (lhs != rhs);
            }
        }

        beginTest ("basic string matching");
        {
            /* Note: the actual expression matching is tested in OSCPatternMatcher, so here we just
               do some basic tests and check if the matching works with multi-part addresses.
             */
            {
                OSCAddressPattern pattern ("/foo/bar");
                expect (! pattern.containsWildcards());

                OSCAddress address ("/foo/bar");
                expect (pattern.matches (address));
            }
            {
                OSCAddressPattern pattern ("/foo/bar/");
                expect (! pattern.containsWildcards());

                OSCAddress address ("/foo/bar");
                expect (pattern.matches (address));
            }
            {
                OSCAddressPattern pattern ("/");
                expect (! pattern.containsWildcards());

                OSCAddress address ("/");
                expect (pattern.matches (address));
            }
            {
                OSCAddressPattern pattern ("/foo/bar");
                expect (! pattern.containsWildcards());

                expect (! pattern.matches (OSCAddress ("/foo/baz")));
                expect (! pattern.matches (OSCAddress ("/foo/bar/baz")));
                expect (! pattern.matches (OSCAddress ("/foo")));
            }
        }

        beginTest ("string matching with wildcards");
        {
            OSCAddressPattern pattern ("/*/*put/slider[0-9]");
            expect (pattern.containsWildcards());

            expect (pattern.matches (OSCAddress ("/mypatch/input/slider0")));
            expect (pattern.matches (OSCAddress ("/myotherpatch/output/slider9")));
            expect (! pattern.matches (OSCAddress ("/myotherpatch/output/slider10")));
            expect (! pattern.matches (OSCAddress ("/output/slider9")));
            expect (! pattern.matches (OSCAddress ("/myotherpatch/output/slider9/position")));
        }

        beginTest ("conversion to/from String");
        {
            {
                OSCAddressPattern ap ("/this/is/a/very/long/address/");
                expectEquals (ap.toString(), String ("/this/is/a/very/long/address"));
            }
            {
                OSCAddressPattern ap ("/*/*put/{fader,slider,knob}[0-9]/ba?/");
                expectEquals (ap.toString(), String ("/*/*put/{fader,slider,knob}[0-9]/ba?"));
            }
        }
    }
};

static OSCAddressPatternTests OSCAddressPatternUnitTests;

//==============================================================================

class OSCPatternMatcherTests : public UnitTest
{
public:
    OSCPatternMatcherTests() : UnitTest ("OSCAddress class / pattern matching", "OSC") {}

    void runTest()
    {
        beginTest ("basic string matching");
        {
            expect (matchOscPattern ("", ""));
            expect (! matchOscPattern ("", "x"));
            expect (! matchOscPattern ("x", ""));
            expect (matchOscPattern ("foo", "foo"));
            expect (! matchOscPattern ("foo", "bar"));
            expect (! matchOscPattern ("ooooo", "oooooo"));
        }

        beginTest ("string matching with '?' wildcard");
        {
            expect (matchOscPattern ("?", "x"));
            expect (! matchOscPattern ("?", ""));
            expect (! matchOscPattern ("?", "xx"));
            expect (! matchOscPattern ("b?r", "br"));
            expect (matchOscPattern ("b?r", "bar"));
            expect (! matchOscPattern ("b?r", "baar"));
            expect (matchOscPattern ("f???o", "fabco"));
            expect (! matchOscPattern ("f???o", "fabo"));
        }

        beginTest ("string matching with '*' wildcard");
        {
            expect (matchOscPattern ("*", ""));
            expect (matchOscPattern ("*", "x"));
            expect (matchOscPattern ("*", "foo"));
            expect (matchOscPattern ("*c", "aaaaaaabc"));
            expect (matchOscPattern ("*c", "aaaaaaabbbcccc"));
            expect (! matchOscPattern ("*c", "aaaaaaabbbcccca"));
            expect (matchOscPattern ("c*", "ccccbbbbaaaa"));
            expect (! matchOscPattern ("c*", "accccbbbaaaa"));

            expect (matchOscPattern ("f*o", "fo"));
            expect (matchOscPattern ("f*o", "fuo"));
            expect (matchOscPattern ("f*o", "fuvwxyzo"));

            expect (matchOscPattern ("*reallyreallylongstringstringstring", "reallyreallylongstringstringstrNOT"
                                                                            "reallyreallylongstringstringstrNOT"
                                                                            "reallyreallylongstringstringstrNOT"
                                                                            "reallyreallylongstringstringstrNOT"
                                                                            "reallyreallylongstringstringstrNOT"
                                                                            "reallyreallylongstringstringstring"));
        }

        beginTest ("string matching with '{..., ...}' pattern");
        {
            expect (matchOscPattern ("{}", ""));
            expect (! matchOscPattern ("{}", "x"));
            expect (matchOscPattern ("{abcde}", "abcde"));
            expect (matchOscPattern ("{abcde,f}", "f"));
            expect (! matchOscPattern ("{abcde,f}", "ff"));
            expect (matchOscPattern ("a{bcd}e", "abcde"));
            expect (matchOscPattern ("a{bcd,bce}e", "abcde"));
            expect (! matchOscPattern ("a{bce,bcf}e", "abcde"));
            expect (! matchOscPattern ("a{bce,bcf}e", "ae"));
            expect (matchOscPattern ("a{bce,,bcf}e", "ae"));
            expect (matchOscPattern ("a{bcd,bcd,bcd}e", "abcde"));
            expect (matchOscPattern ("aaa{bc,def,ghij,klmnopqrstu}eee", "aaaghijeee"));
            expect (matchOscPattern ("{a,b,c}bcde", "abcde"));
            expect (! matchOscPattern ("{abc}bcde", "abcde"));
            expect (matchOscPattern ("bcde{a,b,c}", "bcdea"));
            expect (! matchOscPattern ("bcde{abc}", "bcdea"));
            expect (matchOscPattern ("f{o,}o", "fo"));
            expect (matchOscPattern ("f{,,,,,}o", "fo"));
            expect (matchOscPattern ("foo{b,ba,bar}x", "foobarx"));
            expect (matchOscPattern ("a{bc,de}fg{hij,klm}{n}{}", "adefghijn"));

            // should fail gracefully in case of wrong syntax:
            expect (! matchOscPattern ("not{closing", "notclosing"));
            expect (! matchOscPattern ("not}opening", "notopening"));
            expect (! matchOscPattern ("{{nested}}", "nested"));
            expect (! matchOscPattern ("{a-c}bcde", "abcde"));
            expect (! matchOscPattern ("bcde{a-c}", "abcde"));
        }


        beginTest ("string matching with '[...]' pattern");
        {
            // using [] for a set of chars:

            expect (matchOscPattern ("[]", ""));
            expect (! matchOscPattern ("[]", "x"));
            expect (! matchOscPattern ("[abcde]", "abcde"));
            expect (matchOscPattern ("[abcde]", "a"));
            expect (matchOscPattern ("[abcde]", "b"));
            expect (matchOscPattern ("[abcde]", "c"));
            expect (matchOscPattern ("[abcde]", "d"));
            expect (matchOscPattern ("[abcde]", "e"));
            expect (! matchOscPattern ("[abcde]", "f"));

            expect (matchOscPattern ("f[oo]", "fo"));
            expect (! matchOscPattern ("f[oo]", "foo"));

            expect (matchOscPattern ("fooba[rxz]foo", "foobarfoo"));
            expect (matchOscPattern ("fooba[rxz]foo", "foobaxfoo"));
            expect (matchOscPattern ("fooba[rxz]foo", "foobazfoo"));
            expect (! matchOscPattern ("fooba[rxz]foo", "foobasfoo"));

            expect (matchOscPattern ("foo[abc]foo[defgh]foo[i]foo[]foo", "foobfoohfooifoofoo"));

            // using [] for a range of chars:

            expect (matchOscPattern ("fooba[r-z]foo", "foobarfoo"));
            expect (matchOscPattern ("fooba[r-z]foo", "foobaxfoo"));
            expect (matchOscPattern ("fooba[r-z]foo", "foobazfoo"));
            expect (matchOscPattern ("fooba[r-z]foo", "foobasfoo"));
            expect (! matchOscPattern ("fooba[r-z]foo", "foobaRfoo"));

            expect (! matchOscPattern ("foo[1-8]bar", "foo0bar"));
            expect (matchOscPattern ("foo[1-8]bar", "foo1bar"));
            expect (matchOscPattern ("foo[1-8]bar", "foo6bar"));
            expect (matchOscPattern ("foo[1-8]bar", "foo8bar"));
            expect (! matchOscPattern ("foo[1-8]bar", "foo9bar"));

            // special case: '-' does not have a special meaning if it is at the end of the set.

            expect (matchOscPattern ("foo[abc-]bar", "fooabar"));
            expect (matchOscPattern ("foo[abc-]bar", "foo-bar"));
            expect (matchOscPattern ("foo[-]bar", "foo-bar"));

            // mixing both set and range:

            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]b[a-b]r", "fooabar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]b[a-a]r", "foobbar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]b[aaaa-aaaa-aaaa]r", "foodbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooebar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foogbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooibar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foojbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fookbar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foolbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooobar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foopbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooubar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooybar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foozbar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foo0bar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foo1bar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foo5bar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foo8bar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "foo9bar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooCbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooDbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooEbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooFbar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooGbar"));
            expect (matchOscPattern ("foo[ae-iko-uz1-8D-FX]bar", "fooXbar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]ba[Rr]", "fooZbar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]ba[Rr]", "foobar"));
            expect (! matchOscPattern ("foo[ae-iko-uz1-8D-FX]ba[Rr]", "fooFXbar"));

            // using [!...] for a negated set or range of chars:

            expect (! matchOscPattern ("fooba[!rxz]foo", "foobarfoo"));
            expect (! matchOscPattern ("fooba[!rxz]foo", "foobaxfoo"));
            expect (! matchOscPattern ("fooba[!rxz]foo", "foobazfoo"));
            expect (matchOscPattern ("fooba[!rxz]foo", "foobasfoo"));

            expect (! matchOscPattern ("fooba[!r-z]foo", "foobarfoo"));
            expect (! matchOscPattern ("fooba[!r-z]foo", "foobaxfoo"));
            expect (! matchOscPattern ("fooba[!r-z]foo", "foobazfoo"));
            expect (! matchOscPattern ("fooba[!r-z]foo", "foobasfoo"));
            expect (matchOscPattern ("fooba[!r-z]foo", "foobaRfoo"));

            // special case: '!' does not have a special meaning if it is not the first char in the set.

            expect (matchOscPattern ("foo[ab!c]bar", "fooabar"));
            expect (matchOscPattern ("foo[ab!c]bar", "foo!bar"));
            expect (! matchOscPattern ("foo[ab!c]bar", "fooxbar"));
            expect (! matchOscPattern ("foo[!!]bar", "foo!bar"));
            expect (matchOscPattern ("foo[!!]bar", "fooxbar"));
            expect (! matchOscPattern ("foo[!!]bar", "foobar"));

            // should fail gracefully in case of wrong syntax:

            expect (! matchOscPattern ("notclosin[g", "notclosing"));
            expect (! matchOscPattern ("n]otopening", "notopening"));
            expect (! matchOscPattern ("[[nested]]", "nested"));
            expect (! matchOscPattern ("norangestar[-t]", "norangestart"));
            expect (! matchOscPattern ("norangestar[-t]", "norangestar-"));
        }

        beginTest ("string matching combining patterns");
        {
            expect (matchOscPattern ("*ea*ll[y-z0-9X-Zvwx]??m[o-q]l[e]x{fat,mat,pat}te{}r*?", "reallycomplexpattern"));
        }
    }
};

static OSCPatternMatcherTests OSCPatternMatcherUnitTests;

#endif // JUCE_UNIT_TESTS

} // namespace juce
