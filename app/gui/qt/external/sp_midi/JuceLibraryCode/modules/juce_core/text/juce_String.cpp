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

#if JUCE_MSVC
 #pragma warning (push)
 #pragma warning (disable: 4514 4996)
#endif

NewLine newLine;

#if defined (JUCE_STRINGS_ARE_UNICODE) && ! JUCE_STRINGS_ARE_UNICODE
 #error "JUCE_STRINGS_ARE_UNICODE is deprecated! All strings are now unicode by default."
#endif

#if JUCE_NATIVE_WCHAR_IS_UTF8
 using CharPointer_wchar_t = CharPointer_UTF8;
#elif JUCE_NATIVE_WCHAR_IS_UTF16
 using CharPointer_wchar_t = CharPointer_UTF16;
#else
 using CharPointer_wchar_t = CharPointer_UTF32;
#endif

static inline CharPointer_wchar_t castToCharPointer_wchar_t (const void* t) noexcept
{
    return CharPointer_wchar_t (static_cast<const CharPointer_wchar_t::CharType*> (t));
}

//==============================================================================
// (Mirrors the structure of StringHolder, but without the atomic member, so can be statically constructed)
struct EmptyString
{
    int refCount;
    size_t allocatedBytes;
    String::CharPointerType::CharType text;
};

static const EmptyString emptyString { 0x3fffffff, sizeof (String::CharPointerType::CharType), 0 };

//==============================================================================
class StringHolder
{
public:
    StringHolder() = delete;

    using CharPointerType  = String::CharPointerType;
    using CharType         = String::CharPointerType::CharType;

    //==============================================================================
    static CharPointerType createUninitialisedBytes (size_t numBytes)
    {
        numBytes = (numBytes + 3) & ~(size_t) 3;
        auto s = reinterpret_cast<StringHolder*> (new char [sizeof (StringHolder) - sizeof (CharType) + numBytes]);
        s->refCount.value = 0;
        s->allocatedNumBytes = numBytes;
        return CharPointerType (s->text);
    }

    template <class CharPointer>
    static CharPointerType createFromCharPointer (const CharPointer text)
    {
        if (text.getAddress() == nullptr || text.isEmpty())
            return CharPointerType (&(emptyString.text));

        auto bytesNeeded = sizeof (CharType) + CharPointerType::getBytesRequiredFor (text);
        auto dest = createUninitialisedBytes (bytesNeeded);
        CharPointerType (dest).writeAll (text);
        return dest;
    }

    template <class CharPointer>
    static CharPointerType createFromCharPointer (const CharPointer text, size_t maxChars)
    {
        if (text.getAddress() == nullptr || text.isEmpty() || maxChars == 0)
            return CharPointerType (&(emptyString.text));

        auto end = text;
        size_t numChars = 0;
        size_t bytesNeeded = sizeof (CharType);

        while (numChars < maxChars && ! end.isEmpty())
        {
            bytesNeeded += CharPointerType::getBytesRequiredFor (end.getAndAdvance());
            ++numChars;
        }

        auto dest = createUninitialisedBytes (bytesNeeded);
        CharPointerType (dest).writeWithCharLimit (text, (int) numChars + 1);
        return dest;
    }

    template <class CharPointer>
    static CharPointerType createFromCharPointer (const CharPointer start, const CharPointer end)
    {
        if (start.getAddress() == nullptr || start.isEmpty())
            return CharPointerType (&(emptyString.text));

        auto e = start;
        int numChars = 0;
        auto bytesNeeded = sizeof (CharType);

        while (e < end && ! e.isEmpty())
        {
            bytesNeeded += CharPointerType::getBytesRequiredFor (e.getAndAdvance());
            ++numChars;
        }

        auto dest = createUninitialisedBytes (bytesNeeded);
        CharPointerType (dest).writeWithCharLimit (start, numChars + 1);
        return dest;
    }

    static CharPointerType createFromCharPointer (const CharPointerType start, const CharPointerType end)
    {
        if (start.getAddress() == nullptr || start.isEmpty())
            return CharPointerType (&(emptyString.text));

        auto numBytes = (size_t) (reinterpret_cast<const char*> (end.getAddress())
                                   - reinterpret_cast<const char*> (start.getAddress()));
        auto dest = createUninitialisedBytes (numBytes + sizeof (CharType));
        memcpy (dest.getAddress(), start, numBytes);
        dest.getAddress()[numBytes / sizeof (CharType)] = 0;
        return dest;
    }

    static CharPointerType createFromFixedLength (const char* const src, const size_t numChars)
    {
        auto dest = createUninitialisedBytes (numChars * sizeof (CharType) + sizeof (CharType));
        CharPointerType (dest).writeWithCharLimit (CharPointer_UTF8 (src), (int) (numChars + 1));
        return dest;
    }

    //==============================================================================
    static void retain (const CharPointerType text) noexcept
    {
        auto* b = bufferFromText (text);

        if (! isEmptyString (b))
            ++(b->refCount);
    }

    static inline void release (StringHolder* const b) noexcept
    {
        if (! isEmptyString (b))
            if (--(b->refCount) == -1)
                delete[] reinterpret_cast<char*> (b);
    }

    static void release (const CharPointerType text) noexcept
    {
        release (bufferFromText (text));
    }

    static inline int getReferenceCount (const CharPointerType text) noexcept
    {
        return bufferFromText (text)->refCount.get() + 1;
    }

    //==============================================================================
    static CharPointerType makeUniqueWithByteSize (const CharPointerType text, size_t numBytes)
    {
        auto* b = bufferFromText (text);

        if (isEmptyString (b))
        {
            auto newText = createUninitialisedBytes (numBytes);
            newText.writeNull();
            return newText;
        }

        if (b->allocatedNumBytes >= numBytes && b->refCount.get() <= 0)
            return text;

        auto newText = createUninitialisedBytes (jmax (b->allocatedNumBytes, numBytes));
        memcpy (newText.getAddress(), text.getAddress(), b->allocatedNumBytes);
        release (b);

        return newText;
    }

    static size_t getAllocatedNumBytes (const CharPointerType text) noexcept
    {
        return bufferFromText (text)->allocatedNumBytes;
    }

    //==============================================================================
    Atomic<int> refCount;
    size_t allocatedNumBytes;
    CharType text[1];

private:
    static inline StringHolder* bufferFromText (const CharPointerType text) noexcept
    {
        // (Can't use offsetof() here because of warnings about this not being a POD)
        return reinterpret_cast<StringHolder*> (reinterpret_cast<char*> (text.getAddress())
                    - (reinterpret_cast<size_t> (reinterpret_cast<StringHolder*> (128)->text) - 128));
    }

    static inline bool isEmptyString (StringHolder* other)
    {
        return (other->refCount.get() & 0x30000000) != 0;
    }

    void compileTimeChecks()
    {
        // Let me know if any of these assertions fail on your system!
       #if JUCE_NATIVE_WCHAR_IS_UTF8
        static_assert (sizeof (wchar_t) == 1, "JUCE_NATIVE_WCHAR_IS_* macro has incorrect value");
       #elif JUCE_NATIVE_WCHAR_IS_UTF16
        static_assert (sizeof (wchar_t) == 2, "JUCE_NATIVE_WCHAR_IS_* macro has incorrect value");
       #elif JUCE_NATIVE_WCHAR_IS_UTF32
        static_assert (sizeof (wchar_t) == 4, "JUCE_NATIVE_WCHAR_IS_* macro has incorrect value");
       #else
        #error "native wchar_t size is unknown"
       #endif

        static_assert (sizeof (EmptyString) == sizeof (StringHolder),
                       "StringHolder is not large enough to hold an empty String");
    }
};

JUCE_DECLARE_DEPRECATED_STATIC (const String String::empty;)

//==============================================================================
String::String() noexcept  : text (&(emptyString.text))
{
}

String::~String() noexcept
{
    StringHolder::release (text);
}

String::String (const String& other) noexcept   : text (other.text)
{
    StringHolder::retain (text);
}

void String::swapWith (String& other) noexcept
{
    std::swap (text, other.text);
}

void String::clear() noexcept
{
    StringHolder::release (text);
    text = &(emptyString.text);
}

String& String::operator= (const String& other) noexcept
{
    StringHolder::retain (other.text);
    StringHolder::release (text.atomicSwap (other.text));
    return *this;
}

String::String (String&& other) noexcept   : text (other.text)
{
    other.text = &(emptyString.text);
}

String& String::operator= (String&& other) noexcept
{
    std::swap (text, other.text);
    return *this;
}

inline String::PreallocationBytes::PreallocationBytes (const size_t num) noexcept : numBytes (num) {}

String::String (const PreallocationBytes& preallocationSize)
    : text (StringHolder::createUninitialisedBytes (preallocationSize.numBytes + sizeof (CharPointerType::CharType)))
{
}

void String::preallocateBytes (const size_t numBytesNeeded)
{
    text = StringHolder::makeUniqueWithByteSize (text, numBytesNeeded + sizeof (CharPointerType::CharType));
}

int String::getReferenceCount() const noexcept
{
    return StringHolder::getReferenceCount (text);
}

//==============================================================================
String::String (const char* const t)
    : text (StringHolder::createFromCharPointer (CharPointer_ASCII (t)))
{
    /*  If you get an assertion here, then you're trying to create a string from 8-bit data
        that contains values greater than 127. These can NOT be correctly converted to unicode
        because there's no way for the String class to know what encoding was used to
        create them. The source data could be UTF-8, ASCII or one of many local code-pages.

        To get around this problem, you must be more explicit when you pass an ambiguous 8-bit
        string to the String class - so for example if your source data is actually UTF-8,
        you'd call String (CharPointer_UTF8 ("my utf8 string..")), and it would be able to
        correctly convert the multi-byte characters to unicode. It's *highly* recommended that
        you use UTF-8 with escape characters in your source code to represent extended characters,
        because there's no other way to represent these strings in a way that isn't dependent on
        the compiler, source code editor and platform.

        Note that the Projucer has a handy string literal generator utility that will convert
        any unicode string to a valid C++ string literal, creating ascii escape sequences that will
        work in any compiler.
    */
    jassert (t == nullptr || CharPointer_ASCII::isValidString (t, std::numeric_limits<int>::max()));
}

String::String (const char* const t, const size_t maxChars)
    : text (StringHolder::createFromCharPointer (CharPointer_ASCII (t), maxChars))
{
    /*  If you get an assertion here, then you're trying to create a string from 8-bit data
        that contains values greater than 127. These can NOT be correctly converted to unicode
        because there's no way for the String class to know what encoding was used to
        create them. The source data could be UTF-8, ASCII or one of many local code-pages.

        To get around this problem, you must be more explicit when you pass an ambiguous 8-bit
        string to the String class - so for example if your source data is actually UTF-8,
        you'd call String (CharPointer_UTF8 ("my utf8 string..")), and it would be able to
        correctly convert the multi-byte characters to unicode. It's *highly* recommended that
        you use UTF-8 with escape characters in your source code to represent extended characters,
        because there's no other way to represent these strings in a way that isn't dependent on
        the compiler, source code editor and platform.

        Note that the Projucer has a handy string literal generator utility that will convert
        any unicode string to a valid C++ string literal, creating ascii escape sequences that will
        work in any compiler.
    */
    jassert (t == nullptr || CharPointer_ASCII::isValidString (t, (int) maxChars));
}

String::String (const wchar_t* const t)      : text (StringHolder::createFromCharPointer (castToCharPointer_wchar_t (t))) {}
String::String (const CharPointer_UTF8  t)   : text (StringHolder::createFromCharPointer (t)) {}
String::String (const CharPointer_UTF16 t)   : text (StringHolder::createFromCharPointer (t)) {}
String::String (const CharPointer_UTF32 t)   : text (StringHolder::createFromCharPointer (t)) {}
String::String (const CharPointer_ASCII t)   : text (StringHolder::createFromCharPointer (t)) {}

String::String (CharPointer_UTF8  t, size_t maxChars)   : text (StringHolder::createFromCharPointer (t, maxChars)) {}
String::String (CharPointer_UTF16 t, size_t maxChars)   : text (StringHolder::createFromCharPointer (t, maxChars)) {}
String::String (CharPointer_UTF32 t, size_t maxChars)   : text (StringHolder::createFromCharPointer (t, maxChars)) {}
String::String (const wchar_t* t, size_t maxChars)      : text (StringHolder::createFromCharPointer (castToCharPointer_wchar_t (t), maxChars)) {}

String::String (CharPointer_UTF8  start, CharPointer_UTF8  end)  : text (StringHolder::createFromCharPointer (start, end)) {}
String::String (CharPointer_UTF16 start, CharPointer_UTF16 end)  : text (StringHolder::createFromCharPointer (start, end)) {}
String::String (CharPointer_UTF32 start, CharPointer_UTF32 end)  : text (StringHolder::createFromCharPointer (start, end)) {}

String::String (const std::string& s) : text (StringHolder::createFromFixedLength (s.data(), s.size())) {}
String::String (StringRef s)          : text (StringHolder::createFromCharPointer (s.text)) {}

String String::charToString (juce_wchar character)
{
    String result (PreallocationBytes (CharPointerType::getBytesRequiredFor (character)));
    CharPointerType t (result.text);
    t.write (character);
    t.writeNull();
    return result;
}

//==============================================================================
namespace NumberToStringConverters
{
    enum
    {
        charsNeededForInt = 32,
        charsNeededForDouble = 48
    };

    template <typename Type>
    static char* printDigits (char* t, Type v) noexcept
    {
        *--t = 0;

        do
        {
            *--t = '0' + (char) (v % 10);
            v /= 10;

        } while (v > 0);

        return t;
    }

    // pass in a pointer to the END of a buffer..
    static char* numberToString (char* t, int64 n) noexcept
    {
        if (n >= 0)
            return printDigits (t, static_cast<uint64> (n));

        // NB: this needs to be careful not to call -std::numeric_limits<int64>::min(),
        // which has undefined behaviour
        t = printDigits (t, static_cast<uint64> (-(n + 1)) + 1);
        *--t = '-';
        return t;
    }

    static char* numberToString (char* t, uint64 v) noexcept
    {
        return printDigits (t, v);
    }

    static char* numberToString (char* t, int n) noexcept
    {
        if (n >= 0)
            return printDigits (t, static_cast<unsigned int> (n));

        // NB: this needs to be careful not to call -std::numeric_limits<int>::min(),
        // which has undefined behaviour
        t = printDigits (t, static_cast<unsigned int> (-(n + 1)) + 1);
        *--t = '-';
        return t;
    }

    static char* numberToString (char* t, unsigned int v) noexcept
    {
        return printDigits (t, v);
    }

    static char* numberToString (char* t, long n) noexcept
    {
        if (n >= 0)
            return printDigits (t, static_cast<unsigned long> (n));

        t = printDigits (t, static_cast<unsigned long> (-(n + 1)) + 1);
        *--t = '-';
        return t;
    }

    static char* numberToString (char* t, unsigned long v) noexcept
    {
        return printDigits (t, v);
    }

    struct StackArrayStream  : public std::basic_streambuf<char, std::char_traits<char>>
    {
        explicit StackArrayStream (char* d)
        {
            static const std::locale classicLocale (std::locale::classic());
            imbue (classicLocale);
            setp (d, d + charsNeededForDouble);
        }

        size_t writeDouble (double n, int numDecPlaces, bool useScientificNotation)
        {
            {
                std::ostream o (this);

                if (numDecPlaces > 0)
                {
                    o.setf (useScientificNotation ? std::ios_base::scientific : std::ios_base::fixed);
                    o.precision ((std::streamsize) numDecPlaces);
                }

                o << n;
            }

            return (size_t) (pptr() - pbase());
        }
    };

    static char* doubleToString (char* buffer, double n, int numDecPlaces, bool useScientificNotation, size_t& len) noexcept
    {
        StackArrayStream strm (buffer);
        len = strm.writeDouble (n, numDecPlaces, useScientificNotation);
        jassert (len <= charsNeededForDouble);
        return buffer;
    }

    template <typename IntegerType>
    static String::CharPointerType createFromInteger (IntegerType number)
    {
        char buffer [charsNeededForInt];
        auto* end = buffer + numElementsInArray (buffer);
        auto* start = numberToString (end, number);
        return StringHolder::createFromFixedLength (start, (size_t) (end - start - 1));
    }

    static String::CharPointerType createFromDouble (double number, int numberOfDecimalPlaces, bool useScientificNotation)
    {
        char buffer [charsNeededForDouble];
        size_t len;
        auto start = doubleToString (buffer, number, numberOfDecimalPlaces, useScientificNotation, len);
        return StringHolder::createFromFixedLength (start, len);
    }
}

//==============================================================================
String::String (int number)            : text (NumberToStringConverters::createFromInteger (number)) {}
String::String (unsigned int number)   : text (NumberToStringConverters::createFromInteger (number)) {}
String::String (short number)          : text (NumberToStringConverters::createFromInteger ((int) number)) {}
String::String (unsigned short number) : text (NumberToStringConverters::createFromInteger ((unsigned int) number)) {}
String::String (int64  number)         : text (NumberToStringConverters::createFromInteger (number)) {}
String::String (uint64 number)         : text (NumberToStringConverters::createFromInteger (number)) {}
String::String (long number)           : text (NumberToStringConverters::createFromInteger (number)) {}
String::String (unsigned long number)  : text (NumberToStringConverters::createFromInteger (number)) {}

String::String (float  number)         : text (NumberToStringConverters::createFromDouble ((double) number, 0, false)) {}
String::String (double number)         : text (NumberToStringConverters::createFromDouble (         number, 0, false)) {}
String::String (float  number, int numberOfDecimalPlaces, bool useScientificNotation)  : text (NumberToStringConverters::createFromDouble ((double) number, numberOfDecimalPlaces, useScientificNotation)) {}
String::String (double number, int numberOfDecimalPlaces, bool useScientificNotation)  : text (NumberToStringConverters::createFromDouble (         number, numberOfDecimalPlaces, useScientificNotation)) {}

//==============================================================================
int String::length() const noexcept
{
    return (int) text.length();
}

static size_t findByteOffsetOfEnd (String::CharPointerType text) noexcept
{
    return (size_t) (((char*) text.findTerminatingNull().getAddress()) - (char*) text.getAddress());
}

size_t String::getByteOffsetOfEnd() const noexcept
{
    return findByteOffsetOfEnd (text);
}

juce_wchar String::operator[] (int index) const noexcept
{
    jassert (index == 0 || (index > 0 && index <= (int) text.lengthUpTo ((size_t) index + 1)));
    return text [index];
}

template <typename Type>
struct HashGenerator
{
    template <typename CharPointer>
    static Type calculate (CharPointer t) noexcept
    {
        Type result = {};

        while (! t.isEmpty())
            result = ((Type) multiplier) * result + (Type) t.getAndAdvance();

        return result;
    }

    enum { multiplier = sizeof (Type) > 4 ? 101 : 31 };
};

int String::hashCode() const noexcept       { return (int) HashGenerator<uint32>    ::calculate (text); }
int64 String::hashCode64() const noexcept   { return (int64) HashGenerator<uint64>  ::calculate (text); }
size_t String::hash() const noexcept        { return HashGenerator<size_t>          ::calculate (text); }

//==============================================================================
JUCE_API bool JUCE_CALLTYPE operator== (const String& s1, const String& s2) noexcept            { return s1.compare (s2) == 0; }
JUCE_API bool JUCE_CALLTYPE operator!= (const String& s1, const String& s2) noexcept            { return s1.compare (s2) != 0; }
JUCE_API bool JUCE_CALLTYPE operator== (const String& s1, const char* s2) noexcept              { return s1.compare (s2) == 0; }
JUCE_API bool JUCE_CALLTYPE operator!= (const String& s1, const char* s2) noexcept              { return s1.compare (s2) != 0; }
JUCE_API bool JUCE_CALLTYPE operator== (const String& s1, const wchar_t* s2) noexcept           { return s1.compare (s2) == 0; }
JUCE_API bool JUCE_CALLTYPE operator!= (const String& s1, const wchar_t* s2) noexcept           { return s1.compare (s2) != 0; }
JUCE_API bool JUCE_CALLTYPE operator== (const String& s1, StringRef s2) noexcept                { return s1.getCharPointer().compare (s2.text) == 0; }
JUCE_API bool JUCE_CALLTYPE operator!= (const String& s1, StringRef s2) noexcept                { return s1.getCharPointer().compare (s2.text) != 0; }
JUCE_API bool JUCE_CALLTYPE operator<  (const String& s1, StringRef s2) noexcept                { return s1.getCharPointer().compare (s2.text) < 0; }
JUCE_API bool JUCE_CALLTYPE operator<= (const String& s1, StringRef s2) noexcept                { return s1.getCharPointer().compare (s2.text) <= 0; }
JUCE_API bool JUCE_CALLTYPE operator>  (const String& s1, StringRef s2) noexcept                { return s1.getCharPointer().compare (s2.text) > 0; }
JUCE_API bool JUCE_CALLTYPE operator>= (const String& s1, StringRef s2) noexcept                { return s1.getCharPointer().compare (s2.text) >= 0; }
JUCE_API bool JUCE_CALLTYPE operator== (const String& s1, const CharPointer_UTF8 s2) noexcept   { return s1.getCharPointer().compare (s2) == 0; }
JUCE_API bool JUCE_CALLTYPE operator!= (const String& s1, const CharPointer_UTF8 s2) noexcept   { return s1.getCharPointer().compare (s2) != 0; }
JUCE_API bool JUCE_CALLTYPE operator== (const String& s1, const CharPointer_UTF16 s2) noexcept  { return s1.getCharPointer().compare (s2) == 0; }
JUCE_API bool JUCE_CALLTYPE operator!= (const String& s1, const CharPointer_UTF16 s2) noexcept  { return s1.getCharPointer().compare (s2) != 0; }
JUCE_API bool JUCE_CALLTYPE operator== (const String& s1, const CharPointer_UTF32 s2) noexcept  { return s1.getCharPointer().compare (s2) == 0; }
JUCE_API bool JUCE_CALLTYPE operator!= (const String& s1, const CharPointer_UTF32 s2) noexcept  { return s1.getCharPointer().compare (s2) != 0; }

bool String::equalsIgnoreCase (const wchar_t* const t) const noexcept
{
    return t != nullptr ? text.compareIgnoreCase (castToCharPointer_wchar_t (t)) == 0
                        : isEmpty();
}

bool String::equalsIgnoreCase (const char* const t) const noexcept
{
    return t != nullptr ? text.compareIgnoreCase (CharPointer_UTF8 (t)) == 0
                        : isEmpty();
}

bool String::equalsIgnoreCase (StringRef t) const noexcept
{
    return text.compareIgnoreCase (t.text) == 0;
}

bool String::equalsIgnoreCase (const String& other) const noexcept
{
    return text == other.text
            || text.compareIgnoreCase (other.text) == 0;
}

int String::compare (const String& other) const noexcept           { return (text == other.text) ? 0 : text.compare (other.text); }
int String::compare (const char* const other) const noexcept       { return text.compare (CharPointer_UTF8 (other)); }
int String::compare (const wchar_t* const other) const noexcept    { return text.compare (castToCharPointer_wchar_t (other)); }
int String::compareIgnoreCase (const String& other) const noexcept { return (text == other.text) ? 0 : text.compareIgnoreCase (other.text); }

static int stringCompareRight (String::CharPointerType s1, String::CharPointerType s2) noexcept
{
    for (int bias = 0;;)
    {
        auto c1 = s1.getAndAdvance();
        bool isDigit1 = CharacterFunctions::isDigit (c1);

        auto c2 = s2.getAndAdvance();
        bool isDigit2 = CharacterFunctions::isDigit (c2);

        if (! (isDigit1 || isDigit2))   return bias;
        if (! isDigit1)                 return -1;
        if (! isDigit2)                 return 1;

        if (c1 != c2 && bias == 0)
            bias = c1 < c2 ? -1 : 1;

        jassert (c1 != 0 && c2 != 0);
    }
}

static int stringCompareLeft (String::CharPointerType s1, String::CharPointerType s2) noexcept
{
    for (;;)
    {
        auto c1 = s1.getAndAdvance();
        bool isDigit1 = CharacterFunctions::isDigit (c1);

        auto c2 = s2.getAndAdvance();
        bool isDigit2 = CharacterFunctions::isDigit (c2);

        if (! (isDigit1 || isDigit2))   return 0;
        if (! isDigit1)                 return -1;
        if (! isDigit2)                 return 1;
        if (c1 < c2)                    return -1;
        if (c1 > c2)                    return 1;
    }
}

static int naturalStringCompare (String::CharPointerType s1, String::CharPointerType s2, bool isCaseSensitive) noexcept
{
    bool firstLoop = true;

    for (;;)
    {
        const bool hasSpace1 = s1.isWhitespace();
        const bool hasSpace2 = s2.isWhitespace();

        if ((! firstLoop) && (hasSpace1 ^ hasSpace2))
        {
            if (s1.isEmpty())  return -1;
            if (s2.isEmpty())  return 1;

            return hasSpace2 ? 1 : -1;
        }

        firstLoop = false;

        if (hasSpace1)  s1 = s1.findEndOfWhitespace();
        if (hasSpace2)  s2 = s2.findEndOfWhitespace();

        if (s1.isDigit() && s2.isDigit())
        {
            auto result = (*s1 == '0' || *s2 == '0') ? stringCompareLeft  (s1, s2)
                                                     : stringCompareRight (s1, s2);

            if (result != 0)
                return result;
        }

        auto c1 = s1.getAndAdvance();
        auto c2 = s2.getAndAdvance();

        if (c1 != c2 && ! isCaseSensitive)
        {
            c1 = CharacterFunctions::toUpperCase (c1);
            c2 = CharacterFunctions::toUpperCase (c2);
        }

        if (c1 == c2)
        {
            if (c1 == 0)
                return 0;
        }
        else
        {
            const bool isAlphaNum1 = CharacterFunctions::isLetterOrDigit (c1);
            const bool isAlphaNum2 = CharacterFunctions::isLetterOrDigit (c2);

            if (isAlphaNum2 && ! isAlphaNum1) return -1;
            if (isAlphaNum1 && ! isAlphaNum2) return 1;

            return c1 < c2 ? -1 : 1;
        }

        jassert (c1 != 0 && c2 != 0);
    }
}

int String::compareNatural (StringRef other, bool isCaseSensitive) const noexcept
{
    return naturalStringCompare (getCharPointer(), other.text, isCaseSensitive);
}

//==============================================================================
void String::append (const String& textToAppend, size_t maxCharsToTake)
{
    appendCharPointer (this == &textToAppend ? String (textToAppend).text
                                             : textToAppend.text, maxCharsToTake);
}

void String::appendCharPointer (const CharPointerType textToAppend)
{
    appendCharPointer (textToAppend, textToAppend.findTerminatingNull());
}

void String::appendCharPointer (const CharPointerType startOfTextToAppend,
                                const CharPointerType endOfTextToAppend)
{
    jassert (startOfTextToAppend.getAddress() != nullptr && endOfTextToAppend.getAddress() != nullptr);

    auto extraBytesNeeded = getAddressDifference (endOfTextToAppend.getAddress(),
                                                  startOfTextToAppend.getAddress());
    jassert (extraBytesNeeded >= 0);

    if (extraBytesNeeded > 0)
    {
        auto byteOffsetOfNull = getByteOffsetOfEnd();
        preallocateBytes ((size_t) extraBytesNeeded + byteOffsetOfNull);

        auto* newStringStart = addBytesToPointer (text.getAddress(), (int) byteOffsetOfNull);
        memcpy (newStringStart, startOfTextToAppend.getAddress(), (size_t) extraBytesNeeded);
        CharPointerType (addBytesToPointer (newStringStart, extraBytesNeeded)).writeNull();
    }
}

String& String::operator+= (const wchar_t* t)
{
    appendCharPointer (castToCharPointer_wchar_t (t));
    return *this;
}

String& String::operator+= (const char* t)
{
    appendCharPointer (CharPointer_UTF8 (t)); // (using UTF8 here triggers a faster code-path than ascii)
    return *this;
}

String& String::operator+= (const String& other)
{
    if (isEmpty())
        return operator= (other);

    if (this == &other)
        return operator+= (String (*this));

    appendCharPointer (other.text);
    return *this;
}

String& String::operator+= (StringRef other)
{
    return operator+= (String (other));
}

String& String::operator+= (char ch)
{
    const char asString[] = { ch, 0 };
    return operator+= (asString);
}

String& String::operator+= (wchar_t ch)
{
    const wchar_t asString[] = { ch, 0 };
    return operator+= (asString);
}

#if ! JUCE_NATIVE_WCHAR_IS_UTF32
String& String::operator+= (juce_wchar ch)
{
    const juce_wchar asString[] = { ch, 0 };
    appendCharPointer (CharPointer_UTF32 (asString));
    return *this;
}
#endif

namespace StringHelpers
{
    template <typename T>
    inline String& operationAddAssign (String& str, const T number)
    {
        char buffer [(sizeof(T) * 8) / 2];
        auto* end = buffer + numElementsInArray (buffer);
        auto* start = NumberToStringConverters::numberToString (end, number);

       #if JUCE_STRING_UTF_TYPE == 8
        str.appendCharPointer (String::CharPointerType (start), String::CharPointerType (end));
       #else
        str.appendCharPointer (CharPointer_ASCII (start), CharPointer_ASCII (end));
       #endif

        return str;
    }
}

String& String::operator+= (const int number)          { return StringHelpers::operationAddAssign<int>          (*this, number); }
String& String::operator+= (const int64 number)        { return StringHelpers::operationAddAssign<int64>        (*this, number); }
String& String::operator+= (const uint64 number)       { return StringHelpers::operationAddAssign<uint64>       (*this, number); }

//==============================================================================
JUCE_API String JUCE_CALLTYPE operator+ (const char* s1, const String& s2)    { String s (s1); return s += s2; }
JUCE_API String JUCE_CALLTYPE operator+ (const wchar_t* s1, const String& s2) { String s (s1); return s += s2; }

JUCE_API String JUCE_CALLTYPE operator+ (char s1, const String& s2)           { return String::charToString ((juce_wchar) (uint8) s1) + s2; }
JUCE_API String JUCE_CALLTYPE operator+ (wchar_t s1, const String& s2)        { return String::charToString (s1) + s2; }

JUCE_API String JUCE_CALLTYPE operator+ (String s1, const String& s2)         { return s1 += s2; }
JUCE_API String JUCE_CALLTYPE operator+ (String s1, const char* s2)           { return s1 += s2; }
JUCE_API String JUCE_CALLTYPE operator+ (String s1, const wchar_t* s2)        { return s1 += s2; }
JUCE_API String JUCE_CALLTYPE operator+ (String s1, const std::string& s2)    { return s1 += s2.c_str(); }

JUCE_API String JUCE_CALLTYPE operator+ (String s1, char s2)                  { return s1 += s2; }
JUCE_API String JUCE_CALLTYPE operator+ (String s1, wchar_t s2)               { return s1 += s2; }

#if ! JUCE_NATIVE_WCHAR_IS_UTF32
JUCE_API String JUCE_CALLTYPE operator+ (juce_wchar s1, const String& s2)     { return String::charToString (s1) + s2; }
JUCE_API String JUCE_CALLTYPE operator+ (String s1, juce_wchar s2)            { return s1 += s2; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, juce_wchar s2)         { return s1 += s2; }
#endif

JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, char s2)               { return s1 += s2; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, wchar_t s2)            { return s1 += s2; }

JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, const char* s2)        { return s1 += s2; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, const wchar_t* s2)     { return s1 += s2; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, const String& s2)      { return s1 += s2; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, StringRef s2)          { return s1 += s2; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, const std::string& s2) { return s1 += s2.c_str(); }

JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, uint8  number)         { return s1 += (int) number; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, short  number)         { return s1 += (int) number; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, int    number)         { return s1 += number; }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, long   number)         { return s1 += String (number); }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, unsigned long number)  { return s1 += String (number); }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, int64  number)         { return s1 += String (number); }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, uint64 number)         { return s1 += String (number); }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, float  number)         { return s1 += String (number); }
JUCE_API String& JUCE_CALLTYPE operator<< (String& s1, double number)         { return s1 += String (number); }

JUCE_API OutputStream& JUCE_CALLTYPE operator<< (OutputStream& stream, const String& text)
{
    return operator<< (stream, StringRef (text));
}

JUCE_API OutputStream& JUCE_CALLTYPE operator<< (OutputStream& stream, StringRef text)
{
    auto numBytes = CharPointer_UTF8::getBytesRequiredFor (text.text);

   #if (JUCE_STRING_UTF_TYPE == 8)
    stream.write (text.text.getAddress(), numBytes);
   #else
    // (This avoids using toUTF8() to prevent the memory bloat that it would leave behind
    // if lots of large, persistent strings were to be written to streams).
    HeapBlock<char> temp (numBytes + 1);
    CharPointer_UTF8 (temp).writeAll (text.text);
    stream.write (temp, numBytes);
   #endif

    return stream;
}

//==============================================================================
int String::indexOfChar (juce_wchar character) const noexcept
{
    return text.indexOf (character);
}

int String::indexOfChar (int startIndex, juce_wchar character) const noexcept
{
    auto t = text;

    for (int i = 0; ! t.isEmpty(); ++i)
    {
        if (i >= startIndex)
        {
            if (t.getAndAdvance() == character)
                return i;
        }
        else
        {
            ++t;
        }
    }

    return -1;
}

int String::lastIndexOfChar (juce_wchar character) const noexcept
{
    auto t = text;
    int last = -1;

    for (int i = 0; ! t.isEmpty(); ++i)
        if (t.getAndAdvance() == character)
            last = i;

    return last;
}

int String::indexOfAnyOf (StringRef charactersToLookFor, int startIndex, bool ignoreCase) const noexcept
{
    auto t = text;

    for (int i = 0; ! t.isEmpty(); ++i)
    {
        if (i >= startIndex)
        {
            if (charactersToLookFor.text.indexOf (t.getAndAdvance(), ignoreCase) >= 0)
                return i;
        }
        else
        {
            ++t;
        }
    }

    return -1;
}

int String::indexOf (StringRef other) const noexcept
{
    return other.isEmpty() ? 0 : text.indexOf (other.text);
}

int String::indexOfIgnoreCase (StringRef other) const noexcept
{
    return other.isEmpty() ? 0 : CharacterFunctions::indexOfIgnoreCase (text, other.text);
}

int String::indexOf (int startIndex, StringRef other) const noexcept
{
    if (other.isEmpty())
        return -1;

    auto t = text;

    for (int i = startIndex; --i >= 0;)
    {
        if (t.isEmpty())
            return -1;

        ++t;
    }

    auto found = t.indexOf (other.text);
    return found >= 0 ? found + startIndex : found;
}

int String::indexOfIgnoreCase (const int startIndex, StringRef other) const noexcept
{
    if (other.isEmpty())
        return -1;

    auto t = text;

    for (int i = startIndex; --i >= 0;)
    {
        if (t.isEmpty())
            return -1;

        ++t;
    }

    auto found = CharacterFunctions::indexOfIgnoreCase (t, other.text);
    return found >= 0 ? found + startIndex : found;
}

int String::lastIndexOf (StringRef other) const noexcept
{
    if (other.isNotEmpty())
    {
        auto len = other.length();
        int i = length() - len;

        if (i >= 0)
        {
            for (auto n = text + i; i >= 0; --i)
            {
                if (n.compareUpTo (other.text, len) == 0)
                    return i;

                --n;
            }
        }
    }

    return -1;
}

int String::lastIndexOfIgnoreCase (StringRef other) const noexcept
{
    if (other.isNotEmpty())
    {
        auto len = other.length();
        int i = length() - len;

        if (i >= 0)
        {
            for (auto n = text + i; i >= 0; --i)
            {
                if (n.compareIgnoreCaseUpTo (other.text, len) == 0)
                    return i;

                --n;
            }
        }
    }

    return -1;
}

int String::lastIndexOfAnyOf (StringRef charactersToLookFor, const bool ignoreCase) const noexcept
{
    auto t = text;
    int last = -1;

    for (int i = 0; ! t.isEmpty(); ++i)
        if (charactersToLookFor.text.indexOf (t.getAndAdvance(), ignoreCase) >= 0)
            last = i;

    return last;
}

bool String::contains (StringRef other) const noexcept
{
    return indexOf (other) >= 0;
}

bool String::containsChar (const juce_wchar character) const noexcept
{
    return text.indexOf (character) >= 0;
}

bool String::containsIgnoreCase (StringRef t) const noexcept
{
    return indexOfIgnoreCase (t) >= 0;
}

int String::indexOfWholeWord (StringRef word) const noexcept
{
    if (word.isNotEmpty())
    {
        auto t = text;
        auto wordLen = word.length();
        auto end = (int) t.length() - wordLen;

        for (int i = 0; i <= end; ++i)
        {
            if (t.compareUpTo (word.text, wordLen) == 0
                  && (i == 0 || ! (t - 1).isLetterOrDigit())
                  && ! (t + wordLen).isLetterOrDigit())
                return i;

            ++t;
        }
    }

    return -1;
}

int String::indexOfWholeWordIgnoreCase (StringRef word) const noexcept
{
    if (word.isNotEmpty())
    {
        auto t = text;
        auto wordLen = word.length();
        auto end = (int) t.length() - wordLen;

        for (int i = 0; i <= end; ++i)
        {
            if (t.compareIgnoreCaseUpTo (word.text, wordLen) == 0
                  && (i == 0 || ! (t - 1).isLetterOrDigit())
                  && ! (t + wordLen).isLetterOrDigit())
                return i;

            ++t;
        }
    }

    return -1;
}

bool String::containsWholeWord (StringRef wordToLookFor) const noexcept
{
    return indexOfWholeWord (wordToLookFor) >= 0;
}

bool String::containsWholeWordIgnoreCase (StringRef wordToLookFor) const noexcept
{
    return indexOfWholeWordIgnoreCase (wordToLookFor) >= 0;
}

//==============================================================================
template <typename CharPointer>
struct WildCardMatcher
{
    static bool matches (CharPointer wildcard, CharPointer test, const bool ignoreCase) noexcept
    {
        for (;;)
        {
            auto wc = wildcard.getAndAdvance();

            if (wc == '*')
                return wildcard.isEmpty() || matchesAnywhere (wildcard, test, ignoreCase);

            if (! characterMatches (wc, test.getAndAdvance(), ignoreCase))
                return false;

            if (wc == 0)
                return true;
        }
    }

    static bool characterMatches (const juce_wchar wc, const juce_wchar tc, const bool ignoreCase) noexcept
    {
        return (wc == tc) || (wc == '?' && tc != 0)
                || (ignoreCase && CharacterFunctions::toLowerCase (wc) == CharacterFunctions::toLowerCase (tc));
    }

    static bool matchesAnywhere (const CharPointer wildcard, CharPointer test, const bool ignoreCase) noexcept
    {
        for (; ! test.isEmpty(); ++test)
            if (matches (wildcard, test, ignoreCase))
                return true;

        return false;
    }
};

bool String::matchesWildcard (StringRef wildcard, const bool ignoreCase) const noexcept
{
    return WildCardMatcher<CharPointerType>::matches (wildcard.text, text, ignoreCase);
}

//==============================================================================
String String::repeatedString (StringRef stringToRepeat, int numberOfTimesToRepeat)
{
    if (numberOfTimesToRepeat <= 0)
        return {};

    String result (PreallocationBytes (findByteOffsetOfEnd (stringToRepeat) * (size_t) numberOfTimesToRepeat));
    auto n = result.text;

    while (--numberOfTimesToRepeat >= 0)
        n.writeAll (stringToRepeat.text);

    return result;
}

String String::paddedLeft (const juce_wchar padCharacter, int minimumLength) const
{
    jassert (padCharacter != 0);

    auto extraChars = minimumLength;
    auto end = text;

    while (! end.isEmpty())
    {
        --extraChars;
        ++end;
    }

    if (extraChars <= 0 || padCharacter == 0)
        return *this;

    auto currentByteSize = (size_t) (((char*) end.getAddress()) - (char*) text.getAddress());
    String result (PreallocationBytes (currentByteSize + (size_t) extraChars * CharPointerType::getBytesRequiredFor (padCharacter)));
    auto n = result.text;

    while (--extraChars >= 0)
        n.write (padCharacter);

    n.writeAll (text);
    return result;
}

String String::paddedRight (const juce_wchar padCharacter, int minimumLength) const
{
    jassert (padCharacter != 0);

    auto extraChars = minimumLength;
    CharPointerType end (text);

    while (! end.isEmpty())
    {
        --extraChars;
        ++end;
    }

    if (extraChars <= 0 || padCharacter == 0)
        return *this;

    auto currentByteSize = (size_t) (((char*) end.getAddress()) - (char*) text.getAddress());
    String result (PreallocationBytes (currentByteSize + (size_t) extraChars * CharPointerType::getBytesRequiredFor (padCharacter)));
    auto n = result.text;

    n.writeAll (text);

    while (--extraChars >= 0)
        n.write (padCharacter);

    n.writeNull();
    return result;
}

//==============================================================================
String String::replaceSection (int index, int numCharsToReplace, StringRef stringToInsert) const
{
    if (index < 0)
    {
        // a negative index to replace from?
        jassertfalse;
        index = 0;
    }

    if (numCharsToReplace < 0)
    {
        // replacing a negative number of characters?
        numCharsToReplace = 0;
        jassertfalse;
    }

    auto insertPoint = text;

    for (int i = 0; i < index; ++i)
    {
        if (insertPoint.isEmpty())
        {
            // replacing beyond the end of the string?
            jassertfalse;
            return *this + stringToInsert;
        }

        ++insertPoint;
    }

    auto startOfRemainder = insertPoint;

    for (int i = 0; i < numCharsToReplace && ! startOfRemainder.isEmpty(); ++i)
        ++startOfRemainder;

    if (insertPoint == text && startOfRemainder.isEmpty())
        return stringToInsert.text;

    auto initialBytes = (size_t) (((char*) insertPoint.getAddress()) - (char*) text.getAddress());
    auto newStringBytes = findByteOffsetOfEnd (stringToInsert);
    auto remainderBytes = (size_t) (((char*) startOfRemainder.findTerminatingNull().getAddress()) - (char*) startOfRemainder.getAddress());

    auto newTotalBytes = initialBytes + newStringBytes + remainderBytes;

    if (newTotalBytes <= 0)
        return {};

    String result (PreallocationBytes ((size_t) newTotalBytes));

    auto* dest = (char*) result.text.getAddress();
    memcpy (dest, text.getAddress(), initialBytes);
    dest += initialBytes;
    memcpy (dest, stringToInsert.text.getAddress(), newStringBytes);
    dest += newStringBytes;
    memcpy (dest, startOfRemainder.getAddress(), remainderBytes);
    dest += remainderBytes;
    CharPointerType ((CharPointerType::CharType*) dest).writeNull();

    return result;
}

String String::replace (StringRef stringToReplace, StringRef stringToInsert, const bool ignoreCase) const
{
    auto stringToReplaceLen = stringToReplace.length();
    auto stringToInsertLen = stringToInsert.length();

    int i = 0;
    String result (*this);

    while ((i = (ignoreCase ? result.indexOfIgnoreCase (i, stringToReplace)
                            : result.indexOf (i, stringToReplace))) >= 0)
    {
        result = result.replaceSection (i, stringToReplaceLen, stringToInsert);
        i += stringToInsertLen;
    }

    return result;
}

String String::replaceFirstOccurrenceOf (StringRef stringToReplace, StringRef stringToInsert, const bool ignoreCase) const
{
    auto stringToReplaceLen = stringToReplace.length();
    auto index = ignoreCase ? indexOfIgnoreCase (stringToReplace)
                            : indexOf (stringToReplace);

    if (index >= 0)
        return replaceSection (index, stringToReplaceLen, stringToInsert);

    return *this;
}

struct StringCreationHelper
{
    StringCreationHelper (size_t initialBytes)  : allocatedBytes (initialBytes)
    {
        result.preallocateBytes (allocatedBytes);
        dest = result.getCharPointer();
    }

    StringCreationHelper (const String::CharPointerType s)
        : source (s), allocatedBytes (StringHolder::getAllocatedNumBytes (s))
    {
        result.preallocateBytes (allocatedBytes);
        dest = result.getCharPointer();
    }

    void write (juce_wchar c)
    {
        bytesWritten += String::CharPointerType::getBytesRequiredFor (c);

        if (bytesWritten > allocatedBytes)
        {
            allocatedBytes += jmax ((size_t) 8, allocatedBytes / 16);
            auto destOffset = (size_t) (((char*) dest.getAddress()) - (char*) result.getCharPointer().getAddress());
            result.preallocateBytes (allocatedBytes);
            dest = addBytesToPointer (result.getCharPointer().getAddress(), (int) destOffset);
        }

        dest.write (c);
    }

    String result;
    String::CharPointerType source { nullptr }, dest { nullptr };
    size_t allocatedBytes, bytesWritten = 0;
};

String String::replaceCharacter (const juce_wchar charToReplace, const juce_wchar charToInsert) const
{
    if (! containsChar (charToReplace))
        return *this;

    StringCreationHelper builder (text);

    for (;;)
    {
        auto c = builder.source.getAndAdvance();

        if (c == charToReplace)
            c = charToInsert;

        builder.write (c);

        if (c == 0)
            break;
    }

    return std::move (builder.result);
}

String String::replaceCharacters (StringRef charactersToReplace, StringRef charactersToInsertInstead) const
{
    // Each character in the first string must have a matching one in the
    // second, so the two strings must be the same length.
    jassert (charactersToReplace.length() == charactersToInsertInstead.length());

    StringCreationHelper builder (text);

    for (;;)
    {
        auto c = builder.source.getAndAdvance();
        auto index = charactersToReplace.text.indexOf (c);

        if (index >= 0)
            c = charactersToInsertInstead [index];

        builder.write (c);

        if (c == 0)
            break;
    }

    return std::move (builder.result);
}

//==============================================================================
bool String::startsWith (StringRef other) const noexcept
{
    return text.compareUpTo (other.text, other.length()) == 0;
}

bool String::startsWithIgnoreCase (StringRef other) const noexcept
{
    return text.compareIgnoreCaseUpTo (other.text, other.length()) == 0;
}

bool String::startsWithChar (const juce_wchar character) const noexcept
{
    jassert (character != 0); // strings can't contain a null character!

    return *text == character;
}

bool String::endsWithChar (const juce_wchar character) const noexcept
{
    jassert (character != 0); // strings can't contain a null character!

    if (text.isEmpty())
        return false;

    auto t = text.findTerminatingNull();
    return *--t == character;
}

bool String::endsWith (StringRef other) const noexcept
{
    auto end = text.findTerminatingNull();
    auto otherEnd = other.text.findTerminatingNull();

    while (end > text && otherEnd > other.text)
    {
        --end;
        --otherEnd;

        if (*end != *otherEnd)
            return false;
    }

    return otherEnd == other.text;
}

bool String::endsWithIgnoreCase (StringRef other) const noexcept
{
    auto end = text.findTerminatingNull();
    auto otherEnd = other.text.findTerminatingNull();

    while (end > text && otherEnd > other.text)
    {
        --end;
        --otherEnd;

        if (end.toLowerCase() != otherEnd.toLowerCase())
            return false;
    }

    return otherEnd == other.text;
}

//==============================================================================
String String::toUpperCase() const
{
    StringCreationHelper builder (text);

    for (;;)
    {
        auto c = builder.source.toUpperCase();
        builder.write (c);

        if (c == 0)
            break;

        ++(builder.source);
    }

    return std::move (builder.result);
}

String String::toLowerCase() const
{
    StringCreationHelper builder (text);

    for (;;)
    {
        auto c = builder.source.toLowerCase();
        builder.write (c);

        if (c == 0)
            break;

        ++(builder.source);
    }

    return std::move (builder.result);
}

//==============================================================================
juce_wchar String::getLastCharacter() const noexcept
{
    return isEmpty() ? juce_wchar() : text [length() - 1];
}

String String::substring (int start, const int end) const
{
    if (start < 0)
        start = 0;

    if (end <= start)
        return {};

    int i = 0;
    auto t1 = text;

    while (i < start)
    {
        if (t1.isEmpty())
            return {};

        ++i;
        ++t1;
    }

    auto t2 = t1;

    while (i < end)
    {
        if (t2.isEmpty())
        {
            if (start == 0)
                return *this;

            break;
        }

        ++i;
        ++t2;
    }

    return String (t1, t2);
}

String String::substring (int start) const
{
    if (start <= 0)
        return *this;

    auto t = text;

    while (--start >= 0)
    {
        if (t.isEmpty())
            return {};

        ++t;
    }

    return String (t);
}

String String::dropLastCharacters (const int numberToDrop) const
{
    return String (text, (size_t) jmax (0, length() - numberToDrop));
}

String String::getLastCharacters (const int numCharacters) const
{
    return String (text + jmax (0, length() - jmax (0, numCharacters)));
}

String String::fromFirstOccurrenceOf (StringRef sub, bool includeSubString, bool ignoreCase) const
{
    auto i = ignoreCase ? indexOfIgnoreCase (sub)
                        : indexOf (sub);
    if (i < 0)
        return {};

    return substring (includeSubString ? i : i + sub.length());
}

String String::fromLastOccurrenceOf (StringRef sub, bool includeSubString, bool ignoreCase) const
{
    auto i = ignoreCase ? lastIndexOfIgnoreCase (sub)
                        : lastIndexOf (sub);
    if (i < 0)
        return *this;

    return substring (includeSubString ? i : i + sub.length());
}

String String::upToFirstOccurrenceOf (StringRef sub, bool includeSubString, bool ignoreCase) const
{
    auto i = ignoreCase ? indexOfIgnoreCase (sub)
                        : indexOf (sub);
    if (i < 0)
        return *this;

    return substring (0, includeSubString ? i + sub.length() : i);
}

String String::upToLastOccurrenceOf (StringRef sub, bool includeSubString, bool ignoreCase) const
{
    auto i = ignoreCase ? lastIndexOfIgnoreCase (sub)
                        : lastIndexOf (sub);
    if (i < 0)
        return *this;

    return substring (0, includeSubString ? i + sub.length() : i);
}

static bool isQuoteCharacter (juce_wchar c) noexcept
{
    return c == '"' || c == '\'';
}

bool String::isQuotedString() const
{
    return isQuoteCharacter (*text.findEndOfWhitespace());
}

String String::unquoted() const
{
    if (! isQuoteCharacter (*text))
        return *this;

    auto len = length();
    return substring (1, len - (isQuoteCharacter (text[len - 1]) ? 1 : 0));
}

String String::quoted (juce_wchar quoteCharacter) const
{
    if (isEmpty())
        return charToString (quoteCharacter) + quoteCharacter;

    String t (*this);

    if (! t.startsWithChar (quoteCharacter))
        t = charToString (quoteCharacter) + t;

    if (! t.endsWithChar (quoteCharacter))
        t += quoteCharacter;

    return t;
}

//==============================================================================
static String::CharPointerType findTrimmedEnd (const String::CharPointerType start,
                                               String::CharPointerType end)
{
    while (end > start)
    {
        if (! (--end).isWhitespace())
        {
            ++end;
            break;
        }
    }

    return end;
}

String String::trim() const
{
    if (isNotEmpty())
    {
        auto start = text.findEndOfWhitespace();
        auto end = start.findTerminatingNull();
        auto trimmedEnd = findTrimmedEnd (start, end);

        if (trimmedEnd <= start)
            return {};

        if (text < start || trimmedEnd < end)
            return String (start, trimmedEnd);
    }

    return *this;
}

String String::trimStart() const
{
    if (isNotEmpty())
    {
        auto t = text.findEndOfWhitespace();

        if (t != text)
            return String (t);
    }

    return *this;
}

String String::trimEnd() const
{
    if (isNotEmpty())
    {
        auto end = text.findTerminatingNull();
        auto trimmedEnd = findTrimmedEnd (text, end);

        if (trimmedEnd < end)
            return String (text, trimmedEnd);
    }

    return *this;
}

String String::trimCharactersAtStart (StringRef charactersToTrim) const
{
    auto t = text;

    while (charactersToTrim.text.indexOf (*t) >= 0)
        ++t;

    return t == text ? *this : String (t);
}

String String::trimCharactersAtEnd (StringRef charactersToTrim) const
{
    if (isNotEmpty())
    {
        auto end = text.findTerminatingNull();
        auto trimmedEnd = end;

        while (trimmedEnd > text)
        {
            if (charactersToTrim.text.indexOf (*--trimmedEnd) < 0)
            {
                ++trimmedEnd;
                break;
            }
        }

        if (trimmedEnd < end)
            return String (text, trimmedEnd);
    }

    return *this;
}

//==============================================================================
String String::retainCharacters (StringRef charactersToRetain) const
{
    if (isEmpty())
        return {};

    StringCreationHelper builder (text);

    for (;;)
    {
        auto c = builder.source.getAndAdvance();

        if (charactersToRetain.text.indexOf (c) >= 0)
            builder.write (c);

        if (c == 0)
            break;
    }

    builder.write (0);
    return std::move (builder.result);
}

String String::removeCharacters (StringRef charactersToRemove) const
{
    if (isEmpty())
        return {};

    StringCreationHelper builder (text);

    for (;;)
    {
        auto c = builder.source.getAndAdvance();

        if (charactersToRemove.text.indexOf (c) < 0)
            builder.write (c);

        if (c == 0)
            break;
    }

    return std::move (builder.result);
}

String String::initialSectionContainingOnly (StringRef permittedCharacters) const
{
    for (auto t = text; ! t.isEmpty(); ++t)
        if (permittedCharacters.text.indexOf (*t) < 0)
            return String (text, t);

    return *this;
}

String String::initialSectionNotContaining (StringRef charactersToStopAt) const
{
    for (auto t = text; ! t.isEmpty(); ++t)
        if (charactersToStopAt.text.indexOf (*t) >= 0)
            return String (text, t);

    return *this;
}

bool String::containsOnly (StringRef chars) const noexcept
{
    for (auto t = text; ! t.isEmpty();)
        if (chars.text.indexOf (t.getAndAdvance()) < 0)
            return false;

    return true;
}

bool String::containsAnyOf (StringRef chars) const noexcept
{
    for (auto t = text; ! t.isEmpty();)
        if (chars.text.indexOf (t.getAndAdvance()) >= 0)
            return true;

    return false;
}

bool String::containsNonWhitespaceChars() const noexcept
{
    for (auto t = text; ! t.isEmpty(); ++t)
        if (! t.isWhitespace())
            return true;

    return false;
}

String String::formattedRaw (const char* pf, ...)
{
    size_t bufferSize = 256;

    for (;;)
    {
        va_list args;
        va_start (args, pf);

      #if JUCE_ANDROID
        HeapBlock<char> temp (bufferSize);
        int num = (int) vsnprintf (temp.get(), bufferSize - 1, pf, args);
        if (num >= static_cast<int> (bufferSize))
            num = -1;
      #else
        String wideCharVersion (pf);
        HeapBlock<wchar_t> temp (bufferSize);
        const int num = (int)
       #if JUCE_WINDOWS
            _vsnwprintf
       #else
            vswprintf
       #endif
                (temp.get(), bufferSize - 1, wideCharVersion.toWideCharPointer(), args);
      #endif
        va_end (args);

        if (num > 0)
            return String (temp.get());

        bufferSize += 256;

        if (num == 0 || bufferSize > 65536) // the upper limit is a sanity check to avoid situations where vprintf repeatedly
            break;                          // returns -1 because of an error rather than because it needs more space.
    }

    return {};
}

//==============================================================================
int String::getIntValue() const noexcept            { return text.getIntValue32(); }
int64 String::getLargeIntValue() const noexcept     { return text.getIntValue64(); }
float String::getFloatValue() const noexcept        { return (float) getDoubleValue(); }
double String::getDoubleValue() const noexcept      { return text.getDoubleValue(); }

int String::getTrailingIntValue() const noexcept
{
    int n = 0;
    int mult = 1;
    auto t = text.findTerminatingNull();

    while (--t >= text)
    {
        if (! t.isDigit())
        {
            if (*t == '-')
                n = -n;

            break;
        }

        n += static_cast<juce_wchar> (mult) * (*t - '0');
        mult *= 10;
    }

    return n;
}

static const char hexDigits[] = "0123456789abcdef";

template <typename Type>
static String hexToString (Type v)
{
    String::CharPointerType::CharType buffer[32];
    auto* end = buffer + numElementsInArray (buffer) - 1;
    auto* t = end;
    *t = 0;

    do
    {
        *--t = hexDigits [(int) (v & 15)];
        v >>= 4;

    } while (v != 0);

    return String (String::CharPointerType (t),
                   String::CharPointerType (end));
}

String String::createHex (uint8 n)    { return hexToString (n); }
String String::createHex (uint16 n)   { return hexToString (n); }
String String::createHex (uint32 n)   { return hexToString (n); }
String String::createHex (uint64 n)   { return hexToString (n); }

String String::toHexString (const void* const d, const int size, const int groupSize)
{
    if (size <= 0)
        return {};

    int numChars = (size * 2) + 2;
    if (groupSize > 0)
        numChars += size / groupSize;

    String s (PreallocationBytes ((size_t) numChars * sizeof (CharPointerType::CharType)));

    auto* data = static_cast<const unsigned char*> (d);
    auto dest = s.text;

    for (int i = 0; i < size; ++i)
    {
        const unsigned char nextByte = *data++;
        dest.write ((juce_wchar) hexDigits [nextByte >> 4]);
        dest.write ((juce_wchar) hexDigits [nextByte & 0xf]);

        if (groupSize > 0 && (i % groupSize) == (groupSize - 1) && i < (size - 1))
            dest.write ((juce_wchar) ' ');
    }

    dest.writeNull();
    return s;
}

int   String::getHexValue32() const noexcept    { return CharacterFunctions::HexParser<int>  ::parse (text); }
int64 String::getHexValue64() const noexcept    { return CharacterFunctions::HexParser<int64>::parse (text); }

//==============================================================================
static String getStringFromWindows1252Codepage (const char* data, size_t num)
{
    HeapBlock<juce_wchar> unicode (num + 1);

    for (size_t i = 0; i < num; ++i)
        unicode[i] = CharacterFunctions::getUnicodeCharFromWindows1252Codepage ((uint8) data[i]);

    unicode[num] = 0;
    return CharPointer_UTF32 (unicode);
}

String String::createStringFromData (const void* const unknownData, int size)
{
    auto* data = static_cast<const uint8*> (unknownData);

    if (size <= 0 || data == nullptr)
        return {};

    if (size == 1)
        return charToString ((juce_wchar) data[0]);

    if (CharPointer_UTF16::isByteOrderMarkBigEndian (data)
         || CharPointer_UTF16::isByteOrderMarkLittleEndian (data))
    {
        const int numChars = size / 2 - 1;

        StringCreationHelper builder ((size_t) numChars);

        auto src = reinterpret_cast<const uint16*> (data + 2);

        if (CharPointer_UTF16::isByteOrderMarkBigEndian (data))
        {
            for (int i = 0; i < numChars; ++i)
                builder.write ((juce_wchar) ByteOrder::swapIfLittleEndian (src[i]));
        }
        else
        {
            for (int i = 0; i < numChars; ++i)
                builder.write ((juce_wchar) ByteOrder::swapIfBigEndian (src[i]));
        }

        builder.write (0);
        return std::move (builder.result);
    }

    auto* start = (const char*) data;

    if (size >= 3 && CharPointer_UTF8::isByteOrderMark (data))
    {
        start += 3;
        size -= 3;
    }

    if (CharPointer_UTF8::isValidString (start, size))
        return String (CharPointer_UTF8 (start),
                       CharPointer_UTF8 (start + size));

    return getStringFromWindows1252Codepage (start, (size_t) size);
}

//==============================================================================
static const juce_wchar emptyChar = 0;

template <class CharPointerType_Src, class CharPointerType_Dest>
struct StringEncodingConverter
{
    static CharPointerType_Dest convert (const String& s)
    {
        auto& source = const_cast<String&> (s);

        using DestChar = typename CharPointerType_Dest::CharType;

        if (source.isEmpty())
            return CharPointerType_Dest (reinterpret_cast<const DestChar*> (&emptyChar));

        CharPointerType_Src text (source.getCharPointer());
        auto extraBytesNeeded = CharPointerType_Dest::getBytesRequiredFor (text) + sizeof (typename CharPointerType_Dest::CharType);
        auto endOffset = (text.sizeInBytes() + 3) & ~3u; // the new string must be word-aligned or many Windows
                                                         // functions will fail to read it correctly!
        source.preallocateBytes (endOffset + extraBytesNeeded);
        text = source.getCharPointer();

        void* const newSpace = addBytesToPointer (text.getAddress(), (int) endOffset);
        const CharPointerType_Dest extraSpace (static_cast<DestChar*> (newSpace));

       #if JUCE_DEBUG // (This just avoids spurious warnings from valgrind about the uninitialised bytes at the end of the buffer..)
        auto bytesToClear = (size_t) jmin ((int) extraBytesNeeded, 4);
        zeromem (addBytesToPointer (newSpace, extraBytesNeeded - bytesToClear), bytesToClear);
       #endif

        CharPointerType_Dest (extraSpace).writeAll (text);
        return extraSpace;
    }
};

template <>
struct StringEncodingConverter<CharPointer_UTF8, CharPointer_UTF8>
{
    static CharPointer_UTF8 convert (const String& source) noexcept   { return CharPointer_UTF8 (reinterpret_cast<CharPointer_UTF8::CharType*> (source.getCharPointer().getAddress())); }
};

template <>
struct StringEncodingConverter<CharPointer_UTF16, CharPointer_UTF16>
{
    static CharPointer_UTF16 convert (const String& source) noexcept  { return CharPointer_UTF16 (reinterpret_cast<CharPointer_UTF16::CharType*> (source.getCharPointer().getAddress())); }
};

template <>
struct StringEncodingConverter<CharPointer_UTF32, CharPointer_UTF32>
{
    static CharPointer_UTF32 convert (const String& source) noexcept  { return CharPointer_UTF32 (reinterpret_cast<CharPointer_UTF32::CharType*> (source.getCharPointer().getAddress())); }
};

CharPointer_UTF8  String::toUTF8()  const { return StringEncodingConverter<CharPointerType, CharPointer_UTF8 >::convert (*this); }
CharPointer_UTF16 String::toUTF16() const { return StringEncodingConverter<CharPointerType, CharPointer_UTF16>::convert (*this); }
CharPointer_UTF32 String::toUTF32() const { return StringEncodingConverter<CharPointerType, CharPointer_UTF32>::convert (*this); }

const char* String::toRawUTF8() const
{
    return toUTF8().getAddress();
}

const wchar_t* String::toWideCharPointer() const
{
    return StringEncodingConverter<CharPointerType, CharPointer_wchar_t>::convert (*this).getAddress();
}

std::string String::toStdString() const
{
    return std::string (toRawUTF8());
}

//==============================================================================
template <class CharPointerType_Src, class CharPointerType_Dest>
struct StringCopier
{
    static size_t copyToBuffer (const CharPointerType_Src source, typename CharPointerType_Dest::CharType* const buffer, const size_t maxBufferSizeBytes)
    {
        jassert (((ssize_t) maxBufferSizeBytes) >= 0); // keep this value positive!

        if (buffer == nullptr)
            return CharPointerType_Dest::getBytesRequiredFor (source) + sizeof (typename CharPointerType_Dest::CharType);

        return CharPointerType_Dest (buffer).writeWithDestByteLimit (source, maxBufferSizeBytes);
    }
};

size_t String::copyToUTF8 (CharPointer_UTF8::CharType* const buffer, size_t maxBufferSizeBytes) const noexcept
{
    return StringCopier<CharPointerType, CharPointer_UTF8>::copyToBuffer (text, buffer, maxBufferSizeBytes);
}

size_t String::copyToUTF16 (CharPointer_UTF16::CharType* const buffer, size_t maxBufferSizeBytes) const noexcept
{
    return StringCopier<CharPointerType, CharPointer_UTF16>::copyToBuffer (text, buffer, maxBufferSizeBytes);
}

size_t String::copyToUTF32 (CharPointer_UTF32::CharType* const buffer, size_t maxBufferSizeBytes) const noexcept
{
    return StringCopier<CharPointerType, CharPointer_UTF32>::copyToBuffer (text, buffer, maxBufferSizeBytes);
}

//==============================================================================
size_t String::getNumBytesAsUTF8() const noexcept
{
    return CharPointer_UTF8::getBytesRequiredFor (text);
}

String String::fromUTF8 (const char* const buffer, int bufferSizeBytes)
{
    if (buffer != nullptr)
    {
        if (bufferSizeBytes < 0)
            return String (CharPointer_UTF8 (buffer));

        if (bufferSizeBytes > 0)
        {
            jassert (CharPointer_UTF8::isValidString (buffer, bufferSizeBytes));
            return String (CharPointer_UTF8 (buffer), CharPointer_UTF8 (buffer + bufferSizeBytes));
        }
    }

    return {};
}

#if JUCE_MSVC
 #pragma warning (pop)
#endif

//==============================================================================
StringRef::StringRef() noexcept  : text ((const String::CharPointerType::CharType*) "\0\0\0")
{
}

StringRef::StringRef (const char* stringLiteral) noexcept
   #if JUCE_STRING_UTF_TYPE != 8
    : text (nullptr), stringCopy (stringLiteral)
   #else
    : text (stringLiteral)
   #endif
{
   #if JUCE_STRING_UTF_TYPE != 8
    text = stringCopy.getCharPointer();
   #endif

    jassert (stringLiteral != nullptr); // This must be a valid string literal, not a null pointer!!

   #if JUCE_NATIVE_WCHAR_IS_UTF8
    /*  If you get an assertion here, then you're trying to create a string from 8-bit data
        that contains values greater than 127. These can NOT be correctly converted to unicode
        because there's no way for the String class to know what encoding was used to
        create them. The source data could be UTF-8, ASCII or one of many local code-pages.

        To get around this problem, you must be more explicit when you pass an ambiguous 8-bit
        string to the StringRef class - so for example if your source data is actually UTF-8,
        you'd call StringRef (CharPointer_UTF8 ("my utf8 string..")), and it would be able to
        correctly convert the multi-byte characters to unicode. It's *highly* recommended that
        you use UTF-8 with escape characters in your source code to represent extended characters,
        because there's no other way to represent these strings in a way that isn't dependent on
        the compiler, source code editor and platform.
    */
    jassert (CharPointer_ASCII::isValidString (stringLiteral, std::numeric_limits<int>::max()));
   #endif
}

StringRef::StringRef (String::CharPointerType stringLiteral) noexcept  : text (stringLiteral)
{
    jassert (stringLiteral.getAddress() != nullptr); // This must be a valid string literal, not a null pointer!!
}

StringRef::StringRef (const String& string) noexcept   : text (string.getCharPointer()) {}
StringRef::StringRef (const std::string& string)       : StringRef (string.c_str()) {}

//==============================================================================

static String reduceLengthOfFloatString (const String& input)
{
    const auto start = input.getCharPointer();
    const auto end = start + (int) input.length();
    auto trimStart = end;
    auto trimEnd = trimStart;
    auto exponentTrimStart = end;
    auto exponentTrimEnd = exponentTrimStart;

    decltype (*start) currentChar = '\0';

    for (auto c = end - 1; c > start; --c)
    {
        currentChar = *c;

        if (currentChar == '0' && c + 1 == trimStart)
        {
            --trimStart;
        }
        else if (currentChar == '.')
        {
            if (trimStart == c + 1 && trimStart != end && *trimStart == '0')
                ++trimStart;

            break;
        }
        else if (currentChar == 'e' || currentChar == 'E')
        {
            auto cNext = c + 1;

            if (cNext != end)
            {
                if (*cNext == '-')
                    ++cNext;

                exponentTrimStart = cNext;

                if (cNext != end && *cNext == '+')
                    ++cNext;

                exponentTrimEnd = cNext;
            }

            while (cNext != end && *cNext++ == '0')
                exponentTrimEnd = cNext;

            if (exponentTrimEnd == end)
                exponentTrimStart = c;

            trimStart = c;
            trimEnd = trimStart;
        }
    }

    if ((trimStart != trimEnd && currentChar == '.') || exponentTrimStart != exponentTrimEnd)
    {
        if (trimStart == trimEnd)
            return String (start, exponentTrimStart) + String (exponentTrimEnd, end);

        if (exponentTrimStart == exponentTrimEnd)
            return String (start, trimStart) + String (trimEnd, end);

        if (trimEnd == exponentTrimStart)
            return String (start, trimStart) + String (exponentTrimEnd, end);

        return String (start, trimStart) + String (trimEnd, exponentTrimStart) + String (exponentTrimEnd, end);
    }

    return input;
}

static String serialiseDouble (double input)
{
    auto absInput = std::abs (input);

    if (absInput >= 1.0e6 || absInput <= 1.0e-5)
        return reduceLengthOfFloatString ({ input, 15, true });

    int intInput = (int) input;

    if ((double) intInput == input)
        return { input, 1 };

    auto numberOfDecimalPlaces = [absInput]
    {
        if (absInput < 1.0)
        {
            if (absInput >= 1.0e-3)
            {
                if (absInput >= 1.0e-1) return 16;
                if (absInput >= 1.0e-2) return 17;
                return 18;
            }

            if (absInput >= 1.0e-4) return 19;
            return 20;
        }

        if (absInput < 1.0e3)
        {
            if (absInput < 1.0e1) return 15;
            if (absInput < 1.0e2) return 14;
            return 13;
        }

        if (absInput < 1.0e4) return 12;
        if (absInput < 1.0e5) return 11;
        return 10;
    }();

    return reduceLengthOfFloatString (String (input, numberOfDecimalPlaces));
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

#define STRINGIFY2(X) #X
#define STRINGIFY(X) STRINGIFY2(X)

class StringTests  : public UnitTest
{
public:
    StringTests()
        : UnitTest ("String class", UnitTestCategories::text)
    {}

    template <class CharPointerType>
    struct TestUTFConversion
    {
        static void test (UnitTest& test, Random& r)
        {
            String s (createRandomWideCharString (r));

            typename CharPointerType::CharType buffer [300];

            memset (buffer, 0xff, sizeof (buffer));
            CharPointerType (buffer).writeAll (s.toUTF32());
            test.expectEquals (String (CharPointerType (buffer)), s);

            memset (buffer, 0xff, sizeof (buffer));
            CharPointerType (buffer).writeAll (s.toUTF16());
            test.expectEquals (String (CharPointerType (buffer)), s);

            memset (buffer, 0xff, sizeof (buffer));
            CharPointerType (buffer).writeAll (s.toUTF8());
            test.expectEquals (String (CharPointerType (buffer)), s);

            test.expect (CharPointerType::isValidString (buffer, (int) strlen ((const char*) buffer)));
        }
    };

    static String createRandomWideCharString (Random& r)
    {
        juce_wchar buffer[50] = { 0 };

        for (int i = 0; i < numElementsInArray (buffer) - 1; ++i)
        {
            if (r.nextBool())
            {
                do
                {
                    buffer[i] = (juce_wchar) (1 + r.nextInt (0x10ffff - 1));
                }
                while (! CharPointer_UTF16::canRepresent (buffer[i]));
            }
            else
                buffer[i] = (juce_wchar) (1 + r.nextInt (0xff));
        }

        return CharPointer_UTF32 (buffer);
    }

    void runTest() override
    {
        Random r = getRandom();

        {
            beginTest ("Basics");

            expect (String().length() == 0);
            expect (String() == String());
            String s1, s2 ("abcd");
            expect (s1.isEmpty() && ! s1.isNotEmpty());
            expect (s2.isNotEmpty() && ! s2.isEmpty());
            expect (s2.length() == 4);
            s1 = "abcd";
            expect (s2 == s1 && s1 == s2);
            expect (s1 == "abcd" && s1 == L"abcd");
            expect (String ("abcd") == String (L"abcd"));
            expect (String ("abcdefg", 4) == L"abcd");
            expect (String ("abcdefg", 4) == String (L"abcdefg", 4));
            expect (String::charToString ('x') == "x");
            expect (String::charToString (0) == String());
            expect (s2 + "e" == "abcde" && s2 + 'e' == "abcde");
            expect (s2 + L'e' == "abcde" && s2 + L"e" == "abcde");
            expect (s1.equalsIgnoreCase ("abcD") && s1 < "abce" && s1 > "abbb");
            expect (s1.startsWith ("ab") && s1.startsWith ("abcd") && ! s1.startsWith ("abcde"));
            expect (s1.startsWithIgnoreCase ("aB") && s1.endsWithIgnoreCase ("CD"));
            expect (s1.endsWith ("bcd") && ! s1.endsWith ("aabcd"));
            expectEquals (s1.indexOf (String()), 0);
            expectEquals (s1.indexOfIgnoreCase (String()), 0);
            expect (s1.startsWith (String()) && s1.endsWith (String()) && s1.contains (String()));
            expect (s1.contains ("cd") && s1.contains ("ab") && s1.contains ("abcd"));
            expect (s1.containsChar ('a'));
            expect (! s1.containsChar ('x'));
            expect (! s1.containsChar (0));
            expect (String ("abc foo bar").containsWholeWord ("abc") && String ("abc foo bar").containsWholeWord ("abc"));
        }

        {
            beginTest ("Operations");

            String s ("012345678");
            expect (s.hashCode() != 0);
            expect (s.hashCode64() != 0);
            expect (s.hashCode() != (s + s).hashCode());
            expect (s.hashCode64() != (s + s).hashCode64());
            expect (s.compare (String ("012345678")) == 0);
            expect (s.compare (String ("012345679")) < 0);
            expect (s.compare (String ("012345676")) > 0);
            expect (String("a").compareNatural ("A") == 0);
            expect (String("A").compareNatural ("B") < 0);
            expect (String("a").compareNatural ("B") < 0);
            expect (String("10").compareNatural ("2") > 0);
            expect (String("Abc 10").compareNatural ("aBC 2") > 0);
            expect (String("Abc 1").compareNatural ("aBC 2") < 0);
            expect (s.substring (2, 3) == String::charToString (s[2]));
            expect (s.substring (0, 1) == String::charToString (s[0]));
            expect (s.getLastCharacter() == s [s.length() - 1]);
            expect (String::charToString (s.getLastCharacter()) == s.getLastCharacters (1));
            expect (s.substring (0, 3) == L"012");
            expect (s.substring (0, 100) == s);
            expect (s.substring (-1, 100) == s);
            expect (s.substring (3) == "345678");
            expect (s.indexOf (String (L"45")) == 4);
            expect (String ("444445").indexOf ("45") == 4);
            expect (String ("444445").lastIndexOfChar ('4') == 4);
            expect (String ("45454545x").lastIndexOf (String (L"45")) == 6);
            expect (String ("45454545x").lastIndexOfAnyOf ("456") == 7);
            expect (String ("45454545x").lastIndexOfAnyOf (String (L"456x")) == 8);
            expect (String ("abABaBaBa").lastIndexOfIgnoreCase ("aB") == 6);
            expect (s.indexOfChar (L'4') == 4);
            expect (s + s == "012345678012345678");
            expect (s.startsWith (s));
            expect (s.startsWith (s.substring (0, 4)));
            expect (s.startsWith (s.dropLastCharacters (4)));
            expect (s.endsWith (s.substring (5)));
            expect (s.endsWith (s));
            expect (s.contains (s.substring (3, 6)));
            expect (s.contains (s.substring (3)));
            expect (s.startsWithChar (s[0]));
            expect (s.endsWithChar (s.getLastCharacter()));
            expect (s [s.length()] == 0);
            expect (String ("abcdEFGH").toLowerCase() == String ("abcdefgh"));
            expect (String ("abcdEFGH").toUpperCase() == String ("ABCDEFGH"));

            expect (String (StringRef ("abc")) == "abc");
            expect (String (StringRef ("abc")) == StringRef ("abc"));
            expect (String ("abc") + StringRef ("def") == "abcdef");

            String s2 ("123");
            s2 << ((int) 4) << ((short) 5) << "678" << L"9" << '0';
            s2 += "xyz";
            expect (s2 == "1234567890xyz");
            s2 += (int) 123;
            expect (s2 == "1234567890xyz123");
            s2 += (int64) 123;
            expect (s2 == "1234567890xyz123123");
            s2 << StringRef ("def");
            expect (s2 == "1234567890xyz123123def");

            // int16
            {
                String numStr (std::numeric_limits<int16>::max());
                expect (numStr == "32767");
            }
            {
                String numStr (std::numeric_limits<int16>::min());
                expect (numStr == "-32768");
            }
            {
                String numStr;
                numStr << std::numeric_limits<int16>::max();
                expect (numStr == "32767");
            }
            {
                String numStr;
                numStr << std::numeric_limits<int16>::min();
                expect (numStr == "-32768");
            }
            // int32
            {
                String numStr (std::numeric_limits<int32>::max());
                expect (numStr == "2147483647");
            }
            {
                String numStr (std::numeric_limits<int32>::min());
                expect (numStr == "-2147483648");
            }
            {
                String numStr;
                numStr << std::numeric_limits<int32>::max();
                expect (numStr == "2147483647");
            }
            {
                String numStr;
                numStr << std::numeric_limits<int32>::min();
                expect (numStr == "-2147483648");
            }
            // uint32
            {
                String numStr (std::numeric_limits<uint32>::max());
                expect (numStr == "4294967295");
            }
            {
                String numStr (std::numeric_limits<uint32>::min());
                expect (numStr == "0");
            }
            // int64
            {
                String numStr (std::numeric_limits<int64>::max());
                expect (numStr == "9223372036854775807");
            }
            {
                String numStr (std::numeric_limits<int64>::min());
                expect (numStr == "-9223372036854775808");
            }
            {
                String numStr;
                numStr << std::numeric_limits<int64>::max();
                expect (numStr == "9223372036854775807");
            }
            {
                String numStr;
                numStr << std::numeric_limits<int64>::min();
                expect (numStr == "-9223372036854775808");
            }
            // uint64
            {
                String numStr (std::numeric_limits<uint64>::max());
                expect (numStr == "18446744073709551615");
            }
            {
                String numStr (std::numeric_limits<uint64>::min());
                expect (numStr == "0");
            }
            {
                String numStr;
                numStr << std::numeric_limits<uint64>::max();
                expect (numStr == "18446744073709551615");
            }
            {
                String numStr;
                numStr << std::numeric_limits<uint64>::min();
                expect (numStr == "0");
            }
            // size_t
            {
                String numStr (std::numeric_limits<size_t>::min());
                expect (numStr == "0");
            }

            beginTest ("Numeric conversions");
            expect (String().getIntValue() == 0);
            expect (String().getDoubleValue() == 0.0);
            expect (String().getFloatValue() == 0.0f);
            expect (s.getIntValue() == 12345678);
            expect (s.getLargeIntValue() == (int64) 12345678);
            expect (s.getDoubleValue() == 12345678.0);
            expect (s.getFloatValue() == 12345678.0f);
            expect (String (-1234).getIntValue() == -1234);
            expect (String ((int64) -1234).getLargeIntValue() == -1234);
            expect (String (-1234.56).getDoubleValue() == -1234.56);
            expect (String (-1234.56f).getFloatValue() == -1234.56f);
            expect (String (std::numeric_limits<int>::max()).getIntValue() == std::numeric_limits<int>::max());
            expect (String (std::numeric_limits<int>::min()).getIntValue() == std::numeric_limits<int>::min());
            expect (String (std::numeric_limits<int64>::max()).getLargeIntValue() == std::numeric_limits<int64>::max());
            expect (String (std::numeric_limits<int64>::min()).getLargeIntValue() == std::numeric_limits<int64>::min());
            expect (("xyz" + s).getTrailingIntValue() == s.getIntValue());
            expect (s.getHexValue32() == 0x12345678);
            expect (s.getHexValue64() == (int64) 0x12345678);
            expect (String::toHexString (0x1234abcd).equalsIgnoreCase ("1234abcd"));
            expect (String::toHexString ((int64) 0x1234abcd).equalsIgnoreCase ("1234abcd"));
            expect (String::toHexString ((short) 0x12ab).equalsIgnoreCase ("12ab"));
            expect (String::toHexString ((size_t) 0x12ab).equalsIgnoreCase ("12ab"));
            expect (String::toHexString ((long) 0x12ab).equalsIgnoreCase ("12ab"));
            expect (String::toHexString ((int8)  -1).equalsIgnoreCase ("ff"));
            expect (String::toHexString ((int16) -1).equalsIgnoreCase ("ffff"));
            expect (String::toHexString ((int32) -1).equalsIgnoreCase ("ffffffff"));
            expect (String::toHexString ((int64) -1).equalsIgnoreCase ("ffffffffffffffff"));

            unsigned char data[] = { 1, 2, 3, 4, 0xa, 0xb, 0xc, 0xd };
            expect (String::toHexString (data, 8, 0).equalsIgnoreCase ("010203040a0b0c0d"));
            expect (String::toHexString (data, 8, 1).equalsIgnoreCase ("01 02 03 04 0a 0b 0c 0d"));
            expect (String::toHexString (data, 8, 2).equalsIgnoreCase ("0102 0304 0a0b 0c0d"));

            expectEquals (String (12345.67, 4),     String ("12345.6700"));
            expectEquals (String (12345.67, 6),     String ("12345.670000"));
            expectEquals (String (2589410.5894, 7), String ("2589410.5894000"));
            expectEquals (String (12345.67, 8),     String ("12345.67000000"));
            expectEquals (String (1e19, 4),         String ("10000000000000000000.0000"));
            expectEquals (String (1e-34, 36),       String ("0.000000000000000000000000000000000100"));
            expectEquals (String (1.39, 1),         String ("1.4"));

            expectEquals (String (12345.67, 4,     true), String ("1.2346e+04"));
            expectEquals (String (12345.67, 6,     true), String ("1.234567e+04"));
            expectEquals (String (2589410.5894, 7, true), String ("2.5894106e+06"));
            expectEquals (String (12345.67, 8,     true), String ("1.23456700e+04"));
            expectEquals (String (1e19, 4,         true), String ("1.0000e+19"));
            expectEquals (String (1e-34, 5,        true), String ("1.00000e-34"));
            expectEquals (String (1.39, 1,         true), String ("1.4e+00"));

            beginTest ("Subsections");
            String s3;
            s3 = "abcdeFGHIJ";
            expect (s3.equalsIgnoreCase ("ABCdeFGhiJ"));
            expect (s3.compareIgnoreCase (L"ABCdeFGhiJ") == 0);
            expect (s3.containsIgnoreCase (s3.substring (3)));
            expect (s3.indexOfAnyOf ("xyzf", 2, true) == 5);
            expect (s3.indexOfAnyOf (String (L"xyzf"), 2, false) == -1);
            expect (s3.indexOfAnyOf ("xyzF", 2, false) == 5);
            expect (s3.containsAnyOf (String (L"zzzFs")));
            expect (s3.startsWith ("abcd"));
            expect (s3.startsWithIgnoreCase (String (L"abCD")));
            expect (s3.startsWith (String()));
            expect (s3.startsWithChar ('a'));
            expect (s3.endsWith (String ("HIJ")));
            expect (s3.endsWithIgnoreCase (String (L"Hij")));
            expect (s3.endsWith (String()));
            expect (s3.endsWithChar (L'J'));
            expect (s3.indexOf ("HIJ") == 7);
            expect (s3.indexOf (String (L"HIJK")) == -1);
            expect (s3.indexOfIgnoreCase ("hij") == 7);
            expect (s3.indexOfIgnoreCase (String (L"hijk")) == -1);
            expect (s3.toStdString() == s3.toRawUTF8());

            String s4 (s3);
            s4.append (String ("xyz123"), 3);
            expect (s4 == s3 + "xyz");

            expect (String (1234) < String (1235));
            expect (String (1235) > String (1234));
            expect (String (1234) >= String (1234));
            expect (String (1234) <= String (1234));
            expect (String (1235) >= String (1234));
            expect (String (1234) <= String (1235));

            String s5 ("word word2 word3");
            expect (s5.containsWholeWord (String ("word2")));
            expect (s5.indexOfWholeWord ("word2") == 5);
            expect (s5.containsWholeWord (String (L"word")));
            expect (s5.containsWholeWord ("word3"));
            expect (s5.containsWholeWord (s5));
            expect (s5.containsWholeWordIgnoreCase (String (L"Word2")));
            expect (s5.indexOfWholeWordIgnoreCase ("Word2") == 5);
            expect (s5.containsWholeWordIgnoreCase (String (L"Word")));
            expect (s5.containsWholeWordIgnoreCase ("Word3"));
            expect (! s5.containsWholeWordIgnoreCase (String (L"Wordx")));
            expect (! s5.containsWholeWordIgnoreCase ("xWord2"));
            expect (s5.containsNonWhitespaceChars());
            expect (s5.containsOnly ("ordw23 "));
            expect (! String (" \n\r\t").containsNonWhitespaceChars());

            expect (s5.matchesWildcard (String (L"wor*"), false));
            expect (s5.matchesWildcard ("wOr*", true));
            expect (s5.matchesWildcard (String (L"*word3"), true));
            expect (s5.matchesWildcard ("*word?", true));
            expect (s5.matchesWildcard (String (L"Word*3"), true));
            expect (! s5.matchesWildcard (String (L"*34"), true));
            expect (String ("xx**y").matchesWildcard ("*y", true));
            expect (String ("xx**y").matchesWildcard ("x*y", true));
            expect (String ("xx**y").matchesWildcard ("xx*y", true));
            expect (String ("xx**y").matchesWildcard ("xx*", true));
            expect (String ("xx?y").matchesWildcard ("x??y", true));
            expect (String ("xx?y").matchesWildcard ("xx?y", true));
            expect (! String ("xx?y").matchesWildcard ("xx?y?", true));
            expect (String ("xx?y").matchesWildcard ("xx??", true));

            expectEquals (s5.fromFirstOccurrenceOf (String(), true, false), s5);
            expectEquals (s5.fromFirstOccurrenceOf ("xword2", true, false), s5.substring (100));
            expectEquals (s5.fromFirstOccurrenceOf (String (L"word2"), true, false), s5.substring (5));
            expectEquals (s5.fromFirstOccurrenceOf ("Word2", true, true), s5.substring (5));
            expectEquals (s5.fromFirstOccurrenceOf ("word2", false, false), s5.getLastCharacters (6));
            expectEquals (s5.fromFirstOccurrenceOf ("Word2", false, true), s5.getLastCharacters (6));

            expectEquals (s5.fromLastOccurrenceOf (String(), true, false), s5);
            expectEquals (s5.fromLastOccurrenceOf ("wordx", true, false), s5);
            expectEquals (s5.fromLastOccurrenceOf ("word", true, false), s5.getLastCharacters (5));
            expectEquals (s5.fromLastOccurrenceOf ("worD", true, true), s5.getLastCharacters (5));
            expectEquals (s5.fromLastOccurrenceOf ("word", false, false), s5.getLastCharacters (1));
            expectEquals (s5.fromLastOccurrenceOf ("worD", false, true), s5.getLastCharacters (1));

            expect (s5.upToFirstOccurrenceOf (String(), true, false).isEmpty());
            expectEquals (s5.upToFirstOccurrenceOf ("word4", true, false), s5);
            expectEquals (s5.upToFirstOccurrenceOf ("word2", true, false), s5.substring (0, 10));
            expectEquals (s5.upToFirstOccurrenceOf ("Word2", true, true), s5.substring (0, 10));
            expectEquals (s5.upToFirstOccurrenceOf ("word2", false, false), s5.substring (0, 5));
            expectEquals (s5.upToFirstOccurrenceOf ("Word2", false, true), s5.substring (0, 5));

            expectEquals (s5.upToLastOccurrenceOf (String(), true, false), s5);
            expectEquals (s5.upToLastOccurrenceOf ("zword", true, false), s5);
            expectEquals (s5.upToLastOccurrenceOf ("word", true, false), s5.dropLastCharacters (1));
            expectEquals (s5.dropLastCharacters(1).upToLastOccurrenceOf ("word", true, false), s5.dropLastCharacters (1));
            expectEquals (s5.upToLastOccurrenceOf ("Word", true, true), s5.dropLastCharacters (1));
            expectEquals (s5.upToLastOccurrenceOf ("word", false, false), s5.dropLastCharacters (5));
            expectEquals (s5.upToLastOccurrenceOf ("Word", false, true), s5.dropLastCharacters (5));

            expectEquals (s5.replace ("word", "xyz", false), String ("xyz xyz2 xyz3"));
            expect (s5.replace ("Word", "xyz", true) == "xyz xyz2 xyz3");
            expect (s5.dropLastCharacters (1).replace ("Word", String ("xyz"), true) == L"xyz xyz2 xyz");
            expect (s5.replace ("Word", "", true) == " 2 3");
            expectEquals (s5.replace ("Word2", "xyz", true), String ("word xyz word3"));
            expect (s5.replaceCharacter (L'w', 'x') != s5);
            expectEquals (s5.replaceCharacter ('w', L'x').replaceCharacter ('x', 'w'), s5);
            expect (s5.replaceCharacters ("wo", "xy") != s5);
            expectEquals (s5.replaceCharacters ("wo", "xy").replaceCharacters ("xy", "wo"), s5);
            expectEquals (s5.retainCharacters ("1wordxya"), String ("wordwordword"));
            expect (s5.retainCharacters (String()).isEmpty());
            expect (s5.removeCharacters ("1wordxya") == " 2 3");
            expectEquals (s5.removeCharacters (String()), s5);
            expect (s5.initialSectionContainingOnly ("word") == L"word");
            expect (String ("word").initialSectionContainingOnly ("word") == L"word");
            expectEquals (s5.initialSectionNotContaining (String ("xyz ")), String ("word"));
            expectEquals (s5.initialSectionNotContaining (String (";[:'/")), s5);
            expect (! s5.isQuotedString());
            expect (s5.quoted().isQuotedString());
            expect (! s5.quoted().unquoted().isQuotedString());
            expect (! String ("x'").isQuotedString());
            expect (String ("'x").isQuotedString());

            String s6 (" \t xyz  \t\r\n");
            expectEquals (s6.trim(), String ("xyz"));
            expect (s6.trim().trim() == "xyz");
            expectEquals (s5.trim(), s5);
            expectEquals (s6.trimStart().trimEnd(), s6.trim());
            expectEquals (s6.trimStart().trimEnd(), s6.trimEnd().trimStart());
            expectEquals (s6.trimStart().trimStart().trimEnd().trimEnd(), s6.trimEnd().trimStart());
            expect (s6.trimStart() != s6.trimEnd());
            expectEquals (("\t\r\n " + s6 + "\t\n \r").trim(), s6.trim());
            expect (String::repeatedString ("xyz", 3) == L"xyzxyzxyz");
        }

        {
            beginTest ("UTF conversions");

            TestUTFConversion <CharPointer_UTF32>::test (*this, r);
            TestUTFConversion <CharPointer_UTF8>::test (*this, r);
            TestUTFConversion <CharPointer_UTF16>::test (*this, r);
        }

        {
            beginTest ("StringArray");

            StringArray s;
            s.addTokens ("4,3,2,1,0", ";,", "x");
            expectEquals (s.size(), 5);

            expectEquals (s.joinIntoString ("-"), String ("4-3-2-1-0"));
            s.remove (2);
            expectEquals (s.joinIntoString ("--"), String ("4--3--1--0"));
            expectEquals (s.joinIntoString (StringRef()), String ("4310"));
            s.clear();
            expectEquals (s.joinIntoString ("x"), String());

            StringArray toks;
            toks.addTokens ("x,,", ";,", "");
            expectEquals (toks.size(), 3);
            expectEquals (toks.joinIntoString ("-"), String ("x--"));
            toks.clear();

            toks.addTokens (",x,", ";,", "");
            expectEquals (toks.size(), 3);
            expectEquals (toks.joinIntoString ("-"), String ("-x-"));
            toks.clear();

            toks.addTokens ("x,'y,z',", ";,", "'");
            expectEquals (toks.size(), 3);
            expectEquals (toks.joinIntoString ("-"), String ("x-'y,z'-"));
        }

        {
            beginTest ("var");

            var v1 = 0;
            var v2 = 0.16;
            var v3 = "0.16";
            var v4 = (int64) 0;
            var v5 = 0.0;
            expect (! v2.equals (v1));
            expect (! v1.equals (v2));
            expect (v2.equals (v3));
            expect (! v3.equals (v1));
            expect (! v1.equals (v3));
            expect (v1.equals (v4));
            expect (v4.equals (v1));
            expect (v5.equals (v4));
            expect (v4.equals (v5));
            expect (! v2.equals (v4));
            expect (! v4.equals (v2));
        }

        {
            beginTest ("Significant figures");

            // Integers

            expectEquals (String::toDecimalStringWithSignificantFigures (13, 1), String ("10"));
            expectEquals (String::toDecimalStringWithSignificantFigures (13, 2), String ("13"));
            expectEquals (String::toDecimalStringWithSignificantFigures (13, 3), String ("13.0"));
            expectEquals (String::toDecimalStringWithSignificantFigures (13, 4), String ("13.00"));

            expectEquals (String::toDecimalStringWithSignificantFigures (19368, 1), String ("20000"));
            expectEquals (String::toDecimalStringWithSignificantFigures (19348, 3), String ("19300"));

            expectEquals (String::toDecimalStringWithSignificantFigures (-5, 1), String ("-5"));
            expectEquals (String::toDecimalStringWithSignificantFigures (-5, 3), String ("-5.00"));

            // Zero

            expectEquals (String::toDecimalStringWithSignificantFigures (0, 1), String ("0"));
            expectEquals (String::toDecimalStringWithSignificantFigures (0, 2), String ("0.0"));
            expectEquals (String::toDecimalStringWithSignificantFigures (0, 3), String ("0.00"));

            // Floating point

            expectEquals (String::toDecimalStringWithSignificantFigures (19.0, 1), String ("20"));
            expectEquals (String::toDecimalStringWithSignificantFigures (19.0, 2), String ("19"));
            expectEquals (String::toDecimalStringWithSignificantFigures (19.0, 3), String ("19.0"));
            expectEquals (String::toDecimalStringWithSignificantFigures (19.0, 4), String ("19.00"));

            expectEquals (String::toDecimalStringWithSignificantFigures (-5.45, 1), String ("-5"));
            expectEquals (String::toDecimalStringWithSignificantFigures (-5.45, 3), String ("-5.45"));

            expectEquals (String::toDecimalStringWithSignificantFigures (12345.6789, 9), String ("12345.6789"));
            expectEquals (String::toDecimalStringWithSignificantFigures (12345.6789, 8), String ("12345.679"));
            expectEquals (String::toDecimalStringWithSignificantFigures (12345.6789, 5), String ("12346"));

            expectEquals (String::toDecimalStringWithSignificantFigures (0.00028647, 6), String ("0.000286470"));
            expectEquals (String::toDecimalStringWithSignificantFigures (0.0028647,  6), String ("0.00286470"));
            expectEquals (String::toDecimalStringWithSignificantFigures (2.8647,     6), String ("2.86470"));

            expectEquals (String::toDecimalStringWithSignificantFigures (-0.0000000000019, 1), String ("-0.000000000002"));
        }

        {
            beginTest ("Float trimming");

            {
                StringPairArray tests;
                tests.set ("1", "1");
                tests.set ("1.0", "1.0");
                tests.set ("-1", "-1");
                tests.set ("-100", "-100");
                tests.set ("110", "110");
                tests.set ("9090", "9090");
                tests.set ("1000.0", "1000.0");
                tests.set ("1.0", "1.0");
                tests.set ("-1.00", "-1.0");
                tests.set ("1.20", "1.2");
                tests.set ("1.300", "1.3");
                tests.set ("1.301", "1.301");
                tests.set ("1e", "1");
                tests.set ("-1e+", "-1");
                tests.set ("1e-", "1");
                tests.set ("1e0", "1");
                tests.set ("1e+0", "1");
                tests.set ("1e-0", "1");
                tests.set ("1e000", "1");
                tests.set ("1e+000", "1");
                tests.set ("-1e-000", "-1");
                tests.set ("1e100", "1e100");
                tests.set ("100e100", "100e100");
                tests.set ("100.0e0100", "100.0e100");
                tests.set ("-1e1", "-1e1");
                tests.set ("1e10", "1e10");
                tests.set ("-1e+10", "-1e10");
                tests.set ("1e-10", "1e-10");
                tests.set ("1e0010", "1e10");
                tests.set ("1e-0010", "1e-10");
                tests.set ("1e-1", "1e-1");
                tests.set ("-1.0e1", "-1.0e1");
                tests.set ("1.0e-1", "1.0e-1");
                tests.set ("1.00e-1", "1.0e-1");
                tests.set ("1.001e1", "1.001e1");
                tests.set ("1.010e+1", "1.01e1");
                tests.set ("-1.1000e1", "-1.1e1");

                for (auto& input : tests.getAllKeys())
                    expectEquals (reduceLengthOfFloatString (input), tests[input]);
            }

            {
                std::map<double, String> tests;
                tests[1] = "1.0";
                tests[1.1] = "1.1";
                tests[1.01] = "1.01";
                tests[0.76378] = "7.6378e-1";
                tests[-10] = "-1.0e1";
                tests[10.01] = "1.001e1";
                tests[10691.01] = "1.069101e4";
                tests[0.0123] = "1.23e-2";
                tests[-3.7e-27] = "-3.7e-27";
                tests[1e+40] = "1.0e40";

                for (auto& test : tests)
                    expectEquals (reduceLengthOfFloatString (String (test.first, 15, true)), test.second);
            }
        }

        {
            beginTest ("Serialisation");

            std::map <double, String> tests;

            tests[364] = "364.0";
            tests[1e7] = "1.0e7";
            tests[12345678901] = "1.2345678901e10";

            tests[1234567890123456.7] = "1.234567890123457e15";
            tests[12345678.901234567] = "1.234567890123457e7";
            tests[1234567.8901234567] = "1.234567890123457e6";
            tests[123456.78901234567] = "123456.7890123457";
            tests[12345.678901234567] = "12345.67890123457";
            tests[1234.5678901234567] = "1234.567890123457";
            tests[123.45678901234567] = "123.4567890123457";
            tests[12.345678901234567] = "12.34567890123457";
            tests[1.2345678901234567] = "1.234567890123457";
            tests[0.12345678901234567] = "0.1234567890123457";
            tests[0.012345678901234567] = "0.01234567890123457";
            tests[0.0012345678901234567] = "0.001234567890123457";
            tests[0.00012345678901234567] = "0.0001234567890123457";
            tests[0.000012345678901234567] = "0.00001234567890123457";
            tests[0.0000012345678901234567] = "1.234567890123457e-6";
            tests[0.00000012345678901234567] = "1.234567890123457e-7";

            for (auto& test : tests)
            {
                expectEquals (serialiseDouble (test.first), test.second);
                expectEquals (serialiseDouble (-test.first), "-" + test.second);
            }
        }
    }
};

static StringTests stringUnitTests;

#endif

} // namespace juce
