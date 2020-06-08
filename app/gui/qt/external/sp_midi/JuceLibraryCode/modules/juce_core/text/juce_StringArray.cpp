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

StringArray::StringArray() noexcept
{
}

StringArray::StringArray (const StringArray& other)
    : strings (other.strings)
{
}

StringArray::StringArray (StringArray&& other) noexcept
    : strings (std::move (other.strings))
{
}

StringArray::StringArray (Array<String>&& other) noexcept
    : strings (std::move (other))
{
}

StringArray::StringArray (const String& firstValue)
{
    strings.add (firstValue);
}

StringArray::StringArray (const String* initialStrings, int numberOfStrings)
{
    strings.addArray (initialStrings, numberOfStrings);
}

StringArray::StringArray (const char* const* initialStrings)
{
    strings.addNullTerminatedArray (initialStrings);
}

StringArray::StringArray (const char* const* initialStrings, int numberOfStrings)
{
    strings.addArray (initialStrings, numberOfStrings);
}

StringArray::StringArray (const wchar_t* const* initialStrings)
{
    strings.addNullTerminatedArray (initialStrings);
}

StringArray::StringArray (const wchar_t* const* initialStrings, int numberOfStrings)
{
    strings.addArray (initialStrings, numberOfStrings);
}

StringArray::StringArray (const std::initializer_list<const char*>& stringList)
{
    strings.addArray (stringList);
}

StringArray& StringArray::operator= (const StringArray& other)
{
    strings = other.strings;
    return *this;
}

StringArray& StringArray::operator= (StringArray&& other) noexcept
{
    strings = std::move (other.strings);
    return *this;
}

StringArray::~StringArray()
{
}

bool StringArray::operator== (const StringArray& other) const noexcept
{
    return strings == other.strings;
}

bool StringArray::operator!= (const StringArray& other) const noexcept
{
    return ! operator== (other);
}

void StringArray::swapWith (StringArray& other) noexcept
{
    strings.swapWith (other.strings);
}

void StringArray::clear()
{
    strings.clear();
}

void StringArray::clearQuick()
{
    strings.clearQuick();
}

const String& StringArray::operator[] (int index) const noexcept
{
    if (isPositiveAndBelow (index, strings.size()))
        return strings.getReference (index);

    static String empty;
    return empty;
}

String& StringArray::getReference (int index) noexcept
{
    return strings.getReference (index);
}

void StringArray::add (String newString)
{
    // NB: the local temp copy is to avoid a dangling pointer if the
    // argument being passed-in is a reference into this array.
    strings.add (std::move (newString));
}

void StringArray::insert (int index, String newString)
{
    // NB: the local temp copy is to avoid a dangling pointer if the
    // argument being passed-in is a reference into this array.
    strings.insert (index, std::move (newString));
}

bool StringArray::addIfNotAlreadyThere (const String& newString, bool ignoreCase)
{
    if (contains (newString, ignoreCase))
        return false;

    add (newString);
    return true;
}

void StringArray::addArray (const StringArray& otherArray, int startIndex, int numElementsToAdd)
{
    jassert (this != &otherArray); // can't add from our own elements!

    if (startIndex < 0)
    {
        jassertfalse;
        startIndex = 0;
    }

    if (numElementsToAdd < 0 || startIndex + numElementsToAdd > otherArray.size())
        numElementsToAdd = otherArray.size() - startIndex;

    while (--numElementsToAdd >= 0)
        strings.add (otherArray.strings.getReference (startIndex++));
}

void StringArray::mergeArray (const StringArray& otherArray, bool ignoreCase)
{
    jassert (this != &otherArray); // can't add from our own elements!

    for (auto& s : otherArray)
        addIfNotAlreadyThere (s, ignoreCase);
}

void StringArray::set (int index, String newString)
{
    strings.set (index, std::move (newString));
}

bool StringArray::contains (StringRef stringToLookFor, bool ignoreCase) const
{
    return indexOf (stringToLookFor, ignoreCase) >= 0;
}

int StringArray::indexOf (StringRef stringToLookFor, bool ignoreCase, int i) const
{
    if (i < 0)
        i = 0;

    auto numElements = size();

    if (ignoreCase)
    {
        for (; i < numElements; ++i)
            if (strings.getReference(i).equalsIgnoreCase (stringToLookFor))
                return i;
    }
    else
    {
        for (; i < numElements; ++i)
            if (stringToLookFor == strings.getReference (i))
                return i;
    }

    return -1;
}

void StringArray::move (int currentIndex, int newIndex) noexcept
{
    strings.move (currentIndex, newIndex);
}

//==============================================================================
void StringArray::remove (int index)
{
    strings.remove (index);
}

void StringArray::removeString (StringRef stringToRemove, bool ignoreCase)
{
    if (ignoreCase)
    {
        for (int i = size(); --i >= 0;)
            if (strings.getReference(i).equalsIgnoreCase (stringToRemove))
                strings.remove (i);
    }
    else
    {
        for (int i = size(); --i >= 0;)
            if (stringToRemove == strings.getReference (i))
                strings.remove (i);
    }
}

void StringArray::removeRange (int startIndex, int numberToRemove)
{
    strings.removeRange (startIndex, numberToRemove);
}

//==============================================================================
void StringArray::removeEmptyStrings (bool removeWhitespaceStrings)
{
    if (removeWhitespaceStrings)
    {
        for (int i = size(); --i >= 0;)
            if (! strings.getReference(i).containsNonWhitespaceChars())
                strings.remove (i);
    }
    else
    {
        for (int i = size(); --i >= 0;)
            if (strings.getReference(i).isEmpty())
                strings.remove (i);
    }
}

void StringArray::trim()
{
    for (auto& s : strings)
        s = s.trim();
}

//==============================================================================
void StringArray::sort (bool ignoreCase)
{
    if (ignoreCase)
        std::sort (strings.begin(), strings.end(),
                   [] (const String& a, const String& b) { return a.compareIgnoreCase (b) < 0; });
    else
        std::sort (strings.begin(), strings.end());
}

void StringArray::sortNatural()
{
    std::sort (strings.begin(), strings.end(),
               [] (const String& a, const String& b) { return a.compareNatural (b) < 0; });
}

//==============================================================================
String StringArray::joinIntoString (StringRef separator, int start, int numberToJoin) const
{
    auto last = (numberToJoin < 0) ? size()
                                   : jmin (size(), start + numberToJoin);

    if (start < 0)
        start = 0;

    if (start >= last)
        return {};

    if (start == last - 1)
        return strings.getReference (start);

    auto separatorBytes = separator.text.sizeInBytes() - sizeof (String::CharPointerType::CharType);
    auto bytesNeeded = (size_t) (last - start - 1) * separatorBytes;

    for (int i = start; i < last; ++i)
        bytesNeeded += strings.getReference(i).getCharPointer().sizeInBytes() - sizeof (String::CharPointerType::CharType);

    String result;
    result.preallocateBytes (bytesNeeded);

    auto dest = result.getCharPointer();

    while (start < last)
    {
        auto& s = strings.getReference (start);

        if (! s.isEmpty())
            dest.writeAll (s.getCharPointer());

        if (++start < last && separatorBytes > 0)
            dest.writeAll (separator.text);
    }

    dest.writeNull();
    return result;
}

int StringArray::addTokens (StringRef text, const bool preserveQuotedStrings)
{
    return addTokens (text, " \n\r\t", preserveQuotedStrings ? "\"" : "");
}

int StringArray::addTokens (StringRef text, StringRef breakCharacters, StringRef quoteCharacters)
{
    int num = 0;

    if (text.isNotEmpty())
    {
        for (auto t = text.text;;)
        {
            auto tokenEnd = CharacterFunctions::findEndOfToken (t,
                                                                breakCharacters.text,
                                                                quoteCharacters.text);
            strings.add (String (t, tokenEnd));
            ++num;

            if (tokenEnd.isEmpty())
                break;

            t = ++tokenEnd;
        }
    }

    return num;
}

int StringArray::addLines (StringRef sourceText)
{
    int numLines = 0;
    auto text = sourceText.text;
    bool finished = text.isEmpty();

    while (! finished)
    {
        for (auto startOfLine = text;;)
        {
            auto endOfLine = text;

            switch (text.getAndAdvance())
            {
                case 0:     finished = true; break;
                case '\n':  break;
                case '\r':  if (*text == '\n') ++text; break;
                default:    continue;
            }

            strings.add (String (startOfLine, endOfLine));
            ++numLines;
            break;
        }
    }

    return numLines;
}

StringArray StringArray::fromTokens (StringRef stringToTokenise, bool preserveQuotedStrings)
{
    StringArray s;
    s.addTokens (stringToTokenise, preserveQuotedStrings);
    return s;
}

StringArray StringArray::fromTokens (StringRef stringToTokenise,
                                     StringRef breakCharacters,
                                     StringRef quoteCharacters)
{
    StringArray s;
    s.addTokens (stringToTokenise, breakCharacters, quoteCharacters);
    return s;
}

StringArray StringArray::fromLines (StringRef stringToBreakUp)
{
    StringArray s;
    s.addLines (stringToBreakUp);
    return s;
}

//==============================================================================
void StringArray::removeDuplicates (bool ignoreCase)
{
    for (int i = 0; i < size() - 1; ++i)
    {
        auto s = strings.getReference(i);

        for (int nextIndex = i + 1;;)
        {
            nextIndex = indexOf (s, ignoreCase, nextIndex);

            if (nextIndex < 0)
                break;

            strings.remove (nextIndex);
        }
    }
}

void StringArray::appendNumbersToDuplicates (bool ignoreCase,
                                             bool appendNumberToFirstInstance,
                                             CharPointer_UTF8 preNumberString,
                                             CharPointer_UTF8 postNumberString)
{
    if (preNumberString.getAddress() == nullptr)
        preNumberString = CharPointer_UTF8 (" (");

    if (postNumberString.getAddress() == nullptr)
        postNumberString = CharPointer_UTF8 (")");

    for (int i = 0; i < size() - 1; ++i)
    {
        auto& s = strings.getReference(i);
        auto nextIndex = indexOf (s, ignoreCase, i + 1);

        if (nextIndex >= 0)
        {
            auto original = s;
            int number = 0;

            if (appendNumberToFirstInstance)
                s = original + String (preNumberString) + String (++number) + String (postNumberString);
            else
                ++number;

            while (nextIndex >= 0)
            {
                set (nextIndex, (*this)[nextIndex] + String (preNumberString) + String (++number) + String (postNumberString));
                nextIndex = indexOf (original, ignoreCase, nextIndex + 1);
            }
        }
    }
}

void StringArray::ensureStorageAllocated (int minNumElements)
{
    strings.ensureStorageAllocated (minNumElements);
}

void StringArray::minimiseStorageOverheads()
{
    strings.minimiseStorageOverheads();
}

} // namespace juce
