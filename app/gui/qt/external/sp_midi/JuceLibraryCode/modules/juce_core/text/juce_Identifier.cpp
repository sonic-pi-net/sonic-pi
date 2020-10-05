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

Identifier::Identifier() noexcept {}
Identifier::~Identifier() noexcept {}

Identifier::Identifier (const Identifier& other) noexcept  : name (other.name) {}

Identifier::Identifier (Identifier&& other) noexcept : name (std::move (other.name)) {}

Identifier& Identifier::operator= (Identifier&& other) noexcept
{
    name = std::move (other.name);
    return *this;
}

Identifier& Identifier::operator= (const Identifier& other) noexcept
{
    name = other.name;
    return *this;
}

Identifier::Identifier (const String& nm)
    : name (StringPool::getGlobalPool().getPooledString (nm))
{
    // An Identifier cannot be created from an empty string!
    jassert (nm.isNotEmpty());
}

Identifier::Identifier (const char* nm)
    : name (StringPool::getGlobalPool().getPooledString (nm))
{
    // An Identifier cannot be created from an empty string!
    jassert (nm != nullptr && nm[0] != 0);
}

Identifier::Identifier (String::CharPointerType start, String::CharPointerType end)
    : name (StringPool::getGlobalPool().getPooledString (start, end))
{
    // An Identifier cannot be created from an empty string!
    jassert (start < end);
}

Identifier Identifier::null;

bool Identifier::isValidIdentifier (const String& possibleIdentifier) noexcept
{
    return possibleIdentifier.isNotEmpty()
            && possibleIdentifier.containsOnly ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-:#@$%");
}

} // namespace juce
