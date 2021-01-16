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

static void parseWildcard (const String& pattern, StringArray& result)
{
    result.addTokens (pattern.toLowerCase(), ";,", "\"'");
    result.trim();
    result.removeEmptyStrings();

    // special case for *.*, because people use it to mean "any file", but it
    // would actually ignore files with no extension.
    for (auto& r : result)
        if (r == "*.*")
            r = "*";
}

static bool matchWildcard (const File& file, const StringArray& wildcards)
{
    auto filename = file.getFileName();

    for (auto& w : wildcards)
        if (filename.matchesWildcard (w, true))
            return true;

    return false;
}

WildcardFileFilter::WildcardFileFilter (const String& fileWildcardPatterns,
                                        const String& directoryWildcardPatterns,
                                        const String& desc)
    : FileFilter (desc.isEmpty() ? fileWildcardPatterns
                                 : (desc + " (" + fileWildcardPatterns + ")"))
{
    parseWildcard (fileWildcardPatterns, fileWildcards);
    parseWildcard (directoryWildcardPatterns, directoryWildcards);
}

WildcardFileFilter::~WildcardFileFilter()
{
}

bool WildcardFileFilter::isFileSuitable (const File& file) const
{
    return matchWildcard (file, fileWildcards);
}

bool WildcardFileFilter::isDirectorySuitable (const File& file) const
{
    return matchWildcard (file, directoryWildcards);
}

} // namespace juce
