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

float DirectoryEntry::getEstimatedProgress() const
{
    if (auto it = iterator.lock())
        return it->getEstimatedProgress();

    return 0.0f;
}

JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE ("-Wdeprecated-declarations")
JUCE_BEGIN_IGNORE_WARNINGS_MSVC (4996)

// We implement this in terms of the deprecated DirectoryIterator,
// but the old DirectoryIterator might go away in the future!
RangedDirectoryIterator::RangedDirectoryIterator (const File& directory,
                                                  bool isRecursive,
                                                  const String& wildCard,
                                                  int whatToLookFor)
    : iterator (new DirectoryIterator (directory,
                                       isRecursive,
                                       wildCard,
                                       whatToLookFor))
{
    entry.iterator = iterator;
    increment();
}

JUCE_END_IGNORE_WARNINGS_GCC_LIKE
JUCE_END_IGNORE_WARNINGS_MSVC

bool RangedDirectoryIterator::next()
{
    const auto result = iterator->next (&entry.directory,
                                        &entry.hidden,
                                        &entry.fileSize,
                                        &entry.modTime,
                                        &entry.creationTime,
                                        &entry.readOnly);
    if (result)
        entry.file = iterator->getFile();
    else
        entry = {};

    return result;
}

void RangedDirectoryIterator::increment()
{
    if (iterator != nullptr && ! next())
        iterator = nullptr;
}

} // namespace juce
