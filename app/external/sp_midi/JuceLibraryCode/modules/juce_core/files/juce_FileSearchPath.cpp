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

FileSearchPath::FileSearchPath() {}
FileSearchPath::~FileSearchPath() {}

FileSearchPath::FileSearchPath (const String& path)
{
    init (path);
}

FileSearchPath::FileSearchPath (const FileSearchPath& other)
   : directories (other.directories)
{
}

FileSearchPath& FileSearchPath::operator= (const FileSearchPath& other)
{
    directories = other.directories;
    return *this;
}

FileSearchPath& FileSearchPath::operator= (const String& path)
{
    init (path);
    return *this;
}

void FileSearchPath::init (const String& path)
{
    directories.clear();
    directories.addTokens (path, ";", "\"");
    directories.trim();
    directories.removeEmptyStrings();

    for (auto& d : directories)
        d = d.unquoted();
}

int FileSearchPath::getNumPaths() const
{
    return directories.size();
}

File FileSearchPath::operator[] (int index) const
{
    return File (directories[index]);
}

String FileSearchPath::toString() const
{
    auto dirs = directories;

    for (auto& d : dirs)
        if (d.containsChar (';'))
            d = d.quoted();

    return dirs.joinIntoString (";");
}

void FileSearchPath::add (const File& dir, int insertIndex)
{
    directories.insert (insertIndex, dir.getFullPathName());
}

bool FileSearchPath::addIfNotAlreadyThere (const File& dir)
{
    for (auto& d : directories)
        if (File (d) == dir)
            return false;

    add (dir);
    return true;
}

void FileSearchPath::remove (int index)
{
    directories.remove (index);
}

void FileSearchPath::addPath (const FileSearchPath& other)
{
    for (int i = 0; i < other.getNumPaths(); ++i)
        addIfNotAlreadyThere (other[i]);
}

void FileSearchPath::removeRedundantPaths()
{
    for (int i = directories.size(); --i >= 0;)
    {
        const File d1 (directories[i]);

        for (int j = directories.size(); --j >= 0;)
        {
            const File d2 (directories[j]);

            if (i != j && (d1.isAChildOf (d2) || d1 == d2))
            {
                directories.remove (i);
                break;
            }
        }
    }
}

void FileSearchPath::removeNonExistentPaths()
{
    for (int i = directories.size(); --i >= 0;)
        if (! File (directories[i]).isDirectory())
            directories.remove (i);
}

Array<File> FileSearchPath::findChildFiles (int whatToLookFor, bool recurse, const String& wildcard) const
{
    Array<File> results;
    findChildFiles (results, whatToLookFor, recurse, wildcard);
    return results;
}

int FileSearchPath::findChildFiles (Array<File>& results, int whatToLookFor,
                                    bool recurse, const String& wildcard) const
{
    int total = 0;

    for (auto& d : directories)
        total += File (d).findChildFiles (results, whatToLookFor, recurse, wildcard);

    return total;
}

bool FileSearchPath::isFileInPath (const File& fileToCheck,
                                   const bool checkRecursively) const
{
    for (auto& d : directories)
    {
        if (checkRecursively)
        {
            if (fileToCheck.isAChildOf (File (d)))
                return true;
        }
        else
        {
            if (fileToCheck.getParentDirectory() == File (d))
                return true;
        }
    }

    return false;
}

} // namespace juce
