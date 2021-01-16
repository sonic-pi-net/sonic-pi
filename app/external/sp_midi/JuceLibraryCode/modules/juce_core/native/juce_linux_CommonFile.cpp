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

bool File::copyInternal (const File& dest) const
{
    FileInputStream in (*this);

    if (dest.deleteFile())
    {
        {
            FileOutputStream out (dest);

            if (out.failedToOpen())
                return false;

            if (out.writeFromInputStream (in, -1) == getSize())
                return true;
        }

        dest.deleteFile();
    }

    return false;
}

void File::findFileSystemRoots (Array<File>& destArray)
{
    destArray.add (File ("/"));
}

bool File::isHidden() const
{
    return getFileName().startsWithChar ('.');
}

bool File::isSymbolicLink() const
{
    return getNativeLinkedTarget().isNotEmpty();
}

String File::getNativeLinkedTarget() const
{
    HeapBlock<char> buffer (8194);
    const int numBytes = (int) readlink (getFullPathName().toRawUTF8(), buffer, 8192);
    return String::fromUTF8 (buffer, jmax (0, numBytes));
}

//==============================================================================
class DirectoryIterator::NativeIterator::Pimpl
{
public:
    Pimpl (const File& directory, const String& wc)
        : parentDir (File::addTrailingSeparator (directory.getFullPathName())),
          wildCard (wc), dir (opendir (directory.getFullPathName().toUTF8()))
    {
    }

    ~Pimpl()
    {
        if (dir != nullptr)
            closedir (dir);
    }

    bool next (String& filenameFound,
               bool* const isDir, bool* const isHidden, int64* const fileSize,
               Time* const modTime, Time* const creationTime, bool* const isReadOnly)
    {
        if (dir != nullptr)
        {
            const char* wildcardUTF8 = nullptr;

            for (;;)
            {
                struct dirent* const de = readdir (dir);

                if (de == nullptr)
                    break;

                if (wildcardUTF8 == nullptr)
                    wildcardUTF8 = wildCard.toUTF8();

                if (fnmatch (wildcardUTF8, de->d_name, FNM_CASEFOLD) == 0)
                {
                    filenameFound = CharPointer_UTF8 (de->d_name);

                    updateStatInfoForFile (parentDir + filenameFound, isDir, fileSize, modTime, creationTime, isReadOnly);

                    if (isHidden != nullptr)
                        *isHidden = filenameFound.startsWithChar ('.');

                    return true;
                }
            }
        }

        return false;
    }

private:
    String parentDir, wildCard;
    DIR* dir;

    JUCE_DECLARE_NON_COPYABLE (Pimpl)
};

DirectoryIterator::NativeIterator::NativeIterator (const File& directory, const String& wildCardStr)
    : pimpl (new DirectoryIterator::NativeIterator::Pimpl (directory, wildCardStr))
{
}

DirectoryIterator::NativeIterator::~NativeIterator() {}

bool DirectoryIterator::NativeIterator::next (String& filenameFound,
                                              bool* isDir, bool* isHidden, int64* fileSize,
                                              Time* modTime, Time* creationTime, bool* isReadOnly)
{
    return pimpl->next (filenameFound, isDir, isHidden, fileSize, modTime, creationTime, isReadOnly);
}

} // namespace juce
