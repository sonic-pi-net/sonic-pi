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

//==============================================================================
/**
    Describes the attributes of a file or folder.

    @tags{Core}
*/
class DirectoryEntry final
{
public:
    /** The path to a file or folder. */
    File getFile()              const { return file; }

    /** The time at which the item was last modified. */
    Time getModificationTime()  const { return modTime; }

    /** The time at which the item was created. */
    Time getCreationTime()      const { return creationTime; }

    /** The size of the item. */
    int64 getFileSize()         const { return fileSize; }

    /** True if the item is a directory, false otherwise. */
    bool isDirectory()          const { return directory; }

    /** True if the item is hidden, false otherwise. */
    bool isHidden()             const { return hidden; }

    /** True if the item is read-only, false otherwise. */
    bool isReadOnly()           const { return readOnly; }

    /** The estimated proportion of the range that has been visited
        by the iterator, from 0.0 to 1.0.
    */
    float getEstimatedProgress() const;

private:
    std::weak_ptr<DirectoryIterator> iterator;
    File file;
    Time modTime;
    Time creationTime;
    int64 fileSize  = 0;
    bool directory  = false;
    bool hidden     = false;
    bool readOnly   = false;

    friend class RangedDirectoryIterator;
};

/** A convenience operator so that the expression `*it++` works correctly when
    `it` is an instance of RangedDirectoryIterator.
*/
inline const DirectoryEntry& operator* (const DirectoryEntry& e) noexcept { return e; }

//==============================================================================
/**
    Allows iterating over files and folders using C++11 range-for syntax.

    In the following example, we recursively find all hidden files in a
    specific directory.

    @code
    std::vector<File> hiddenFiles;

    for (DirectoryEntry entry : RangedDirectoryIterator (File ("/path/to/folder"), isRecursive))
        if (entry.isHidden())
            hiddenFiles.push_back (entry.getFile());
    @endcode

    @tags{Core}
*/
class RangedDirectoryIterator final
{
public:
    using difference_type   = std::ptrdiff_t;
    using value_type        = DirectoryEntry;
    using reference         = DirectoryEntry;
    using pointer           = void;
    using iterator_category = std::input_iterator_tag;

    /** The default-constructed iterator acts as the 'end' sentinel. */
    RangedDirectoryIterator() = default;

    /** Creates a RangedDirectoryIterator for a given directory.

        The resulting iterator can be used directly in a 'range-for' expression.

        @param directory        the directory to search in
        @param isRecursive      whether all the subdirectories should also be searched
        @param wildCard         the file pattern to match. This may contain multiple patterns
                                separated by a semi-colon or comma, e.g. "*.jpg;*.png"
        @param whatToLookFor    a value from the File::TypesOfFileToFind enum, specifying
                                whether to look for files, directories, or both.
    */
    RangedDirectoryIterator (const File& directory,
                             bool isRecursive,
                             const String& wildCard = "*",
                             int whatToLookFor = File::findFiles);

    /** Returns true if both iterators are in their end/sentinel state,
        otherwise returns false.
    */
    bool operator== (const RangedDirectoryIterator& other) const noexcept
    {
        return iterator == nullptr && other.iterator == nullptr;
    }

    /** Returns the inverse of operator== */
    bool operator!= (const RangedDirectoryIterator& other) const noexcept
    {
        return ! operator== (other);
    }

    /** Return an object containing metadata about the file or folder to
        which the iterator is currently pointing.
    */
    const DirectoryEntry& operator* () const noexcept { return  entry; }
    const DirectoryEntry* operator->() const noexcept { return &entry; }

    /** Moves the iterator along to the next file. */
    RangedDirectoryIterator& operator++()
    {
        increment();
        return *this;
    }

    /** Moves the iterator along to the next file.

        @returns an object containing metadata about the file or folder to
                 to which the iterator was previously pointing.
    */
    DirectoryEntry operator++ (int)
    {
        auto result = *(*this);
        ++(*this);
        return result;
    }

private:
    bool next();
    void increment();

    std::shared_ptr<DirectoryIterator> iterator;
    DirectoryEntry entry;
};

/** Returns the iterator that was passed in.
    Provided for range-for compatibility.
*/
inline RangedDirectoryIterator begin (const RangedDirectoryIterator& it) { return it; }

/** Returns a default-constructed sentinel value.
    Provided for range-for compatibility.
*/
inline RangedDirectoryIterator end   (const RangedDirectoryIterator&)    { return {}; }

} // namespace juce
