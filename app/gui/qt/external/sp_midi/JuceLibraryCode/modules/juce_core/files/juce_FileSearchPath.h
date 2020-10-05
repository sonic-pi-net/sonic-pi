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
    Represents a set of folders that make up a search path.

    @see File

    @tags{Core}
*/
class JUCE_API  FileSearchPath
{
public:
    //==============================================================================
    /** Creates an empty search path. */
    FileSearchPath();

    /** Creates a search path from a string of pathnames.

        The path can be semicolon- or comma-separated, e.g.
        "/foo/bar;/foo/moose;/fish/moose"

        The separate folders are tokenised and added to the search path.
    */
    FileSearchPath (const String& path);

    /** Creates a copy of another search path. */
    FileSearchPath (const FileSearchPath&);

    /** Copies another search path. */
    FileSearchPath& operator= (const FileSearchPath&);

    /** Destructor. */
    ~FileSearchPath();

    /** Uses a string containing a list of pathnames to re-initialise this list.

        This search path is cleared and the semicolon- or comma-separated folders
        in this string are added instead. e.g. "/foo/bar;/foo/moose;/fish/moose"
    */
    FileSearchPath& operator= (const String& path);

    //==============================================================================
    /** Returns the number of folders in this search path.
        @see operator[]
    */
    int getNumPaths() const;

    /** Returns one of the folders in this search path.
        The file returned isn't guaranteed to actually be a valid directory.
        @see getNumPaths
    */
    File operator[] (int index) const;

    /** Returns the search path as a semicolon-separated list of directories. */
    String toString() const;

    //==============================================================================
    /** Adds a new directory to the search path.

        The new directory is added to the end of the list if the insertIndex parameter is
        less than zero, otherwise it is inserted at the given index.
    */
    void add (const File& directoryToAdd,
              int insertIndex = -1);

    /** Adds a new directory to the search path if it's not already in there.

        @return true if the directory has been added, false otherwise.
    */
    bool addIfNotAlreadyThere (const File& directoryToAdd);

    /** Removes a directory from the search path. */
    void remove (int indexToRemove);

    /** Merges another search path into this one.
        This will remove any duplicate directories.
    */
    void addPath (const FileSearchPath&);

    /** Removes any directories that are actually subdirectories of one of the other directories in the search path.

        If the search is intended to be recursive, there's no point having nested folders in the search
        path, because they'll just get searched twice and you'll get duplicate results.

        e.g. if the path is "c:\abc\de;c:\abc", this method will simplify it to "c:\abc"
    */
    void removeRedundantPaths();

    /** Removes any directories that don't actually exist. */
    void removeNonExistentPaths();

    //==============================================================================
    /** Searches the path for a wildcard.

        This will search all the directories in the search path in order and return
        an array of the files that were found.

        @param whatToLookFor            a value from the File::TypesOfFileToFind enum, specifying whether to
                                        return files, directories, or both.
        @param searchRecursively        whether to recursively search the subdirectories too
        @param wildCardPattern          a pattern to match against the filenames
        @returns the number of files added to the array
        @see File::findChildFiles
    */
    Array<File> findChildFiles (int whatToLookFor,
                                bool searchRecursively,
                                const String& wildCardPattern = "*") const;

    /** Searches the path for a wildcard.
        Note that there's a newer, better version of this method which returns the results
        array, and in almost all cases, you should use that one instead! This one is kept around
        mainly for legacy code to use.
    */
    int findChildFiles (Array<File>& results,
                        int whatToLookFor,
                        bool searchRecursively,
                        const String& wildCardPattern = "*") const;

    //==============================================================================
    /** Finds out whether a file is inside one of the path's directories.

        This will return true if the specified file is a child of one of the
        directories specified by this path. Note that this doesn't actually do any
        searching or check that the files exist - it just looks at the pathnames
        to work out whether the file would be inside a directory.

        @param fileToCheck      the file to look for
        @param checkRecursively if true, then this will return true if the file is inside a
                                subfolder of one of the path's directories (at any depth). If false
                                it will only return true if the file is actually a direct child
                                of one of the directories.
        @see File::isAChildOf

    */
    bool isFileInPath (const File& fileToCheck,
                       bool checkRecursively) const;

private:
    //==============================================================================
    StringArray directories;

    void init (const String&);

    JUCE_LEAK_DETECTOR (FileSearchPath)
};

} // namespace juce
