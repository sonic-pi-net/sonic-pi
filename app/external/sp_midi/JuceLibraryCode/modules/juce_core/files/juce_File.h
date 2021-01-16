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

#if ! DOXYGEN && (JUCE_MAC || JUCE_IOS)
 #if __LP64__
  using OSType = unsigned int;
 #else
  using OSType = unsigned long;
 #endif
#endif

namespace juce
{

//==============================================================================
/**
    Represents a local file or directory.

    This class encapsulates the absolute pathname of a file or directory, and
    has methods for finding out about the file and changing its properties.

    To read or write to the file, there are methods for returning an input or
    output stream.

    @see FileInputStream, FileOutputStream

    @tags{Core}
*/
class JUCE_API  File final
{
public:
    //==============================================================================
    /** Creates an (invalid) file object.

        The file is initially set to an empty path, so getFullPathName() will return
        an empty string.

        You can use its operator= method to point it at a proper file.
    */
    File() = default;

    /** Creates a file from an absolute path.

        If the path supplied is a relative path, it is taken to be relative
        to the current working directory (see File::getCurrentWorkingDirectory()),
        but this isn't a recommended way of creating a file, because you
        never know what the CWD is going to be.

        On the Mac/Linux, the path can include "~" notation for referring to
        user home directories.
    */
    File (const String& absolutePath);

    /** Creates a copy of another file object. */
    File (const File&);

    /** Destructor. */
    ~File() = default;

    /** Sets the file based on an absolute pathname.

        If the path supplied is a relative path, it is taken to be relative
        to the current working directory (see File::getCurrentWorkingDirectory()),
        but this isn't a recommended way of creating a file, because you
        never know what the CWD is going to be.

        On the Mac/Linux, the path can include "~" notation for referring to
        user home directories.
    */
    File& operator= (const String& newAbsolutePath);

    /** Copies from another file object. */
    File& operator= (const File& otherFile);

    /** Move constructor */
    File (File&&) noexcept;

    /** Move assignment operator */
    File& operator= (File&&) noexcept;

    //==============================================================================
    /** Checks whether the file actually exists.

        @returns    true if the file exists, either as a file or a directory.
        @see existsAsFile, isDirectory
    */
    bool exists() const;

    /** Checks whether the file exists and is a file rather than a directory.

        @returns    true only if this is a real file, false if it's a directory
                    or doesn't exist
        @see exists, isDirectory
    */
    bool existsAsFile() const;

    /** Checks whether the file is a directory that exists.

        @returns    true only if the file is a directory which actually exists, so
                    false if it's a file or doesn't exist at all
        @see exists, existsAsFile
    */
    bool isDirectory() const;

    /** Checks whether the path of this file represents the root of a file system,
        irrespective of its existence.

        This will return true for "C:", "D:", etc on Windows and "/" on other
        platforms.
    */
    bool isRoot() const;

    /** Returns the size of the file in bytes.

        @returns    the number of bytes in the file, or 0 if it doesn't exist.
    */
    int64 getSize() const;

    /** Utility function to convert a file size in bytes to a neat string description.

        So for example 100 would return "100 bytes", 2000 would return "2 KB",
        2000000 would produce "2 MB", etc.
    */
    static String descriptionOfSizeInBytes (int64 bytes);

    //==============================================================================
    /** Returns the complete, absolute path of this file.

        This includes the filename and all its parent folders. On Windows it'll
        also include the drive letter prefix; on Mac or Linux it'll be a complete
        path starting from the root folder.

        If you just want the file's name, you should use getFileName() or
        getFileNameWithoutExtension().

        @see getFileName, getRelativePathFrom
    */
    const String& getFullPathName() const noexcept          { return fullPath; }

    /** Returns the last section of the pathname.

        Returns just the final part of the path - e.g. if the whole path
        is "/moose/fish/foo.txt" this will return "foo.txt".

        For a directory, it returns the final part of the path - e.g. for the
        directory "/moose/fish" it'll return "fish".

        If the filename begins with a dot, it'll return the whole filename, e.g. for
        "/moose/.fish", it'll return ".fish"

        @see getFullPathName, getFileNameWithoutExtension
    */
    String getFileName() const;

    /** Creates a relative path that refers to a file relatively to a given directory.

        e.g. File ("/moose/foo.txt").getRelativePathFrom (File ("/moose/fish/haddock"))
             would return "../../foo.txt".

        If it's not possible to navigate from one file to the other, an absolute
        path is returned. If the paths are invalid, an empty string may also be
        returned.

        @param directoryToBeRelativeTo  the directory which the resultant string will
                                        be relative to. If this is actually a file rather than
                                        a directory, its parent directory will be used instead.
                                        If it doesn't exist, it's assumed to be a directory.
        @see getChildFile, isAbsolutePath
    */
    String getRelativePathFrom (const File& directoryToBeRelativeTo) const;

    //==============================================================================
    /** Returns the file's extension.

        Returns the file extension of this file, also including the dot.

        e.g. "/moose/fish/foo.txt" would return ".txt"

        @see hasFileExtension, withFileExtension, getFileNameWithoutExtension
    */
    String getFileExtension() const;

    /** Checks whether the file has a given extension.

        @param extensionToTest  the extension to look for - it doesn't matter whether or
                                not this string has a dot at the start, so ".wav" and "wav"
                                will have the same effect. To compare with multiple extensions, this
                                parameter can contain multiple strings, separated by semi-colons -
                                so, for example: hasFileExtension (".jpeg;png;gif") would return
                                true if the file has any of those three extensions.

        @see getFileExtension, withFileExtension, getFileNameWithoutExtension
    */
    bool hasFileExtension (StringRef extensionToTest) const;

    /** Returns a version of this file with a different file extension.

        e.g. File ("/moose/fish/foo.txt").withFileExtension ("html") returns "/moose/fish/foo.html"

        @param newExtension     the new extension, either with or without a dot at the start (this
                                doesn't make any difference). To get remove a file's extension altogether,
                                pass an empty string into this function.

        @see getFileName, getFileExtension, hasFileExtension, getFileNameWithoutExtension
    */
    File withFileExtension (StringRef newExtension) const;

    /** Returns the last part of the filename, without its file extension.

        e.g. for "/moose/fish/foo.txt" this will return "foo".

        @see getFileName, getFileExtension, hasFileExtension, withFileExtension
    */
    String getFileNameWithoutExtension() const;

    //==============================================================================
    /** Returns a 32-bit hash-code that identifies this file.

        This is based on the filename. Obviously it's possible, although unlikely, that
        two files will have the same hash-code.
    */
    int hashCode() const;

    /** Returns a 64-bit hash-code that identifies this file.

        This is based on the filename. Obviously it's possible, although unlikely, that
        two files will have the same hash-code.
    */
    int64 hashCode64() const;

    //==============================================================================
    /** Returns a file that represents a relative (or absolute) sub-path of the current one.

        This will find a child file or directory of the current object.

        e.g.
            File ("/moose/fish").getChildFile ("foo.txt") will produce "/moose/fish/foo.txt".
            File ("/moose/fish").getChildFile ("haddock/foo.txt") will produce "/moose/fish/haddock/foo.txt".
            File ("/moose/fish").getChildFile ("../foo.txt") will produce "/moose/foo.txt".

        If the string is actually an absolute path, it will be treated as such, e.g.
            File ("/moose/fish").getChildFile ("/foo.txt") will produce "/foo.txt"

        @see getSiblingFile, getParentDirectory, getRelativePathFrom, isAChildOf
    */
    File getChildFile (StringRef relativeOrAbsolutePath) const;

    /** Returns a file which is in the same directory as this one.

        This is equivalent to getParentDirectory().getChildFile (name).

        @see getChildFile, getParentDirectory
    */
    File getSiblingFile (StringRef siblingFileName) const;

    //==============================================================================
    /** Returns the directory that contains this file or directory.

        e.g. for "/moose/fish/foo.txt" this will return "/moose/fish".

        If you are already at the root directory ("/" or "C:") then this method will
        return the root directory.
    */
    File getParentDirectory() const;

    /** Checks whether a file is somewhere inside a directory.

        Returns true if this file is somewhere inside a subdirectory of the directory
        that is passed in. Neither file actually has to exist, because the function
        just checks the paths for similarities.

        e.g. File ("/moose/fish/foo.txt").isAChildOf ("/moose") is true.
             File ("/moose/fish/foo.txt").isAChildOf ("/moose/fish") is also true.
    */
    bool isAChildOf (const File& potentialParentDirectory) const;

    //==============================================================================
    /** Chooses a filename relative to this one that doesn't already exist.

        If this file is a directory, this will return a child file of this
        directory that doesn't exist, by adding numbers to a prefix and suffix until
        it finds one that isn't already there.

        If the prefix + the suffix doesn't exist, it won't bother adding a number.

        e.g. File ("/moose/fish").getNonexistentChildFile ("foo", ".txt", true) might
             return "/moose/fish/foo(2).txt" if there's already a file called "foo.txt".

        @param prefix                   the string to use for the filename before the number
        @param suffix                   the string to add to the filename after the number
        @param putNumbersInBrackets     if true, this will create filenames in the
                                        format "prefix(number)suffix", if false, it will leave the
                                        brackets out.
    */
    File getNonexistentChildFile (const String& prefix,
                                  const String& suffix,
                                  bool putNumbersInBrackets = true) const;

    /** Chooses a filename for a sibling file to this one that doesn't already exist.

        If this file doesn't exist, this will just return itself, otherwise it
        will return an appropriate sibling that doesn't exist, e.g. if a file
        "/moose/fish/foo.txt" exists, this might return "/moose/fish/foo(2).txt".

        @param putNumbersInBrackets     whether to add brackets around the numbers that
                                        get appended to the new filename.
    */
    File getNonexistentSibling (bool putNumbersInBrackets = true) const;

    //==============================================================================
    /** Compares the pathnames for two files. */
    bool operator== (const File&) const;
    /** Compares the pathnames for two files. */
    bool operator!= (const File&) const;
    /** Compares the pathnames for two files. */
    bool operator< (const File&) const;
    /** Compares the pathnames for two files. */
    bool operator> (const File&) const;

    //==============================================================================
    /** Checks whether a file can be created or written to.

        @returns    true if it's possible to create and write to this file. If the file
                    doesn't already exist, this will check its parent directory to
                    see if writing is allowed.
        @see setReadOnly
    */
    bool hasWriteAccess() const;

    /** Changes the write-permission of a file or directory.

        @param shouldBeReadOnly     whether to add or remove write-permission
        @param applyRecursively     if the file is a directory and this is true, it will
                                    recurse through all the subfolders changing the permissions
                                    of all files
        @returns    true if it manages to change the file's permissions.
        @see hasWriteAccess
    */
    bool setReadOnly (bool shouldBeReadOnly,
                      bool applyRecursively = false) const;

    /** Changes the execute-permissions of a file.

        @param shouldBeExecutable   whether to add or remove execute-permission
        @returns    true if it manages to change the file's permissions.
    */
    bool setExecutePermission (bool shouldBeExecutable) const;

    /** Returns true if this file is a hidden or system file.
        The criteria for deciding whether a file is hidden are platform-dependent.
    */
    bool isHidden() const;

    /** Returns a unique identifier for the file, if one is available.

        Depending on the OS and file-system, this may be a unix inode number or
        a win32 file identifier, or 0 if it fails to find one. The number will
        be unique on the filesystem, but not globally.
    */
    uint64 getFileIdentifier() const;

    //==============================================================================
    /** Returns the last modification time of this file.

        @returns    the time, or an invalid time if the file doesn't exist.
        @see setLastModificationTime, getLastAccessTime, getCreationTime
    */
    Time getLastModificationTime() const;

    /** Returns the last time this file was accessed.

        @returns    the time, or an invalid time if the file doesn't exist.
        @see setLastAccessTime, getLastModificationTime, getCreationTime
    */
    Time getLastAccessTime() const;

    /** Returns the time that this file was created.

        @returns    the time, or an invalid time if the file doesn't exist.
        @see getLastModificationTime, getLastAccessTime
    */
    Time getCreationTime() const;

    /** Changes the modification time for this file.

        @param newTime  the time to apply to the file
        @returns true if it manages to change the file's time.
        @see getLastModificationTime, setLastAccessTime, setCreationTime
    */
    bool setLastModificationTime (Time newTime) const;

    /** Changes the last-access time for this file.

        @param newTime  the time to apply to the file
        @returns true if it manages to change the file's time.
        @see getLastAccessTime, setLastModificationTime, setCreationTime
    */
    bool setLastAccessTime (Time newTime) const;

    /** Changes the creation date for this file.

        @param newTime  the time to apply to the file
        @returns true if it manages to change the file's time.
        @see getCreationTime, setLastModificationTime, setLastAccessTime
    */
    bool setCreationTime (Time newTime) const;

    /** If possible, this will try to create a version string for the given file.

        The OS may be able to look at the file and give a version for it - e.g. with
        executables, bundles, dlls, etc. If no version is available, this will
        return an empty string.
    */
    String getVersion() const;

    //==============================================================================
    /** Creates an empty file if it doesn't already exist.

        If the file that this object refers to doesn't exist, this will create a file
        of zero size.

        If it already exists or is a directory, this method will do nothing.

        If the parent directories of the File do not exist then this method will
        recursively create the parent directories.

        @returns    a result to indicate whether the file was created successfully,
                    or an error message if it failed.
        @see createDirectory
    */
    Result create() const;

    /** Creates a new directory for this filename.

        This will try to create the file as a directory, and will also create
        any parent directories it needs in order to complete the operation.

        @returns    a result to indicate whether the directory was created successfully, or
                    an error message if it failed.
        @see create
    */
    Result createDirectory() const;

    /** Deletes a file.

        If this file is actually a directory, it may not be deleted correctly if it
        contains files. See deleteRecursively() as a better way of deleting directories.

        If this file is a symlink, then the symlink will be deleted and not the target
        of the symlink.

        @returns    true if the file has been successfully deleted (or if it didn't exist to
                    begin with).
        @see deleteRecursively
    */
    bool deleteFile() const;

    /** Deletes a file or directory and all its subdirectories.

        If this file is a directory, this will try to delete it and all its subfolders. If
        it's just a file, it will just try to delete the file.


        @param followSymlinks If true, then any symlink pointing to a directory will also
                              recursively delete the contents of that directory
        @returns              true if the file and all its subfolders have been successfully
                              deleted (or if it didn't exist to begin with).
        @see deleteFile
    */
    bool deleteRecursively (bool followSymlinks = false) const;

    /** Moves this file or folder to the trash.

        @returns true if the operation succeeded. It could fail if the trash is full, or
                 if the file is write-protected, so you should check the return value
                 and act appropriately.
    */
    bool moveToTrash() const;

    /** Moves or renames a file.

        Tries to move a file to a different location.
        If the target file already exists, this will attempt to delete it first, and
        will fail if this can't be done.

        Note that the destination file isn't the directory to put it in, it's the actual
        filename that you want the new file to have.

        Also note that on some OSes (e.g. Windows), moving files between different
        volumes may not be possible.

        @returns    true if the operation succeeds
    */
    bool moveFileTo (const File& targetLocation) const;

    /** Copies a file.

        Tries to copy a file to a different location.
        If the target file already exists, this will attempt to delete it first, and
        will fail if this can't be done.

        @returns    true if the operation succeeds
    */
    bool copyFileTo (const File& targetLocation) const;

    /** Replaces a file.

        Replace the file in the given location, assuming the replaced files identity.
        Depending on the file system this will preserve file attributes such as
        creation date, short file name, etc.

        If replacement succeeds the original file is deleted.

        @returns    true if the operation succeeds
    */
    bool replaceFileIn (const File& targetLocation) const;

    /** Copies a directory.

        Tries to copy an entire directory, recursively.

        If this file isn't a directory or if any target files can't be created, this
        will return false.

        @param newDirectory    the directory that this one should be copied to. Note that this
                               is the name of the actual directory to create, not the directory
                               into which the new one should be placed, so there must be enough
                               write privileges to create it if it doesn't exist. Any files inside
                               it will be overwritten by similarly named ones that are copied.
    */
    bool copyDirectoryTo (const File& newDirectory) const;

    //==============================================================================
    /** Used in file searching, to specify whether to return files, directories, or both.
    */
    enum TypesOfFileToFind
    {
        findDirectories             = 1,    /**< Use this flag to indicate that you want to find directories. */
        findFiles                   = 2,    /**< Use this flag to indicate that you want to find files. */
        findFilesAndDirectories     = 3,    /**< Use this flag to indicate that you want to find both files and directories. */
        ignoreHiddenFiles           = 4     /**< Add this flag to avoid returning any hidden files in the results. */
    };

    /** Searches this directory for files matching a wildcard pattern.

        Assuming that this file is a directory, this method will search it
        for either files or subdirectories whose names match a filename pattern.
        Note that the order in which files are returned is completely undefined!

        @param whatToLookFor            a value from the TypesOfFileToFind enum, specifying whether to
                                        return files, directories, or both. If the ignoreHiddenFiles flag
                                        is also added to this value, hidden files won't be returned
        @param searchRecursively        if true, all subdirectories will be recursed into to do
                                        an exhaustive search
        @param wildCardPattern          the filename pattern to search for, e.g. "*.txt"
        @returns                        the set of files that were found

        @see getNumberOfChildFiles, RangedDirectoryIterator
    */
    Array<File> findChildFiles (int whatToLookFor,
                                bool searchRecursively,
                                const String& wildCardPattern = "*") const;

    /** Searches inside a directory for files matching a wildcard pattern.
        Note that there's a newer, better version of this method which returns the results
        array, and in almost all cases, you should use that one instead! This one is kept around
        mainly for legacy code to use.
    */
    int findChildFiles (Array<File>& results, int whatToLookFor,
                        bool searchRecursively, const String& wildCardPattern = "*") const;

    /** Searches inside a directory and counts how many files match a wildcard pattern.

        Assuming that this file is a directory, this method will search it
        for either files or subdirectories whose names match a filename pattern,
        and will return the number of matches found.

        This isn't a recursive call, and will only search this directory, not
        its children.

        @param whatToLookFor    a value from the TypesOfFileToFind enum, specifying whether to
                                count files, directories, or both. If the ignoreHiddenFiles flag
                                is also added to this value, hidden files won't be counted
        @param wildCardPattern  the filename pattern to search for, e.g. "*.txt"
        @returns                the number of matches found

        @see findChildFiles, RangedDirectoryIterator
    */
    int getNumberOfChildFiles (int whatToLookFor,
                               const String& wildCardPattern = "*") const;

    /** Returns true if this file is a directory that contains one or more subdirectories.
        @see isDirectory, findChildFiles
    */
    bool containsSubDirectories() const;

    //==============================================================================
    /** Creates a stream to read from this file.

        Note that this is an old method, and actually it's usually best to avoid it and
        instead use an RAII pattern with an FileInputStream directly, e.g.
        @code
        FileInputStream input (fileToOpen);

        if (input.openedOk())
        {
            input.read (etc...
        }
        @endcode

        @returns    a stream that will read from this file (initially positioned at the
                    start of the file), or nullptr if the file can't be opened for some reason
        @see createOutputStream, loadFileAsData
    */
    std::unique_ptr<FileInputStream> createInputStream() const;

    /** Creates a stream to write to this file.

        Note that this is an old method, and actually it's usually best to avoid it and
        instead use an RAII pattern with an FileOutputStream directly, e.g.
        @code
        FileOutputStream output (fileToOpen);

        if (output.openedOk())
        {
            output.read etc...
        }
        @endcode

        If the file exists, the stream that is returned will be positioned ready for
        writing at the end of the file. If you want to write to the start of the file,
        replacing the existing content, then you can do the following:
        @code
        FileOutputStream output (fileToOverwrite);

        if (output.openedOk())
        {
            output.setPosition (0);
            output.truncate();
            ...
        }
        @endcode

        @returns    a stream that will write to this file (initially positioned at the
                    end of the file), or nullptr if the file can't be opened for some reason
        @see createInputStream, appendData, appendText
    */
    std::unique_ptr<FileOutputStream> createOutputStream (size_t bufferSize = 0x8000) const;

    //==============================================================================
    /** Loads a file's contents into memory as a block of binary data.

        Of course, trying to load a very large file into memory will blow up, so
        it's better to check first.

        @param result   the data block to which the file's contents should be appended - note
                        that if the memory block might already contain some data, you
                        might want to clear it first
        @returns        true if the file could all be read into memory
    */
    bool loadFileAsData (MemoryBlock& result) const;

    /** Reads a file into memory as a string.

        Attempts to load the entire file as a zero-terminated string.

        This makes use of InputStream::readEntireStreamAsString, which can
        read either UTF-16 or UTF-8 file formats.
    */
    String loadFileAsString() const;

    /** Reads the contents of this file as text and splits it into lines, which are
        appended to the given StringArray.
    */
    void readLines (StringArray& destLines) const;

    //==============================================================================
    /** Appends a block of binary data to the end of the file.

        This will try to write the given buffer to the end of the file.

        @returns false if it can't write to the file for some reason
    */
    bool appendData (const void* dataToAppend,
                     size_t numberOfBytes) const;

    /** Replaces this file's contents with a given block of data.

        This will delete the file and replace it with the given data.

        A nice feature of this method is that it's safe - instead of deleting
        the file first and then re-writing it, it creates a new temporary file,
        writes the data to that, and then moves the new file to replace the existing
        file. This means that if the power gets pulled out or something crashes,
        you're a lot less likely to end up with a corrupted or unfinished file..

        Returns true if the operation succeeds, or false if it fails.

        @see appendText
    */
    bool replaceWithData (const void* dataToWrite,
                          size_t numberOfBytes) const;

    /** Appends a string to the end of the file.

        This will try to append a text string to the file, as either 16-bit unicode
        or 8-bit characters in the default system encoding.

        It can also write the 'ff fe' unicode header bytes before the text to indicate
        the endianness of the file.

        If lineEndings is nullptr, then line endings in the text won't be modified. If you
        pass "\\n" or "\\r\\n" then this function will replace any existing line feeds.

        @see replaceWithText
    */
    bool appendText (const String& textToAppend,
                     bool asUnicode = false,
                     bool writeUnicodeHeaderBytes = false,
                     const char* lineEndings = "\r\n") const;

    /** Replaces this file's contents with a given text string.

        This will delete the file and replace it with the given text.

        A nice feature of this method is that it's safe - instead of deleting
        the file first and then re-writing it, it creates a new temporary file,
        writes the text to that, and then moves the new file to replace the existing
        file. This means that if the power gets pulled out or something crashes,
        you're a lot less likely to end up with an empty file..

        For an explanation of the parameters here, see the appendText() method.

        Returns true if the operation succeeds, or false if it fails.

        @see appendText
    */
    bool replaceWithText (const String& textToWrite,
                          bool asUnicode = false,
                          bool writeUnicodeHeaderBytes = false,
                          const char* lineEndings = "\r\n") const;

    /** Attempts to scan the contents of this file and compare it to another file, returning
        true if this is possible and they match byte-for-byte.
    */
    bool hasIdenticalContentTo (const File& other) const;

    //==============================================================================
    /** Creates a set of files to represent each file root.

        e.g. on Windows this will create files for "c:\", "d:\" etc according
        to which ones are available. On the Mac/Linux, this will probably
        just add a single entry for "/".
    */
    static void findFileSystemRoots (Array<File>& results);

    /** Finds the name of the drive on which this file lives.
        @returns the volume label of the drive, or an empty string if this isn't possible
    */
    String getVolumeLabel() const;

    /** Returns the serial number of the volume on which this file lives.
        @returns the serial number, or zero if there's a problem doing this
    */
    int getVolumeSerialNumber() const;

    /** Returns the number of bytes free on the drive that this file lives on.

        @returns the number of bytes free, or 0 if there's a problem finding this out
        @see getVolumeTotalSize
    */
    int64 getBytesFreeOnVolume() const;

    /** Returns the total size of the drive that contains this file.

        @returns the total number of bytes that the volume can hold
        @see getBytesFreeOnVolume
    */
    int64 getVolumeTotalSize() const;

    /** Returns true if this file is on a CD or DVD drive. */
    bool isOnCDRomDrive() const;

    /** Returns true if this file is on a hard disk.

        This will fail if it's a network drive, but will still be true for
        removable hard-disks.
    */
    bool isOnHardDisk() const;

    /** Returns true if this file is on a removable disk drive.

        This might be a usb-drive, a CD-rom, or maybe a network drive.
    */
    bool isOnRemovableDrive() const;

    //==============================================================================
    /** Launches the file as a process.

        - if the file is executable, this will run it.

        - if it's a document of some kind, it will launch the document with its
        default viewer application.

        - if it's a folder, it will be opened in Explorer, Finder, or equivalent.

        @see revealToUser
    */
    bool startAsProcess (const String& parameters = String()) const;

    /** Opens Finder, Explorer, or whatever the OS uses, to show the user this file's location.
        @see startAsProcess
    */
    void revealToUser() const;

    //==============================================================================
    /** A set of types of location that can be passed to the getSpecialLocation() method.
    */
    enum SpecialLocationType
    {
        /** The user's home folder. This is the same as using File ("~"). */
        userHomeDirectory,

        /** The user's default documents folder. On Windows, this might be the user's
            "My Documents" folder. On the Mac it'll be their "Documents" folder. Linux
            doesn't tend to have one of these, so it might just return their home folder.
        */
        userDocumentsDirectory,

        /** The folder that contains the user's desktop objects. */
        userDesktopDirectory,

        /** The most likely place where a user might store their music files. */
        userMusicDirectory,

        /** The most likely place where a user might store their movie files. */
        userMoviesDirectory,

        /** The most likely place where a user might store their picture files. */
        userPicturesDirectory,

        /** The folder in which applications store their persistent user-specific settings.
            On Windows, this might be "\Documents and Settings\username\Application Data".
            On the Mac, it might be "~/Library". If you're going to store your settings in here,
            always create your own sub-folder to put them in, to avoid making a mess.
            On GNU/Linux it is "~/.config".
        */
        userApplicationDataDirectory,

        /** An equivalent of the userApplicationDataDirectory folder that is shared by all users
            of the computer, rather than just the current user.

            On the Mac it'll be "/Library", on Windows, it could be something like
            "\Documents and Settings\All Users\Application Data".

            On GNU/Linux it is "/opt".

            Depending on the setup, this folder may be read-only.
        */
        commonApplicationDataDirectory,

        /** A place to put documents which are shared by all users of the machine.
            On Windows this may be somewhere like "C:\Users\Public\Documents", on OSX it
            will be something like "/Users/Shared". Other OSes may have no such concept
            though, so be careful.
        */
        commonDocumentsDirectory,

        /** The folder that should be used for temporary files.
            Always delete them when you're finished, to keep the user's computer tidy!
        */
        tempDirectory,

        /** Returns this application's executable file.

            If running as a plug-in or DLL, this will (where possible) be the DLL rather than the
            host app.

            On the mac this will return the unix binary, not the package folder - see
            currentApplicationFile for that.

            See also invokedExecutableFile, which is similar, but if the exe was launched from a
            file link, invokedExecutableFile will return the name of the link.
        */
        currentExecutableFile,

        /** Returns this application's location.

            If running as a plug-in or DLL, this will (where possible) be the DLL rather than the
            host app.

            On the mac this will return the package folder (if it's in one), not the unix binary
            that's inside it - compare with currentExecutableFile.
        */
        currentApplicationFile,

        /** Returns the file that was invoked to launch this executable.
            This may differ from currentExecutableFile if the app was started from e.g. a link - this
            will return the name of the link that was used, whereas currentExecutableFile will return
            the actual location of the target executable.
        */
        invokedExecutableFile,

        /** In a plugin, this will return the path of the host executable. */
        hostApplicationPath,

       #if JUCE_WINDOWS || DOXYGEN
        /** On a Windows machine, returns the location of the Windows/System32 folder. */
        windowsSystemDirectory,
       #endif

        /** The directory in which applications normally get installed.
            So on windows, this would be something like "C:\Program Files", on the
            Mac "/Applications", or "/usr" on linux.
        */
        globalApplicationsDirectory,

       #if JUCE_WINDOWS || DOXYGEN
        /** On a Windows machine, returns the directory in which 32 bit applications
            normally get installed. On a 64 bit machine this would be something like
            "C:\Program Files (x86)", whereas for 32 bit machines this would match
            globalApplicationsDirectory and be something like "C:\Program Files".

            @see globalApplicationsDirectory
        */
        globalApplicationsDirectoryX86
       #endif
    };

    /** Finds the location of a special type of file or directory, such as a home folder or
        documents folder.

        @see SpecialLocationType
    */
    static File JUCE_CALLTYPE getSpecialLocation (const SpecialLocationType type);

    //==============================================================================
    /** Returns a temporary file in the system's temp directory.
        This will try to return the name of a non-existent temp file.
        To get the temp folder, you can use getSpecialLocation (File::tempDirectory).
    */
    static File createTempFile (StringRef fileNameEnding);

    //==============================================================================
    /** Returns the current working directory.
        @see setAsCurrentWorkingDirectory
    */
    static File getCurrentWorkingDirectory();

    /** Sets the current working directory to be this file.

        For this to work the file must point to a valid directory.

        @returns true if the current directory has been changed.
        @see getCurrentWorkingDirectory
    */
    bool setAsCurrentWorkingDirectory() const;

    //==============================================================================
    /** The system-specific file separator character.
        On Windows, this will be '\', on Mac/Linux, it'll be '/'
    */
    static juce_wchar getSeparatorChar();

    /** The system-specific file separator character, as a string.
        On Windows, this will be '\', on Mac/Linux, it'll be '/'
    */
    static StringRef getSeparatorString();

    //==============================================================================
    /** Returns a version of a filename with any illegal characters removed.

        This will return a copy of the given string after removing characters
        that are not allowed in a legal filename, and possibly shortening the
        string if it's too long.

        Because this will remove slashes, don't use it on an absolute pathname - use
        createLegalPathName() for that.

        @see createLegalPathName
    */
    static String createLegalFileName (const String& fileNameToFix);

    /** Returns a version of a path with any illegal characters removed.

        Similar to createLegalFileName(), but this won't remove slashes, so can
        be used on a complete pathname.

        @see createLegalFileName
    */
    static String createLegalPathName (const String& pathNameToFix);

    /** Indicates whether filenames are case-sensitive on the current operating system. */
    static bool areFileNamesCaseSensitive();

    /** Returns true if the string seems to be a fully-specified absolute path. */
    static bool isAbsolutePath (StringRef path);

    /** Creates a file that simply contains this string, without doing the sanity-checking
        that the normal constructors do.

        Best to avoid this unless you really know what you're doing.
    */
    static File createFileWithoutCheckingPath (const String& absolutePath) noexcept;

    /** Adds a separator character to the end of a path if it doesn't already have one. */
    static String addTrailingSeparator (const String& path);

    //==============================================================================
    /** Tries to create a symbolic link and returns a boolean to indicate success */
    bool createSymbolicLink (const File& linkFileToCreate, bool overwriteExisting) const;

    /** Returns true if this file is a link or alias that can be followed using getLinkedTarget(). */
    bool isSymbolicLink() const;

    /** If this file is a link or alias, this returns the file that it points to.
        If the file isn't actually link, it'll just return itself.
    */
    File getLinkedTarget() const;

    /** Create a symbolic link to a native path and return a boolean to indicate success.

        Use this method if you want to create a link to a relative path or a special native
        file path (such as a device file on Windows).
    */
    static bool createSymbolicLink (const File& linkFileToCreate,
                                    const String& nativePathOfTarget,
                                    bool overwriteExisting);

    /** This returns the native path that the symbolic link points to. The returned path
        is a native path of the current OS and can be a relative, absolute or special path. */
    String getNativeLinkedTarget() const;

   #if JUCE_WINDOWS || DOXYGEN
    /** Windows ONLY - Creates a win32 .LNK shortcut file that links to this file. */
    bool createShortcut (const String& description, const File& linkFileToCreate) const;

    /** Windows ONLY - Returns true if this is a win32 .LNK file. */
    bool isShortcut() const;
   #else

   #endif

    //==============================================================================
   #if JUCE_MAC || JUCE_IOS || DOXYGEN
    /** OSX ONLY - Finds the OSType of a file from the its resources. */
    OSType getMacOSType() const;

    /** OSX ONLY - Returns true if this file is actually a bundle. */
    bool isBundle() const;
   #endif

   #if JUCE_MAC || DOXYGEN
    /** OSX ONLY - Adds this file to the OSX dock */
    void addToDock() const;
   #endif

    //==============================================================================
    /** Comparator for files */
    struct NaturalFileComparator
    {
        NaturalFileComparator (bool shouldPutFoldersFirst) noexcept : foldersFirst (shouldPutFoldersFirst) {}

        int compareElements (const File& firstFile, const File& secondFile) const
        {
            if (foldersFirst && (firstFile.isDirectory() != secondFile.isDirectory()))
                return firstFile.isDirectory() ? -1 : 1;

           #if NAMES_ARE_CASE_SENSITIVE
            return firstFile.getFullPathName().compareNatural (secondFile.getFullPathName(), true);
           #else
            return firstFile.getFullPathName().compareNatural (secondFile.getFullPathName(), false);
           #endif
        }

        bool foldersFirst;
    };

    /* These static objects are deprecated because it's too easy to accidentally use them indirectly
       during a static constructor, which leads to very obscure order-of-initialisation bugs.
       Use File::getSeparatorChar() and File::getSeparatorString(), and instead of File::nonexistent,
       just use File() or {}.
    */
    JUCE_DEPRECATED_STATIC (static const juce_wchar separator;)
    JUCE_DEPRECATED_STATIC (static const StringRef separatorString;)
    JUCE_DEPRECATED_STATIC (static const File nonexistent;)

private:
    //==============================================================================
    String fullPath;

    static String parseAbsolutePath (const String&);
    String getPathUpToLastSlash() const;

    Result createDirectoryInternal (const String&) const;
    bool copyInternal (const File&) const;
    bool moveInternal (const File&) const;
    bool replaceInternal (const File&) const;
    bool setFileTimesInternal (int64 m, int64 a, int64 c) const;
    void getFileTimesInternal (int64& m, int64& a, int64& c) const;
    bool setFileReadOnlyInternal (bool) const;
    bool setFileExecutableInternal (bool) const;
};

} // namespace juce
