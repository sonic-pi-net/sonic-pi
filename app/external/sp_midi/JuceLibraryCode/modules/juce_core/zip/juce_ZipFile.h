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
    Decodes a ZIP file from a stream.

    This can enumerate the items in a ZIP file and can create suitable stream objects
    to read each one.

    @tags{Core}
*/
class JUCE_API  ZipFile
{
public:
    /** Creates a ZipFile to read a specific file. */
    explicit ZipFile (const File& file);

    //==============================================================================
    /** Creates a ZipFile for a given stream.

        @param inputStream                  the stream to read from
        @param deleteStreamWhenDestroyed    if set to true, the object passed-in
                                            will be deleted when this ZipFile object is deleted
    */
    ZipFile (InputStream* inputStream, bool deleteStreamWhenDestroyed);

    /** Creates a ZipFile for a given stream.
        The stream will not be owned or deleted by this class - if you want the ZipFile to
        manage the stream's lifetime, use the other constructor.
    */
    explicit ZipFile (InputStream& inputStream);

    /** Creates a ZipFile for an input source.

        The inputSource object will be owned by the zip file, which will delete
        it later when not needed.
    */
    explicit ZipFile (InputSource* inputSource);

    /** Destructor. */
    ~ZipFile();

    //==============================================================================
    /**
        Contains information about one of the entries in a ZipFile.

        @see ZipFile::getEntry
    */
    struct ZipEntry
    {
        /** The name of the file, which may also include a partial pathname. */
        String filename;

        /** The file's original size. */
        int64 uncompressedSize;

        /** The last time the file was modified. */
        Time fileTime;

        /** True if the zip entry is a symbolic link. */
        bool isSymbolicLink;

        /** Platform specific data. Depending on how the zip file was created this
            may contain macOS and Linux file types, permissions and
            setuid/setgid/sticky bits.
        */
        uint32 externalFileAttributes;
    };

    //==============================================================================
    /** Returns the number of items in the zip file. */
    int getNumEntries() const noexcept;

    /** Returns a structure that describes one of the entries in the zip file.
        This may return a nullptr if the index is out of range.
        @see ZipFile::ZipEntry
    */
    const ZipEntry* getEntry (int index) const noexcept;

    /** Returns the index of the first entry with a given filename.
        This uses a case-sensitive comparison to look for a filename in the
        list of entries. It might return -1 if no match is found.

        @see ZipFile::ZipEntry
    */
    int getIndexOfFileName (const String& fileName, bool ignoreCase = false) const noexcept;

    /** Returns a structure that describes one of the entries in the zip file.

        This uses a case-sensitive comparison to look for a filename in the
        list of entries. It might return 0 if no match is found.

        @see ZipFile::ZipEntry
    */
    const ZipEntry* getEntry (const String& fileName, bool ignoreCase = false) const noexcept;

    /** Sorts the list of entries, based on the filename. */
    void sortEntriesByFilename();

    //==============================================================================
    /** Creates a stream that can read from one of the zip file's entries.

        The stream that is returned must be deleted by the caller (and
        a nullptr might be returned if a stream can't be opened for some reason).

        The stream must not be used after the ZipFile object that created
        has been deleted.

        Note that if the ZipFile was created with a user-supplied InputStream object,
        then all the streams which are created by this method will by trying to share
        the same source stream, so cannot be safely used on  multiple threads! (But if
        you create the ZipFile from a File or InputSource, then it is safe to do this).
    */
    InputStream* createStreamForEntry (int index);

    /** Creates a stream that can read from one of the zip file's entries.

        The stream that is returned must be deleted by the caller (and
        a nullptr might be returned if a stream can't be opened for some reason).

        The stream must not be used after the ZipFile object that created
        has been deleted.

        Note that if the ZipFile was created with a user-supplied InputStream object,
        then all the streams which are created by this method will by trying to share
        the same source stream, so cannot be safely used on  multiple threads! (But if
        you create the ZipFile from a File or InputSource, then it is safe to do this).
    */
    InputStream* createStreamForEntry (const ZipEntry& entry);

    //==============================================================================
    /** Uncompresses all of the files in the zip file.

        This will expand all the entries into a target directory. The relative
        paths of the entries are used.

        @param targetDirectory      the root folder to uncompress to
        @param shouldOverwriteFiles whether to overwrite existing files with similarly-named ones
        @returns success if the file is successfully unzipped
    */
    Result uncompressTo (const File& targetDirectory,
                         bool shouldOverwriteFiles = true);

    /** Uncompresses one of the entries from the zip file.

        This will expand the entry and write it in a target directory. The entry's path is used to
        determine which subfolder of the target should contain the new file.

        @param index                the index of the entry to uncompress - this must be a valid index
                                    between 0 and (getNumEntries() - 1).
        @param targetDirectory      the root folder to uncompress into
        @param shouldOverwriteFiles whether to overwrite existing files with similarly-named ones
        @returns success if all the files are successfully unzipped
    */
    Result uncompressEntry (int index,
                            const File& targetDirectory,
                            bool shouldOverwriteFiles = true);


    //==============================================================================
    /** Used to create a new zip file.

        Create a ZipFile::Builder object, and call its addFile() method to add some files,
        then you can write it to a stream with write().
    */
    class JUCE_API  Builder
    {
    public:
        /** Creates an empty builder object. */
        Builder();

        /** Destructor. */
        ~Builder();

        /** Adds a file to the list of items which will be added to the archive.
            The file isn't read immediately: the files will be read later when the writeToStream()
            method is called.

            The compressionLevel can be between 0 (no compression), and 9 (maximum compression).
            If the storedPathName parameter is specified, you can customise the partial pathname that
            will be stored for this file.
        */
        void addFile (const File& fileToAdd, int compressionLevel,
                      const String& storedPathName = String());

        /** Adds a stream to the list of items which will be added to the archive.

            @param streamToRead this stream isn't read immediately - a pointer to the stream is
                                stored, then used later when the writeToStream() method is called, and
                                deleted by the Builder object when no longer needed, so be very careful
                                about its lifetime and the lifetime of any objects on which it depends!
                                This must not be null.
            @param compressionLevel     this can be between 0 (no compression), and 9 (maximum compression).
            @param storedPathName       the partial pathname that will be stored for this file
            @param fileModificationTime the timestamp that will be stored as the last modification time
                                        of this entry
        */
        void addEntry (InputStream* streamToRead, int compressionLevel,
                       const String& storedPathName, Time fileModificationTime);

        /** Generates the zip file, writing it to the specified stream.
            If the progress parameter is non-null, it will be updated with an approximate
            progress status between 0 and 1.0
        */
        bool writeToStream (OutputStream& target, double* progress) const;

        //==============================================================================
    private:
        struct Item;
        OwnedArray<Item> items;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Builder)
    };

private:
    //==============================================================================
    struct ZipInputStream;
    struct ZipEntryHolder;

    OwnedArray<ZipEntryHolder> entries;
    CriticalSection lock;
    InputStream* inputStream = nullptr;
    std::unique_ptr<InputStream> streamToDelete;
    std::unique_ptr<InputSource> inputSource;

   #if JUCE_DEBUG
    struct OpenStreamCounter
    {
        OpenStreamCounter() = default;
        ~OpenStreamCounter();

        int numOpenStreams = 0;
    };

    OpenStreamCounter streamCounter;
   #endif

    void init();

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ZipFile)
};

} // namespace juce
