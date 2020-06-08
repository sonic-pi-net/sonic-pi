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

//==============================================================================
/**
    Manages a temporary file, which will be deleted when this object is deleted.

    This object is intended to be used as a stack based object, using its scope
    to make sure the temporary file isn't left lying around.

    For example:

    @code
    {
        File myTargetFile ("~/myfile.txt");

        // this will choose a file called something like "~/myfile_temp239348.txt"
        // which definitely doesn't exist at the time the constructor is called.
        TemporaryFile temp (myTargetFile);

        // create a stream to the temporary file, and write some data to it...
        if (auto out = std::unique_ptr<FileOutputStream> (temp.getFile().createOutputStream()))
        {
            out->write ( ...etc )
            out.reset(); // (deletes the stream)

            // ..now we've finished writing, this will rename the temp file to
            // make it replace the target file we specified above.
            bool succeeded = temp.overwriteTargetFileWithTemporary();
        }

        // ..and even if something went wrong and our overwrite failed,
        // as the TemporaryFile object goes out of scope here, it'll make sure
        // that the temp file gets deleted.
    }
    @endcode

    @see File, FileOutputStream

    @tags{Core}
*/
class JUCE_API  TemporaryFile
{
public:
    //==============================================================================
    enum OptionFlags
    {
        useHiddenFile = 1,          /**< Indicates that the temporary file should be hidden -
                                         i.e. its name should start with a dot. */
        putNumbersInBrackets = 2    /**< Indicates that when numbers are appended to make sure
                                         the file is unique, they should go in brackets rather
                                         than just being appended (see File::getNonexistentSibling() )*/
    };

    //==============================================================================
    /** Creates a randomly-named temporary file in the default temp directory.

        @param suffix       a file suffix to use for the file
        @param optionFlags  a combination of the values listed in the OptionFlags enum
        The file will not be created until you write to it. And remember that when
        this object is deleted, the file will also be deleted!
    */
    TemporaryFile (const String& suffix = String(),
                   int optionFlags = 0);

    /** Creates a temporary file in the same directory as a specified file.

        This is useful if you have a file that you want to overwrite, but don't
        want to harm the original file if the write operation fails. You can
        use this to create a temporary file next to the target file, then
        write to the temporary file, and finally use overwriteTargetFileWithTemporary()
        to replace the target file with the one you've just written.

        This class won't create any files until you actually write to them. And remember
        that when this object is deleted, the temporary file will also be deleted!

        @param targetFile   the file that you intend to overwrite - the temporary
                            file will be created in the same directory as this
        @param optionFlags  a combination of the values listed in the OptionFlags enum
    */
    TemporaryFile (const File& targetFile,
                   int optionFlags = 0);

    /** Creates a temporary file using an explicit filename.
        The other constructors are a better choice than this one, unless for some reason
        you need to explicitly specify the temporary file you want to use.

        @param targetFile    the file that you intend to overwrite
        @param temporaryFile the temporary file to be used
    */
    TemporaryFile (const File& targetFile,
                   const File& temporaryFile);

    /** Destructor.

        When this object is deleted it will make sure that its temporary file is
        also deleted! If the operation fails, it'll throw an assertion in debug
        mode.
    */
    ~TemporaryFile();

    //==============================================================================
    /** Returns the temporary file. */
    const File& getFile() const noexcept                { return temporaryFile; }

    /** Returns the target file that was specified in the constructor. */
    const File& getTargetFile() const noexcept          { return targetFile; }

    /** Tries to move the temporary file to overwrite the target file that was
        specified in the constructor.

        If you used the constructor that specified a target file, this will attempt
        to replace that file with the temporary one.

        Before calling this, make sure:
        - that you've actually written to the temporary file
        - that you've closed any open streams that you were using to write to it
        - and that you don't have any streams open to the target file, which would
          prevent it being overwritten

        If the file move succeeds, this returns true, and the temporary file will
        have disappeared. If it fails, the temporary file will probably still exist,
        but will be deleted when this object is destroyed.
    */
    bool overwriteTargetFileWithTemporary() const;

    /** Attempts to delete the temporary file, if it exists.
        @returns true if the file is successfully deleted (or if it didn't exist).
    */
    bool deleteTemporaryFile() const;


private:
    //==============================================================================
    const File temporaryFile, targetFile;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TemporaryFile)
};

} // namespace juce
