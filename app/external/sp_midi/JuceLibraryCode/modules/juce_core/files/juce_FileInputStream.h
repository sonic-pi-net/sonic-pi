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
    An input stream that reads from a local file.

    @see InputStream, FileOutputStream, File::createInputStream

    @tags{Core}
*/
class JUCE_API  FileInputStream  : public InputStream
{
public:
    //==============================================================================
    /** Creates a FileInputStream to read from the given file.

        After creating a FileInputStream, you should use openedOk() or failedToOpen()
        to make sure that it's OK before trying to read from it! If it failed, you
        can call getStatus() to get more error information.
    */
    explicit FileInputStream (const File& fileToRead);

    /** Destructor. */
    ~FileInputStream() override;

    //==============================================================================
    /** Returns the file that this stream is reading from. */
    const File& getFile() const noexcept                { return file; }

    /** Returns the status of the file stream.
        The result will be ok if the file opened successfully. If an error occurs while
        opening or reading from the file, this will contain an error message.
    */
    const Result& getStatus() const noexcept            { return status; }

    /** Returns true if the stream couldn't be opened for some reason.
        @see getResult()
    */
    bool failedToOpen() const noexcept                  { return status.failed(); }

    /** Returns true if the stream opened without problems.
        @see getResult()
    */
    bool openedOk() const noexcept                      { return status.wasOk(); }


    //==============================================================================
    int64 getTotalLength() override;
    int read (void*, int) override;
    bool isExhausted() override;
    int64 getPosition() override;
    bool setPosition (int64) override;

private:
    //==============================================================================
    const File file;
    void* fileHandle = nullptr;
    int64 currentPosition = 0;
    Result status { Result::ok() };

    void openHandle();
    size_t readInternal (void*, size_t);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FileInputStream)
};

} // namespace juce
