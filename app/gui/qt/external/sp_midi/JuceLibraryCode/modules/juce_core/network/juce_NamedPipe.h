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
    A cross-process pipe that can have data written to and read from it.

    Two processes can use NamedPipe objects to exchange blocks of data.

    @see InterprocessConnection

    @tags{Core}
*/
class JUCE_API  NamedPipe  final
{
public:
    //==============================================================================
    /** Creates a NamedPipe. */
    NamedPipe();

    /** Destructor. */
    ~NamedPipe();

    //==============================================================================
    /** Tries to open a pipe that already exists.
        Returns true if it succeeds.
    */
    bool openExisting (const String& pipeName);

    /** Tries to create a new pipe.
        Returns true if it succeeds.
        If mustNotExist is true then it will fail if a pipe is already
        open with the same name.
    */
    bool createNewPipe (const String& pipeName, bool mustNotExist = false);

    /** Closes the pipe, if it's open. */
    void close();

    /** True if the pipe is currently open. */
    bool isOpen() const;

    /** Returns the last name that was used to try to open this pipe. */
    String getName() const;

    //==============================================================================
    /** Reads data from the pipe.

        This will block until another thread has written enough data into the pipe to fill
        the number of bytes specified, or until another thread calls the cancelPendingReads()
        method.

        If the operation fails, it returns -1, otherwise, it will return the number of
        bytes read.

        If timeOutMilliseconds is less than zero, it will wait indefinitely, otherwise
        this is a maximum timeout for reading from the pipe.
    */
    int read (void* destBuffer, int maxBytesToRead, int timeOutMilliseconds);

    /** Writes some data to the pipe.
        @returns the number of bytes written, or -1 on failure.
    */
    int write (const void* sourceBuffer, int numBytesToWrite, int timeOutMilliseconds);

private:
    //==============================================================================
    JUCE_PUBLIC_IN_DLL_BUILD (class Pimpl)
    std::unique_ptr<Pimpl> pimpl;
    String currentPipeName;
    ReadWriteLock lock;

    bool openInternal (const String& pipeName, bool createPipe, bool mustNotExist);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (NamedPipe)
};

} // namespace juce
