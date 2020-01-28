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

int64 juce_fileSetPosition (void* handle, int64 pos);


//==============================================================================
FileInputStream::FileInputStream (const File& f)
    : file (f),
      fileHandle (nullptr),
      currentPosition (0),
      status (Result::ok())
{
    openHandle();
}

int64 FileInputStream::getTotalLength()
{
    // You should always check that a stream opened successfully before using it!
    jassert (openedOk());

    return file.getSize();
}

int FileInputStream::read (void* buffer, int bytesToRead)
{
    // You should always check that a stream opened successfully before using it!
    jassert (openedOk());

    // The buffer should never be null, and a negative size is probably a
    // sign that something is broken!
    jassert (buffer != nullptr && bytesToRead >= 0);

    const size_t num = readInternal (buffer, (size_t) bytesToRead);
    currentPosition += (int64) num;

    return (int) num;
}

bool FileInputStream::isExhausted()
{
    return currentPosition >= getTotalLength();
}

int64 FileInputStream::getPosition()
{
    return currentPosition;
}

bool FileInputStream::setPosition (int64 pos)
{
    // You should always check that a stream opened successfully before using it!
    jassert (openedOk());

    if (pos != currentPosition)
        currentPosition = juce_fileSetPosition (fileHandle, pos);

    return currentPosition == pos;
}

} // namespace juce
