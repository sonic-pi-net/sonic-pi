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

static inline int calcBufferStreamBufferSize (int requestedSize, InputStream* source) noexcept
{
    // You need to supply a real stream when creating a BufferedInputStream
    jassert (source != nullptr);

    requestedSize = jmax (256, requestedSize);
    auto sourceSize = source->getTotalLength();

    if (sourceSize >= 0 && sourceSize < requestedSize)
        return jmax (32, (int) sourceSize);

    return requestedSize;
}

//==============================================================================
BufferedInputStream::BufferedInputStream (InputStream* sourceStream, int size, bool takeOwnership)
   : source (sourceStream, takeOwnership),
     bufferSize (calcBufferStreamBufferSize (size, sourceStream)),
     position (sourceStream->getPosition()),
     bufferStart (position)
{
    buffer.malloc (bufferSize);
}

BufferedInputStream::BufferedInputStream (InputStream& sourceStream, int size)
   : BufferedInputStream (&sourceStream, size, false)
{
}

BufferedInputStream::~BufferedInputStream()
{
}

//==============================================================================
char BufferedInputStream::peekByte()
{
    ensureBuffered();
    return position < lastReadPos ? *(buffer + (int) (position - bufferStart)) : 0;
}

int64 BufferedInputStream::getTotalLength()
{
    return source->getTotalLength();
}

int64 BufferedInputStream::getPosition()
{
    return position;
}

bool BufferedInputStream::setPosition (int64 newPosition)
{
    position = jmax ((int64) 0, newPosition);
    return true;
}

bool BufferedInputStream::isExhausted()
{
    return position >= lastReadPos && source->isExhausted();
}

void BufferedInputStream::ensureBuffered()
{
    auto bufferEndOverlap = lastReadPos - bufferOverlap;

    if (position < bufferStart || position >= bufferEndOverlap)
    {
        int bytesRead;

        if (position < lastReadPos
             && position >= bufferEndOverlap
             && position >= bufferStart)
        {
            auto bytesToKeep = (int) (lastReadPos - position);
            memmove (buffer, buffer + (int) (position - bufferStart), (size_t) bytesToKeep);

            bufferStart = position;

            bytesRead = source->read (buffer + bytesToKeep,
                                      (int) (bufferSize - bytesToKeep));

            lastReadPos += bytesRead;
            bytesRead += bytesToKeep;
        }
        else
        {
            bufferStart = position;
            source->setPosition (bufferStart);
            bytesRead = source->read (buffer, bufferSize);
            lastReadPos = bufferStart + bytesRead;
        }

        while (bytesRead < bufferSize)
            buffer [bytesRead++] = 0;
    }
}

int BufferedInputStream::read (void* destBuffer, int maxBytesToRead)
{
    jassert (destBuffer != nullptr && maxBytesToRead >= 0);

    if (position >= bufferStart
         && position + maxBytesToRead <= lastReadPos)
    {
        memcpy (destBuffer, buffer + (int) (position - bufferStart), (size_t) maxBytesToRead);
        position += maxBytesToRead;
        return maxBytesToRead;
    }

    if (position < bufferStart || position >= lastReadPos)
        ensureBuffered();

    int bytesRead = 0;

    while (maxBytesToRead > 0)
    {
        auto numToRead = jmin (maxBytesToRead, (int) (lastReadPos - position));

        if (numToRead > 0)
        {
            memcpy (destBuffer, buffer + (int) (position - bufferStart), (size_t) numToRead);
            maxBytesToRead -= numToRead;
            bytesRead += numToRead;
            position += numToRead;
            destBuffer = static_cast<char*> (destBuffer) + numToRead;
        }

        auto oldLastReadPos = lastReadPos;
        ensureBuffered();

        if (oldLastReadPos == lastReadPos)
            break; // if ensureBuffered() failed to read any more data, bail out

        if (isExhausted())
            break;
    }

    return bytesRead;
}

String BufferedInputStream::readString()
{
    if (position >= bufferStart
         && position < lastReadPos)
    {
        auto maxChars = (int) (lastReadPos - position);
        auto* src = buffer + (int) (position - bufferStart);

        for (int i = 0; i < maxChars; ++i)
        {
            if (src[i] == 0)
            {
                position += i + 1;
                return String::fromUTF8 (src, i);
            }
        }
    }

    return InputStream::readString();
}

} // namespace juce
