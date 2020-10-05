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

void Logger::outputDebugString (const String& text)
{
    char* data = text.toUTF8().getAddress();
    const size_t length = CharPointer_UTF8::getBytesRequiredFor (text.getCharPointer());
    const size_t chunkSize = 1023;

    size_t position = 0;
    size_t numToRead = jmin (chunkSize, length);

    while (numToRead > 0)
    {
        __android_log_print (ANDROID_LOG_INFO, "JUCE", "%s", data + position);

        position += numToRead;
        numToRead = jmin (chunkSize, length - position);
    }
}

} // namespace juce
