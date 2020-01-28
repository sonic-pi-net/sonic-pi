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

NamedPipe::NamedPipe() {}

NamedPipe::~NamedPipe()
{
    close();
}

bool NamedPipe::openExisting (const String& pipeName)
{
    close();

    ScopedWriteLock sl (lock);
    currentPipeName = pipeName;
    return openInternal (pipeName, false, false);
}

bool NamedPipe::isOpen() const
{
    return pimpl != nullptr;
}

bool NamedPipe::createNewPipe (const String& pipeName, bool mustNotExist)
{
    close();

    ScopedWriteLock sl (lock);
    currentPipeName = pipeName;
    return openInternal (pipeName, true, mustNotExist);
}

String NamedPipe::getName() const
{
    return currentPipeName;
}

// other methods for this class are implemented in the platform-specific files

} // namespace juce
