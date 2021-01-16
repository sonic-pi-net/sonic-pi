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

namespace LinuxEventLoop
{
    /** Registers a callback that will be called when a file descriptor is ready for I/O.

        This will add the given file descriptor to the internal set of file descriptors
        that will be passed to the poll() call. When this file descriptor has data to read
        the readCallback will be called.

        @param fd            the file descriptor to be monitored
        @param readCallback  a callback that will be called when the file descriptor has
                             data to read. The file descriptor will be passed as an argument
        @param eventMask     a bit mask specifying the events you are interested in for the
                             file descriptor. The possible values for this are defined in
                             <poll.h>
    */
    void registerFdCallback (int fd, std::function<void (int)> readCallback, short eventMask = 1 /*POLLIN*/);

    /** Unregisters a previously registered file descriptor.

        @see registerFdCallback
    */
    void unregisterFdCallback (int fd);
}

} // namespace juce
