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

#if JUCE_PROJUCER_LIVE_BUILD && (defined (__APPLE_CPP__) || defined(__APPLE_CC__))

 // This hack is a workaround for a bug (?) in Apple's 10.11 SDK headers
 // which cause some configurations of Clang to throw out a spurious error..
 #include <CoreFoundation/CFAvailability.h>
 #undef CF_OPTIONS
 #define CF_OPTIONS(_type, _name) _type _name; enum

 // This is a workaround for the Xcode 9 version of NSUUID.h causing some errors
 // in the live-build engine.
 #define _Nullable
 #define _Nonnull

 // A workaround for compiling the 10.15 headers with an older compiler version
 #undef API_UNAVAILABLE_BEGIN
 #define API_UNAVAILABLE_BEGIN(...)
 #undef API_UNAVAILABLE_END
 #define API_UNAVAILABLE_END
#endif
