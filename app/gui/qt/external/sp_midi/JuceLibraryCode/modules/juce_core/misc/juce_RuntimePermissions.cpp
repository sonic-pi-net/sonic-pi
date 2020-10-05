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

#if ! JUCE_ANDROID  // We currently don't request runtime permissions on any other platform
                    // than Android, so this file contains a dummy implementation for those.
                    // This may change in the future.

void RuntimePermissions::request (PermissionID, Callback callback)   { callback (true); }
bool RuntimePermissions::isRequired (PermissionID) { return false; }
bool RuntimePermissions::isGranted (PermissionID) { return true; }

#endif

} // namespace juce
