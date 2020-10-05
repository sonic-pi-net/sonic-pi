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
    Contains functions to control the system's master volume.

    @tags{Audio}
*/
class JUCE_API  SystemAudioVolume
{
public:
    //==============================================================================
    /** Returns the operating system's current volume level in the range 0 to 1.0 */
    static float JUCE_CALLTYPE getGain();

    /** Attempts to set the operating system's current volume level.
        @param newGain  the level, between 0 and 1.0
        @returns true if the operation succeeds
    */
    static bool JUCE_CALLTYPE setGain (float newGain);

    /** Returns true if the system's audio output is currently muted. */
    static bool JUCE_CALLTYPE isMuted();

    /** Attempts to mute the operating system's audio output.
        @param shouldBeMuted    true if you want it to be muted
        @returns true if the operation succeeds
    */
    static bool JUCE_CALLTYPE setMuted (bool shouldBeMuted);

private:
    SystemAudioVolume(); // Don't instantiate this class, just call its static fns.
    JUCE_DECLARE_NON_COPYABLE (SystemAudioVolume)
};

} // namespace juce
