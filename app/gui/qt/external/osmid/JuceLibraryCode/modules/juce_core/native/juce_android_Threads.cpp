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

/*
    Note that a lot of methods that you'd expect to find in this file actually
    live in juce_posix_SharedCode.h!
*/

//==============================================================================
// sets the process to 0=low priority, 1=normal, 2=high, 3=realtime
JUCE_API void JUCE_CALLTYPE Process::setPriority (ProcessPriority prior)
{
    // TODO

    struct sched_param param;
    int policy, maxp, minp;

    const int p = (int) prior;

    if (p <= 1)
        policy = SCHED_OTHER;
    else
        policy = SCHED_RR;

    minp = sched_get_priority_min (policy);
    maxp = sched_get_priority_max (policy);

    if (p < 2)
        param.sched_priority = 0;
    else if (p == 2 )
        // Set to middle of lower realtime priority range
        param.sched_priority = minp + (maxp - minp) / 4;
    else
        // Set to middle of higher realtime priority range
        param.sched_priority = minp + (3 * (maxp - minp) / 4);

    pthread_setschedparam (pthread_self(), policy, &param);
}

JUCE_API bool JUCE_CALLTYPE juce_isRunningUnderDebugger() noexcept
{
    StringArray lines;
    File ("/proc/self/status").readLines (lines);

    for (int i = lines.size(); --i >= 0;) // (NB - it's important that this runs in reverse order)
        if (lines[i].upToFirstOccurrenceOf (":", false, false).trim().equalsIgnoreCase ("TracerPid"))
            return (lines[i].fromFirstOccurrenceOf (":", false, false).trim().getIntValue() > 0);

    return false;
}

JUCE_API void JUCE_CALLTYPE Process::raisePrivilege() {}
JUCE_API void JUCE_CALLTYPE Process::lowerPrivilege() {}

} // namespace juce
