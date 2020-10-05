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

WaitableEvent::WaitableEvent (bool manualReset) noexcept
    : useManualReset (manualReset)
{
}

bool WaitableEvent::wait (int timeOutMilliseconds) const
{
    std::unique_lock<std::mutex> lock (mutex);

    if (! triggered)
    {
        if (timeOutMilliseconds < 0)
        {
            condition.wait (lock, [this] { return triggered == true; });
        }
        else
        {
            if (! condition.wait_for (lock, std::chrono::milliseconds (timeOutMilliseconds),
                                      [this] { return triggered == true; }))
            {
                return false;
            }
        }
    }

    if (! useManualReset)
        reset();

    return true;
}

void WaitableEvent::signal() const
{
    std::unique_lock<std::mutex> lock (mutex);

    triggered = true;
    condition.notify_all();
}

void WaitableEvent::reset() const
{
    triggered = false;
}

} // namespace juce
