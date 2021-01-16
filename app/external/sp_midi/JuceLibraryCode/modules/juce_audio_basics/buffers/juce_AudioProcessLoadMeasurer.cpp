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

AudioProcessLoadMeasurer::AudioProcessLoadMeasurer() {}
AudioProcessLoadMeasurer::~AudioProcessLoadMeasurer() {}

void AudioProcessLoadMeasurer::reset()
{
    reset (0, 0);
}

void AudioProcessLoadMeasurer::reset (double sampleRate, int blockSize)
{
    cpuUsageMs = 0;
    xruns = 0;

    if (sampleRate > 0.0 && blockSize > 0)
    {
        msPerBlock = 1000.0 * blockSize / sampleRate;
        timeToCpuScale = (msPerBlock > 0.0) ? (1.0 / msPerBlock) : 0.0;
    }
    else
    {
        msPerBlock = 0;
        timeToCpuScale = 0;
    }
}

void AudioProcessLoadMeasurer::registerBlockRenderTime (double milliseconds)
{
    const double filterAmount = 0.2;
    cpuUsageMs += filterAmount * (milliseconds - cpuUsageMs);

    if (milliseconds > msPerBlock)
        ++xruns;
}

double AudioProcessLoadMeasurer::getLoadAsProportion() const   { return jlimit (0.0, 1.0, timeToCpuScale * cpuUsageMs); }
double AudioProcessLoadMeasurer::getLoadAsPercentage() const   { return 100.0 * getLoadAsProportion(); }

int AudioProcessLoadMeasurer::getXRunCount() const             { return xruns; }

AudioProcessLoadMeasurer::ScopedTimer::ScopedTimer (AudioProcessLoadMeasurer& p)
   : owner (p), startTime (Time::getMillisecondCounterHiRes())
{
}

AudioProcessLoadMeasurer::ScopedTimer::~ScopedTimer()
{
    owner.registerBlockRenderTime (Time::getMillisecondCounterHiRes() - startTime);
}

} // namespace juce
