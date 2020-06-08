/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

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
