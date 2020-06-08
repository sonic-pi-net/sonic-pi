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

static void appendToFile (const File& f, const String& s)
{
    if (f.getFullPathName().isNotEmpty())
    {
        FileOutputStream out (f);

        if (! out.failedToOpen())
            out << s << newLine;
    }
}

PerformanceCounter::PerformanceCounter (const String& name, int runsPerPrintout, const File& loggingFile)
    : runsPerPrint (runsPerPrintout), startTime (0), outputFile (loggingFile)
{
    stats.name = name;
    appendToFile (outputFile, "**** Counter for \"" + name + "\" started at: " + Time::getCurrentTime().toString (true, true));
}

PerformanceCounter::~PerformanceCounter()
{
    if (stats.numRuns > 0)
        printStatistics();
}

PerformanceCounter::Statistics::Statistics() noexcept
    : averageSeconds(), maximumSeconds(), minimumSeconds(), totalSeconds(), numRuns()
{
}

void PerformanceCounter::Statistics::clear() noexcept
{
    averageSeconds = maximumSeconds = minimumSeconds = totalSeconds = 0;
    numRuns = 0;
}

void PerformanceCounter::Statistics::addResult (double elapsed) noexcept
{
    if (numRuns == 0)
    {
        maximumSeconds = elapsed;
        minimumSeconds = elapsed;
    }
    else
    {
        maximumSeconds = jmax (maximumSeconds, elapsed);
        minimumSeconds = jmin (minimumSeconds, elapsed);
    }

    ++numRuns;
    totalSeconds += elapsed;
}

static String timeToString (double secs)
{
    return String ((int64) (secs * (secs < 0.01 ? 1000000.0 : 1000.0) + 0.5))
                    + (secs < 0.01 ? " microsecs" : " millisecs");
}

String PerformanceCounter::Statistics::toString() const
{
    MemoryOutputStream s;

    s << "Performance count for \"" << name << "\" over " << numRuns << " run(s)" << newLine
      << "Average = "   << timeToString (averageSeconds)
      << ", minimum = " << timeToString (minimumSeconds)
      << ", maximum = " << timeToString (maximumSeconds)
      << ", total = "   << timeToString (totalSeconds);

    return s.toString();
}

void PerformanceCounter::start() noexcept
{
    startTime = Time::getHighResolutionTicks();
}

bool PerformanceCounter::stop()
{
    stats.addResult (Time::highResolutionTicksToSeconds (Time::getHighResolutionTicks() - startTime));

    if (stats.numRuns < runsPerPrint)
        return false;

    printStatistics();
    return true;
}

void PerformanceCounter::printStatistics()
{
    const String desc (getStatisticsAndReset().toString());

    Logger::outputDebugString (desc);
    appendToFile (outputFile, desc);
}

PerformanceCounter::Statistics PerformanceCounter::getStatisticsAndReset()
{
    Statistics s (stats);
    stats.clear();

    if (s.numRuns > 0)
        s.averageSeconds = s.totalSeconds / s.numRuns;

    return s;
}

} // namespace juce
