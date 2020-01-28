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

void Logger::outputDebugString (const String& text)
{
    std::cerr << text << std::endl;
}

//==============================================================================
SystemStats::OperatingSystemType SystemStats::getOperatingSystemType()
{
    return Linux;
}

String SystemStats::getOperatingSystemName()
{
    return "Linux";
}

bool SystemStats::isOperatingSystem64Bit()
{
   #if JUCE_64BIT
    return true;
   #else
    //xxx not sure how to find this out?..
    return false;
   #endif
}

//==============================================================================
static inline String getCpuInfo (const char* key)
{
    return readPosixConfigFileValue ("/proc/cpuinfo", key);
}

String SystemStats::getDeviceDescription()
{
    return getCpuInfo ("Hardware");
}

String SystemStats::getCpuVendor()
{
    auto v = getCpuInfo ("vendor_id");

    if (v.isEmpty())
        v = getCpuInfo ("model name");

    return v;
}

String SystemStats::getCpuModel()
{
    return getCpuInfo ("model name");
}

int SystemStats::getCpuSpeedInMegaherz()
{
    return roundToInt (getCpuInfo ("cpu MHz").getFloatValue());
}

int SystemStats::getMemorySizeInMegabytes()
{
    struct sysinfo sysi;

    if (sysinfo (&sysi) == 0)
        return (int) (sysi.totalram * sysi.mem_unit / (1024 * 1024));

    return 0;
}

int SystemStats::getPageSize()
{
    return (int) sysconf (_SC_PAGESIZE);
}

//==============================================================================
String SystemStats::getLogonName()
{
    if (const char* user = getenv ("USER"))
        return CharPointer_UTF8 (user);

    if (struct passwd* const pw = getpwuid (getuid()))
        return CharPointer_UTF8 (pw->pw_name);

    return {};
}

String SystemStats::getFullUserName()
{
    return getLogonName();
}

String SystemStats::getComputerName()
{
    char name [256] = { 0 };
    if (gethostname (name, sizeof (name) - 1) == 0)
        return name;

    return {};
}

static String getLocaleValue (nl_item key)
{
    const char* oldLocale = ::setlocale (LC_ALL, "");
    String result (String::fromUTF8 (nl_langinfo (key)));
    ::setlocale (LC_ALL, oldLocale);
    return result;
}

String SystemStats::getUserLanguage()    { return getLocaleValue (_NL_IDENTIFICATION_LANGUAGE); }
String SystemStats::getUserRegion()      { return getLocaleValue (_NL_IDENTIFICATION_TERRITORY); }
String SystemStats::getDisplayLanguage() { return getUserLanguage() + "-" + getUserRegion(); }

//==============================================================================
void CPUInformation::initialise() noexcept
{
    auto flags = getCpuInfo ("flags");
    hasMMX   = flags.contains ("mmx");
    hasSSE   = flags.contains ("sse");
    hasSSE2  = flags.contains ("sse2");
    hasSSE3  = flags.contains ("sse3");
    has3DNow = flags.contains ("3dnow");
    hasSSSE3 = flags.contains ("ssse3");
    hasSSE41 = flags.contains ("sse4_1");
    hasSSE42 = flags.contains ("sse4_2");
    hasAVX   = flags.contains ("avx");
    hasAVX2  = flags.contains ("avx2");

    numLogicalCPUs  = getCpuInfo ("processor").getIntValue() + 1;

    // Assume CPUs in all sockets have the same number of cores
    numPhysicalCPUs = getCpuInfo ("cpu cores").getIntValue() * (getCpuInfo ("physical id").getIntValue() + 1);

    if (numPhysicalCPUs <= 0)
        numPhysicalCPUs = numLogicalCPUs;
}

//==============================================================================
uint32 juce_millisecondsSinceStartup() noexcept
{
    timespec t;
    clock_gettime (CLOCK_MONOTONIC, &t);

    return (uint32) (t.tv_sec * 1000 + t.tv_nsec / 1000000);
}

int64 Time::getHighResolutionTicks() noexcept
{
    timespec t;
    clock_gettime (CLOCK_MONOTONIC, &t);

    return (t.tv_sec * (int64) 1000000) + (t.tv_nsec / 1000);
}

int64 Time::getHighResolutionTicksPerSecond() noexcept
{
    return 1000000;  // (microseconds)
}

double Time::getMillisecondCounterHiRes() noexcept
{
    return getHighResolutionTicks() * 0.001;
}

bool Time::setSystemTimeToThisTime() const
{
    timeval t;
    t.tv_sec = millisSinceEpoch / 1000;
    t.tv_usec = (millisSinceEpoch - t.tv_sec * 1000) * 1000;

    return settimeofday (&t, 0) == 0;
}

JUCE_API bool JUCE_CALLTYPE juce_isRunningUnderDebugger() noexcept
{
   #if JUCE_BSD
    return false;
   #else
    return readPosixConfigFileValue ("/proc/self/status", "TracerPid")
             .getIntValue() > 0;
   #endif
}

} // namespace juce
