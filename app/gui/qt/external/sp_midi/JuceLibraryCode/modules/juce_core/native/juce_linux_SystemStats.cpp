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

#if JUCE_BELA
extern "C" int cobalt_thread_mode();
#endif

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

String SystemStats::getDeviceManufacturer()
{
    return {};
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

int SystemStats::getCpuSpeedInMegahertz()
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
    if (auto user = getenv ("USER"))
        return String::fromUTF8 (user);

    if (auto pw = getpwuid (getuid()))
        return String::fromUTF8 (pw->pw_name);

    return {};
}

String SystemStats::getFullUserName()
{
    return getLogonName();
}

String SystemStats::getComputerName()
{
    char name[256] = {};

    if (gethostname (name, sizeof (name) - 1) == 0)
        return name;

    return {};
}

static String getLocaleValue (nl_item key)
{
    auto oldLocale = ::setlocale (LC_ALL, "");
    auto result = String::fromUTF8 (nl_langinfo (key));
    ::setlocale (LC_ALL, oldLocale);
    return result;
}

String SystemStats::getUserLanguage()     { return getLocaleValue (_NL_IDENTIFICATION_LANGUAGE); }
String SystemStats::getUserRegion()       { return getLocaleValue (_NL_IDENTIFICATION_TERRITORY); }
String SystemStats::getDisplayLanguage()  { return getUserLanguage() + "-" + getUserRegion(); }

//==============================================================================
void CPUInformation::initialise() noexcept
{
    auto flags = getCpuInfo ("flags");

    hasMMX             = flags.contains ("mmx");
    hasFMA3            = flags.contains ("fma");
    hasFMA4            = flags.contains ("fma4");
    hasSSE             = flags.contains ("sse");
    hasSSE2            = flags.contains ("sse2");
    hasSSE3            = flags.contains ("sse3");
    has3DNow           = flags.contains ("3dnow");
    hasSSSE3           = flags.contains ("ssse3");
    hasSSE41           = flags.contains ("sse4_1");
    hasSSE42           = flags.contains ("sse4_2");
    hasAVX             = flags.contains ("avx");
    hasAVX2            = flags.contains ("avx2");
    hasAVX512F         = flags.contains ("avx512f");
    hasAVX512BW        = flags.contains ("avx512bw");
    hasAVX512CD        = flags.contains ("avx512cd");
    hasAVX512DQ        = flags.contains ("avx512dq");
    hasAVX512ER        = flags.contains ("avx512er");
    hasAVX512IFMA      = flags.contains ("avx512ifma");
    hasAVX512PF        = flags.contains ("avx512pf");
    hasAVX512VBMI      = flags.contains ("avx512vbmi");
    hasAVX512VL        = flags.contains ("avx512vl");
    hasAVX512VPOPCNTDQ = flags.contains ("avx512_vpopcntdq");

    numLogicalCPUs  = getCpuInfo ("processor").getIntValue() + 1;

    // Assume CPUs in all sockets have the same number of cores
    numPhysicalCPUs = getCpuInfo ("cpu cores").getIntValue() * (getCpuInfo ("physical id").getIntValue() + 1);

    if (numPhysicalCPUs <= 0)
        numPhysicalCPUs = numLogicalCPUs;
}

//==============================================================================
uint32 juce_millisecondsSinceStartup() noexcept
{
    return (uint32) (Time::getHighResolutionTicks() / 1000);
}

int64 Time::getHighResolutionTicks() noexcept
{
    timespec t;

   #if JUCE_BELA
    if (cobalt_thread_mode() == 0x200 /*XNRELAX*/)
        clock_gettime (CLOCK_MONOTONIC, &t);
    else
        __wrap_clock_gettime (CLOCK_MONOTONIC, &t);
   #else
    clock_gettime (CLOCK_MONOTONIC, &t);
   #endif

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

    return settimeofday (&t, nullptr) == 0;
}

JUCE_API bool JUCE_CALLTYPE juce_isRunningUnderDebugger() noexcept
{
   #if JUCE_BSD
    return false;
   #else
    return readPosixConfigFileValue ("/proc/self/status", "TracerPid").getIntValue() > 0;
   #endif
}

} // namespace juce
