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

ScopedAutoReleasePool::ScopedAutoReleasePool()
{
    pool = [[NSAutoreleasePool alloc] init];
}

ScopedAutoReleasePool::~ScopedAutoReleasePool()
{
    [((NSAutoreleasePool*) pool) release];
}

//==============================================================================
void Logger::outputDebugString (const String& text)
{
    // Would prefer to use std::cerr here, but avoiding it for
    // the moment, due to clang JIT linkage problems.
    fputs (text.toRawUTF8(), stderr);
    fputs ("\n", stderr);
    fflush (stderr);
}

//==============================================================================
namespace SystemStatsHelpers
{
   #if JUCE_INTEL && ! JUCE_NO_INLINE_ASM
    static void doCPUID (uint32& a, uint32& b, uint32& c, uint32& d, uint32 type)
    {
        uint32 la = a, lb = b, lc = c, ld = d;

       #if JUCE_32BIT && defined (__pic__)
        asm ("mov %%ebx, %%edi\n"
             "cpuid\n"
             "xchg %%edi, %%ebx\n"
               : "=a" (la), "=D" (lb), "=c" (lc), "=d" (ld)
               : "a" (type), "c" (0));
       #else
        asm ("cpuid\n"
               : "=a" (la), "=b" (lb), "=c" (lc), "=d" (ld)
               : "a" (type), "c" (0));
       #endif

        a = la; b = lb; c = lc; d = ld;
    }
   #endif
}

//==============================================================================
void CPUInformation::initialise() noexcept
{
   #if JUCE_INTEL && ! JUCE_NO_INLINE_ASM
    uint32 a = 0, b = 0, d = 0, c = 0;
    SystemStatsHelpers::doCPUID (a, b, c, d, 1);

    hasMMX   = (d & (1u << 23)) != 0;
    hasSSE   = (d & (1u << 25)) != 0;
    hasSSE2  = (d & (1u << 26)) != 0;
    has3DNow = (b & (1u << 31)) != 0;
    hasSSE3  = (c & (1u <<  0)) != 0;
    hasSSSE3 = (c & (1u <<  9)) != 0;
    hasFMA3  = (c & (1u << 12)) != 0;
    hasSSE41 = (c & (1u << 19)) != 0;
    hasSSE42 = (c & (1u << 20)) != 0;
    hasAVX   = (c & (1u << 28)) != 0;

    SystemStatsHelpers::doCPUID (a, b, c, d, 0x80000001);
    hasFMA4  = (c & (1u << 16)) != 0;

    SystemStatsHelpers::doCPUID (a, b, c, d, 7);
    hasAVX2            = (b & (1u <<  5)) != 0;
    hasAVX512F         = (b & (1u << 16)) != 0;
    hasAVX512DQ        = (b & (1u << 17)) != 0;
    hasAVX512IFMA      = (b & (1u << 21)) != 0;
    hasAVX512PF        = (b & (1u << 26)) != 0;
    hasAVX512ER        = (b & (1u << 27)) != 0;
    hasAVX512CD        = (b & (1u << 28)) != 0;
    hasAVX512BW        = (b & (1u << 30)) != 0;
    hasAVX512VL        = (b & (1u << 31)) != 0;
    hasAVX512VBMI      = (c & (1u <<  1)) != 0;
    hasAVX512VPOPCNTDQ = (c & (1u << 14)) != 0;
   #endif

    numLogicalCPUs = (int) [[NSProcessInfo processInfo] activeProcessorCount];

    unsigned int physicalcpu = 0;
    size_t len = sizeof (physicalcpu);

    if (sysctlbyname ("hw.physicalcpu", &physicalcpu, &len, nullptr, 0) >= 0)
        numPhysicalCPUs = (int) physicalcpu;

    if (numPhysicalCPUs <= 0)
        numPhysicalCPUs = numLogicalCPUs;
}

//==============================================================================
#if ! JUCE_IOS
static String getOSXVersion()
{
    JUCE_AUTORELEASEPOOL
    {
        NSDictionary* dict = [NSDictionary dictionaryWithContentsOfFile:
                                    nsStringLiteral ("/System/Library/CoreServices/SystemVersion.plist")];

        return nsStringToJuce ([dict objectForKey: nsStringLiteral ("ProductVersion")]);
    }
}
#endif

SystemStats::OperatingSystemType SystemStats::getOperatingSystemType()
{
   #if JUCE_IOS
    return iOS;
   #else
    StringArray parts;
    parts.addTokens (getOSXVersion(), ".", StringRef());

    jassert (parts[0].getIntValue() == 10);
    const int major = parts[1].getIntValue();
    jassert (major > 2);

    return (OperatingSystemType) (major + MacOSX_10_4 - 4);
   #endif
}

String SystemStats::getOperatingSystemName()
{
   #if JUCE_IOS
    return "iOS " + nsStringToJuce ([[UIDevice currentDevice] systemVersion]);
   #else
    return "Mac OSX " + getOSXVersion();
   #endif
}

String SystemStats::getDeviceDescription()
{
   #if JUCE_IOS
    const char* name = "hw.machine";
   #else
    const char* name = "hw.model";
   #endif

    size_t size;

    if (sysctlbyname (name, nullptr, &size, nullptr, 0) >= 0)
    {
        HeapBlock<char> model (size);

        if (sysctlbyname (name, model, &size, nullptr, 0) >= 0)
        {
            String description (model.get());

           #if JUCE_IOS
            if (description == "x86_64") // running in the simulator
            {
                if (auto* userInfo = [[NSProcessInfo processInfo] environment])
                {
                    if (auto* simDeviceName = [userInfo objectForKey: @"SIMULATOR_DEVICE_NAME"])
                        return nsStringToJuce (simDeviceName);
                }
            }
          #endif

            return description;
        }
    }

    return {};
}

String SystemStats::getDeviceManufacturer()
{
    return "Apple";
}

bool SystemStats::isOperatingSystem64Bit()
{
   #if JUCE_IOS
    return false;
   #elif JUCE_64BIT
    return true;
   #else
    return getOperatingSystemType() >= MacOSX_10_6;
   #endif
}

int SystemStats::getMemorySizeInMegabytes()
{
    uint64 mem = 0;
    size_t memSize = sizeof (mem);
    int mib[] = { CTL_HW, HW_MEMSIZE };
    sysctl (mib, 2, &mem, &memSize, nullptr, 0);
    return (int) (mem / (1024 * 1024));
}

String SystemStats::getCpuVendor()
{
   #if JUCE_INTEL && ! JUCE_NO_INLINE_ASM
    uint32 dummy = 0;
    uint32 vendor[4] = { 0 };

    SystemStatsHelpers::doCPUID (dummy, vendor[0], vendor[2], vendor[1], 0);

    return String (reinterpret_cast<const char*> (vendor), 12);
   #else
    return {};
   #endif
}

String SystemStats::getCpuModel()
{
    char name[65] = { 0 };
    size_t size = sizeof (name) - 1;

    if (sysctlbyname ("machdep.cpu.brand_string", &name, &size, nullptr, 0) >= 0)
        return String (name);

    return {};
}

int SystemStats::getCpuSpeedInMegahertz()
{
    uint64 speedHz = 0;
    size_t speedSize = sizeof (speedHz);
    int mib[] = { CTL_HW, HW_CPU_FREQ };
    sysctl (mib, 2, &speedHz, &speedSize, nullptr, 0);

   #if JUCE_BIG_ENDIAN
    if (speedSize == 4)
        speedHz >>= 32;
   #endif

    return (int) (speedHz / 1000000);
}

//==============================================================================
String SystemStats::getLogonName()
{
    return nsStringToJuce (NSUserName());
}

String SystemStats::getFullUserName()
{
    return nsStringToJuce (NSFullUserName());
}

String SystemStats::getComputerName()
{
    char name[256] = { 0 };
    if (gethostname (name, sizeof (name) - 1) == 0)
        return String (name).upToLastOccurrenceOf (".local", false, true);

    return {};
}

static String getLocaleValue (CFStringRef key)
{
    CFLocaleRef cfLocale = CFLocaleCopyCurrent();
    const String result (String::fromCFString ((CFStringRef) CFLocaleGetValue (cfLocale, key)));
    CFRelease (cfLocale);
    return result;
}

String SystemStats::getUserLanguage()   { return getLocaleValue (kCFLocaleLanguageCode); }
String SystemStats::getUserRegion()     { return getLocaleValue (kCFLocaleCountryCode); }

String SystemStats::getDisplayLanguage()
{
    CFArrayRef cfPrefLangs = CFLocaleCopyPreferredLanguages();
    const String result (String::fromCFString ((CFStringRef) CFArrayGetValueAtIndex (cfPrefLangs, 0)));
    CFRelease (cfPrefLangs);
    return result;
}

//==============================================================================
/*  NB: these are kept outside the HiResCounterInfo struct and initialised to 1 to avoid
    division-by-zero errors if some other static constructor calls us before this file's
    static constructors have had a chance to fill them in correctly..
*/
static uint64 hiResCounterNumerator = 0, hiResCounterDenominator = 1;

class HiResCounterInfo
{
public:
    HiResCounterInfo()
    {
        mach_timebase_info_data_t timebase;
        (void) mach_timebase_info (&timebase);

        if (timebase.numer % 1000000 == 0)
        {
            hiResCounterNumerator   = timebase.numer / 1000000;
            hiResCounterDenominator = timebase.denom;
        }
        else
        {
            hiResCounterNumerator   = timebase.numer;
            hiResCounterDenominator = timebase.denom * (uint64) 1000000;
        }

        highResTimerFrequency = (timebase.denom * (uint64) 1000000000) / timebase.numer;
        highResTimerToMillisecRatio = hiResCounterNumerator / (double) hiResCounterDenominator;
    }

    uint32 millisecondsSinceStartup() const noexcept
    {
        return (uint32) ((mach_absolute_time() * hiResCounterNumerator) / hiResCounterDenominator);
    }

    double getMillisecondCounterHiRes() const noexcept
    {
        return mach_absolute_time() * highResTimerToMillisecRatio;
    }

    int64 highResTimerFrequency;

private:
    double highResTimerToMillisecRatio;
};

static HiResCounterInfo hiResCounterInfo;

uint32 juce_millisecondsSinceStartup() noexcept         { return hiResCounterInfo.millisecondsSinceStartup(); }
double Time::getMillisecondCounterHiRes() noexcept      { return hiResCounterInfo.getMillisecondCounterHiRes(); }
int64  Time::getHighResolutionTicksPerSecond() noexcept { return hiResCounterInfo.highResTimerFrequency; }
int64  Time::getHighResolutionTicks() noexcept          { return (int64) mach_absolute_time(); }

bool Time::setSystemTimeToThisTime() const
{
    jassertfalse;
    return false;
}

//==============================================================================
int SystemStats::getPageSize()
{
    return (int) NSPageSize();
}

} // namespace juce
