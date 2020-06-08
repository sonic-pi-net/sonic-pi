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

//==============================================================================
namespace AndroidStatsHelpers
{
    #define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
     STATICMETHOD (getProperty, "getProperty", "(Ljava/lang/String;)Ljava/lang/String;")

    DECLARE_JNI_CLASS (SystemClass, "java/lang/System")
    #undef JNI_CLASS_MEMBERS

    #define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
     STATICMETHOD (getDefault, "getDefault", "()Ljava/util/Locale;") \
     METHOD (getCountry, "getCountry", "()Ljava/lang/String;") \
     METHOD (getLanguage, "getLanguage", "()Ljava/lang/String;")

    DECLARE_JNI_CLASS (JavaLocale, "java/util/Locale")
    #undef JNI_CLASS_MEMBERS

    static inline String getSystemProperty (const String& name)
    {
        return juceString (LocalRef<jstring> ((jstring) getEnv()->CallStaticObjectMethod (SystemClass,
                                                                                          SystemClass.getProperty,
                                                                                          javaString (name).get())));
    }

    static inline String getLocaleValue (bool isRegion)
    {
        auto* env = getEnv();
        LocalRef<jobject> locale (env->CallStaticObjectMethod (JavaLocale, JavaLocale.getDefault));

        auto stringResult = isRegion ? env->CallObjectMethod (locale.get(), JavaLocale.getCountry)
                                     : env->CallObjectMethod (locale.get(), JavaLocale.getLanguage);

        return juceString (LocalRef<jstring> ((jstring) stringResult));
    }

    static inline String getAndroidOsBuildValue (const char* fieldName)
    {
        return juceString (LocalRef<jstring> ((jstring) getEnv()->GetStaticObjectField (
                            AndroidBuild, getEnv()->GetStaticFieldID (AndroidBuild, fieldName, "Ljava/lang/String;"))));
    }
}

//==============================================================================
SystemStats::OperatingSystemType SystemStats::getOperatingSystemType()
{
    return Android;
}

String SystemStats::getOperatingSystemName()
{
    return "Android " + AndroidStatsHelpers::getSystemProperty ("os.version");
}

String SystemStats::getDeviceDescription()
{
    return AndroidStatsHelpers::getAndroidOsBuildValue ("MODEL")
            + "-" + AndroidStatsHelpers::getAndroidOsBuildValue ("SERIAL");
}

String SystemStats::getDeviceManufacturer()
{
    return AndroidStatsHelpers::getAndroidOsBuildValue ("MANUFACTURER");
}

bool SystemStats::isOperatingSystem64Bit()
{
   #if JUCE_64BIT
    return true;
   #else
    return false;
   #endif
}

String SystemStats::getCpuVendor()
{
    return AndroidStatsHelpers::getSystemProperty ("os.arch");
}

String SystemStats::getCpuModel()
{
    return readPosixConfigFileValue ("/proc/cpuinfo", "Hardware");
}

int SystemStats::getCpuSpeedInMegahertz()
{
    int maxFreqKHz = 0;

    for (int i = 0; i < getNumCpus(); ++i)
    {
        int freqKHz = File ("/sys/devices/system/cpu/cpu" + String(i) + "/cpufreq/cpuinfo_max_freq")
                        .loadFileAsString()
                        .getIntValue();

        maxFreqKHz = jmax (freqKHz, maxFreqKHz);
    }

    return maxFreqKHz / 1000;
}

int SystemStats::getMemorySizeInMegabytes()
{
   #if __ANDROID_API__ >= 9
    struct sysinfo sysi;

    if (sysinfo (&sysi) == 0)
        return static_cast<int> ((sysi.totalram * sysi.mem_unit) / (1024 * 1024));
   #endif

    return 0;
}

int SystemStats::getPageSize()
{
    return static_cast<int> (sysconf (_SC_PAGESIZE));
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


String SystemStats::getUserLanguage()    { return AndroidStatsHelpers::getLocaleValue (false); }
String SystemStats::getUserRegion()      { return AndroidStatsHelpers::getLocaleValue (true); }
String SystemStats::getDisplayLanguage() { return getUserLanguage() + "-" + getUserRegion(); }

//==============================================================================
void CPUInformation::initialise() noexcept
{
    numPhysicalCPUs = numLogicalCPUs = jmax ((int) 1, (int) android_getCpuCount());

    auto cpuFamily   = android_getCpuFamily();
    auto cpuFeatures = android_getCpuFeatures();

    if (cpuFamily == ANDROID_CPU_FAMILY_X86 || cpuFamily == ANDROID_CPU_FAMILY_X86_64)
    {
        hasMMX = hasSSE = hasSSE2 = (cpuFamily == ANDROID_CPU_FAMILY_X86_64);

        hasSSSE3 = ((cpuFeatures & ANDROID_CPU_X86_FEATURE_SSSE3)  != 0);
        hasSSE41 = ((cpuFeatures & ANDROID_CPU_X86_FEATURE_SSE4_1) != 0);
        hasSSE42 = ((cpuFeatures & ANDROID_CPU_X86_FEATURE_SSE4_2) != 0);
        hasAVX   = ((cpuFeatures & ANDROID_CPU_X86_FEATURE_AVX)    != 0);
        hasAVX2  = ((cpuFeatures & ANDROID_CPU_X86_FEATURE_AVX2)   != 0);

        // Google does not distinguish between MMX, SSE, SSE2, SSE3 and SSSE3. So
        // I assume (and quick Google searches seem to confirm this) that there are
        // only devices out there that either support all of this or none of this.
        if (hasSSSE3)
            hasMMX = hasSSE = hasSSE2 = hasSSE3 = true;
    }
    else if (cpuFamily == ANDROID_CPU_FAMILY_ARM)
    {
        hasNeon = ((cpuFeatures & ANDROID_CPU_ARM_FEATURE_NEON) != 0);
    }
    else if (cpuFamily == ANDROID_CPU_FAMILY_ARM64)
    {
        // all arm 64-bit cpus have neon
        hasNeon = true;
    }
}

//==============================================================================
uint32 juce_millisecondsSinceStartup() noexcept
{
    timespec t;
    clock_gettime (CLOCK_MONOTONIC, &t);

    return static_cast<uint32> (t.tv_sec) * 1000U + static_cast<uint32> (t.tv_nsec) / 1000000U;
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
    jassertfalse;
    return false;
}

} // namespace juce
