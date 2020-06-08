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

#if JUCE_MSVC
 #pragma intrinsic (__cpuid)
 #pragma intrinsic (__rdtsc)
#endif

void Logger::outputDebugString (const String& text)
{
    OutputDebugString ((text + "\n").toWideCharPointer());
}

//==============================================================================
#ifdef JUCE_DLL_BUILD
 JUCE_API void* juceDLL_malloc (size_t sz)    { return std::malloc (sz); }
 JUCE_API void  juceDLL_free (void* block)    { std::free (block); }
#endif

//==============================================================================

#if JUCE_MINGW || JUCE_CLANG
static void callCPUID (int result[4], uint32 type)
{
  uint32 la = result[0], lb = result[1], lc = result[2], ld = result[3];

  asm ("mov %%ebx, %%esi \n\t"
       "cpuid \n\t"
       "xchg %%esi, %%ebx"
       : "=a" (la), "=S" (lb), "=c" (lc), "=d" (ld) : "a" (type)
        #if JUCE_64BIT
     , "b" (lb), "c" (lc), "d" (ld)
        #endif
       );

  result[0] = la; result[1] = lb; result[2] = lc; result[3] = ld;
}
#else
static void callCPUID (int result[4], int infoType)
{
   #if JUCE_PROJUCER_LIVE_BUILD
    std::fill (result, result + 4, 0);
   #else
    __cpuid (result, infoType);
   #endif
}
#endif

String SystemStats::getCpuVendor()
{
    int info[4] = { 0 };
    callCPUID (info, 0);

    char v [12];
    memcpy (v, info + 1, 4);
    memcpy (v + 4, info + 3, 4);
    memcpy (v + 8, info + 2, 4);

    return String (v, 12);
}

String SystemStats::getCpuModel()
{
    char name[65] = { 0 };
    int info[4] = { 0 };

    callCPUID (info, 0x80000000);

    const int numExtIDs = info[0];

    if ((unsigned) numExtIDs < 0x80000004)  // if brand string is unsupported
        return {};

    callCPUID (info, 0x80000002);
    memcpy (name, info, sizeof (info));

    callCPUID (info, 0x80000003);
    memcpy (name + 16, info, sizeof (info));

    callCPUID (info, 0x80000004);
    memcpy (name + 32, info, sizeof (info));

    return String (name).trim();
}

static int findNumberOfPhysicalCores() noexcept
{
   #if JUCE_MINGW
    // Not implemented in MinGW
    jassertfalse;

    return 1;
   #else

    int numPhysicalCores = 0;
    DWORD bufferSize = 0;
    GetLogicalProcessorInformation (nullptr, &bufferSize);

    if (auto numBuffers = (size_t) (bufferSize / sizeof (SYSTEM_LOGICAL_PROCESSOR_INFORMATION)))
    {
        HeapBlock<SYSTEM_LOGICAL_PROCESSOR_INFORMATION> buffer (numBuffers);

        if (GetLogicalProcessorInformation (buffer, &bufferSize))
            for (size_t i = 0; i < numBuffers; ++i)
                if (buffer[i].Relationship == RelationProcessorCore)
                    ++numPhysicalCores;
    }

    return numPhysicalCores;
   #endif // JUCE_MINGW
}

//==============================================================================
void CPUInformation::initialise() noexcept
{
    int info[4] = { 0 };
    callCPUID (info, 1);

    // NB: IsProcessorFeaturePresent doesn't work on XP
    hasMMX   = (info[3] & (1 << 23)) != 0;
    hasSSE   = (info[3] & (1 << 25)) != 0;
    hasSSE2  = (info[3] & (1 << 26)) != 0;
    hasSSE3  = (info[2] & (1 <<  0)) != 0;
    hasAVX   = (info[2] & (1 << 28)) != 0;
    hasFMA3  = (info[2] & (1 << 12)) != 0;
    hasSSSE3 = (info[2] & (1 <<  9)) != 0;
    hasSSE41 = (info[2] & (1 << 19)) != 0;
    hasSSE42 = (info[2] & (1 << 20)) != 0;
    has3DNow = (info[1] & (1 << 31)) != 0;

    callCPUID (info, 0x80000001);
    hasFMA4  = (info[2] & (1 << 16)) != 0;

    callCPUID (info, 7);

    hasAVX2            = (info[1] & (1 << 5))   != 0;
    hasAVX512F         = (info[1] & (1u << 16)) != 0;
    hasAVX512DQ        = (info[1] & (1u << 17)) != 0;
    hasAVX512IFMA      = (info[1] & (1u << 21)) != 0;
    hasAVX512PF        = (info[1] & (1u << 26)) != 0;
    hasAVX512ER        = (info[1] & (1u << 27)) != 0;
    hasAVX512CD        = (info[1] & (1u << 28)) != 0;
    hasAVX512BW        = (info[1] & (1u << 30)) != 0;
    hasAVX512VL        = (info[1] & (1u << 31)) != 0;
    hasAVX512VBMI      = (info[2] & (1u <<  1)) != 0;
    hasAVX512VPOPCNTDQ = (info[2] & (1u << 14)) != 0;

    SYSTEM_INFO systemInfo;
    GetNativeSystemInfo (&systemInfo);
    numLogicalCPUs  = (int) systemInfo.dwNumberOfProcessors;
    numPhysicalCPUs = findNumberOfPhysicalCores();

    if (numPhysicalCPUs <= 0)
        numPhysicalCPUs = numLogicalCPUs;
}

#if JUCE_MSVC && JUCE_CHECK_MEMORY_LEAKS
struct DebugFlagsInitialiser
{
    DebugFlagsInitialiser()
    {
        _CrtSetDbgFlag (_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
    }
};

static DebugFlagsInitialiser debugFlagsInitialiser;
#endif

//==============================================================================
#if JUCE_MINGW
 static uint32 getWindowsVersion()
 {
     auto filename = _T("kernel32.dll");
     DWORD handle = 0;

     if (auto size = GetFileVersionInfoSize (filename, &handle))
     {
         HeapBlock<char> data (size);

         if (GetFileVersionInfo (filename, handle, size, data))
         {
             VS_FIXEDFILEINFO* info = nullptr;
             UINT verSize = 0;

             if (VerQueryValue (data, (LPCTSTR) _T("\\"), (void**) &info, &verSize))
                 if (size > 0 && info != nullptr && info->dwSignature == 0xfeef04bd)
                     return (uint32) info->dwFileVersionMS;
         }
     }

     return 0;
 }
#else
 RTL_OSVERSIONINFOW getWindowsVersionInfo()
 {
     RTL_OSVERSIONINFOW versionInfo = { 0 };

     if (auto* moduleHandle = ::GetModuleHandleW (L"ntdll.dll"))
     {
         using RtlGetVersion = LONG (WINAPI*) (PRTL_OSVERSIONINFOW);

         if (auto* rtlGetVersion = (RtlGetVersion) ::GetProcAddress (moduleHandle, "RtlGetVersion"))
         {
             versionInfo.dwOSVersionInfoSize = sizeof (versionInfo);
             LONG STATUS_SUCCESS = 0;

             if (rtlGetVersion (&versionInfo) != STATUS_SUCCESS)
                 versionInfo = { 0 };
         }
     }

     return versionInfo;
 }
#endif

SystemStats::OperatingSystemType SystemStats::getOperatingSystemType()
{
   #if JUCE_MINGW
    auto v = getWindowsVersion();
    auto major = (v >> 16) & 0xff;
    auto minor = (v >> 0)  & 0xff;
   #else
    auto versionInfo = getWindowsVersionInfo();
    auto major = versionInfo.dwMajorVersion;
    auto minor = versionInfo.dwMinorVersion;
   #endif

    jassert (major <= 10); // need to add support for new version!

    if (major == 10)                 return Windows10;
    if (major == 6 && minor == 3)    return Windows8_1;
    if (major == 6 && minor == 2)    return Windows8_0;
    if (major == 6 && minor == 1)    return Windows7;
    if (major == 6 && minor == 0)    return WinVista;
    if (major == 5 && minor == 1)    return WinXP;
    if (major == 5 && minor == 0)    return Win2000;

    jassertfalse;
    return UnknownOS;
}

String SystemStats::getOperatingSystemName()
{
    const char* name = "Unknown OS";

    switch (getOperatingSystemType())
    {
        case Windows10:         name = "Windows 10";        break;
        case Windows8_1:        name = "Windows 8.1";       break;
        case Windows8_0:        name = "Windows 8.0";       break;
        case Windows7:          name = "Windows 7";         break;
        case WinVista:          name = "Windows Vista";     break;
        case WinXP:             name = "Windows XP";        break;
        case Win2000:           name = "Windows 2000";      break;
        default:                jassertfalse; break; // !! new type of OS?
    }

    return name;
}

String SystemStats::getDeviceDescription()
{
   #if WINAPI_FAMILY == WINAPI_FAMILY_DESKTOP_APP
    return "Windows (Desktop)";
   #elif WINAPI_FAMILY == WINAPI_FAMILY_PC_APP
    return "Windows (Store)";
   #elif WINAPI_FAMILY == WINAPI_FAMILY_PHONE_APP
    return "Windows (Phone)";
   #elif WINAPI_FAMILY == WINAPI_FAMILY_SYSTEM
    return "Windows (System)";
   #elif WINAPI_FAMILY == WINAPI_FAMILY_SERVER
    return "Windows (Server)";
   #else
    return "Windows";
   #endif
}

String SystemStats::getDeviceManufacturer()
{
    return {};
}

bool SystemStats::isOperatingSystem64Bit()
{
   #if JUCE_64BIT
    return true;
   #else
    typedef BOOL (WINAPI* LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);

    LPFN_ISWOW64PROCESS fnIsWow64Process
        = (LPFN_ISWOW64PROCESS) GetProcAddress (GetModuleHandleA ("kernel32"), "IsWow64Process");

    BOOL isWow64 = FALSE;

    return fnIsWow64Process != nullptr
            && fnIsWow64Process (GetCurrentProcess(), &isWow64)
            && isWow64 != FALSE;
   #endif
}

//==============================================================================
int SystemStats::getMemorySizeInMegabytes()
{
    MEMORYSTATUSEX mem;
    mem.dwLength = sizeof (mem);
    GlobalMemoryStatusEx (&mem);
    return (int) (mem.ullTotalPhys / (1024 * 1024)) + 1;
}

//==============================================================================
String SystemStats::getEnvironmentVariable (const String& name, const String& defaultValue)
{
    auto len = GetEnvironmentVariableW (name.toWideCharPointer(), nullptr, 0);

    if (len == 0)
        return String (defaultValue);

    HeapBlock<WCHAR> buffer (len);
    len = GetEnvironmentVariableW (name.toWideCharPointer(), buffer, len);

    return String (CharPointer_wchar_t (buffer),
                   CharPointer_wchar_t (buffer + len));
}

//==============================================================================
uint32 juce_millisecondsSinceStartup() noexcept
{
    return (uint32) timeGetTime();
}

//==============================================================================
class HiResCounterHandler
{
public:
    HiResCounterHandler()
        : hiResTicksOffset (0)
    {
        // This macro allows you to override the default timer-period
        // used on Windows. By default this is set to 1, because that has
        // always been the value used in JUCE apps, and changing it could
        // affect the behaviour of existing code, but you may wish to make
        // it larger (or set it to 0 to use the system default) to make your
        // app less demanding on the CPU.
        // For more info, see win32 documentation about the timeBeginPeriod
        // function.
       #ifndef JUCE_WIN32_TIMER_PERIOD
        #define JUCE_WIN32_TIMER_PERIOD 1
       #endif

       #if JUCE_WIN32_TIMER_PERIOD > 0
        auto res = timeBeginPeriod (JUCE_WIN32_TIMER_PERIOD);
        ignoreUnused (res);
        jassert (res == TIMERR_NOERROR);
       #endif

        LARGE_INTEGER f;
        QueryPerformanceFrequency (&f);
        hiResTicksPerSecond = f.QuadPart;
        hiResTicksScaleFactor = 1000.0 / hiResTicksPerSecond;
    }

    inline int64 getHighResolutionTicks() noexcept
    {
        LARGE_INTEGER ticks;
        QueryPerformanceCounter (&ticks);
        return ticks.QuadPart + hiResTicksOffset;
    }

    inline double getMillisecondCounterHiRes() noexcept
    {
        return getHighResolutionTicks() * hiResTicksScaleFactor;
    }

    int64 hiResTicksPerSecond, hiResTicksOffset;
    double hiResTicksScaleFactor;
};

static HiResCounterHandler hiResCounterHandler;

int64  Time::getHighResolutionTicksPerSecond() noexcept  { return hiResCounterHandler.hiResTicksPerSecond; }
int64  Time::getHighResolutionTicks() noexcept           { return hiResCounterHandler.getHighResolutionTicks(); }
double Time::getMillisecondCounterHiRes() noexcept       { return hiResCounterHandler.getMillisecondCounterHiRes(); }

//==============================================================================
static int64 juce_getClockCycleCounter() noexcept
{
   #if JUCE_MSVC
    // MS intrinsics version...
    return (int64) __rdtsc();

   #elif JUCE_GCC || JUCE_CLANG
    // GNU inline asm version...
    unsigned int hi = 0, lo = 0;

    __asm__ __volatile__ (
        "xor %%eax, %%eax               \n\
         xor %%edx, %%edx               \n\
         rdtsc                          \n\
         movl %%eax, %[lo]              \n\
         movl %%edx, %[hi]"
         :
         : [hi] "m" (hi),
           [lo] "m" (lo)
         : "cc", "eax", "ebx", "ecx", "edx", "memory");

    return (int64) ((((uint64) hi) << 32) | lo);
   #else
    #error "unknown compiler?"
   #endif
}

int SystemStats::getCpuSpeedInMegahertz()
{
    auto cycles = juce_getClockCycleCounter();
    auto millis = Time::getMillisecondCounter();
    int lastResult = 0;

    for (;;)
    {
        int n = 1000000;
        while (--n > 0) {}

        auto millisElapsed = Time::getMillisecondCounter() - millis;
        auto cyclesNow = juce_getClockCycleCounter();

        if (millisElapsed > 80)
        {
            auto newResult = (int) (((cyclesNow - cycles) / millisElapsed) / 1000);

            if (millisElapsed > 500 || (lastResult == newResult && newResult > 100))
                return newResult;

            lastResult = newResult;
        }
    }
}


//==============================================================================
bool Time::setSystemTimeToThisTime() const
{
    SYSTEMTIME st;

    st.wDayOfWeek = 0;
    st.wYear           = (WORD) getYear();
    st.wMonth          = (WORD) (getMonth() + 1);
    st.wDay            = (WORD) getDayOfMonth();
    st.wHour           = (WORD) getHours();
    st.wMinute         = (WORD) getMinutes();
    st.wSecond         = (WORD) getSeconds();
    st.wMilliseconds   = (WORD) (millisSinceEpoch % 1000);

    // do this twice because of daylight saving conversion problems - the
    // first one sets it up, the second one kicks it in.
    // NB: the local variable is here to avoid analysers warning about having
    // two identical sub-expressions in the return statement
    auto firstCallToSetTimezone = SetLocalTime (&st) != 0;
    return firstCallToSetTimezone && SetLocalTime (&st) != 0;
}

int SystemStats::getPageSize()
{
    SYSTEM_INFO systemInfo;
    GetNativeSystemInfo (&systemInfo);

    return (int) systemInfo.dwPageSize;
}

//==============================================================================
String SystemStats::getLogonName()
{
    TCHAR text [256] = { 0 };
    auto len = (DWORD) numElementsInArray (text) - 1;
    GetUserName (text, &len);
    return String (text, len);
}

String SystemStats::getFullUserName()
{
    return getLogonName();
}

String SystemStats::getComputerName()
{
    TCHAR text[128] = { 0 };
    auto len = (DWORD) numElementsInArray (text) - 1;
    GetComputerNameEx (ComputerNamePhysicalDnsHostname, text, &len);
    return String (text, len);
}

static String getLocaleValue (LCID locale, LCTYPE key, const char* defaultValue)
{
    TCHAR buffer [256] = { 0 };
    if (GetLocaleInfo (locale, key, buffer, 255) > 0)
        return buffer;

    return defaultValue;
}

String SystemStats::getUserLanguage()     { return getLocaleValue (LOCALE_USER_DEFAULT, LOCALE_SISO639LANGNAME,  "en"); }
String SystemStats::getUserRegion()       { return getLocaleValue (LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, "US"); }

String SystemStats::getDisplayLanguage()
{
    DynamicLibrary dll ("kernel32.dll");
    JUCE_LOAD_WINAPI_FUNCTION (dll, GetUserDefaultUILanguage, getUserDefaultUILanguage, LANGID, (void))

    if (getUserDefaultUILanguage == nullptr)
        return "en";

    auto langID = MAKELCID (getUserDefaultUILanguage(), SORT_DEFAULT);

    auto mainLang = getLocaleValue (langID, LOCALE_SISO639LANGNAME, "en");
    auto region   = getLocaleValue (langID, LOCALE_SISO3166CTRYNAME, nullptr);

    if (region.isNotEmpty())
        mainLang << '-' << region;

    return mainLang;
}

} // namespace juce
