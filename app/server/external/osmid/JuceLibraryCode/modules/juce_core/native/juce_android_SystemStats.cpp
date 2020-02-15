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

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD) \
 STATICMETHOD (newProxyInstance, "newProxyInstance", "(Ljava/lang/ClassLoader;[Ljava/lang/Class;Ljava/lang/reflect/InvocationHandler;)Ljava/lang/Object;") \

 DECLARE_JNI_CLASS (JavaProxy, "java/lang/reflect/Proxy");
#undef JNI_CLASS_MEMBERS

JNIClassBase::JNIClassBase (const char* cp)   : classPath (cp), classRef (0)
{
    getClasses().add (this);
}

JNIClassBase::~JNIClassBase()
{
    getClasses().removeFirstMatchingValue (this);
}

Array<JNIClassBase*>& JNIClassBase::getClasses()
{
    static Array<JNIClassBase*> classes;
    return classes;
}

void JNIClassBase::initialise (JNIEnv* env)
{
    classRef = (jclass) env->NewGlobalRef (env->FindClass (classPath));
    jassert (classRef != 0);

    initialiseFields (env);
}

void JNIClassBase::release (JNIEnv* env)
{
    env->DeleteGlobalRef (classRef);
}

void JNIClassBase::initialiseAllClasses (JNIEnv* env)
{
    const Array<JNIClassBase*>& classes = getClasses();
    for (int i = classes.size(); --i >= 0;)
        classes.getUnchecked(i)->initialise (env);
}

void JNIClassBase::releaseAllClasses (JNIEnv* env)
{
    const Array<JNIClassBase*>& classes = getClasses();
    for (int i = classes.size(); --i >= 0;)
        classes.getUnchecked(i)->release (env);
}

jmethodID JNIClassBase::resolveMethod (JNIEnv* env, const char* methodName, const char* params)
{
    jmethodID m = env->GetMethodID (classRef, methodName, params);
    jassert (m != 0);
    return m;
}

jmethodID JNIClassBase::resolveStaticMethod (JNIEnv* env, const char* methodName, const char* params)
{
    jmethodID m = env->GetStaticMethodID (classRef, methodName, params);
    jassert (m != 0);
    return m;
}

jfieldID JNIClassBase::resolveField (JNIEnv* env, const char* fieldName, const char* signature)
{
    jfieldID f = env->GetFieldID (classRef, fieldName, signature);
    jassert (f != 0);
    return f;
}

jfieldID JNIClassBase::resolveStaticField (JNIEnv* env, const char* fieldName, const char* signature)
{
    jfieldID f = env->GetStaticFieldID (classRef, fieldName, signature);
    jassert (f != 0);
    return f;
}

//==============================================================================
LocalRef<jobject> CreateJavaInterface (AndroidInterfaceImplementer* implementer,
                                       const StringArray& interfaceNames,
                                       LocalRef<jobject> subclass)
{
    auto* env = getEnv();

    implementer->javaSubClass = GlobalRef (subclass);

    // you need to override at least one interface
    jassert (interfaceNames.size() > 0);

    auto classArray = LocalRef<jobject> (env->NewObjectArray (interfaceNames.size(), JavaClass, nullptr));
    LocalRef<jobject> classLoader;

    for (auto i = 0; i < interfaceNames.size(); ++i)
    {
        auto aClass = LocalRef<jobject> (env->FindClass (interfaceNames[i].toRawUTF8()));

        if (aClass != nullptr)
        {
            if (i == 0)
                classLoader = LocalRef<jobject> (env->CallObjectMethod (aClass, JavaClass.getClassLoader));

            env->SetObjectArrayElement ((jobjectArray) classArray.get(), i, aClass);
        }
        else
        {
            // interface class not found
            jassertfalse;
        }
    }

    auto invocationHandler = LocalRef<jobject> (env->CallStaticObjectMethod (JuceAppActivity,
                                                                             JuceAppActivity.createInvocationHandler,
                                                                             reinterpret_cast<jlong> (implementer)));

    return LocalRef<jobject> (env->CallStaticObjectMethod (JavaProxy, JavaProxy.newProxyInstance,
                                                           classLoader.get(), classArray.get(),
                                                           invocationHandler.get()));
}

LocalRef<jobject> CreateJavaInterface (AndroidInterfaceImplementer* implementer,
                                       const StringArray& interfaceNames)
{
    return CreateJavaInterface (implementer, interfaceNames,
                                LocalRef<jobject> (getEnv()->NewObject (JavaObject,
                                                                        JavaObject.constructor)));
}

LocalRef<jobject> CreateJavaInterface (AndroidInterfaceImplementer* implementer,
                                       const String& interfaceName)
{
    return CreateJavaInterface (implementer, StringArray (interfaceName));
}

jobject AndroidInterfaceImplementer::invoke (jobject /*proxy*/, jobject method, jobjectArray args)
{
    auto* env = getEnv();
    return env->CallObjectMethod (method, Method.invoke, javaSubClass.get(), args);
}

jobject juce_invokeImplementer (JNIEnv* env, jlong thisPtr, jobject proxy, jobject method, jobjectArray args)
{
    setEnv (env);
    return reinterpret_cast<AndroidInterfaceImplementer*> (thisPtr)->invoke (proxy, method, args);
}

void juce_dispatchDelete (JNIEnv* env, jlong thisPtr)
{
    setEnv (env);
    delete reinterpret_cast<AndroidInterfaceImplementer*> (thisPtr);
}

JUCE_JNI_CALLBACK (JUCE_JOIN_MACRO (JUCE_ANDROID_ACTIVITY_CLASSNAME, _00024NativeInvocationHandler), dispatchInvoke,
                   jobject, (JNIEnv* env, jobject /*object*/, jlong thisPtr, jobject proxy, jobject method, jobjectArray args))
{
    return juce_invokeImplementer (env, thisPtr, proxy, method, args);
}

JUCE_JNI_CALLBACK (JUCE_JOIN_MACRO (JUCE_ANDROID_ACTIVITY_CLASSNAME, _00024NativeInvocationHandler), dispatchFinalize,
                   void, (JNIEnv* env, jobject /*object*/, jlong thisPtr))
{
    juce_dispatchDelete (env, thisPtr);
}

//==============================================================================
JavaVM* androidJNIJavaVM = nullptr;

class JniEnvThreadHolder
{
public:
    static JniEnvThreadHolder& getInstance() noexcept
    {
        // You cann only use JNI functions AFTER JNI_OnLoad was called
        jassert (androidJNIJavaVM != nullptr);

        try
        {
            if (instance == nullptr)
                instance = new JniEnvThreadHolder;
        }
        catch (...)
        {
            jassertfalse;
            std::terminate();
        }

        return *instance;
    }

    static JNIEnv* getEnv()   { return reinterpret_cast<JNIEnv*> (pthread_getspecific (getInstance().threadKey)); }

    static void setEnv (JNIEnv* env)
    {
        // env must not be a nullptr
        jassert (env != nullptr);

       #if JUCE_DEBUG
        JNIEnv* oldenv = reinterpret_cast<JNIEnv*> (pthread_getspecific (getInstance().threadKey));

        // This thread is already attached to the JavaVM and you trying to attach
        // it to a different instance of the VM.
        jassert (oldenv == nullptr || oldenv == env);
       #endif

        pthread_setspecific (getInstance().threadKey, env);
    }

private:
    pthread_key_t threadKey;

    static void threadDetach (void* p)
    {
        if (JNIEnv* env = reinterpret_cast<JNIEnv*> (p))
        {
            ignoreUnused (env);

            androidJNIJavaVM->DetachCurrentThread();
        }
    }

    JniEnvThreadHolder()
    {
        pthread_key_create (&threadKey, threadDetach);
    }

    static JniEnvThreadHolder* instance;
};

JniEnvThreadHolder* JniEnvThreadHolder::instance = nullptr;

//==============================================================================
JNIEnv* attachAndroidJNI() noexcept
{
    auto* env = JniEnvThreadHolder::getEnv();

    if (env == nullptr)
    {
        androidJNIJavaVM->AttachCurrentThread (&env, nullptr);
        setEnv (env);
    }

    return env;
}

JNIEnv* getEnv() noexcept
{
    auto* env = JniEnvThreadHolder::getEnv();

    // You are trying to use a JUCE function on a thread that was not created by JUCE.
    // You need to first call setEnv on this thread before using JUCE
    jassert (env != nullptr);

    return env;
}

void setEnv (JNIEnv* env) noexcept   { JniEnvThreadHolder::setEnv (env); }

extern "C" jint JNI_OnLoad (JavaVM* vm, void*)
{
    // Huh? JNI_OnLoad was called two times!
    jassert (androidJNIJavaVM == nullptr);

    androidJNIJavaVM = vm;
    return JNI_VERSION_1_2;
}

//==============================================================================
AndroidSystem::AndroidSystem() : screenWidth (0), screenHeight (0), dpi (160)
{
}

void AndroidSystem::initialise (JNIEnv* env, jobject act, jstring file, jstring dataDir)
{
    setEnv (env);

    screenWidth = screenHeight = 0;
    dpi = 160;
    JNIClassBase::initialiseAllClasses (env);

    activity = GlobalRef (act);
    appFile = juceString (env, file);
    appDataDir = juceString (env, dataDir);
}

void AndroidSystem::shutdown (JNIEnv* env)
{
    activity.clear();

    JNIClassBase::releaseAllClasses (env);
}

AndroidSystem android;

//==============================================================================
namespace AndroidStatsHelpers
{
    #define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD) \
     STATICMETHOD (getProperty, "getProperty", "(Ljava/lang/String;)Ljava/lang/String;")

    DECLARE_JNI_CLASS (SystemClass, "java/lang/System");
    #undef JNI_CLASS_MEMBERS

    static inline String getSystemProperty (const String& name)
    {
        return juceString (LocalRef<jstring> ((jstring) getEnv()->CallStaticObjectMethod (SystemClass,
                                                                                          SystemClass.getProperty,
                                                                                          javaString (name).get())));
    }

    static inline String getLocaleValue (bool isRegion)
    {
        return juceString (LocalRef<jstring> ((jstring) getEnv()->CallStaticObjectMethod (JuceAppActivity,
                                                                                          JuceAppActivity.getLocaleValue,
                                                                                          isRegion)));
    }

    #define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD)
    DECLARE_JNI_CLASS (BuildClass, "android/os/Build");
    #undef JNI_CLASS_MEMBERS

    static inline String getAndroidOsBuildValue (const char* fieldName)
    {
        return juceString (LocalRef<jstring> ((jstring) getEnv()->GetStaticObjectField (
                            BuildClass, getEnv()->GetStaticFieldID (BuildClass, fieldName, "Ljava/lang/String;"))));
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

int SystemStats::getCpuSpeedInMegaherz()
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
