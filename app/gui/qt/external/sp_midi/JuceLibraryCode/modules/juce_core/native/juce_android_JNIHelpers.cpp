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
static const uint8 invocationHandleByteCode[] = {
31,139,8,8,170,94,229,91,0,3,105,110,118,111,99,97,116,105,111,110,95,104,97,110,100,108,101,114,46,100,101,120,0,109,
148,75,107,19,81,20,199,207,189,119,38,169,181,109,98,218,138,72,23,81,20,68,193,169,218,130,144,42,130,162,53,140,15,
104,200,66,187,232,52,185,181,147,78,103,98,58,141,33,184,168,197,47,224,66,168,187,186,241,83,168,184,16,247,162,31,193,
69,151,174,220,40,234,255,62,154,4,237,192,239,62,206,57,115,30,115,239,153,186,236,12,79,95,154,165,159,187,91,95,227,
217,197,247,87,42,31,231,191,156,157,58,241,253,199,220,201,79,75,175,118,58,14,81,147,136,58,213,153,2,217,231,131,32,
154,32,35,63,164,246,0,102,244,13,48,48,129,33,139,121,138,153,125,5,131,131,119,82,204,203,156,168,1,214,65,10,186,224,
53,120,7,62,131,61,144,131,237,57,112,30,92,4,183,192,93,176,4,154,160,11,182,193,11,97,252,171,216,46,200,144,137,59,
100,243,26,6,35,0,46,9,166,116,131,155,89,113,159,27,125,214,214,116,216,174,23,185,241,89,208,181,8,173,99,240,48,106,
247,99,182,198,156,149,231,245,204,232,136,246,203,173,189,65,189,61,7,209,31,60,167,48,239,26,119,90,183,35,84,190,66,
175,201,230,218,204,43,201,81,172,30,76,131,25,66,180,190,133,169,81,107,139,164,243,112,123,25,154,253,194,53,232,17,
231,2,74,190,140,106,212,190,57,205,201,97,99,168,207,209,223,71,61,147,202,182,57,104,59,74,11,45,212,255,56,251,60,251,
50,251,166,157,81,81,71,80,83,129,206,252,238,133,239,101,162,242,72,237,217,186,246,251,171,106,51,248,242,162,183,218,
183,207,204,133,113,152,94,37,86,38,215,47,251,190,79,142,175,198,211,126,45,89,247,90,73,20,122,141,205,154,244,202,24,
110,199,237,164,22,164,97,18,207,7,113,61,146,173,18,29,247,235,65,212,14,215,188,32,142,147,84,235,188,202,106,43,121,
178,81,162,130,223,8,218,129,23,5,241,35,239,222,114,67,214,210,18,77,14,200,180,93,176,28,201,18,162,245,197,45,185,18,
193,214,59,48,218,255,102,119,100,186,154,212,75,196,170,196,171,101,26,127,120,64,84,183,22,201,160,69,249,122,184,209,
12,210,218,234,205,48,14,162,176,43,105,108,95,162,130,173,73,26,90,217,215,100,66,35,25,141,145,66,91,94,79,226,84,118,
82,114,219,65,180,41,137,115,54,62,197,142,57,196,132,186,86,207,182,156,95,156,111,115,98,10,182,43,4,123,43,24,219,19,
185,127,206,70,247,159,237,77,62,208,159,98,160,71,157,129,62,117,169,223,171,25,234,247,171,200,155,181,62,231,162,121,
231,169,178,41,26,185,186,207,44,111,228,234,142,243,162,137,171,250,219,41,246,239,56,217,181,190,251,214,167,250,127,
252,5,76,239,47,69,120,4,0,0};

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 STATICMETHOD (newProxyInstance, "newProxyInstance", "(Ljava/lang/ClassLoader;[Ljava/lang/Class;Ljava/lang/reflect/InvocationHandler;)Ljava/lang/Object;") \

 DECLARE_JNI_CLASS (JavaProxy, "java/lang/reflect/Proxy")
#undef JNI_CLASS_MEMBERS

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor, "<init>", "(J)V") \
 METHOD (clear, "clear", "()V") \
 CALLBACK (juce_invokeImplementer, "dispatchInvoke", "(JLjava/lang/Object;Ljava/lang/reflect/Method;[Ljava/lang/Object;)Ljava/lang/Object;") \
 CALLBACK (juce_dispatchDelete, "dispatchFinalize", "(J)V")

 DECLARE_JNI_CLASS_WITH_BYTECODE (JuceInvocationHandler, "com/roli/juce/JuceInvocationHandler", 10, invocationHandleByteCode, sizeof (invocationHandleByteCode))
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD       (findClass,            "findClass",            "(Ljava/lang/String;)Ljava/lang/Class;") \
 STATICMETHOD (getSystemClassLoader, "getSystemClassLoader", "()Ljava/lang/ClassLoader;")

DECLARE_JNI_CLASS (JavaClassLoader, "java/lang/ClassLoader")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor, "<init>", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/ClassLoader;)V")

DECLARE_JNI_CLASS (AndroidDexClassLoader, "dalvik/system/DexClassLoader")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor, "<init>", "(Ljava/nio/ByteBuffer;Ljava/lang/ClassLoader;)V")

DECLARE_JNI_CLASS_WITH_MIN_SDK (AndroidInMemoryDexClassLoader, "dalvik/system/InMemoryDexClassLoader", 26)
#undef JNI_CLASS_MEMBERS

//==============================================================================
struct SystemJavaClassComparator
{
    static int compareElements (JNIClassBase* first, JNIClassBase* second)
    {
        auto isSysClassA = isSystemClass (first);
        auto isSysClassB = isSystemClass (second);

        if ((! isSysClassA) && (! isSysClassB))
        {
            return DefaultElementComparator<bool>::compareElements (first != nullptr  ? first->byteCode  != nullptr : false,
                                                                    second != nullptr ? second->byteCode != nullptr : false);
        }

        return DefaultElementComparator<bool>::compareElements (isSystemClass (first),
                                                                isSystemClass (second));
    }

    static bool isSystemClass (JNIClassBase* cls)
    {
        if (cls == nullptr)
            return false;

        String path (cls->getClassPath());

        return path.startsWith ("java/")
            || path.startsWith ("android/")
            || path.startsWith ("dalvik/");
    }
};

//==============================================================================
JNIClassBase::JNIClassBase (const char* cp, int classMinSDK, const void* bc, size_t n)
    : classPath (cp), byteCode (bc), byteCodeSize (n), minSDK (classMinSDK), classRef (nullptr)
{
    SystemJavaClassComparator comparator;

    getClasses().addSorted (comparator, this);
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

// Get code cache directory without yet having a context object
static File getCodeCacheDirectory()
{
    int pid = getpid();
    File cmdline("/proc/" + String(pid) + "/cmdline");

    auto bundleId = cmdline.loadFileAsString().trimStart().trimEnd();

    if (bundleId.isEmpty())
        return {};

    return File("/data/data/" + bundleId + "/code_cache");
}

void JNIClassBase::initialise (JNIEnv* env)
{
    auto sdkVersion = getAndroidSDKVersion();

    if (sdkVersion >= minSDK)
    {
        LocalRef<jstring> classNameAndPackage (javaString (String (classPath).replaceCharacter (L'/', L'.')));
        static Array<GlobalRef> byteCodeLoaders;

        if (! SystemJavaClassComparator::isSystemClass(this))
        {
            LocalRef<jobject> defaultClassLoader (env->CallStaticObjectMethod (JavaClassLoader, JavaClassLoader.getSystemClassLoader));
            tryLoadingClassWithClassLoader (env, defaultClassLoader.get());

            if (classRef == nullptr)
            {
                for (auto& byteCodeLoader : byteCodeLoaders)
                {
                    tryLoadingClassWithClassLoader (env, byteCodeLoader.get());

                    if (classRef != nullptr)
                        break;
                }

                // fallback by trying to load the class from bytecode
                if (byteCode != nullptr)
                {
                    LocalRef<jobject> byteCodeClassLoader;

                    MemoryOutputStream uncompressedByteCode;

                    {
                        MemoryInputStream rawGZipData (byteCode, byteCodeSize, false);
                        GZIPDecompressorInputStream gzipStream (&rawGZipData, false, GZIPDecompressorInputStream::gzipFormat);
                        uncompressedByteCode.writeFromInputStream (gzipStream, -1);
                    }

                    if (sdkVersion >= 26)
                    {
                        LocalRef<jbyteArray> byteArray (env->NewByteArray ((jsize) uncompressedByteCode.getDataSize()));
                        jboolean isCopy;
                        auto* dst = env->GetByteArrayElements (byteArray.get(), &isCopy);
                        memcpy (dst, uncompressedByteCode.getData(), uncompressedByteCode.getDataSize());
                        env->ReleaseByteArrayElements (byteArray.get(), dst, 0);

                        LocalRef<jobject> byteBuffer (env->CallStaticObjectMethod (JavaByteBuffer, JavaByteBuffer.wrap, byteArray.get()));

                        byteCodeClassLoader = LocalRef<jobject> (env->NewObject (AndroidInMemoryDexClassLoader,
                                                                                 AndroidInMemoryDexClassLoader.constructor,
                                                                                 byteBuffer.get(), defaultClassLoader.get()));
                    }
                    else if (uncompressedByteCode.getDataSize() >= 32)
                    {
                        auto codeCacheDir = getCodeCacheDirectory();

                        // The dex file has an embedded 20-byte long SHA-1 signature at offset 12
                        auto fileName = String::toHexString ((char*)uncompressedByteCode.getData() + 12, 20, 0) + ".dex";
                        auto dexFile = codeCacheDir.getChildFile (fileName);
                        auto optimizedDirectory = codeCacheDir.getChildFile ("optimized_cache");
                        optimizedDirectory.createDirectory();

                        if (dexFile.replaceWithData (uncompressedByteCode.getData(), uncompressedByteCode.getDataSize()))
                        {
                            byteCodeClassLoader = LocalRef<jobject> (env->NewObject (AndroidDexClassLoader,
                                                                                     AndroidDexClassLoader.constructor,
                                                                                     javaString (dexFile.getFullPathName()).get(),
                                                                                     javaString (optimizedDirectory.getFullPathName()).get(),
                                                                                     nullptr,
                                                                                     defaultClassLoader.get()));
                        }
                        else
                        {
                            // can't write to cache folder
                            jassertfalse;
                        }
                    }

                    if (byteCodeClassLoader != nullptr)
                    {
                        tryLoadingClassWithClassLoader (env, byteCodeClassLoader.get());
                        byteCodeLoaders.add (GlobalRef(byteCodeClassLoader));
                    }
                }
            }
        }

        if (classRef == nullptr)
            classRef = (jclass) env->NewGlobalRef (LocalRef<jobject> (env->FindClass (classPath)));

        jassert (classRef != nullptr);
        initialiseFields (env);
    }
}

void JNIClassBase::tryLoadingClassWithClassLoader (JNIEnv* env, jobject classLoader)
{
    LocalRef<jstring> classNameAndPackage (javaString (String (classPath).replaceCharacter (L'/', L'.')));

    // Android SDK <= 19 has a bug where the class loader might throw an exception but still return
    // a non-nullptr. So don't assign the result of this call to a jobject just yet...
    auto classObj = env->CallObjectMethod (classLoader, JavaClassLoader.findClass, classNameAndPackage.get());

    if (jthrowable exception = env->ExceptionOccurred ())
    {
        env->ExceptionClear();
        classObj = nullptr;
    }

    // later versions of Android don't throw at all, so re-check the object
    if (classObj != nullptr)
        classRef = (jclass) env->NewGlobalRef (LocalRef<jobject> (classObj));
}

void JNIClassBase::release (JNIEnv* env)
{
    if (classRef != nullptr)
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
    jassert (m != nullptr);
    return m;
}

jmethodID JNIClassBase::resolveStaticMethod (JNIEnv* env, const char* methodName, const char* params)
{
    jmethodID m = env->GetStaticMethodID (classRef, methodName, params);
    jassert (m != nullptr);
    return m;
}

jfieldID JNIClassBase::resolveField (JNIEnv* env, const char* fieldName, const char* signature)
{
    jfieldID f = env->GetFieldID (classRef, fieldName, signature);
    jassert (f != nullptr);
    return f;
}

jfieldID JNIClassBase::resolveStaticField (JNIEnv* env, const char* fieldName, const char* signature)
{
    jfieldID f = env->GetStaticFieldID (classRef, fieldName, signature);
    jassert (f != nullptr);
    return f;
}

void JNIClassBase::resolveCallbacks (JNIEnv* env, const Array<JNINativeMethod>& nativeCallbacks)
{
    if (nativeCallbacks.size() > 0)
        env->RegisterNatives (classRef, nativeCallbacks.begin(), (jint) nativeCallbacks.size());
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

    auto invocationHandler = LocalRef<jobject> (env->NewObject (JuceInvocationHandler, JuceInvocationHandler.constructor,
                                                                reinterpret_cast<jlong> (implementer)));

    // CreateJavaInterface() is expected to be called just once for a given implementer
    jassert (implementer->invocationHandler == nullptr);

    implementer->invocationHandler = GlobalRef (invocationHandler);

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

AndroidInterfaceImplementer::~AndroidInterfaceImplementer()
{
    clear();
}

void AndroidInterfaceImplementer::clear()
{
    if (invocationHandler != nullptr)
        getEnv()->CallVoidMethod (invocationHandler,
                                  JuceInvocationHandler.clear);
}

jobject AndroidInterfaceImplementer::invoke (jobject /*proxy*/, jobject method, jobjectArray args)
{
    auto* env = getEnv();
    return env->CallObjectMethod (method, JavaMethod.invoke, javaSubClass.get(), args);
}

jobject juce_invokeImplementer (JNIEnv*, jobject /*object*/, jlong host, jobject proxy,
                                jobject method, jobjectArray args)
{
    if (auto* myself = reinterpret_cast<AndroidInterfaceImplementer*> (host))
        return myself->invoke (proxy, method, args);

    return nullptr;
}

void juce_dispatchDelete (JNIEnv*, jobject /*object*/, jlong host)
{
    if (auto* myself = reinterpret_cast<AndroidInterfaceImplementer*> (host))
        delete myself;
}

//==============================================================================
jobject ActivityLifecycleCallbacks::invoke (jobject proxy, jobject method, jobjectArray args)
{
    auto* env = getEnv();

    auto methodName = juceString ((jstring) env->CallObjectMethod (method, JavaMethod.getName));

    auto activity = env->GetArrayLength (args) > 0 ? env->GetObjectArrayElement (args, 0) : (jobject) nullptr;
    auto bundle   = env->GetArrayLength (args) > 1 ? env->GetObjectArrayElement (args, 1) : (jobject) nullptr;

    if      (methodName == "onActivityPreCreated")             { onActivityPreCreated (activity, bundle);            return nullptr; }
    else if (methodName == "onActivityPreDestroyed")           { onActivityPreDestroyed (activity);                  return nullptr; }
    else if (methodName == "onActivityPrePaused")              { onActivityPrePaused (activity);                     return nullptr; }
    else if (methodName == "onActivityPreResumed")             { onActivityPreResumed (activity);                    return nullptr; }
    else if (methodName == "onActivityPreSaveInstanceState")   { onActivityPreSaveInstanceState (activity, bundle);  return nullptr; }
    else if (methodName == "onActivityPreStarted")             { onActivityPreStarted (activity);                    return nullptr; }
    else if (methodName == "onActivityPreStopped")             { onActivityPreStopped (activity);                    return nullptr; }
    else if (methodName == "onActivityCreated")                { onActivityCreated (activity, bundle);               return nullptr; }
    else if (methodName == "onActivityDestroyed")              { onActivityDestroyed (activity);                     return nullptr; }
    else if (methodName == "onActivityPaused")                 { onActivityPaused (activity);                        return nullptr; }
    else if (methodName == "onActivityResumed")                { onActivityResumed (activity);                       return nullptr; }
    else if (methodName == "onActivitySaveInstanceState")      { onActivitySaveInstanceState (activity, bundle);     return nullptr; }
    else if (methodName == "onActivityStarted")                { onActivityStarted (activity);                       return nullptr; }
    else if (methodName == "onActivityStopped")                { onActivityStopped (activity);                       return nullptr; }
    else if (methodName == "onActivityPostCreated")            { onActivityPostCreated (activity, bundle);           return nullptr; }
    else if (methodName == "onActivityPostDestroyed")          { onActivityPostDestroyed (activity);                 return nullptr; }
    else if (methodName == "onActivityPostPaused")             { onActivityPostPaused (activity);                    return nullptr; }
    else if (methodName == "onActivityPostResumed")            { onActivityPostResumed (activity);                   return nullptr; }
    else if (methodName == "onActivityPostSaveInstanceState")  { onActivityPostSaveInstanceState (activity, bundle); return nullptr; }
    else if (methodName == "onActivityPostStarted")            { onActivityPostStarted (activity);                   return nullptr; }
    else if (methodName == "onActivityPostStopped")            { onActivityPostStopped (activity);                   return nullptr; }

    return AndroidInterfaceImplementer::invoke (proxy, method, args);
}

//==============================================================================
int getAndroidSDKVersion()
{
    // this is used so often that we need to cache this
    static int sdkVersion = []
    {
        // don't use any jni helpers as they might not have been initialised yet
        // when this method is used
        auto* env = getEnv();

        auto buildVersion = env->FindClass ("android/os/Build$VERSION");
        jassert (buildVersion != nullptr);

        auto sdkVersionField = env->GetStaticFieldID (buildVersion, "SDK_INT", "I");
        jassert (sdkVersionField != nullptr);

        return env->GetStaticIntField (buildVersion, sdkVersionField);
    }();

    return sdkVersion;
}

bool isPermissionDeclaredInManifest (const String& requestedPermission)
{
    auto* env = getEnv();

    LocalRef<jobject> pkgManager (env->CallObjectMethod (getAppContext().get(), AndroidContext.getPackageManager));
    LocalRef<jobject> pkgName (env->CallObjectMethod (getAppContext().get(), AndroidContext.getPackageName));
    LocalRef<jobject> pkgInfo (env->CallObjectMethod (pkgManager.get(), AndroidPackageManager.getPackageInfo,
                                                      pkgName.get(), 0x00001000 /* PERMISSIONS */));

    LocalRef<jobjectArray> permissions ((jobjectArray) env->GetObjectField (pkgInfo.get(), AndroidPackageInfo.requestedPermissions));
    int n = env->GetArrayLength (permissions);

    for (int i = 0; i < n; ++i)
    {
        LocalRef<jstring> jstr ((jstring) env->GetObjectArrayElement (permissions, i));
        String permissionId (juceString (jstr));

        if (permissionId == requestedPermission)
            return true;
    }

    return false;
}

//==============================================================================
// This byte-code is generated from native/java/com/roli/juce/FragmentOverlay.java with min sdk version 16
// See juce_core/native/java/README.txt on how to generate this byte-code.
static const uint8 javaFragmentOverlay[] =
{31,139,8,8,197,105,229,91,0,3,70,114,97,103,109,101,110,116,79,118,101,114,108,97,121,46,100,101,120,0,133,149,93,136,
28,69,16,199,171,231,243,62,39,235,93,60,206,243,244,214,136,73,4,201,156,104,32,186,107,184,36,34,236,58,248,145,11,251,
112,241,101,216,29,215,137,123,51,155,153,217,35,1,65,61,2,201,131,8,10,126,97,2,17,84,16,204,91,30,228,240,73,130,95,40,
104,124,9,137,47,9,248,230,23,8,65,52,130,255,234,238,201,133,35,226,178,191,169,234,234,238,170,234,154,158,238,78,116,
100,100,254,129,157,116,170,242,197,47,171,219,126,253,250,218,79,107,167,30,234,189,251,229,123,127,239,158,219,187,86,
124,127,193,33,234,19,209,145,214,131,19,164,127,187,96,187,151,148,125,4,108,22,74,214,33,241,167,179,120,84,32,63,213,
237,186,65,244,130,69,244,12,228,121,147,232,34,248,29,252,1,174,130,191,192,63,224,118,140,217,9,154,224,121,240,34,88,
5,39,192,171,224,117,240,14,56,13,62,0,31,129,51,224,51,240,21,56,15,46,128,203,224,55,240,39,112,108,162,105,48,15,30,6,
77,240,44,56,1,94,3,167,193,25,176,6,62,7,223,2,164,73,72,135,176,76,114,193,16,24,214,107,29,5,147,188,102,0,247,114,
125,199,48,216,214,109,210,99,92,173,143,105,253,21,140,25,215,250,219,208,61,173,191,15,125,147,214,63,54,85,221,88,255,
4,250,45,90,63,7,125,66,235,223,200,88,130,166,136,243,52,100,12,3,217,221,169,219,91,116,30,51,196,227,84,63,203,91,181,
156,38,53,255,54,41,77,154,149,210,161,59,164,84,126,108,172,120,78,74,139,170,82,186,116,151,158,191,69,74,155,238,38,
181,102,65,164,163,40,157,127,67,142,146,38,44,108,251,193,86,53,236,87,120,44,103,190,84,229,10,148,253,151,116,127,217,
147,84,28,140,243,80,71,75,190,131,43,182,90,255,34,54,220,20,130,221,15,55,187,208,187,152,161,38,135,197,49,241,134,
251,225,138,51,12,95,30,241,76,94,255,207,152,195,107,74,171,130,14,192,163,11,235,24,205,136,41,74,170,38,170,60,74,75,
11,240,184,112,163,71,87,182,251,11,136,251,180,39,223,163,138,127,245,127,226,187,50,254,184,140,207,181,229,189,195,19,
249,253,165,21,206,231,166,113,230,55,145,37,60,93,55,71,239,57,210,82,233,195,178,46,66,83,238,55,238,45,117,30,97,72,
221,210,99,156,122,156,196,197,110,218,252,88,22,118,151,163,164,120,114,37,202,122,225,209,29,135,194,149,144,68,131,68,
147,140,102,64,34,160,217,32,76,58,89,26,119,252,176,223,247,31,141,195,94,218,45,103,213,104,250,122,111,59,77,10,152,
252,134,20,53,154,188,222,147,230,254,222,65,210,233,69,53,154,11,218,233,178,159,165,189,216,63,52,104,71,254,134,240,
53,154,8,56,3,191,23,38,93,127,177,200,226,164,91,35,209,34,171,213,104,4,252,12,2,50,90,77,178,91,77,54,176,128,197,108,
53,217,12,14,54,104,242,224,77,92,216,237,94,154,71,228,182,251,253,3,207,197,57,89,157,176,8,201,237,196,249,114,156,
231,52,214,141,138,61,89,119,192,169,228,228,162,21,164,73,23,230,44,76,138,253,81,62,232,193,92,73,147,61,237,34,94,137,
139,163,202,68,83,27,45,79,132,104,69,52,148,38,251,178,40,44,34,242,74,77,247,204,164,201,254,232,240,32,202,139,167,
162,140,67,199,105,146,107,111,213,255,238,211,179,221,52,89,44,194,172,160,113,173,104,251,104,127,125,2,141,102,202,
201,190,180,19,209,72,38,231,75,221,206,11,78,201,42,184,0,46,185,158,113,95,141,118,64,62,94,167,237,230,214,109,211,
174,119,252,77,26,19,219,93,175,126,238,248,18,85,205,173,247,204,194,246,22,190,57,215,123,4,22,18,54,62,111,235,229,
151,172,31,45,123,21,39,201,13,216,226,154,101,138,147,182,33,190,3,39,29,72,103,124,195,55,207,178,188,19,120,63,150,
247,130,73,235,119,67,185,103,249,126,224,179,163,188,35,28,90,191,39,68,85,181,249,174,16,21,117,46,240,249,106,84,149,
127,190,63,76,61,134,207,21,62,160,68,121,230,84,148,206,247,211,191,48,134,254,198,216,6,0,0};

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (construct,   "<init>",   "()V") \
 METHOD (close,       "close",    "()V") \
 CALLBACK (FragmentOverlay::onActivityResultNative, "onActivityResultNative", "(JIILandroid/content/Intent;)V") \
 CALLBACK (FragmentOverlay::onCreateNative,         "onCreateNative",         "(JLandroid/os/Bundle;)V") \
 CALLBACK (FragmentOverlay::onStartNative,          "onStartNative",          "(J)V") \
 CALLBACK (FragmentOverlay::onRequestPermissionsResultNative, "onRequestPermissionsResultNative", "(JI[Ljava/lang/String;[I)V")

 DECLARE_JNI_CLASS_WITH_BYTECODE (JuceFragmentOverlay, "com/roli/juce/FragmentOverlay", 16, javaFragmentOverlay, sizeof(javaFragmentOverlay))
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (show,   "show",   "(Landroid/app/FragmentManager;Ljava/lang/String;)V")

 DECLARE_JNI_CLASS (AndroidDialogFragment, "android/app/DialogFragment")
#undef JNI_CLASS_MEMBERS

//==============================================================================
FragmentOverlay::FragmentOverlay()
    : native (LocalRef<jobject> (getEnv()->NewObject (JuceFragmentOverlay, JuceFragmentOverlay.construct)))
{}

FragmentOverlay::~FragmentOverlay()
{
    auto* env = getEnv();

    env->CallVoidMethod (native.get(), JuceFragmentOverlay.close);
}

void FragmentOverlay::open()
{
    auto* env = getEnv();

    LocalRef<jobject> bundle (env->NewObject (AndroidBundle, AndroidBundle.constructor));
    env->CallVoidMethod (bundle.get(), AndroidBundle.putLong, javaString ("cppThis").get(), (jlong) this);
    env->CallVoidMethod (native.get(), AndroidFragment.setArguments, bundle.get());

    LocalRef<jobject> fm (env->CallObjectMethod (getCurrentActivity().get(), AndroidActivity.getFragmentManager));
    env->CallVoidMethod (native.get(), AndroidDialogFragment.show, fm.get(), javaString ("FragmentOverlay").get());
}

void FragmentOverlay::onActivityResultNative (JNIEnv* env, jobject, jlong host,
                                              jint requestCode, jint resultCode, jobject data)
{
    if (auto* myself = reinterpret_cast<FragmentOverlay*> (host))
        myself->onActivityResult (requestCode, resultCode, LocalRef<jobject> (env->NewLocalRef (data)));
}

void FragmentOverlay::onCreateNative (JNIEnv* env, jobject, jlong host, jobject bundle)
{
    if (auto* myself = reinterpret_cast<FragmentOverlay*> (host))
        myself->onCreated (LocalRef<jobject> (env->NewLocalRef (bundle)));
}

void FragmentOverlay::onStartNative (JNIEnv*, jobject, jlong host)
{
    if (auto* myself = reinterpret_cast<FragmentOverlay*> (host))
        myself->onStart();
}

void FragmentOverlay::onRequestPermissionsResultNative (JNIEnv* env, jobject, jlong host, jint requestCode,
                                                        jobjectArray jPermissions, jintArray jGrantResults)
{
    if (auto* myself = reinterpret_cast<FragmentOverlay*> (host))
    {
        Array<int> grantResults;
        int n = (jGrantResults != nullptr ? env->GetArrayLength (jGrantResults) : 0);

        if (n > 0)
        {
            auto* data = env->GetIntArrayElements (jGrantResults, nullptr);

            for (int i = 0; i < n; ++i)
                grantResults.add (data[i]);

            env->ReleaseIntArrayElements (jGrantResults, data, 0);
        }

        myself->onRequestPermissionsResult (requestCode,
                                            javaStringArrayToJuce (LocalRef<jobjectArray> (jPermissions)),
                                            grantResults);
    }
}

jobject FragmentOverlay::getNativeHandle()
{
    return native.get();
}

//==============================================================================
class ActivityLauncher   : public FragmentOverlay
{
public:
    ActivityLauncher (const LocalRef<jobject>& intentToUse,
                      int requestCodeToUse,
                      std::function<void(int, int, LocalRef<jobject>)> && callbackToUse)
        : intent (intentToUse), requestCode (requestCodeToUse), callback (std::move (callbackToUse))
    {}

    void onStart() override
    {
        getEnv()->CallVoidMethod (getNativeHandle(), AndroidFragment.startActivityForResult,
                                  intent.get(), requestCode);
    }

    void onActivityResult (int activityRequestCode, int resultCode, LocalRef<jobject> data) override
    {
        if (callback)
            callback (activityRequestCode, resultCode, std::move (data));

        getEnv()->CallVoidMethod (getNativeHandle(), JuceFragmentOverlay.close);
        delete this;
    }

private:
    GlobalRef intent;
    int requestCode;
    std::function<void(int, int, LocalRef<jobject>)> callback;
};

void startAndroidActivityForResult (const LocalRef<jobject>& intent, int requestCode,
                                    std::function<void(int, int, LocalRef<jobject>)> && callback)
{
    auto* activityLauncher = new ActivityLauncher (intent, requestCode, std::move (callback));
    activityLauncher->open();
}

//==============================================================================
bool androidHasSystemFeature (const String& property)
{
    LocalRef<jobject> appContext (getAppContext());

    if (appContext != nullptr)
    {
        auto* env = getEnv();

        LocalRef<jobject> packageManager (env->CallObjectMethod (appContext.get(), AndroidContext.getPackageManager));

        if (packageManager != nullptr)
            return env->CallBooleanMethod (packageManager.get(),
                                           AndroidPackageManager.hasSystemFeature,
                                           javaString (property).get()) != 0;
    }

    // unable to get app's context
    jassertfalse;
    return false;
}

String audioManagerGetProperty (const String& property)
{
    if (getAndroidSDKVersion() >= 17)
    {
        auto* env = getEnv();
        LocalRef<jobject> audioManager (env->CallObjectMethod (getAppContext().get(), AndroidContext.getSystemService,
                                                               javaString ("audio").get()));

        if (audioManager != nullptr)
        {
            LocalRef<jstring> jProperty (javaString (property));

            auto methodID = env->GetMethodID (AndroidAudioManager, "getProperty", "(Ljava/lang/String;)Ljava/lang/String;");

            if (methodID != nullptr)
                return juceString (LocalRef<jstring> ((jstring) env->CallObjectMethod (audioManager.get(),
                                                                                       methodID,
                                                                                       javaString (property).get())));
        }
    }

    return {};
}

}
