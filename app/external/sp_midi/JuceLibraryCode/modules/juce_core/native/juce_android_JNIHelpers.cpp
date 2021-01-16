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

//==============================================================================
static const uint8 invocationHandleByteCode[] =
{31,139,8,8,215,115,161,94,0,3,105,110,118,111,99,97,116,105,111,110,72,97,110,100,108,101,66,121,116,101,67,111,100,101,46,
100,101,120,0,109,148,65,107,19,65,20,199,223,236,78,146,90,211,116,77,141,214,88,33,151,130,7,117,91,172,80,73,17,161,32,53,93,
17,108,233,65,5,217,38,155,102,219,237,110,220,108,99,172,8,173,40,42,244,36,245,226,65,232,165,138,7,15,226,65,193,147,120,244,
166,130,95,192,155,23,189,21,68,252,207,206,180,137,218,133,223,204,155,247,222,206,123,111,119,230,85,156,86,247,208,201,
83,84,121,127,155,63,122,245,209,24,205,141,63,156,124,186,176,229,110,12,151,158,157,126,219,76,39,136,234,68,212,154,25,201,
146,122,38,56,81,158,164,126,15,248,10,160,162,95,128,129,99,24,82,152,71,152,92,123,24,86,116,162,53,204,203,26,209,29,112,
15,108,128,231,224,37,248,2,126,128,4,252,6,192,56,184,6,102,65,21,220,2,171,224,1,120,2,94,128,215,224,29,248,0,62,129,111,224,
59,248,169,203,184,72,157,146,36,115,233,82,185,118,131,189,160,7,232,138,171,154,204,95,200,53,77,218,83,170,214,180,146,
35,77,238,153,139,107,212,99,27,35,141,122,213,218,80,181,239,83,250,108,60,51,234,139,247,213,148,191,68,188,61,13,149,208,142,
97,24,228,180,99,63,194,69,206,122,44,111,233,50,223,186,33,52,7,32,93,30,2,35,68,25,229,1,95,46,235,140,173,5,97,17,107,153,
97,154,203,245,212,89,216,17,103,24,33,71,81,141,88,215,135,52,226,44,131,90,121,252,141,250,184,172,109,170,222,233,219,67,83,
33,234,191,158,186,155,122,156,218,108,38,69,212,52,106,204,210,209,223,180,243,48,53,139,60,214,88,251,219,203,178,116,236,
223,165,190,117,50,254,15,42,243,49,215,119,163,51,196,74,148,47,45,149,157,243,126,51,40,219,145,27,248,19,182,95,241,156,240,
196,188,221,180,41,97,149,44,203,34,110,137,113,208,42,7,139,102,184,216,240,204,121,188,98,238,250,94,145,242,86,197,246,154,
238,130,105,251,126,16,197,54,115,186,22,6,55,26,69,202,90,98,91,211,179,253,57,243,226,236,188,83,142,138,148,235,208,197,126,
246,172,231,20,17,173,173,14,157,170,7,95,115,215,104,255,187,93,112,162,90,80,41,18,155,33,109,166,68,125,87,118,137,202,237,
112,174,65,137,178,231,216,33,25,21,183,81,183,163,114,237,156,235,219,158,187,236,80,102,91,35,66,46,56,212,85,221,182,36,93,
169,73,46,198,81,168,199,71,66,77,103,60,240,35,167,21,145,241,215,242,146,83,165,68,61,12,90,55,137,71,53,23,1,155,182,183,
132,237,216,193,84,70,203,23,181,185,210,113,156,146,84,102,146,246,99,188,127,153,14,235,253,185,94,72,155,164,105,236,208,0,
235,231,196,116,113,134,87,87,248,186,174,225,246,50,1,123,163,235,236,179,206,216,138,248,207,198,63,103,65,204,219,61,66,235,
232,19,122,71,175,224,29,253,34,65,237,158,145,164,118,223,208,13,41,199,231,170,32,223,89,23,62,5,169,23,247,135,25,82,31,223,
169,130,140,43,250,140,174,252,197,61,226,133,246,253,34,37,15,170,196,133,44,122,218,31,165,24,139,249,12,5,0,0,0,0};

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

 DECLARE_JNI_CLASS_WITH_BYTECODE (JuceInvocationHandler, "com/rmsl/juce/JuceInvocationHandler", 10, invocationHandleByteCode, sizeof (invocationHandleByteCode))
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
// This byte-code is generated from native/java/com/rmsl/juce/FragmentOverlay.java with min sdk version 16
// See juce_core/native/java/README.txt on how to generate this byte-code.
static const uint8 javaFragmentOverlay[] =
{31,139,8,8,26,116,161,94,0,3,106,97,118,97,70,114,97,103,109,101,110,116,79,118,101,114,108,97,121,46,100,101,120,0,133,149,
77,136,28,69,20,199,255,53,253,181,159,179,147,221,184,140,235,198,140,43,70,197,224,172,104,36,56,99,216,152,32,204,100,226,71,
54,204,97,227,165,153,105,39,189,206,118,79,186,123,150,4,20,53,4,146,131,8,6,252,130,28,114,80,65,48,8,226,65,196,83,8,66,64,
65,146,75,252,184,152,179,160,160,4,17,5,255,175,187,58,27,150,136,195,252,250,189,122,245,234,189,170,215,213,85,93,239,248,
216,226,163,187,96,79,85,156,198,103,91,86,175,30,189,252,253,193,79,203,15,189,242,199,245,246,129,179,245,238,53,27,24,0,56,
222,126,108,26,250,183,155,182,7,145,217,199,200,86,149,201,58,37,255,248,156,143,18,229,87,186,93,47,0,47,155,192,11,148,87,
12,224,7,242,27,249,157,220,32,127,145,127,200,93,244,217,69,154,228,37,242,42,57,73,206,144,55,201,89,242,62,57,79,62,36,31,
147,11,228,34,185,76,174,144,107,228,103,242,43,249,147,216,22,80,38,139,228,9,210,36,47,146,51,228,45,114,158,92,32,95,146,175,
201,183,132,211,4,167,3,46,19,14,25,33,163,122,173,227,100,70,214,76,24,62,93,223,41,58,91,186,13,237,227,104,125,66,235,111,
208,103,82,235,239,81,47,106,253,3,234,83,90,255,196,200,234,38,250,23,212,183,104,253,18,245,105,173,127,147,230,82,152,133,
204,179,144,230,40,112,118,119,235,246,130,158,199,28,196,47,235,23,121,135,150,101,100,227,239,76,165,129,249,84,218,216,150,
202,44,142,197,21,111,79,165,137,74,42,29,220,163,199,47,164,210,194,189,200,214,172,0,157,37,211,229,55,98,103,210,160,69,108,
87,173,172,134,131,146,248,202,204,87,42,82,129,188,255,71,221,159,247,4,37,155,126,69,214,209,76,223,193,117,43,91,255,50,55,
220,44,147,61,194,48,187,217,187,28,177,38,199,212,41,245,182,243,209,186,61,202,88,69,200,72,89,255,47,28,35,107,10,43,10,135,
25,209,161,117,2,115,106,22,65,197,96,149,199,177,178,196,136,75,183,70,116,210,246,96,137,121,159,47,166,239,49,203,127,227,
127,242,59,105,254,201,52,191,212,86,246,142,12,148,247,23,150,100,62,183,205,179,56,5,83,21,117,221,108,189,231,160,101,166,
143,166,117,81,154,124,191,73,111,174,139,71,33,213,77,237,99,215,253,192,79,246,96,235,211,145,219,91,243,130,228,217,117,47,
234,187,39,30,94,117,215,93,168,6,84,19,133,102,11,170,133,249,150,27,116,163,208,239,86,221,193,160,186,223,119,251,97,47,31,
85,67,249,102,111,39,12,18,154,170,141,84,212,48,115,179,39,140,171,79,13,131,110,223,171,97,123,171,19,174,85,163,181,184,95,
93,29,118,188,234,166,244,53,76,183,100,6,213,190,27,244,170,203,73,228,7,189,26,84,27,102,187,209,104,201,179,213,66,161,221,
132,213,110,138,65,4,45,70,187,41,102,114,164,129,153,35,183,9,97,117,250,97,236,193,233,12,6,135,143,250,49,204,174,155,184,
112,186,126,188,230,199,49,38,122,94,178,55,234,13,101,42,49,28,182,90,97,208,163,57,114,131,228,144,23,15,251,52,151,194,96,
111,39,241,215,253,228,68,102,194,236,102,203,51,46,91,30,70,194,96,95,228,185,137,135,98,174,233,158,185,48,56,228,29,27,122,
113,242,156,23,73,106,63,12,98,29,173,242,223,125,122,180,19,6,203,137,27,37,152,212,138,182,143,15,54,6,96,60,202,130,236,11,
187,30,198,162,116,124,170,91,113,34,83,50,19,41,192,54,56,197,194,206,26,246,83,30,168,99,143,177,227,254,178,83,60,253,14,22,
212,3,78,177,126,233,244,10,30,55,118,220,55,79,219,187,216,73,167,39,105,129,178,248,121,155,175,191,102,254,100,90,39,121,
146,220,130,165,254,54,13,117,206,42,168,239,200,57,155,210,158,220,244,205,139,204,239,4,217,143,249,189,96,96,227,110,200,247,
172,220,15,114,118,228,119,132,141,141,123,66,85,178,182,220,21,170,148,157,11,114,190,22,42,89,124,185,63,12,237,35,231,138,
28,80,42,63,115,74,153,46,247,211,191,81,33,150,205,216,6,0,0,0,0};

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (construct,   "<init>",   "()V") \
 METHOD (close,       "close",    "()V") \
 CALLBACK (FragmentOverlay::onActivityResultNative, "onActivityResultNative", "(JIILandroid/content/Intent;)V") \
 CALLBACK (FragmentOverlay::onCreateNative,         "onCreateNative",         "(JLandroid/os/Bundle;)V") \
 CALLBACK (FragmentOverlay::onStartNative,          "onStartNative",          "(J)V") \
 CALLBACK (FragmentOverlay::onRequestPermissionsResultNative, "onRequestPermissionsResultNative", "(JI[Ljava/lang/String;[I)V")

 DECLARE_JNI_CLASS_WITH_BYTECODE (JuceFragmentOverlay, "com/rmsl/juce/FragmentOverlay", 16, javaFragmentOverlay, sizeof(javaFragmentOverlay))
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
                      std::function<void (int, int, LocalRef<jobject>)> && callbackToUse)
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
    std::function<void (int, int, LocalRef<jobject>)> callback;
};

void startAndroidActivityForResult (const LocalRef<jobject>& intent, int requestCode,
                                    std::function<void (int, int, LocalRef<jobject>)> && callback)
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
