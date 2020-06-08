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
extern JNIEnv* getEnv() noexcept;

//==============================================================================
template <typename JavaType>
class LocalRef
{
public:
    explicit inline LocalRef() noexcept                 : obj (nullptr) {}
    explicit inline LocalRef (JavaType o) noexcept      : obj (o) {}
    inline LocalRef (const LocalRef& other) noexcept    : obj (retain (other.obj)) {}
    inline LocalRef (LocalRef&& other) noexcept         : obj (nullptr) { std::swap (obj, other.obj); }
    ~LocalRef()                                         { clear(); }

    void clear()
    {
        if (obj != nullptr)
        {
            getEnv()->DeleteLocalRef (obj);
            obj = nullptr;
        }
    }

    LocalRef& operator= (const LocalRef& other)
    {
        JavaType newObj = retain (other.obj);
        clear();
        obj = newObj;
        return *this;
    }

    LocalRef& operator= (LocalRef&& other)
    {
        clear();
        std::swap (other.obj, obj);
        return *this;
    }

    inline operator JavaType() const noexcept   { return obj; }
    inline JavaType get() const noexcept        { return obj; }

private:
    JavaType obj;

    static JavaType retain (JavaType obj)
    {
        return obj == nullptr ? nullptr : (JavaType) getEnv()->NewLocalRef (obj);
    }
};

//==============================================================================
class GlobalRef
{
public:
    inline GlobalRef() noexcept                             : obj (nullptr) {}
    inline explicit GlobalRef (const LocalRef<jobject>& o)  : obj (retain (o.get(), getEnv())) {}
    inline explicit GlobalRef (const LocalRef<jobject>& o, JNIEnv* env)  : obj (retain (o.get(), env)) {}
    inline GlobalRef (const GlobalRef& other)           : obj (retain (other.obj, getEnv())) {}
    inline GlobalRef (GlobalRef && other) noexcept      : obj (nullptr) { std::swap (other.obj, obj); }
    ~GlobalRef()                                             { clear(); }


    inline void clear()                                 { if (obj != nullptr) clear (getEnv()); }
    inline void clear (JNIEnv* env)
    {
        if (obj != nullptr)
        {
            env->DeleteGlobalRef (obj);
            obj = nullptr;
        }
    }

    inline GlobalRef& operator= (const GlobalRef& other)
    {
        jobject newObj = retain (other.obj, getEnv());
        clear();
        obj = newObj;
        return *this;
    }

    inline GlobalRef& operator= (GlobalRef&& other)
    {
        clear();
        std::swap (obj, other.obj);

        return *this;
    }

    //==============================================================================
    inline operator jobject() const noexcept    { return obj; }
    inline jobject get() const noexcept         { return obj; }

    //==============================================================================
    #define DECLARE_CALL_TYPE_METHOD(returnType, typeName) \
        returnType call##typeName##Method (jmethodID methodID, ... ) const \
        { \
            va_list args; \
            va_start (args, methodID); \
            returnType result = getEnv()->Call##typeName##MethodV (obj, methodID, args); \
            va_end (args); \
            return result; \
        }

    DECLARE_CALL_TYPE_METHOD (jobject, Object)
    DECLARE_CALL_TYPE_METHOD (jboolean, Boolean)
    DECLARE_CALL_TYPE_METHOD (jbyte, Byte)
    DECLARE_CALL_TYPE_METHOD (jchar, Char)
    DECLARE_CALL_TYPE_METHOD (jshort, Short)
    DECLARE_CALL_TYPE_METHOD (jint, Int)
    DECLARE_CALL_TYPE_METHOD (jlong, Long)
    DECLARE_CALL_TYPE_METHOD (jfloat, Float)
    DECLARE_CALL_TYPE_METHOD (jdouble, Double)
    #undef DECLARE_CALL_TYPE_METHOD

    void callVoidMethod (jmethodID methodID, ... ) const
    {
        va_list args;
        va_start (args, methodID);
        getEnv()->CallVoidMethodV (obj, methodID, args);
        va_end (args);
    }

private:
    //==============================================================================
    jobject obj = nullptr;

    static inline jobject retain (jobject obj, JNIEnv* env)
    {
        return obj == nullptr ? nullptr : env->NewGlobalRef (obj);
    }
};


//==============================================================================
extern LocalRef<jobject> getAppContext() noexcept;
extern LocalRef<jobject> getCurrentActivity() noexcept;
extern LocalRef<jobject> getMainActivity() noexcept;

//==============================================================================
struct SystemJavaClassComparator;
class JNIClassBase
{
public:
    explicit JNIClassBase (const char* classPath, int minSDK, const void* byteCode, size_t byteCodeSize);
    virtual ~JNIClassBase();

    inline operator jclass() const noexcept  { return classRef; }

    static void initialiseAllClasses (JNIEnv*);
    static void releaseAllClasses (JNIEnv*);

    inline const char* getClassPath() const noexcept { return classPath; }

protected:
    virtual void initialiseFields (JNIEnv*) = 0;

    jmethodID resolveMethod (JNIEnv*, const char* methodName, const char* params);
    jmethodID resolveStaticMethod (JNIEnv*, const char* methodName, const char* params);
    jfieldID resolveField (JNIEnv*, const char* fieldName, const char* signature);
    jfieldID resolveStaticField (JNIEnv*, const char* fieldName, const char* signature);
    void resolveCallbacks (JNIEnv*, const Array<JNINativeMethod>&);

private:
    friend struct SystemJavaClassComparator;

    const char* const classPath;
    const void* byteCode;
    size_t byteCodeSize;

    int minSDK;
    jclass classRef = nullptr;

    static Array<JNIClassBase*>& getClasses();
    void initialise (JNIEnv*);
    void release (JNIEnv*);
    void tryLoadingClassWithClassLoader (JNIEnv* env, jobject classLoader);

    JUCE_DECLARE_NON_COPYABLE (JNIClassBase)
};

//==============================================================================
#define CREATE_JNI_METHOD(methodID, stringName, params)          methodID = resolveMethod (env, stringName, params);
#define CREATE_JNI_STATICMETHOD(methodID, stringName, params)    methodID = resolveStaticMethod (env, stringName, params);
#define CREATE_JNI_FIELD(fieldID, stringName, signature)         fieldID  = resolveField (env, stringName, signature);
#define CREATE_JNI_STATICFIELD(fieldID, stringName, signature)   fieldID  = resolveStaticField (env, stringName, signature);
#define CREATE_JNI_CALLBACK(callbackName, stringName, signature) callbacks.add ({stringName, signature, (void*) callbackName});
#define DECLARE_JNI_METHOD(methodID, stringName, params)         jmethodID methodID;
#define DECLARE_JNI_FIELD(fieldID, stringName, signature)        jfieldID  fieldID;
#define DECLARE_JNI_CALLBACK(fieldID, stringName, signature)

#define DECLARE_JNI_CLASS_WITH_BYTECODE(CppClassName, javaPath, minSDK, byteCodeData, byteCodeSize) \
    class CppClassName ## _Class   : public JNIClassBase \
    { \
    public: \
        CppClassName ## _Class() : JNIClassBase (javaPath, minSDK, byteCodeData, byteCodeSize) {} \
    \
        void initialiseFields (JNIEnv* env) \
        { \
            Array<JNINativeMethod> callbacks; \
            JNI_CLASS_MEMBERS (CREATE_JNI_METHOD, CREATE_JNI_STATICMETHOD, CREATE_JNI_FIELD, CREATE_JNI_STATICFIELD, CREATE_JNI_CALLBACK); \
            resolveCallbacks (env, callbacks); \
        } \
    \
        JNI_CLASS_MEMBERS (DECLARE_JNI_METHOD, DECLARE_JNI_METHOD, DECLARE_JNI_FIELD, DECLARE_JNI_FIELD, DECLARE_JNI_CALLBACK) \
    }; \
    static CppClassName ## _Class CppClassName;

//==============================================================================
#define DECLARE_JNI_CLASS_WITH_MIN_SDK(CppClassName, javaPath, minSDK) \
    DECLARE_JNI_CLASS_WITH_BYTECODE (CppClassName, javaPath, minSDK, nullptr, 0)

//==============================================================================
#define DECLARE_JNI_CLASS(CppClassName, javaPath) \
    DECLARE_JNI_CLASS_WITH_MIN_SDK (CppClassName, javaPath, 16)

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (getAssets,                            "getAssets",                       "()Landroid/content/res/AssetManager;") \
 METHOD (getSystemService,                     "getSystemService",                "(Ljava/lang/String;)Ljava/lang/Object;") \
 METHOD (getPackageManager,                    "getPackageManager",               "()Landroid/content/pm/PackageManager;") \
 METHOD (getPackageName,                       "getPackageName",                  "()Ljava/lang/String;") \
 METHOD (getResources,                         "getResources",                    "()Landroid/content/res/Resources;") \
 METHOD (bindService,                          "bindService",                     "(Landroid/content/Intent;Landroid/content/ServiceConnection;I)Z") \
 METHOD (unbindService,                        "unbindService",                   "(Landroid/content/ServiceConnection;)V") \
 METHOD (startActivity,                        "startActivity",                   "(Landroid/content/Intent;)V") \
 METHOD (getContentResolver,                   "getContentResolver",              "()Landroid/content/ContentResolver;") \
 METHOD (getApplicationContext,                "getApplicationContext",           "()Landroid/content/Context;") \
 METHOD (getApplicationInfo,                   "getApplicationInfo",              "()Landroid/content/pm/ApplicationInfo;") \
 METHOD (checkCallingOrSelfPermission,         "checkCallingOrSelfPermission",    "(Ljava/lang/String;)I") \
 METHOD (getCacheDir,                          "getCacheDir",                     "()Ljava/io/File;")

DECLARE_JNI_CLASS (AndroidContext, "android/content/Context")
#undef JNI_CLASS_MEMBERS

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (finish,                               "finish",                          "()V") \
 METHOD (getWindowManager,                     "getWindowManager",                "()Landroid/view/WindowManager;") \
 METHOD (setRequestedOrientation,              "setRequestedOrientation",         "(I)V") \
 METHOD (startIntentSenderForResult,           "startIntentSenderForResult",      "(Landroid/content/IntentSender;ILandroid/content/Intent;III)V") \
 METHOD (moveTaskToBack,                       "moveTaskToBack",                  "(Z)Z") \
 METHOD (startActivityForResult,               "startActivityForResult",          "(Landroid/content/Intent;I)V") \
 METHOD (getFragmentManager,                   "getFragmentManager",              "()Landroid/app/FragmentManager;") \
 METHOD (setContentView,                       "setContentView",                  "(Landroid/view/View;)V") \
 METHOD (getWindow,                            "getWindow",                       "()Landroid/view/Window;")

DECLARE_JNI_CLASS (AndroidActivity, "android/app/Activity")
#undef JNI_CLASS_MEMBERS

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (startActivityForResult,               "startActivityForResult",          "(Landroid/content/Intent;I)V") \
 METHOD (setArguments,                         "setArguments",                    "(Landroid/os/Bundle;)V")

DECLARE_JNI_CLASS_WITH_MIN_SDK (AndroidFragment, "android/app/Fragment", 11)
#undef JNI_CLASS_MEMBERS

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (build,          "build",          "()Landroid/media/AudioAttributes;") \
  METHOD (constructor,    "<init>",         "()V") \
  METHOD (setContentType, "setContentType", "(I)Landroid/media/AudioAttributes$Builder;") \
  METHOD (setUsage,       "setUsage",       "(I)Landroid/media/AudioAttributes$Builder;")

DECLARE_JNI_CLASS_WITH_MIN_SDK (AndroidAudioAttributesBuilder, "android/media/AudioAttributes$Builder", 21)
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (abandonAudioFocus, "abandonAudioFocus", "(Landroid/media/AudioManager$OnAudioFocusChangeListener;)I") \
  METHOD (requestAudioFocus, "requestAudioFocus", "(Landroid/media/AudioManager$OnAudioFocusChangeListener;II)I")

DECLARE_JNI_CLASS (AndroidAudioManager, "android/media/AudioManager")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (createBitmap,     "createBitmap", "(IILandroid/graphics/Bitmap$Config;)Landroid/graphics/Bitmap;") \
  STATICMETHOD (createBitmapFrom, "createBitmap", "(Landroid/graphics/Bitmap;IIIILandroid/graphics/Matrix;Z)Landroid/graphics/Bitmap;") \
  METHOD (compress,  "compress",  "(Landroid/graphics/Bitmap$CompressFormat;ILjava/io/OutputStream;)Z") \
  METHOD (getHeight, "getHeight", "()I") \
  METHOD (getWidth,  "getWidth",  "()I") \
  METHOD (recycle,   "recycle",   "()V") \
  METHOD (setPixel,  "setPixel",  "(III)V") \
  METHOD (getPixels, "getPixels",  "([IIIIIII)V")

DECLARE_JNI_CLASS (AndroidBitmap, "android/graphics/Bitmap")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (valueOf, "valueOf", "(Ljava/lang/String;)Landroid/graphics/Bitmap$Config;")

DECLARE_JNI_CLASS (AndroidBitmapConfig, "android/graphics/Bitmap$Config")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (decodeByteArray, "decodeByteArray", "([BII)Landroid/graphics/Bitmap;")

DECLARE_JNI_CLASS (AndroidBitmapFactory, "android/graphics/BitmapFactory")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK)    \
  METHOD (constructor,        "<init>",             "()V") \
  METHOD (containsKey,        "containsKey",        "(Ljava/lang/String;)Z") \
  METHOD (get,                "get",                "(Ljava/lang/String;)Ljava/lang/Object;") \
  METHOD (getBoolean,         "getBoolean",         "(Ljava/lang/String;)Z") \
  METHOD (getBundle,          "getBundle",          "(Ljava/lang/String;)Landroid/os/Bundle;") \
  METHOD (getCharSequence,    "getCharSequence",    "(Ljava/lang/String;)Ljava/lang/CharSequence;") \
  METHOD (getInt,             "getInt",             "(Ljava/lang/String;)I") \
  METHOD (getLong,            "getLong",            "(Ljava/lang/String;)J") \
  METHOD (getLongArray,       "getLongArray",       "(Ljava/lang/String;)[J") \
  METHOD (getParcelable,      "getParcelable",      "(Ljava/lang/String;)Landroid/os/Parcelable;") \
  METHOD (getString,          "getString",          "(Ljava/lang/String;)Ljava/lang/String;") \
  METHOD (getStringArrayList, "getStringArrayList", "(Ljava/lang/String;)Ljava/util/ArrayList;") \
  METHOD (keySet,             "keySet",             "()Ljava/util/Set;") \
  METHOD (putBoolean,         "putBoolean",         "(Ljava/lang/String;Z)V") \
  METHOD (putBundle,          "putBundle",          "(Ljava/lang/String;Landroid/os/Bundle;)V") \
  METHOD (putFloat,           "putFloat",           "(Ljava/lang/String;F)V") \
  METHOD (putInt,             "putInt",             "(Ljava/lang/String;I)V") \
  METHOD (putLong,            "putLong",            "(Ljava/lang/String;J)V") \
  METHOD (putLongArray,       "putLongArray",       "(Ljava/lang/String;[J)V") \
  METHOD (putString,          "putString",          "(Ljava/lang/String;Ljava/lang/String;)V") \
  METHOD (putStringArrayList, "putStringArrayList", "(Ljava/lang/String;Ljava/util/ArrayList;)V")

DECLARE_JNI_CLASS (AndroidBundle, "android/os/Bundle")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (dumpReferenceTables, "dumpReferenceTables", "()V")

  DECLARE_JNI_CLASS (AndroidDebug, "android/os/Debug")
#undef JNI_CLASS_MEMBERS

#define JUCE_LOG_JNI_REFERENCES_TABLE getEnv()->CallStaticVoidMethod (AndroidDebug, AndroidDebug.dumpReferenceTables);

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (getRotation, "getRotation", "()I") \
 METHOD (getMetrics,  "getMetrics",  "(Landroid/util/DisplayMetrics;)V" ) \
 METHOD (getSize,     "getSize",     "(Landroid/graphics/Point;)V" )

DECLARE_JNI_CLASS (AndroidDisplay, "android/view/Display")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (constructor,           "<init>",      "()V") \
  METHOD (constructorWithLooper, "<init>",      "(Landroid/os/Looper;)V") \
  METHOD (post,                  "post",        "(Ljava/lang/Runnable;)Z") \
  METHOD (postDelayed,           "postDelayed", "(Ljava/lang/Runnable;J)Z") \

DECLARE_JNI_CLASS (AndroidHandler, "android/os/Handler")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (constructor, "<init>",     "(Ljava/lang/String;)V") \
  METHOD (getLooper,   "getLooper",  "()Landroid/os/Looper;") \
  METHOD (join,        "join",       "()V") \
  METHOD (start,       "start",      "()V")

DECLARE_JNI_CLASS (AndroidHandlerThread, "android/os/HandlerThread")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (createChooser, "createChooser", "(Landroid/content/Intent;Ljava/lang/CharSequence;)Landroid/content/Intent;") \
  METHOD (addCategory,                    "addCategory",    "(Ljava/lang/String;)Landroid/content/Intent;") \
  METHOD (constructor,                    "<init>",         "()V") \
  METHOD (constructorWithContextAndClass, "<init>",         "(Landroid/content/Context;Ljava/lang/Class;)V") \
  METHOD (constructWithString,            "<init>",         "(Ljava/lang/String;)V") \
  METHOD (constructWithUri,               "<init>",         "(Ljava/lang/String;Landroid/net/Uri;)V") \
  METHOD (getAction,                      "getAction",      "()Ljava/lang/String;") \
  METHOD (getCategories,                  "getCategories",  "()Ljava/util/Set;") \
  METHOD (getData,                        "getData",        "()Landroid/net/Uri;") \
  METHOD (getExtras,                      "getExtras",      "()Landroid/os/Bundle;") \
  METHOD (getIntExtra,                    "getIntExtra",    "(Ljava/lang/String;I)I") \
  METHOD (getStringExtra,                 "getStringExtra", "(Ljava/lang/String;)Ljava/lang/String;") \
  METHOD (putExtra,                       "putExtra",       "(Ljava/lang/String;Ljava/lang/CharSequence;)Landroid/content/Intent;") \
  METHOD (putExtras,                      "putExtras",      "(Landroid/os/Bundle;)Landroid/content/Intent;") \
  METHOD (putExtraString,                 "putExtra",       "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;") \
  METHOD (putExtraStrings,                "putExtra",       "(Ljava/lang/String;[Ljava/lang/String;)Landroid/content/Intent;") \
  METHOD (putExtraParcelable,             "putExtra",       "(Ljava/lang/String;Landroid/os/Parcelable;)Landroid/content/Intent;") \
  METHOD (putParcelableArrayListExtra,    "putParcelableArrayListExtra", "(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;") \
  METHOD (setAction,                      "setAction",      "(Ljava/lang/String;)Landroid/content/Intent;") \
  METHOD (setFlags,                       "setFlags",       "(I)Landroid/content/Intent;") \
  METHOD (setPackage,                     "setPackage",     "(Ljava/lang/String;)Landroid/content/Intent;") \
  METHOD (setType,                        "setType",        "(Ljava/lang/String;)Landroid/content/Intent;") \

DECLARE_JNI_CLASS (AndroidIntent, "android/content/Intent")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor,    "<init>",        "()V") \
 METHOD (postRotate,     "postRotate",    "(FFF)Z") \
 METHOD (postScale,      "postScale",     "(FFFF)Z") \
 METHOD (postTranslate,  "postTranslate", "(FF)Z") \
 METHOD (setValues,      "setValues",     "([F)V") \
 METHOD (mapRect,        "mapRect",       "(Landroid/graphics/RectF;)Z")

DECLARE_JNI_CLASS (AndroidMatrix, "android/graphics/Matrix")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (getPackageInfo, "getPackageInfo", "(Ljava/lang/String;I)Landroid/content/pm/PackageInfo;") \
 METHOD (resolveActivity, "resolveActivity", "(Landroid/content/Intent;I)Landroid/content/pm/ResolveInfo;") \
 METHOD (hasSystemFeature, "hasSystemFeature", "(Ljava/lang/String;)Z")

DECLARE_JNI_CLASS (AndroidPackageManager, "android/content/pm/PackageManager")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 FIELD (requestedPermissions,   "requestedPermissions",   "[Ljava/lang/String;") \
 FIELD (activities,             "activities",             "[Landroid/content/pm/ActivityInfo;") \
 FIELD (providers,              "providers",              "[Landroid/content/pm/ProviderInfo;")

 DECLARE_JNI_CLASS (AndroidPackageInfo, "android/content/pm/PackageInfo")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 FIELD (name,        "name",        "Ljava/lang/String;") \
 FIELD (packageName, "packageName", "Ljava/lang/String;")

 DECLARE_JNI_CLASS (AndroidPackageItemInfo, "android/content/pm/PackageItemInfo")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor,   "<init>",           "(I)V") \
 METHOD (setColor,      "setColor",         "(I)V") \
 METHOD (setAlpha,      "setAlpha",         "(I)V") \
 METHOD (setTypeface,   "setTypeface",      "(Landroid/graphics/Typeface;)Landroid/graphics/Typeface;") \
 METHOD (ascent,        "ascent",           "()F") \
 METHOD (descent,       "descent",          "()F") \
 METHOD (setTextSize,   "setTextSize",      "(F)V") \
 METHOD (getTextWidths, "getTextWidths",    "(Ljava/lang/String;[F)I") \
 METHOD (setTextScaleX, "setTextScaleX",    "(F)V") \
 METHOD (getTextPath,   "getTextPath",      "(Ljava/lang/String;IIFFLandroid/graphics/Path;)V") \
 METHOD (getCharsPath,  "getTextPath",      "([CIIFFLandroid/graphics/Path;)V") \
 METHOD (setShader,     "setShader",        "(Landroid/graphics/Shader;)Landroid/graphics/Shader;") \

DECLARE_JNI_CLASS (AndroidPaint, "android/graphics/Paint")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (create,          "<init>",           "(Landroid/graphics/Bitmap;)V") \
 METHOD (setMatrix,       "setMatrix",        "(Landroid/graphics/Matrix;)V") \
 METHOD (drawPath,        "drawPath",         "(Landroid/graphics/Path;Landroid/graphics/Paint;)V") \
 METHOD (drawBitmap,      "drawBitmap",       "([IIIFFIIZLandroid/graphics/Paint;)V") \
 METHOD (getClipBounds,   "getClipBounds",    "()Landroid/graphics/Rect;")

 DECLARE_JNI_CLASS (AndroidCanvas, "android/graphics/Canvas")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (getActivity, "getActivity", "(Landroid/content/Context;ILandroid/content/Intent;I)Landroid/app/PendingIntent;") \
  METHOD (getIntentSender, "getIntentSender", "()Landroid/content/IntentSender;")

DECLARE_JNI_CLASS (AndroidPendingIntent, "android/app/PendingIntent")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (toString, "toString", "()Ljava/lang/String;")

DECLARE_JNI_CLASS_WITH_MIN_SDK (AndroidRange, "android/util/Range", 21)
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (create,   "<init>",   "(II)V") \
 FIELD  (x,        "x",        "I") \
 FIELD  (y,        "y",        "I")

DECLARE_JNI_CLASS (AndroidPoint, "android/graphics/Point")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor,   "<init>",   "(IIII)V") \
 FIELD (left,           "left",     "I") \
 FIELD (right,          "right",    "I") \
 FIELD (top,            "top",      "I") \
 FIELD (bottom,         "bottom",   "I")

DECLARE_JNI_CLASS (AndroidRect, "android/graphics/Rect")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (getIdentifier,     "getIdentifier",     "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)I") \
  METHOD (openRawResourceFd, "openRawResourceFd", "(I)Landroid/content/res/AssetFileDescriptor;")

DECLARE_JNI_CLASS (AndroidResources, "android/content/res/Resources")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (getHeight, "getHeight", "()I") \
  METHOD (getWidth,  "getWidth",  "()I")

DECLARE_JNI_CLASS_WITH_MIN_SDK (AndroidSize, "android/util/Size", 21)
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (parse, "parse", "(Ljava/lang/String;)Landroid/net/Uri;") \
  METHOD (toString, "toString", "()Ljava/lang/String;")

DECLARE_JNI_CLASS (AndroidUri, "android/net/Uri")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (construct,                 "<init>",                    "(Landroid/content/Context;)V") \
 METHOD (layout,                    "layout",                    "(IIII)V") \
 METHOD (getLeft,                   "getLeft",                   "()I") \
 METHOD (getTop,                    "getTop",                    "()I") \
 METHOD (getWidth,                  "getWidth",                  "()I") \
 METHOD (getHeight,                 "getHeight",                 "()I") \
 METHOD (getLocationOnScreen,       "getLocationOnScreen",       "([I)V") \
 METHOD (getParent,                 "getParent",                 "()Landroid/view/ViewParent;") \
 METHOD (bringToFront,              "bringToFront",              "()V") \
 METHOD (requestFocus,              "requestFocus",              "()Z") \
 METHOD (hasFocus,                  "hasFocus",                  "()Z") \
 METHOD (invalidate,                "invalidate",                "(IIII)V") \
 METHOD (setVisibility,             "setVisibility",             "(I)V") \
 METHOD (setLayoutParams,           "setLayoutParams",           "(Landroid/view/ViewGroup$LayoutParams;)V") \
 METHOD (setSystemUiVisibility,     "setSystemUiVisibility",     "(I)V") \
 METHOD (findViewById,              "findViewById",              "(I)Landroid/view/View;") \
 METHOD (getRootView,               "getRootView",               "()Landroid/view/View;") \
 METHOD (addOnLayoutChangeListener, "addOnLayoutChangeListener", "(Landroid/view/View$OnLayoutChangeListener;)V")

DECLARE_JNI_CLASS (AndroidView, "android/view/View")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (addView,    "addView",    "(Landroid/view/View;)V") \
 METHOD (removeView, "removeView", "(Landroid/view/View;)V")

DECLARE_JNI_CLASS (AndroidViewGroup, "android/view/ViewGroup")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (getDefaultDisplay, "getDefaultDisplay", "()Landroid/view/Display;")

DECLARE_JNI_CLASS (AndroidWindowManager, "android/view/WindowManager")
#undef JNI_CLASS_MEMBERS

//==============================================================================
#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (constructor, "<init>",   "(I)V") \
  METHOD (add,         "add",      "(Ljava/lang/Object;)Z") \
  METHOD (iterator,    "iterator", "()Ljava/util/Iterator;") \
  METHOD (get,         "get",      "(I)Ljava/lang/Object;") \
  METHOD (size,        "size",     "()I")

DECLARE_JNI_CLASS (JavaArrayList, "java/util/ArrayList")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (valueOf, "valueOf", "(Z)Ljava/lang/Boolean;") \
  METHOD (booleanValue, "booleanValue", "()Z")

DECLARE_JNI_CLASS (JavaBoolean, "java/lang/Boolean")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (get,        "get",       "([B)Ljava/nio/ByteBuffer;") \
  METHOD (remaining,  "remaining", "()I") \
  STATICMETHOD (wrap, "wrap",      "([B)Ljava/nio/ByteBuffer;")

DECLARE_JNI_CLASS (JavaByteBuffer, "java/nio/ByteBuffer")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (toString, "toString", "()Ljava/lang/String;")

DECLARE_JNI_CLASS (JavaCharSequence, "java/lang/CharSequence")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (forName, "forName", "(Ljava/lang/String;)Ljava/lang/Class;") \
  METHOD (getName,           "getName",           "()Ljava/lang/String;") \
  METHOD (getModifiers,      "getModifiers",      "()I")            \
  METHOD (isAnnotation,      "isAnnotation",      "()Z") \
  METHOD (isAnonymousClass,  "isAnonymousClass",  "()Z") \
  METHOD (isArray,           "isArray",           "()Z") \
  METHOD (isEnum,            "isEnum",            "()Z") \
  METHOD (isInterface,       "isInterface",       "()Z") \
  METHOD (isLocalClass,      "isLocalClass",      "()Z") \
  METHOD (isMemberClass,     "isMemberClass",     "()Z") \
  METHOD (isPrimitive,       "isPrimitive",       "()Z") \
  METHOD (isSynthetic,       "isSynthetic",       "()Z") \
  METHOD (getComponentType,  "getComponentType",  "()Ljava/lang/Class;") \
  METHOD (getSuperclass,     "getSuperclass",     "()Ljava/lang/Class;") \
  METHOD (getClassLoader,    "getClassLoader",    "()Ljava/lang/ClassLoader;") \

DECLARE_JNI_CLASS (JavaClass, "java/lang/Class")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (toString, "toString", "()Ljava/lang/String;")

DECLARE_JNI_CLASS (JavaEnum, "java/lang/Enum")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor,     "<init>",          "(Ljava/lang/String;)V") \
 METHOD (getAbsolutePath, "getAbsolutePath", "()Ljava/lang/String;") \
 METHOD (length,          "length",          "()J")

DECLARE_JNI_CLASS (JavaFile, "java/io/File")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor, "<init>", "(Ljava/lang/String;)V") \
 METHOD (close,       "close",  "()V") \
 METHOD (read,        "read",   "([B)I")

DECLARE_JNI_CLASS (JavaFileInputStream, "java/io/FileInputStream")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor, "<init>", "(Ljava/lang/String;)V") \
 METHOD (close,       "close",  "()V") \
 METHOD (write,       "write",  "([BII)V")

DECLARE_JNI_CLASS (JavaFileOutputStream, "java/io/FileOutputStream")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (constructor,             "<init>", "()V") \
  METHOD (constructorWithCapacity, "<init>", "(I)V")

DECLARE_JNI_CLASS (JavaHashMap, "java/util/HashMap")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  STATICMETHOD (parseInt, "parseInt", "(Ljava/lang/String;I)I") \
  STATICMETHOD (valueOf,  "valueOf",  "(I)Ljava/lang/Integer;") \
  METHOD (intValue, "intValue", "()I")

DECLARE_JNI_CLASS (JavaInteger, "java/lang/Integer")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (hasNext, "hasNext", "()Z") \
  METHOD (next,    "next",    "()Ljava/lang/Object;")

DECLARE_JNI_CLASS (JavaIterator, "java/util/Iterator")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (get,  "get",  "(I)Ljava/lang/Object;") \
  METHOD (size, "size", "()I")

DECLARE_JNI_CLASS (JavaList, "java/util/List")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (constructor, "<init>", "(J)V")

DECLARE_JNI_CLASS (JavaLong, "java/lang/Long")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (get,    "get",    "(Ljava/lang/Object;)Ljava/lang/Object;") \
  METHOD (keySet, "keySet", "()Ljava/util/Set;") \
  METHOD (put,    "put",    "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")

DECLARE_JNI_CLASS (JavaMap, "java/util/Map")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (getName,           "getName",           "()Ljava/lang/String;") \
  METHOD (getModifiers,      "getModifiers",      "()I")            \
  METHOD (getParameterTypes, "getParameterTypes", "()[Ljava/lang/Class;") \
  METHOD (getReturnType,     "getReturnType",     "()Ljava/lang/Class;") \
  METHOD (invoke,            "invoke",            "(Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;") \
  METHOD (hashCode,          "hashCode",          "()I") \
  METHOD (equals,            "equals",            "(Ljava/lang/Object;)Z") \

DECLARE_JNI_CLASS (JavaMethod, "java/lang/reflect/Method")
#undef JNI_CLASS_MEMBERS


#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (constructor, "<init>", "()V") \
  METHOD (getClass, "getClass", "()Ljava/lang/Class;") \
  METHOD (toString, "toString", "()Ljava/lang/String;")

DECLARE_JNI_CLASS (JavaObject, "java/lang/Object")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (contains, "contains", "(Ljava/lang/Object;)Z") \
  METHOD (iterator, "iterator", "()Ljava/util/Iterator;") \
  METHOD (size,     "size",     "()I")

DECLARE_JNI_CLASS (JavaSet, "java/util/Set")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
  METHOD (concat,   "concat",   "(Ljava/lang/String;)Ljava/lang/String;") \
  METHOD (getBytes, "getBytes", "()[B")

DECLARE_JNI_CLASS (JavaString, "java/lang/String")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK)
DECLARE_JNI_CLASS (AndroidBuild, "android/os/Build")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK)
DECLARE_JNI_CLASS (AndroidBuildVersion, "android/os/Build$VERSION")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (registerActivityLifecycleCallbacks,   "registerActivityLifecycleCallbacks",   "(Landroid/app/Application$ActivityLifecycleCallbacks;)V") \
 METHOD (unregisterActivityLifecycleCallbacks, "unregisterActivityLifecycleCallbacks", "(Landroid/app/Application$ActivityLifecycleCallbacks;)V")

 DECLARE_JNI_CLASS (AndroidApplication, "android/app/Application")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (constructor,     "<init>",          "(Landroid/content/Context;)V") \
 METHOD (getHolder,       "getHolder",       "()Landroid/view/SurfaceHolder;") \
 METHOD (getParent,       "getParent",       "()Landroid/view/ViewParent;")

 DECLARE_JNI_CLASS (AndroidSurfaceView, "android/view/SurfaceView")
#undef JNI_CLASS_MEMBERS


#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD, CALLBACK) \
 METHOD (getSurface,     "getSurface",     "()Landroid/view/Surface;") \
 METHOD (addCallback,    "addCallback",    "(Landroid/view/SurfaceHolder$Callback;)V") \
 METHOD (removeCallback, "removeCallback", "(Landroid/view/SurfaceHolder$Callback;)V")

 DECLARE_JNI_CLASS (AndroidSurfaceHolder, "android/view/SurfaceHolder")
#undef JNI_CLASS_MEMBERS

//==============================================================================
namespace
{
    inline String juceString (JNIEnv* env, jstring s)
    {
        if (s == nullptr)
            return {};

        const char* const utf8 = env->GetStringUTFChars (s, nullptr);
        CharPointer_UTF8 utf8CP (utf8);
        const String result (utf8CP);
        env->ReleaseStringUTFChars (s, utf8);
        return result;
    }

    inline String juceString (jstring s)
    {
        return juceString (getEnv(), s);
    }

    inline LocalRef<jstring> javaString (const String& s)
    {
        return LocalRef<jstring> (getEnv()->NewStringUTF (s.toUTF8()));
    }

    inline LocalRef<jstring> javaStringFromChar (const juce_wchar c)
    {
        char utf8[8] = { 0 };
        CharPointer_UTF8 (utf8).write (c);
        return LocalRef<jstring> (getEnv()->NewStringUTF (utf8));
    }

    inline LocalRef<jobjectArray> juceStringArrayToJava (const StringArray& juceArray)
    {
        auto* env = getEnv();

        LocalRef<jobjectArray> result (env->NewObjectArray ((jsize) juceArray.size(),
                                                            JavaString,
                                                            javaString ("").get()));

        for (int i = 0; i < juceArray.size(); ++i)
            env->SetObjectArrayElement (result, i, javaString (juceArray [i]).get());

        return result;
    }

    inline StringArray javaStringArrayToJuce (const LocalRef<jobjectArray>& javaArray)
    {
        if (javaArray.get() == nullptr)
            return {};

        auto* env = getEnv();

        StringArray result;

        for (int i = 0; i < env->GetArrayLength (javaArray.get()); ++i)
        {
            LocalRef<jstring> javaString ((jstring) env->GetObjectArrayElement (javaArray.get(), i));
            result.add (juceString (javaString.get()));
        }

        return result;
    }

    inline bool jniCheckHasExceptionOccurredAndClear()
    {
        auto* env = getEnv();

        LocalRef<jobject> exception (env->ExceptionOccurred());

        if (exception != nullptr)
        {
            env->ExceptionClear();
            return true;
        }

        return false;
    }
}

//==============================================================================
int getAndroidSDKVersion();
bool isPermissionDeclaredInManifest (const String& requestedPermission);

//==============================================================================
class AndroidInterfaceImplementer;

// This function takes ownership of the implementer. When the returned GlobalRef
// goes out of scope (and no other Java routine has a reference on the return-value)
// then the implementer will be deleted as well.
LocalRef<jobject> CreateJavaInterface (AndroidInterfaceImplementer* implementer,
                                       const StringArray& interfaceNames,
                                       LocalRef<jobject> subclass);

//==============================================================================
jobject juce_invokeImplementer (JNIEnv*, jobject, jlong, jobject, jobject, jobjectArray);
void    juce_dispatchDelete    (JNIEnv*, jobject, jlong);

//==============================================================================
class AndroidInterfaceImplementer
{
protected:
    virtual ~AndroidInterfaceImplementer();
    virtual jobject invoke (jobject proxy, jobject method, jobjectArray args);
    void clear();

    //==============================================================================
    friend LocalRef<jobject> CreateJavaInterface (AndroidInterfaceImplementer*, const StringArray&, LocalRef<jobject>);
    friend jobject juce_invokeImplementer (JNIEnv*, jobject, jlong, jobject, jobject, jobjectArray);
    friend void    juce_dispatchDelete    (JNIEnv*, jobject, jlong);
private:
    GlobalRef javaSubClass;
    GlobalRef invocationHandler;
};

LocalRef<jobject> CreateJavaInterface (AndroidInterfaceImplementer* implementer,
                                       const StringArray& interfaceNames);
LocalRef<jobject> CreateJavaInterface (AndroidInterfaceImplementer* implementer,
                                       const String& interfaceName);

//==============================================================================
class ActivityLifecycleCallbacks     : public AndroidInterfaceImplementer
{
public:
    virtual void onActivityPreCreated            (jobject /*activity*/, jobject /*bundle*/)  {}
    virtual void onActivityPreDestroyed          (jobject /*activity*/)                      {}
    virtual void onActivityPrePaused             (jobject /*activity*/)                      {}
    virtual void onActivityPreResumed            (jobject /*activity*/)                      {}
    virtual void onActivityPreSaveInstanceState  (jobject /*activity*/, jobject /*bundle*/)  {}
    virtual void onActivityPreStarted            (jobject /*activity*/)                      {}
    virtual void onActivityPreStopped            (jobject /*activity*/)                      {}

    virtual void onActivityCreated               (jobject /*activity*/, jobject /*bundle*/)  {}
    virtual void onActivityDestroyed             (jobject /*activity*/)                      {}
    virtual void onActivityPaused                (jobject /*activity*/)                      {}
    virtual void onActivityResumed               (jobject /*activity*/)                      {}
    virtual void onActivitySaveInstanceState     (jobject /*activity*/, jobject /*bundle*/)  {}
    virtual void onActivityStarted               (jobject /*activity*/)                      {}
    virtual void onActivityStopped               (jobject /*activity*/)                      {}

    virtual void onActivityPostCreated           (jobject /*activity*/, jobject /*bundle*/)  {}
    virtual void onActivityPostDestroyed         (jobject /*activity*/)                      {}
    virtual void onActivityPostPaused            (jobject /*activity*/)                      {}
    virtual void onActivityPostResumed           (jobject /*activity*/)                      {}
    virtual void onActivityPostSaveInstanceState (jobject /*activity*/, jobject /*bundle*/)  {}
    virtual void onActivityPostStarted           (jobject /*activity*/)                      {}
    virtual void onActivityPostStopped           (jobject /*activity*/)                      {}

private:
    jobject invoke (jobject, jobject, jobjectArray) override;
};

//==============================================================================
struct SurfaceHolderCallback    : AndroidInterfaceImplementer
{
    virtual ~SurfaceHolderCallback() override = default;

    virtual void surfaceChanged (LocalRef<jobject> holder, int format, int width, int height) = 0;
    virtual void surfaceCreated (LocalRef<jobject> holder) = 0;
    virtual void surfaceDestroyed (LocalRef<jobject> holder) = 0;

private:
    jobject invoke (jobject proxy, jobject method, jobjectArray args) override
    {
        auto* env = getEnv();
        auto methodName = juceString ((jstring) env->CallObjectMethod (method, JavaMethod.getName));
        LocalRef<jobject> holder (env->GetArrayLength (args) > 0 ? env->GetObjectArrayElement (args, 0) : (jobject) nullptr);

        if (methodName == "surfaceChanged")
        {
            int intArgs[3];

            for (int i = 0; i < 3; ++i)
            {
                LocalRef<jobject> boxedType (env->GetObjectArrayElement (args, 1 + i));
                intArgs[i] = env->CallIntMethod (boxedType.get(), JavaInteger.intValue);
            }

            surfaceChanged (std::move (holder), intArgs[0], intArgs[1], intArgs[2]);
        }
        else if (methodName == "surfaceCreated")
        {
            surfaceCreated (std::move (holder));
        }
        else if (methodName == "surfaceDestroyed")
        {
            surfaceDestroyed (std::move (holder));
        }
        else
        {
            return AndroidInterfaceImplementer::invoke (proxy, method, args);
        }

        return nullptr;
    }
};

//==============================================================================
class FragmentOverlay
{
public:
    FragmentOverlay();
    virtual ~FragmentOverlay();

    void open();

    virtual void onCreated (LocalRef<jobject> /*bundle*/) {}
    virtual void onStart() {}
    virtual void onRequestPermissionsResult (int /*requestCode*/,
                                             const StringArray& /*permissions*/,
                                             const Array<int>& /*grantResults*/) {}
    virtual void onActivityResult (int /*requestCode*/, int /*resultCode*/, LocalRef<jobject> /*data*/) {}

protected:
    jobject getNativeHandle();

private:

    GlobalRef native;

public:
    /* internal: do not use */
    static void onActivityResultNative (JNIEnv*, jobject, jlong, jint, jint, jobject);
    static void onCreateNative (JNIEnv*, jobject, jlong, jobject);
    static void onStartNative (JNIEnv*, jobject, jlong);
    static void onRequestPermissionsResultNative (JNIEnv*, jobject, jlong, jint,
                                                  jobjectArray, jintArray);
};

//==============================================================================
// Allows you to start an activity without requiring to have an activity
void startAndroidActivityForResult (const LocalRef<jobject>& intent, int requestCode,
                                    std::function<void(int, int, LocalRef<jobject>)> && callback);

//==============================================================================
bool androidHasSystemFeature (const String& property);
String audioManagerGetProperty (const String& property);

} // namespace juce
