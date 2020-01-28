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
  METHOD (constructor, "<init>",     "(Landroid/content/Context;Landroid/media/MediaScannerConnection$MediaScannerConnectionClient;)V") \
  METHOD (connect,     "connect",    "()V") \
  METHOD (disconnect,  "disconnect", "()V") \
  METHOD (scanFile,    "scanFile",   "(Ljava/lang/String;Ljava/lang/String;)V") \

DECLARE_JNI_CLASS (MediaScannerConnection, "android/media/MediaScannerConnection");
#undef JNI_CLASS_MEMBERS

//==============================================================================
class MediaScannerConnectionClient : public AndroidInterfaceImplementer
{
public:
    virtual void onMediaScannerConnected() = 0;
    virtual void onScanCompleted() = 0;

private:
    jobject invoke (jobject proxy, jobject method, jobjectArray args) override
    {
        auto* env = getEnv();

        auto methodName = juceString ((jstring) env->CallObjectMethod (method, Method.getName));

        if (methodName == "onMediaScannerConnected")
        {
            onMediaScannerConnected();
            return nullptr;
        }
        else if (methodName == "onScanCompleted")
        {
            onScanCompleted();
            return nullptr;
        }

        return AndroidInterfaceImplementer::invoke (proxy, method, args);
    }
};

//==============================================================================
bool File::isOnCDRomDrive() const
{
    return false;
}

bool File::isOnHardDisk() const
{
    return true;
}

bool File::isOnRemovableDrive() const
{
    return false;
}

String File::getVersion() const
{
    return {};
}

static File getSpecialFile (jmethodID type)
{
    return File (juceString (LocalRef<jstring> ((jstring) getEnv()->CallStaticObjectMethod (JuceAppActivity, type))));
}

File File::getSpecialLocation (const SpecialLocationType type)
{
    switch (type)
    {
        case userHomeDirectory:
        case userApplicationDataDirectory:
        case userDesktopDirectory:
        case commonApplicationDataDirectory:
            return File (android.appDataDir);

        case userDocumentsDirectory:
        case commonDocumentsDirectory:  return getSpecialFile (JuceAppActivity.getDocumentsFolder);
        case userPicturesDirectory:     return getSpecialFile (JuceAppActivity.getPicturesFolder);
        case userMusicDirectory:        return getSpecialFile (JuceAppActivity.getMusicFolder);
        case userMoviesDirectory:       return getSpecialFile (JuceAppActivity.getMoviesFolder);

        case globalApplicationsDirectory:
            return File ("/system/app");

        case tempDirectory:
        {
            File tmp = File (android.appDataDir).getChildFile (".temp");
            tmp.createDirectory();
            return File (tmp.getFullPathName());
        }

        case invokedExecutableFile:
        case currentExecutableFile:
        case currentApplicationFile:
        case hostApplicationPath:
            return juce_getExecutableFile();

        default:
            jassertfalse; // unknown type?
            break;
    }

    return {};
}

bool File::moveToTrash() const
{
    if (! exists())
        return true;

    // TODO
    return false;
}

JUCE_API bool JUCE_CALLTYPE Process::openDocument (const String& fileName, const String&)
{
    const LocalRef<jstring> t (javaString (fileName));
    android.activity.callVoidMethod (JuceAppActivity.launchURL, t.get());
    return true;
}

void File::revealToUser() const
{
}

//==============================================================================
class SingleMediaScanner : public MediaScannerConnectionClient
{
public:
    SingleMediaScanner (const String& filename)
        : msc (getEnv()->NewObject (MediaScannerConnection,
                                    MediaScannerConnection.constructor,
                                    android.activity.get(),
                                    CreateJavaInterface (this, "android/media/MediaScannerConnection$MediaScannerConnectionClient").get())),
          file (filename)
    {
        getEnv()->CallVoidMethod (msc.get(), MediaScannerConnection.connect);
    }

    void onMediaScannerConnected() override
    {
        auto* env = getEnv();

        env->CallVoidMethod (msc.get(), MediaScannerConnection.scanFile, javaString (file).get(), 0);
    }

    void onScanCompleted() override
    {
        getEnv()->CallVoidMethod (msc.get(), MediaScannerConnection.disconnect);
    }

private:
    GlobalRef msc;
    String file;
};

void FileOutputStream::flushInternal()
{
    if (fileHandle != 0)
    {
        if (fsync (getFD (fileHandle)) == -1)
            status = getResultForErrno();

        // This stuff tells the OS to asynchronously update the metadata
        // that the OS has cached aboud the file - this metadata is used
        // when the device is acting as a USB drive, and unless it's explicitly
        // refreshed, it'll get out of step with the real file.
        new SingleMediaScanner (file.getFullPathName());
    }
}

} // namespace juce
