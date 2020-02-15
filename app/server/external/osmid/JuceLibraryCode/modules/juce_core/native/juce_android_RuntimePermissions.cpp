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

static void handleAndroidCallback (bool permissionWasGranted, RuntimePermissions::Callback* callbackPtr)
{
    if (callbackPtr == nullptr)
    {
        // got a nullptr passed in from java! this should never happen...
        jassertfalse;
        return;
    }

    std::unique_ptr<RuntimePermissions::Callback> uptr (callbackPtr);

    if (RuntimePermissions::Callback callbackObj = *uptr)
        callbackObj (permissionWasGranted);
}

JUCE_JNI_CALLBACK (JUCE_ANDROID_ACTIVITY_CLASSNAME,
                   androidRuntimePermissionsCallback,
                   void, (JNIEnv* env, jobject, jboolean permissionsGranted, jlong callbackPtr))
{
    setEnv (env);
    handleAndroidCallback (permissionsGranted != 0,
                           reinterpret_cast<RuntimePermissions::Callback*> (callbackPtr));
}

void RuntimePermissions::request (PermissionID permission, Callback callback)
{
    if (! android.activity.callBooleanMethod  (JuceAppActivity.isPermissionDeclaredInManifest, (jint) permission))
    {
        // Error! If you want to be able to request this runtime permission, you
        // also need to declare it in your app's manifest. You can do so via
        // the Projucer. Otherwise this can't work.
        jassertfalse;

        callback (false);
        return;
    }

    if (JUCE_ANDROID_API_VERSION < 23)
    {
        // There is no runtime permission system on API level below 23. As long as the
        // permission is in the manifest (seems to be the case), we can simply ask Android
        // if the app has the permission, and then directly call through to the callback.
        callback (isGranted (permission));
        return;
    }

    // we need to move the callback object to the heap so Java can keep track of the pointer
    // and asynchronously pass it back to us (to be called and then deleted)
    Callback* callbackPtr = new Callback (std::move (callback));
    android.activity.callVoidMethod (JuceAppActivity.requestRuntimePermission, permission, (jlong) callbackPtr);
}

bool RuntimePermissions::isRequired (PermissionID /*permission*/)
{
    return JUCE_ANDROID_API_VERSION >= 23;
}

bool RuntimePermissions::isGranted (PermissionID permission)
{
    return android.activity.callBooleanMethod (JuceAppActivity.isPermissionGranted, permission);
}

} // namespace juce
