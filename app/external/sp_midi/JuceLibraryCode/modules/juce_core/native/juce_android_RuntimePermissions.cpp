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
static String jucePermissionToAndroidPermission (RuntimePermissions::PermissionID permission)
{
    switch (permission)
    {
        case RuntimePermissions::recordAudio:            return "android.permission.RECORD_AUDIO";
        case RuntimePermissions::bluetoothMidi:          return "android.permission.ACCESS_COARSE_LOCATION";
        case RuntimePermissions::readExternalStorage:    return "android.permission.READ_EXTERNAL_STORAGE";
        case RuntimePermissions::writeExternalStorage:   return "android.permission.WRITE_EXTERNAL_STORAGE";
        case RuntimePermissions::camera:                 return "android.permission.CAMERA";
    }

    // invalid permission
    jassertfalse;
    return {};
}

static RuntimePermissions::PermissionID androidPermissionToJucePermission (const String& permission)
{
    if      (permission == "android.permission.RECORD_AUDIO")             return RuntimePermissions::recordAudio;
    else if (permission == "android.permission.ACCESS_COARSE_LOCATION")   return RuntimePermissions::bluetoothMidi;
    else if (permission == "android.permission.READ_EXTERNAL_STORAGE")    return RuntimePermissions::readExternalStorage;
    else if (permission == "android.permission.WRITE_EXTERNAL_STORAGE")   return RuntimePermissions::writeExternalStorage;
    else if (permission == "android.permission.CAMERA")                   return RuntimePermissions::camera;

    return static_cast<RuntimePermissions::PermissionID> (-1);
}

//==============================================================================
struct PermissionsRequest
{
    PermissionsRequest() {}

    // using "= default" on the following method triggers an internal compiler error
    // in Android NDK 17
    PermissionsRequest (const PermissionsRequest& o)
        : callback (o.callback), permission (o.permission)
    {}

    PermissionsRequest (PermissionsRequest&& o)
        : callback (std::move (o.callback)), permission (o.permission)
    {
        o.permission = static_cast<RuntimePermissions::PermissionID> (-1);
    }

    PermissionsRequest (RuntimePermissions::Callback && callbackToUse,
                        RuntimePermissions::PermissionID permissionToRequest)
        : callback (std::move (callbackToUse)), permission (permissionToRequest)
    {}

    PermissionsRequest& operator= (const PermissionsRequest & o)
    {
        callback   = o.callback;
        permission = o.permission;
        return *this;
    }

    PermissionsRequest& operator= (PermissionsRequest && o)
    {
        callback   = std::move (o.callback);
        permission = o.permission;
        return *this;
    }

    RuntimePermissions::Callback callback;
    RuntimePermissions::PermissionID permission;
};

//==============================================================================
struct PermissionsOverlay   : FragmentOverlay
{
    PermissionsOverlay (CriticalSection& cs) : overlayGuard (cs) {}
    ~PermissionsOverlay() override = default;

    struct PermissionResult
    {
        PermissionsRequest request;
        bool granted;
    };

    void onStart() override    { onRequestPermissionsResult (0, {}, {}); }

    void onRequestPermissionsResult (int /*requestCode*/,
                                     const StringArray& permissions,
                                     const Array<int>& grantResults) override
    {
        std::vector<PermissionResult> results;

        {
            ScopedLock lock (overlayGuard);

            for (auto it = requests.begin(); it != requests.end();)
            {
                auto& request = *it;

                if (RuntimePermissions::isGranted (request.permission))
                {
                    results.push_back ({std::move (request), true});
                    it = requests.erase (it);
                }
                else
                {
                    ++it;
                }
            }

            auto n = permissions.size();

            for (int i = 0; i < n; ++i)
            {
                auto permission = androidPermissionToJucePermission (permissions[i]);
                auto granted = (grantResults.getReference (i) == 0);

                for (auto it = requests.begin(); it != requests.end();)
                {
                    auto& request = *it;

                    if (request.permission == permission)
                    {
                        results.push_back ({std::move (request), granted});
                        it = requests.erase (it);
                    }
                    else
                    {
                        ++it;
                    }
                }
            }
        }

        for (const auto& result : results)
            if (result.request.callback)
                result.request.callback (result.granted);

        {
            auto* env = getEnv();
            ScopedLock lock (overlayGuard);

            if (requests.size() > 0)
            {
                auto &request = requests.front();

                StringArray permissionsArray{
                        jucePermissionToAndroidPermission (request.permission)};
                auto jPermissionsArray = juceStringArrayToJava (permissionsArray);


                auto requestPermissionsMethodID
                    = env->GetMethodID(AndroidFragment, "requestPermissions", "([Ljava/lang/String;I)V");

                // this code should only be reached for SDKs >= 23, so this method should be
                // be available
                jassert(requestPermissionsMethodID != nullptr);

                env->CallVoidMethod (getNativeHandle(), requestPermissionsMethodID, jPermissionsArray.get (), 0);
            }
            else
            {
                getSingleton() = nullptr;
            }
        }
    }

    static std::unique_ptr<PermissionsOverlay>& getSingleton()
    {
        static std::unique_ptr<PermissionsOverlay> instance;
        return instance;
    }

    CriticalSection& overlayGuard;
    std::vector<PermissionsRequest> requests;
};

//==============================================================================
void RuntimePermissions::request (PermissionID permission, Callback callback)
{
    auto requestedPermission = jucePermissionToAndroidPermission (permission);

    if (! isPermissionDeclaredInManifest (requestedPermission))
    {
        // Error! If you want to be able to request this runtime permission, you
        // also need to declare it in your app's manifest. You can do so via
        // the Projucer. Otherwise this can't work.
        jassertfalse;

        callback (false);
        return;
    }

    auto alreadyGranted = isGranted (permission);

    if (alreadyGranted || getAndroidSDKVersion() < 23)
    {
        callback (alreadyGranted);
        return;
    }

    PermissionsRequest request (std::move (callback), permission);

    static CriticalSection overlayGuard;
    ScopedLock lock (overlayGuard);

    std::unique_ptr<PermissionsOverlay>& overlay = PermissionsOverlay::getSingleton();

    bool alreadyOpen = true;

    if (overlay == nullptr)
    {
        overlay.reset (new PermissionsOverlay (overlayGuard));
        alreadyOpen = false;
    }

    overlay->requests.push_back (std::move (request));

    if (! alreadyOpen)
        overlay->open();
}

bool RuntimePermissions::isRequired (PermissionID /*permission*/)
{
    return getAndroidSDKVersion() >= 23;
}

bool RuntimePermissions::isGranted (PermissionID permission)
{
    auto* env = getEnv();

    auto requestedPermission = jucePermissionToAndroidPermission (permission);
    int result = env->CallIntMethod (getAppContext().get(), AndroidContext.checkCallingOrSelfPermission,
                                     javaString (requestedPermission).get());


    return result == 0 /* PERMISSION_GRANTED */;
}

} // namespace juce
