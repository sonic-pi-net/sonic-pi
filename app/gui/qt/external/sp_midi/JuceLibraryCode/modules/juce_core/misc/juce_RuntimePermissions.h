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
/**
    Class to handle app runtime permissions for certain functionality on some platforms.

    The use of this class is currently only required if the app should run on
    Android API level 23 and higher.

    On lower API levels, the permissions are specified in the app manifest. On iOS,
    runtime permission requests are handled automatically by the Apple APIs and not
    manually in the app code. On Windows, OS X, and Linux, runtime permissions are not
    used at all. In all these cases, request() will simply call through to the
    callback with no overhead and pass true (making it safe to use on all platforms).

    For example, to enable audio recording on Android in your cross-platform app,
    you could modify your code as follows:

    Old code:

        audioDeviceManager.initialise (2, 2, nullptr, true, String(), nullptr);

    New code:

        RuntimePermissions::request (
            RuntimePermissions::audioRecording,
            [this] (bool wasGranted)
            {
                 if (! wasGranted)
                 {
                     // e.g. display an error or initialise with 0 input channels
                     return;
                 }

                 audioDeviceManager.initialise (2, 2, nullptr, true, String(), nullptr);
            }
        );

    @tags{Core}
*/
class JUCE_API  RuntimePermissions
{
public:
    //==============================================================================
    enum PermissionID
    {
        /** Permission to access the microphone (required on Android).
            You need to request this, for example, to initialise an AudioDeviceManager with
            a non-zero number of input channels, and to open the default audio input device.
        */
        recordAudio = 1,

        /** Permission to scan for and pair to Bluetooth MIDI devices (required on Android).
            You need to request this before calling BluetoothMidiDevicePairingDialogue::open(),
            otherwise no devices will be found.
        */
        bluetoothMidi = 2,

        /** Permission to read from external storage such as SD cards */
        readExternalStorage = 3,

        /** Permission to write to external storage such as SD cards */
        writeExternalStorage = 4,

        /** Permission to use camera */
        camera = 5
    };

    //==============================================================================
    /** Function type of runtime permission request callbacks. */
    using Callback = std::function<void (bool)>;

    //==============================================================================
    /** Call this method to request a runtime permission.

        @param permission  The PermissionID of the permission you want to request.

        @param callback    The callback to be called after the request has been granted
                           or denied; the argument passed will be true if the permission
                           has been granted and false otherwise.

        If no runtime request is required or possible to obtain the permission, the
        callback will be called immediately. The argument passed in will be true
        if the permission is granted or no permission is required on this platform,
        and false otherwise.

        If a runtime request is required to obtain the permission, the callback
        will be called asynchronously after the OS has granted or denied the requested
        permission (typically by displaying a dialog box to the user and waiting until
        the user has responded).
    */
    static void request (PermissionID permission, Callback callback);

    /** Returns whether a runtime request is required to obtain the permission
        on the current platform.
    */
    static bool isRequired (PermissionID permission);

    /** Returns true if the app has been already granted this permission, either
        via a previous runtime request or otherwise, or no permission is necessary.

        Note that this can be false even if isRequired returns false. In this case,
        the permission can not be obtained at all at runtime.
    */
    static bool isGranted (PermissionID permission);
};

} // namespace juce
