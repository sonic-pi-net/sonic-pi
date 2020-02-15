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

#pragma once

#undef T

//==============================================================================
#if JUCE_MAC || JUCE_IOS

 #if JUCE_IOS
  #import <Foundation/Foundation.h>
  #import <UIKit/UIKit.h>
  #import <CoreData/CoreData.h>
  #import <MobileCoreServices/MobileCoreServices.h>
  #include <sys/fcntl.h>
 #else
  #import <Cocoa/Cocoa.h>
  #if (! defined MAC_OS_X_VERSION_10_12) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12
   #define NSEventModifierFlagCommand       NSCommandKeyMask
   #define NSEventModifierFlagControl       NSControlKeyMask
   #define NSEventModifierFlagHelp          NSHelpKeyMask
   #define NSEventModifierFlagNumericPad    NSNumericPadKeyMask
   #define NSEventModifierFlagOption        NSAlternateKeyMask
   #define NSEventModifierFlagShift         NSShiftKeyMask
   #define NSCompositingOperationSourceOver NSCompositeSourceOver
   #define NSEventMaskApplicationDefined    NSApplicationDefinedMask
   #define NSEventTypeApplicationDefined    NSApplicationDefined
   #define NSEventTypeCursorUpdate          NSCursorUpdate
   #define NSEventTypeMouseMoved            NSMouseMoved
   #define NSEventTypeLeftMouseDown         NSLeftMouseDown
   #define NSEventTypeRightMouseDown        NSRightMouseDown
   #define NSEventTypeOtherMouseDown        NSOtherMouseDown
   #define NSEventTypeLeftMouseUp           NSLeftMouseUp
   #define NSEventTypeRightMouseUp          NSRightMouseUp
   #define NSEventTypeOtherMouseUp          NSOtherMouseUp
   #define NSEventTypeLeftMouseDragged      NSLeftMouseDragged
   #define NSEventTypeRightMouseDragged     NSRightMouseDragged
   #define NSEventTypeOtherMouseDragged     NSOtherMouseDragged
   #define NSEventTypeScrollWheel           NSScrollWheel
   #define NSEventTypeKeyDown               NSKeyDown
   #define NSEventTypeKeyUp                 NSKeyUp
   #define NSEventTypeFlagsChanged          NSFlagsChanged
   #define NSEventMaskAny                   NSAnyEventMask
   #define NSWindowStyleMaskBorderless      NSBorderlessWindowMask
   #define NSWindowStyleMaskClosable        NSClosableWindowMask
   #define NSWindowStyleMaskFullScreen      NSFullScreenWindowMask
   #define NSWindowStyleMaskMiniaturizable  NSMiniaturizableWindowMask
   #define NSWindowStyleMaskResizable       NSResizableWindowMask
   #define NSWindowStyleMaskTitled          NSTitledWindowMask
   #define NSAlertStyleCritical             NSCriticalAlertStyle
   #define NSControlSizeRegular             NSRegularControlSize
   #define NSEventTypeMouseEntered          NSMouseEntered
   #define NSEventTypeMouseExited           NSMouseExited
   #define NSAlertStyleInformational        NSInformationalAlertStyle
   #define NSEventTypeTabletPoint           NSTabletPoint
   #define NSEventTypeTabletProximity       NSTabletProximity
  #endif
  #import <CoreAudio/HostTime.h>
  #include <sys/dir.h>
 #endif

 #include <sys/socket.h>
 #include <sys/sysctl.h>
 #include <sys/stat.h>
 #include <sys/param.h>
 #include <sys/mount.h>
 #include <sys/utsname.h>
 #include <sys/mman.h>
 #include <fnmatch.h>
 #include <utime.h>
 #include <dlfcn.h>
 #include <ifaddrs.h>
 #include <net/if_dl.h>
 #include <mach/mach_time.h>
 #include <mach-o/dyld.h>
 #include <objc/runtime.h>
 #include <objc/objc.h>
 #include <objc/message.h>

//==============================================================================
#elif JUCE_WINDOWS
 #if JUCE_MSVC
  #ifndef _CPPRTTI
   #error "You're compiling without RTTI enabled! This is needed for a lot of JUCE classes, please update your compiler settings!"
  #endif

  #ifndef _CPPUNWIND
   #error "You're compiling without exceptions enabled! This is needed for a lot of JUCE classes, please update your compiler settings!"
  #endif

  #pragma warning (push, 0) // disable all warnings whilst including system headers
 #endif

 #define NOMINMAX

 #define _WINSOCK_DEPRECATED_NO_WARNINGS 1
 #define STRICT 1
 #define WIN32_LEAN_AND_MEAN 1
 #if JUCE_MINGW
  #define _WIN32_WINNT 0x0501
 #else
  #define _WIN32_WINNT 0x0602
 #endif
 #define _UNICODE 1
 #define UNICODE 1
 #ifndef _WIN32_IE
  #define _WIN32_IE 0x0500
 #endif

 #include <windows.h>
 #include <shellapi.h>
 #include <tchar.h>
 #include <stddef.h>
 #include <ctime>
 #include <wininet.h>
 #include <nb30.h>
 #include <winsock2.h>
 #include <ws2tcpip.h>
 #include <iphlpapi.h>
 #include <mapi.h>
 #include <float.h>
 #include <process.h>
 #include <shlobj.h>
 #include <shlwapi.h>
 #include <mmsystem.h>

 #if JUCE_MINGW
  #include <basetyps.h>
  #include <sys/time.h>
  #ifndef alloca
   #define alloca __builtin_alloca
  #endif
 #else
  #include <crtdbg.h>
  #include <comutil.h>
 #endif

 #ifndef S_FALSE
  #define S_FALSE (1) // (apparently some obscure win32 dev environments don't define this)
 #endif

 #undef PACKED

 #if JUCE_MSVC
  #pragma warning (pop)
  #pragma warning (4: 4511 4512 4100)
 #endif

 #if JUCE_MSVC && ! JUCE_DONT_AUTOLINK_TO_WIN32_LIBRARIES
  #pragma comment (lib, "kernel32.lib")
  #pragma comment (lib, "user32.lib")
  #pragma comment (lib, "wininet.lib")
  #pragma comment (lib, "advapi32.lib")
  #pragma comment (lib, "ws2_32.lib")
  #pragma comment (lib, "version.lib")
  #pragma comment (lib, "shlwapi.lib")
  #pragma comment (lib, "winmm.lib")

  #ifdef _NATIVE_WCHAR_T_DEFINED
   #ifdef _DEBUG
    #pragma comment (lib, "comsuppwd.lib")
   #else
    #pragma comment (lib, "comsuppw.lib")
   #endif
  #else
   #ifdef _DEBUG
    #pragma comment (lib, "comsuppd.lib")
   #else
    #pragma comment (lib, "comsupp.lib")
   #endif
  #endif
 #endif

 /* Used with DynamicLibrary to simplify importing functions from a win32 DLL.

    dll: the DynamicLibrary object
    functionName: function to import
    localFunctionName: name you want to use to actually call it (must be different)
    returnType: the return type
    params: list of params (bracketed)
 */
 #define JUCE_LOAD_WINAPI_FUNCTION(dll, functionName, localFunctionName, returnType, params) \
    typedef returnType (WINAPI *type##localFunctionName) params; \
    type##localFunctionName localFunctionName = (type##localFunctionName) dll.getFunction (#functionName);

//==============================================================================
#elif JUCE_LINUX
 #include <sched.h>
 #include <pthread.h>
 #include <sys/time.h>
 #include <errno.h>
 #include <sys/stat.h>
 #include <sys/dir.h>
 #include <sys/ptrace.h>
 #include <sys/vfs.h>
 #include <sys/wait.h>
 #include <sys/mman.h>
 #include <fnmatch.h>
 #include <utime.h>
 #include <pwd.h>
 #include <fcntl.h>
 #include <dlfcn.h>
 #include <netdb.h>
 #include <arpa/inet.h>
 #include <netinet/in.h>
 #include <sys/types.h>
 #include <sys/ioctl.h>
 #include <sys/socket.h>
 #include <net/if.h>
 #include <sys/sysinfo.h>
 #include <sys/file.h>
 #include <sys/prctl.h>
 #include <signal.h>
 #include <stddef.h>

//==============================================================================
#elif JUCE_ANDROID
 #include <jni.h>
 #include <pthread.h>
 #include <sched.h>
 #include <sys/time.h>
 #include <utime.h>
 #include <errno.h>
 #include <fcntl.h>
 #include <dlfcn.h>
 #include <sys/stat.h>
 #include <sys/statfs.h>
 #include <sys/ptrace.h>
 #include <sys/sysinfo.h>
 #include <sys/mman.h>
 #include <pwd.h>
 #include <dirent.h>
 #include <fnmatch.h>
 #include <sys/wait.h>
 #include <android/api-level.h>

 // If you are getting include errors here, then you to re-build the Projucer
 // and re-save your .jucer file.
 #include <cpu-features.h>
#endif

// Need to clear various moronic redefinitions made by system headers..
#undef max
#undef min
#undef direct
#undef check
