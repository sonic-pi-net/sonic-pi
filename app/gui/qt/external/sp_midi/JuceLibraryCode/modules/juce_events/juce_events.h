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


/*******************************************************************************
 The block below describes the properties of this module, and is read by
 the Projucer to automatically generate project code that uses it.
 For details about the syntax and how to create or use a module, see the
 JUCE Module Format.txt file.


 BEGIN_JUCE_MODULE_DECLARATION

  ID:                 juce_events
  vendor:             juce
  version:            5.4.7
  name:               JUCE message and event handling classes
  description:        Classes for running an application's main event loop and sending/receiving messages, timers, etc.
  website:            http://www.juce.com/juce
  license:            ISC

  dependencies:       juce_core

 END_JUCE_MODULE_DECLARATION

*******************************************************************************/


#pragma once
#define JUCE_EVENTS_H_INCLUDED

#include <juce_core/juce_core.h>

//==============================================================================
/** Config: JUCE_EXECUTE_APP_SUSPEND_ON_BACKGROUND_TASK
    Will execute your application's suspend method on an iOS background task, giving
    you extra time to save your applications state.
*/
#ifndef JUCE_EXECUTE_APP_SUSPEND_ON_BACKGROUND_TASK
 #define JUCE_EXECUTE_APP_SUSPEND_ON_BACKGROUND_TASK 0
#endif

#if JUCE_EVENTS_INCLUDE_WINRT_WRAPPER && JUCE_WINDOWS
 // If this header file is missing then you are probably attempting to use WinRT
 // functionality without the WinRT libraries installed on your system. Try installing
 // the latest Windows Standalone SDK and maybe also adding the path to the WinRT
 // headers to your build system. This path should have the form
 // "C:\Program Files (x86)\Windows Kits\10\Include\10.0.14393.0\winrt".
 #include <inspectable.h>
 #include <hstring.h>
#endif

#include "messages/juce_MessageManager.h"
#include "messages/juce_Message.h"
#include "messages/juce_MessageListener.h"
#include "messages/juce_CallbackMessage.h"
#include "messages/juce_DeletedAtShutdown.h"
#include "messages/juce_NotificationType.h"
#include "messages/juce_ApplicationBase.h"
#include "messages/juce_Initialisation.h"
#include "messages/juce_MountedVolumeListChangeDetector.h"
#include "broadcasters/juce_ActionBroadcaster.h"
#include "broadcasters/juce_ActionListener.h"
#include "broadcasters/juce_AsyncUpdater.h"
#include "broadcasters/juce_ChangeListener.h"
#include "broadcasters/juce_ChangeBroadcaster.h"
#include "timers/juce_Timer.h"
#include "timers/juce_MultiTimer.h"
#include "interprocess/juce_InterprocessConnection.h"
#include "interprocess/juce_InterprocessConnectionServer.h"
#include "interprocess/juce_ConnectedChildProcess.h"
#include "interprocess/juce_NetworkServiceDiscovery.h"

#if JUCE_LINUX
 #include "native/juce_linux_EventLoop.h"
#endif

#if JUCE_WINDOWS
 #if JUCE_EVENTS_INCLUDE_WIN32_MESSAGE_WINDOW
  #include "native/juce_win32_HiddenMessageWindow.h"
 #endif
 #if JUCE_EVENTS_INCLUDE_WINRT_WRAPPER
  #include "native/juce_win32_WinRTWrapper.h"
 #endif
#endif
