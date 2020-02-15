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

  ID:               juce_audio_devices
  vendor:           juce
  version:          5.2.0
  name:             JUCE audio and MIDI I/O device classes
  description:      Classes to play and record from audio and MIDI I/O devices
  website:          http://www.juce.com/juce
  license:          ISC

  dependencies:     juce_audio_basics, juce_events
  OSXFrameworks:    CoreAudio CoreMIDI AudioToolbox
  iOSFrameworks:    CoreAudio CoreMIDI AudioToolbox AVFoundation
  linuxPackages:    alsa
  mingwLibs:        winmm

 END_JUCE_MODULE_DECLARATION

*******************************************************************************/


#pragma once
#define JUCE_AUDIO_DEVICES_H_INCLUDED

#include <juce_events/juce_events.h>
#include <juce_audio_basics/juce_audio_basics.h>

#if JUCE_MODULE_AVAILABLE_juce_graphics
#include <juce_graphics/juce_graphics.h>
#endif

//==============================================================================
/** Config: JUCE_ASIO
    Enables ASIO audio devices (MS Windows only).
    Turning this on means that you'll need to have the Steinberg ASIO SDK installed
    on your Windows build machine.

    See the comments in the ASIOAudioIODevice class's header file for more
    info about this.
*/
#ifndef JUCE_ASIO
 #define JUCE_ASIO 0
#endif

/** Config: JUCE_WASAPI
    Enables WASAPI audio devices (Windows Vista and above). See also the
    JUCE_WASAPI_EXCLUSIVE flag.
*/
#ifndef JUCE_WASAPI
 #define JUCE_WASAPI 1
#endif

/** Config: JUCE_WASAPI_EXCLUSIVE
    Enables WASAPI audio devices in exclusive mode (Windows Vista and above).
*/
#ifndef JUCE_WASAPI_EXCLUSIVE
 #define JUCE_WASAPI_EXCLUSIVE 0
#endif


/** Config: JUCE_DIRECTSOUND
    Enables DirectSound audio (MS Windows only).
*/
#ifndef JUCE_DIRECTSOUND
 #define JUCE_DIRECTSOUND 1
#endif

/** Config: JUCE_ALSA
    Enables ALSA audio devices (Linux only).
*/
#ifndef JUCE_ALSA
 #define JUCE_ALSA 1
#endif

/** Config: JUCE_JACK
    Enables JACK audio devices (Linux only).
*/
#ifndef JUCE_JACK
 #define JUCE_JACK 0
#endif

/** Config: JUCE_USE_ANDROID_OPENSLES
    Enables OpenSLES devices (Android only).
*/
#ifndef JUCE_USE_ANDROID_OPENSLES
 #if JUCE_ANDROID_API_VERSION > 8
  #define JUCE_USE_ANDROID_OPENSLES 1
 #else
  #define JUCE_USE_ANDROID_OPENSLES 0
 #endif
#endif

/** Config: JUCE_USE_WINRT_MIDI
    ***
    EXPERIMENTAL - Microsoft's Bluetooth MIDI stack has multiple issues,
    use at your own risk!
    ***

    Enables the use of the Windows Runtime API for MIDI, which supports
    Bluetooth Low Energy connections on computers with the Anniversary Update
    of Windows 10.

    To compile with this flag requires version 10.0.14393.0 of the Windows
    Standalone SDK and you must add the path to the WinRT headers. This path
    should be something similar to
    "C:\Program Files (x86)\Windows Kits\10\Include\10.0.14393.0\winrt".
*/
#ifndef JUCE_USE_WINRT_MIDI
 #define JUCE_USE_WINRT_MIDI 0
#endif


//==============================================================================
#include "midi_io/juce_MidiInput.h"
#include "midi_io/juce_MidiMessageCollector.h"
#include "midi_io/juce_MidiOutput.h"
#include "audio_io/juce_AudioIODevice.h"
#include "audio_io/juce_AudioIODeviceType.h"
#include "audio_io/juce_SystemAudioVolume.h"
#include "sources/juce_AudioSourcePlayer.h"
#include "sources/juce_AudioTransportSource.h"
#include "audio_io/juce_AudioDeviceManager.h"

#if JUCE_IOS
 #include "native/juce_ios_Audio.h"
#endif
