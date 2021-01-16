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

#ifdef JUCE_AUDIO_DEVICES_H_INCLUDED
 /* When you add this cpp file to your project, you mustn't include it in a file where you've
    already included any other headers - just put it inside a file on its own, possibly with your config
    flags preceding it, but don't include anything else. That also includes avoiding any automatic prefix
    header files that the compiler may be using.
 */
 #error "Incorrect use of JUCE cpp file"
#endif

#define JUCE_CORE_INCLUDE_OBJC_HELPERS 1
#define JUCE_CORE_INCLUDE_COM_SMART_PTR 1
#define JUCE_CORE_INCLUDE_JNI_HELPERS 1
#define JUCE_CORE_INCLUDE_NATIVE_HEADERS 1
#define JUCE_EVENTS_INCLUDE_WIN32_MESSAGE_WINDOW 1

#ifndef JUCE_USE_WINRT_MIDI
 #define JUCE_USE_WINRT_MIDI 0
#endif

#if JUCE_USE_WINRT_MIDI
 #define JUCE_EVENTS_INCLUDE_WINRT_WRAPPER 1
#endif

#include "juce_audio_devices.h"

//==============================================================================
#if JUCE_MAC
 #define Point CarbonDummyPointName
 #define Component CarbonDummyCompName
 #import <CoreAudio/AudioHardware.h>
 #import <CoreMIDI/MIDIServices.h>
 #import <AudioToolbox/AudioServices.h>
 #undef Point
 #undef Component

#elif JUCE_IOS
 #import <AudioToolbox/AudioToolbox.h>
 #import <AVFoundation/AVFoundation.h>
 #import <CoreMIDI/MIDIServices.h>

 #if TARGET_OS_SIMULATOR
  #import <CoreMIDI/MIDINetworkSession.h>
 #endif

//==============================================================================
#elif JUCE_WINDOWS
 #if JUCE_WASAPI
  #include <mmreg.h>
 #endif

 #if JUCE_USE_WINRT_MIDI && JUCE_MSVC
  /* If you cannot find any of the header files below then you are probably
     attempting to use the Windows 10 Bluetooth Low Energy API. For this to work you
     need to install version 10.0.14393.0 of the Windows Standalone SDK and you may
     need to add the path to the WinRT headers to your build system. This path should
     have the form "C:\Program Files (x86)\Windows Kits\10\Include\10.0.14393.0\winrt".

     Also please note that Microsoft's Bluetooth MIDI stack has multiple issues, so
     this API is EXPERIMENTAL - use at your own risk!
  */
  #include <windows.devices.h>
  #include <windows.devices.midi.h>
  #include <windows.devices.enumeration.h>

  JUCE_BEGIN_IGNORE_WARNINGS_MSVC (4265)
  #include <wrl/event.h>
  JUCE_END_IGNORE_WARNINGS_MSVC

  JUCE_BEGIN_IGNORE_WARNINGS_MSVC (4467)
  #include <robuffer.h>
  JUCE_END_IGNORE_WARNINGS_MSVC
 #endif

 #if JUCE_ASIO
  /* This is very frustrating - we only need to use a handful of definitions from
     a couple of the header files in Steinberg's ASIO SDK, and it'd be easy to copy
     about 30 lines of code into this cpp file to create a fully stand-alone ASIO
     implementation...

     ..unfortunately that would break Steinberg's license agreement for use of
     their SDK, so I'm not allowed to do this.

     This means that anyone who wants to use JUCE's ASIO abilities will have to:

     1) Agree to Steinberg's licensing terms and download the ASIO SDK
         (see http://www.steinberg.net/en/company/developers.html).

     2) Enable this code with a global definition #define JUCE_ASIO 1.

     3) Make sure that your header search path contains the iasiodrv.h file that
        comes with the SDK. (Only about a handful of the SDK header files are actually
        needed - so to simplify things, you could just copy these into your JUCE directory).
  */
  #include <iasiodrv.h>
 #endif

//==============================================================================
#elif JUCE_LINUX
 #if JUCE_ALSA
  /* Got an include error here? If so, you've either not got ALSA installed, or you've
     not got your paths set up correctly to find its header files.

     The package you need to install to get ASLA support is "libasound2-dev".

     If you don't have the ALSA library and don't want to build JUCE with audio support,
     just set the JUCE_ALSA flag to 0.
  */
  #include <alsa/asoundlib.h>
 #endif

 #if JUCE_JACK
  /* Got an include error here? If so, you've either not got jack-audio-connection-kit
     installed, or you've not got your paths set up correctly to find its header files.

     The package you need to install to get JACK support is "libjack-dev".

     If you don't have the jack-audio-connection-kit library and don't want to build
     JUCE with low latency audio support, just set the JUCE_JACK flag to 0.
  */
  #include <jack/jack.h>
 #endif

 #if JUCE_BELA
  /* Got an include error here? If so, you've either not got the bela headers
     installed, or you've not got your paths set up correctly to find its header
     files.
  */
  #include <Bela.h>
  #include <Midi.h>
 #endif

 #undef SIZEOF

//==============================================================================
#elif JUCE_ANDROID

 #if JUCE_USE_ANDROID_OPENSLES
  #include <SLES/OpenSLES.h>
  #include <SLES/OpenSLES_Android.h>
  #include <SLES/OpenSLES_AndroidConfiguration.h>
 #endif

 #if JUCE_USE_ANDROID_OBOE
  #if JUCE_USE_ANDROID_OPENSLES
   #error "Oboe cannot be enabled at the same time as openSL! Please disable JUCE_USE_ANDROID_OPENSLES"
  #endif

  JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE ("-Wunused-parameter",
                                       "-Wzero-as-null-pointer-constant",
                                       "-Winconsistent-missing-destructor-override",
                                       "-Wshadow-field-in-constructor",
                                       "-Wshadow-field")
   #include <oboe/Oboe.h>
  JUCE_END_IGNORE_WARNINGS_GCC_LIKE
 #endif

#endif

#include "audio_io/juce_AudioDeviceManager.cpp"
#include "audio_io/juce_AudioIODevice.cpp"
#include "audio_io/juce_AudioIODeviceType.cpp"
#include "midi_io/juce_MidiMessageCollector.cpp"
#include "midi_io/juce_MidiDevices.cpp"
#include "sources/juce_AudioSourcePlayer.cpp"
#include "sources/juce_AudioTransportSource.cpp"
#include "native/juce_MidiDataConcatenator.h"

//==============================================================================
#if JUCE_MAC
 #include "native/juce_mac_CoreAudio.cpp"
 #include "native/juce_mac_CoreMidi.cpp"

//==============================================================================
#elif JUCE_IOS
 #include "native/juce_ios_Audio.cpp"
 #include "native/juce_mac_CoreMidi.cpp"

//==============================================================================
#elif JUCE_WINDOWS

 #if JUCE_WASAPI
  #include "native/juce_win32_WASAPI.cpp"
 #endif

 #if JUCE_DIRECTSOUND
  #include "native/juce_win32_DirectSound.cpp"
 #endif

 #include "native/juce_win32_Midi.cpp"

 #if JUCE_ASIO
  #include "native/juce_win32_ASIO.cpp"
 #endif

//==============================================================================
#elif JUCE_LINUX
 #if JUCE_ALSA
  #include "native/juce_linux_ALSA.cpp"
 #endif

 #if JUCE_JACK
  #include "native/juce_linux_JackAudio.cpp"
 #endif

 #if JUCE_BELA
  #include "native/juce_linux_Bela.cpp"
 #else
  #include "native/juce_linux_Midi.cpp"
 #endif

//==============================================================================
#elif JUCE_ANDROID
 #include "native/juce_android_Audio.cpp"
 #include "native/juce_android_Midi.cpp"

 #if JUCE_USE_ANDROID_OPENSLES || JUCE_USE_ANDROID_OBOE
  #include "native/juce_android_HighPerformanceAudioHelpers.h"

  #if JUCE_USE_ANDROID_OPENSLES
   #include "native/juce_android_OpenSL.cpp"
  #endif

  #if JUCE_USE_ANDROID_OBOE
   #include "native/juce_android_Oboe.cpp"
  #endif
 #endif

#endif

#if ! JUCE_SYSTEMAUDIOVOL_IMPLEMENTED
namespace juce
{
    // None of these methods are available. (On Windows you might need to enable WASAPI for this)
    float JUCE_CALLTYPE SystemAudioVolume::getGain()         { jassertfalse; return 0.0f; }
    bool  JUCE_CALLTYPE SystemAudioVolume::setGain (float)   { jassertfalse; return false; }
    bool  JUCE_CALLTYPE SystemAudioVolume::isMuted()         { jassertfalse; return false; }
    bool  JUCE_CALLTYPE SystemAudioVolume::setMuted (bool)   { jassertfalse; return false; }
}
#endif
