/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

#ifdef JUCE_AUDIO_FORMATS_H_INCLUDED
 /* When you add this cpp file to your project, you mustn't include it in a file where you've
    already included any other headers - just put it inside a file on its own, possibly with your config
    flags preceding it, but don't include anything else. That also includes avoiding any automatic prefix
    header files that the compiler may be using.
 */
 #error "Incorrect use of JUCE cpp file"
#endif

#define JUCE_CORE_INCLUDE_COM_SMART_PTR 1
#define JUCE_CORE_INCLUDE_JNI_HELPERS 1
#define JUCE_CORE_INCLUDE_NATIVE_HEADERS 1

#include "juce_audio_formats.h"

//==============================================================================
#if JUCE_MAC
 #include <AudioToolbox/AudioToolbox.h>

#elif JUCE_IOS
 #import <AudioToolbox/AudioToolbox.h>
 #import <AVFoundation/AVFoundation.h>

//==============================================================================
#elif JUCE_WINDOWS && JUCE_USE_WINDOWS_MEDIA_FORMAT
 #include <wmsdk.h>
#endif

//==============================================================================
#include "format/juce_AudioFormat.cpp"
#include "format/juce_AudioFormatManager.cpp"
#include "format/juce_AudioFormatReader.cpp"
#include "format/juce_AudioFormatReaderSource.cpp"
#include "format/juce_AudioFormatWriter.cpp"
#include "format/juce_AudioSubsectionReader.cpp"
#include "format/juce_BufferingAudioFormatReader.cpp"
#include "sampler/juce_Sampler.cpp"
#include "codecs/juce_AiffAudioFormat.cpp"
#include "codecs/juce_CoreAudioFormat.cpp"
#include "codecs/juce_FlacAudioFormat.cpp"
#include "codecs/juce_MP3AudioFormat.cpp"
#include "codecs/juce_OggVorbisAudioFormat.cpp"
#include "codecs/juce_WavAudioFormat.cpp"
#include "codecs/juce_LAMEEncoderAudioFormat.cpp"

#if JUCE_WINDOWS && JUCE_USE_WINDOWS_MEDIA_FORMAT
 #include "codecs/juce_WindowsMediaAudioFormat.cpp"
#endif
