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
    Some shared helpers methods for using the high-performance audio paths on
    Android devices (OpenSL and Oboe).

    @tags{Audio}
*/
namespace AndroidHighPerformanceAudioHelpers
{
    //==============================================================================
    static double getNativeSampleRate()
    {
        return audioManagerGetProperty ("android.media.property.OUTPUT_SAMPLE_RATE").getDoubleValue();
    }

    static int getNativeBufferSizeHint()
    {
        // This property is a hint of a native buffer size but it does not guarantee the size used.
        auto deviceBufferSize = audioManagerGetProperty ("android.media.property.OUTPUT_FRAMES_PER_BUFFER").getIntValue();

        if (deviceBufferSize == 0)
            return 192;

        return deviceBufferSize;
    }

    static bool isProAudioDevice()
    {
        static bool isSapaSupported = SystemStats::getDeviceManufacturer().containsIgnoreCase ("SAMSUNG")
                                     && DynamicLibrary().open ("libapa_jni.so");

        return androidHasSystemFeature ("android.hardware.audio.pro") || isSapaSupported;
    }

    static bool hasLowLatencyAudioPath()
    {
        return androidHasSystemFeature ("android.hardware.audio.low_latency");
    }

    static bool canUseHighPerformanceAudioPath (int nativeBufferSize, int requestedBufferSize, int requestedSampleRate)
    {
        return ((requestedBufferSize % nativeBufferSize) == 0)
               && (requestedSampleRate == getNativeSampleRate())
               && isProAudioDevice();
    }

    //==============================================================================
    static int getMinimumBuffersToEnqueue (int nativeBufferSize, double requestedSampleRate)
    {
        if (canUseHighPerformanceAudioPath (nativeBufferSize, nativeBufferSize, (int) requestedSampleRate))
        {
            // see https://developer.android.com/ndk/guides/audio/opensl/opensl-prog-notes.html#sandp
            // "For Android 4.2 (API level 17) and earlier, a buffer count of two or more is required
            //  for lower latency. Beginning with Android 4.3 (API level 18), a buffer count of one
            //  is sufficient for lower latency."
            return (getAndroidSDKVersion() >= 18 ? 1 : 2);
        }

        // not using low-latency path so we can use the absolute minimum number of buffers to queue
        return 1;
    }

    static int buffersToQueueForBufferDuration (int nativeBufferSize, int bufferDurationInMs, double sampleRate) noexcept
    {
        auto maxBufferFrames = static_cast<int> (std::ceil (bufferDurationInMs * sampleRate / 1000.0));
        auto maxNumBuffers   = static_cast<int> (std::ceil (static_cast<double> (maxBufferFrames)
                                                  / static_cast<double> (nativeBufferSize)));

        return jmax (getMinimumBuffersToEnqueue (nativeBufferSize, sampleRate), maxNumBuffers);
    }

    static int getMaximumBuffersToEnqueue (int nativeBufferSize, double maximumSampleRate) noexcept
    {
        static constexpr int maxBufferSizeMs = 200;

        return jmax (8, buffersToQueueForBufferDuration (nativeBufferSize, maxBufferSizeMs, maximumSampleRate));
    }

    static Array<int> getAvailableBufferSizes (int nativeBufferSize, Array<double> availableSampleRates)
    {
        auto minBuffersToQueue = getMinimumBuffersToEnqueue (nativeBufferSize, getNativeSampleRate());
        auto maxBuffersToQueue = getMaximumBuffersToEnqueue (nativeBufferSize, findMaximum (availableSampleRates.getRawDataPointer(),
                                                                                            availableSampleRates.size()));

        Array<int> bufferSizes;

        for (int i = minBuffersToQueue; i <= maxBuffersToQueue; ++i)
            bufferSizes.add (i * nativeBufferSize);

        return bufferSizes;
    }

    static int getDefaultBufferSize (int nativeBufferSize, double currentSampleRate)
    {
        static constexpr int defaultBufferSizeForLowLatencyDeviceMs = 40;
        static constexpr int defaultBufferSizeForStandardLatencyDeviceMs = 100;

        auto defaultBufferLength = (hasLowLatencyAudioPath() ? defaultBufferSizeForLowLatencyDeviceMs
                                                             : defaultBufferSizeForStandardLatencyDeviceMs);

        auto defaultBuffersToEnqueue = buffersToQueueForBufferDuration (nativeBufferSize, defaultBufferLength, currentSampleRate);
        return defaultBuffersToEnqueue * nativeBufferSize;
    }
}

} // namespace juce
