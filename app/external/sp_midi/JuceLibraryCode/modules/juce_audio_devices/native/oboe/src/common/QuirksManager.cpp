/*
 * Copyright 2019 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <oboe/AudioStreamBuilder.h>
#include <oboe/Oboe.h>

#include "QuirksManager.h"

using namespace oboe;

int32_t QuirksManager::DeviceQuirks::clipBufferSize(AudioStream &stream,
            int32_t requestedSize) {
    if (!OboeGlobals::areWorkaroundsEnabled()) {
        return requestedSize;
    }
    int bottomMargin = kDefaultBottomMarginInBursts;
    int topMargin = kDefaultTopMarginInBursts;
    if (isMMapUsed(stream)) {
        if (stream.getSharingMode() == SharingMode::Exclusive) {
            bottomMargin = getExclusiveBottomMarginInBursts();
            topMargin = getExclusiveTopMarginInBursts();
        }
    } else {
        bottomMargin = kLegacyBottomMarginInBursts;
    }

    int32_t burst = stream.getFramesPerBurst();
    int32_t minSize = bottomMargin * burst;
    int32_t adjustedSize = requestedSize;
    if (adjustedSize < minSize ) {
        adjustedSize = minSize;
    } else {
        int32_t maxSize = stream.getBufferCapacityInFrames() - (topMargin * burst);
        if (adjustedSize > maxSize ) {
            adjustedSize = maxSize;
        }
    }
    return adjustedSize;
}

class SamsungDeviceQuirks : public  QuirksManager::DeviceQuirks {
public:
    SamsungDeviceQuirks() {
        std::string arch = getPropertyString("ro.arch");
        isExynos = (arch.rfind("exynos", 0) == 0); // starts with?
    }

    virtual ~SamsungDeviceQuirks() = default;

    int32_t getExclusiveBottomMarginInBursts() const override {
        // TODO Make this conditional on build version when MMAP timing improves.
        return isExynos ? kBottomMarginExynos : kBottomMarginOther;
    }

    int32_t getExclusiveTopMarginInBursts() const override {
        return kTopMargin;
    }

private:
    // Stay farther away from DSP position on Exynos devices.
    static constexpr int32_t kBottomMarginExynos = 2;
    static constexpr int32_t kBottomMarginOther = 1;
    static constexpr int32_t kTopMargin = 1;
    bool isExynos = false;
};

QuirksManager::QuirksManager() {
    std::string manufacturer = getPropertyString("ro.product.manufacturer");
    if (manufacturer == "samsung") {
        mDeviceQuirks = std::make_unique<SamsungDeviceQuirks>();
    } else {
        mDeviceQuirks = std::make_unique<DeviceQuirks>();
    }
}

bool QuirksManager::isConversionNeeded(
        const AudioStreamBuilder &builder,
        AudioStreamBuilder &childBuilder) {
    bool conversionNeeded = false;
    const bool isLowLatency = builder.getPerformanceMode() == PerformanceMode::LowLatency;
    const bool isInput = builder.getDirection() == Direction::Input;
    const bool isFloat = builder.getFormat() == AudioFormat::Float;

    // If a SAMPLE RATE is specified for low latency then let the native code choose an optimal rate.
    // TODO There may be a problem if the devices supports low latency
    //      at a higher rate than the default.
    if (builder.getSampleRate() != oboe::Unspecified
            && builder.getSampleRateConversionQuality() != SampleRateConversionQuality::None
            && isLowLatency
            ) {
        childBuilder.setSampleRate(oboe::Unspecified); // native API decides the best sample rate
        conversionNeeded = true;
    }

    // Data Format
    // OpenSL ES and AAudio before P do not support FAST path for FLOAT capture.
    if (isFloat
            && isInput
            && builder.isFormatConversionAllowed()
            && isLowLatency
            && (!builder.willUseAAudio() || (getSdkVersion() < __ANDROID_API_P__))
            ) {
        childBuilder.setFormat(AudioFormat::I16); // needed for FAST track
        conversionNeeded = true;
    }

    // Channel Count
    if (builder.getChannelCount() != oboe::Unspecified
            && builder.isChannelConversionAllowed()) {
        if (OboeGlobals::areWorkaroundsEnabled()
                && builder.getChannelCount() == 2 // stereo?
                && isInput
                && isLowLatency
                && (!builder.willUseAAudio() && (getSdkVersion() == __ANDROID_API_O__))) {
            // Workaround for heap size regression in O.
            // b/66967812 AudioRecord does not allow FAST track for stereo capture in O
            childBuilder.setChannelCount(1);
            conversionNeeded = true;
        }
        // Note that MMAP does not support mono in 8.1. But that would only matter on Pixel 1
        // phones and they have almost all been updated to 9.0.
    }

    return conversionNeeded;
}
