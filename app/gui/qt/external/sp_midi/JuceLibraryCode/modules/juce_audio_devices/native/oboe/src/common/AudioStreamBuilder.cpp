/*
 * Copyright 2016 The Android Open Source Project
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

#include <sys/types.h>

#include "aaudio/AudioStreamAAudio.h"
#include "FilterAudioStream.h"
#include "OboeDebug.h"
#include "oboe/Oboe.h"
#include "oboe/AudioStreamBuilder.h"
#include "opensles/AudioInputStreamOpenSLES.h"
#include "opensles/AudioOutputStreamOpenSLES.h"
#include "opensles/AudioStreamOpenSLES.h"
#include "QuirksManager.h"

bool oboe::OboeGlobals::mWorkaroundsEnabled = true;

namespace oboe {

/**
 * The following default values are used when oboe does not have any better way of determining the optimal values
 * for an audio stream. This can happen when:
 *
 * - Client is creating a stream on API < 26 (OpenSLES) but has not supplied the optimal sample
 * rate and/or frames per burst
 * - Client is creating a stream on API 16 (OpenSLES) where AudioManager.PROPERTY_OUTPUT_* values
 * are not available
 */
int32_t DefaultStreamValues::SampleRate = 48000; // Common rate for mobile audio and video
int32_t DefaultStreamValues::FramesPerBurst = 192; // 4 msec at 48000 Hz
int32_t DefaultStreamValues::ChannelCount = 2; // Stereo

constexpr int kBufferSizeInBurstsForLowLatencyStreams = 2;

#ifndef OBOE_ENABLE_AAUDIO
// Set OBOE_ENABLE_AAUDIO to 0 if you want to disable the AAudio API.
// This might be useful if you want to force all the unit tests to use OpenSL ES.
#define OBOE_ENABLE_AAUDIO 1
#endif

bool AudioStreamBuilder::isAAudioSupported() {
    return AudioStreamAAudio::isSupported() && OBOE_ENABLE_AAUDIO;
}

bool AudioStreamBuilder::isAAudioRecommended() {
    // See https://github.com/google/oboe/issues/40,
    // AAudio may not be stable on Android O, depending on how it is used.
    // To be safe, use AAudio only on O_MR1 and above.
    return (getSdkVersion() >= __ANDROID_API_O_MR1__) && isAAudioSupported();
}

AudioStream *AudioStreamBuilder::build() {
    AudioStream *stream = nullptr;
    if (isAAudioRecommended() && mAudioApi != AudioApi::OpenSLES) {
        stream = new AudioStreamAAudio(*this);
    } else if (isAAudioSupported() && mAudioApi == AudioApi::AAudio) {
        stream = new AudioStreamAAudio(*this);
        LOGE("Creating AAudio stream on 8.0 because it was specified. This is error prone.");
    } else {
        if (getDirection() == oboe::Direction::Output) {
            stream = new AudioOutputStreamOpenSLES(*this);
        } else if (getDirection() == oboe::Direction::Input) {
            stream = new AudioInputStreamOpenSLES(*this);
        }
    }
    return stream;
}

bool AudioStreamBuilder::isCompatible(AudioStreamBase &other) {
    return getSampleRate() == other.getSampleRate()
           && getFormat() == other.getFormat()
           && getChannelCount() == other.getChannelCount();
}

Result AudioStreamBuilder::openStream(AudioStream **streamPP) {
    Result result = Result::OK;
    LOGI("%s() %s -------- %s --------",
         __func__, getDirection() == Direction::Input ? "INPUT" : "OUTPUT", getVersionText());

    if (streamPP == nullptr) {
        return Result::ErrorNull;
    }
    *streamPP = nullptr;

    AudioStream *streamP = nullptr;

    // Maybe make a FilterInputStream.
    AudioStreamBuilder childBuilder(*this);
    // Check need for conversion and modify childBuilder for optimal stream.
    bool conversionNeeded = QuirksManager::getInstance().isConversionNeeded(*this, childBuilder);
    // Do we need to make a child stream and convert.
    if (conversionNeeded) {
        AudioStream *tempStream;

        result = childBuilder.openStream(&tempStream);
        if (result != Result::OK) {
            return result;
        }

        if (isCompatible(*tempStream)) {
            // Everything matches so we can just use the child stream directly.
            *streamPP = tempStream;
            return result;
        } else {
            AudioStreamBuilder parentBuilder = *this;
            // Build a stream that is as close as possible to the childStream.
            if (getFormat() == oboe::AudioFormat::Unspecified) {
                parentBuilder.setFormat(tempStream->getFormat());
            }
            if (getChannelCount() == oboe::Unspecified) {
                parentBuilder.setChannelCount(tempStream->getChannelCount());
            }
            if (getSampleRate() == oboe::Unspecified) {
                parentBuilder.setSampleRate(tempStream->getSampleRate());
            }

            // Use childStream in a FilterAudioStream.
            LOGI("%s() create a FilterAudioStream for data conversion.", __func__);
            FilterAudioStream *filterStream = new FilterAudioStream(parentBuilder, tempStream);
            result = filterStream->configureFlowGraph();
            if (result !=  Result::OK) {
                filterStream->close();
                delete filterStream;
                // Just open streamP the old way.
            } else {
                streamP = static_cast<AudioStream *>(filterStream);
            }
        }
    }

    if (streamP == nullptr) {
        streamP = build();
        if (streamP == nullptr) {
            return Result::ErrorNull;
        }
    }

    result = streamP->open(); // TODO review API
    if (result == Result::OK) {

        int32_t  optimalBufferSize = -1;
        // Use a reasonable default buffer size.
        if (streamP->getDirection() == Direction::Input) {
            // For input, small size does not improve latency because the stream is usually
            // run close to empty. And a low size can result in XRuns so always use the maximum.
            optimalBufferSize = streamP->getBufferCapacityInFrames();
        } else if (streamP->getPerformanceMode() == PerformanceMode::LowLatency
                && streamP->getDirection() == Direction::Output)  { // Output check is redundant.
            optimalBufferSize = streamP->getFramesPerBurst() *
                                    kBufferSizeInBurstsForLowLatencyStreams;
        }
        if (optimalBufferSize >= 0) {
            auto setBufferResult = streamP->setBufferSizeInFrames(optimalBufferSize);
            if (!setBufferResult) {
                LOGW("Failed to setBufferSizeInFrames(%d). Error was %s",
                     optimalBufferSize,
                     convertToText(setBufferResult.error()));
            }
        }

        *streamPP = streamP;
    } else {
        delete streamP;
    }
    return result;
}

Result AudioStreamBuilder::openManagedStream(oboe::ManagedStream &stream) {
    stream.reset();
    AudioStream *streamptr;
    auto result = openStream(&streamptr);
    stream.reset(streamptr);
    return result;
}

Result AudioStreamBuilder::openStream(std::shared_ptr<AudioStream> &sharedStream) {
    sharedStream.reset();
    AudioStream *streamptr;
    auto result = openStream(&streamptr);
    if (result == Result::OK) {
        sharedStream.reset(streamptr);
        // Save a weak_ptr in the stream for use with callbacks.
        streamptr->setWeakThis(sharedStream);
    }
    return result;
}

} // namespace oboe
