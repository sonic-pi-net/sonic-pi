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

#include <cassert>
#include <stdint.h>
#include <stdlib.h>

#include "aaudio/AAudioLoader.h"
#include "aaudio/AudioStreamAAudio.h"
#include "common/AudioClock.h"
#include "common/OboeDebug.h"
#include "oboe/Utilities.h"

#ifdef __ANDROID__
#include <sys/system_properties.h>
#include <common/QuirksManager.h>

#endif

#ifndef OBOE_FIX_FORCE_STARTING_TO_STARTED
// Workaround state problems in AAudio
// TODO Which versions does this occur in? Verify fixed in Q.
#define OBOE_FIX_FORCE_STARTING_TO_STARTED 1
#endif // OBOE_FIX_FORCE_STARTING_TO_STARTED

using namespace oboe;
AAudioLoader *AudioStreamAAudio::mLibLoader = nullptr;

// 'C' wrapper for the data callback method
static aaudio_data_callback_result_t oboe_aaudio_data_callback_proc(
        AAudioStream *stream,
        void *userData,
        void *audioData,
        int32_t numFrames) {

    AudioStreamAAudio *oboeStream = reinterpret_cast<AudioStreamAAudio*>(userData);
    if (oboeStream != nullptr) {
        return static_cast<aaudio_data_callback_result_t>(
                oboeStream->callOnAudioReady(stream, audioData, numFrames));

    } else {
        return static_cast<aaudio_data_callback_result_t>(DataCallbackResult::Stop);
    }
}

// This runs in its own thread.
// Only one of these threads will be launched from internalErrorCallback().
// It calls app error callbacks from a static function in case the stream gets deleted.
static void oboe_aaudio_error_thread_proc(AudioStreamAAudio *oboeStream,
                                          Result error) {
    LOGD("%s() - entering >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", __func__);
    oboeStream->requestStop();
    if (oboeStream->getCallback() != nullptr) {
        oboeStream->getCallback()->onErrorBeforeClose(oboeStream, error);
    }
    oboeStream->close();
    if (oboeStream->getCallback() != nullptr) {
        // Warning, oboeStream may get deleted by this callback.
        oboeStream->getCallback()->onErrorAfterClose(oboeStream, error);
    }
    LOGD("%s() - exiting <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<", __func__);
}

// This runs in its own thread.
// Only one of these threads will be launched from internalErrorCallback().
// Prevents deletion of the stream if the app is using AudioStreamBuilder::openSharedStream()
static void oboe_aaudio_error_thread_proc_shared(std::shared_ptr<AudioStream> sharedStream,
                                          Result error) {
    AudioStreamAAudio *oboeStream = reinterpret_cast<AudioStreamAAudio*>(sharedStream.get());
    oboe_aaudio_error_thread_proc(oboeStream, error);
}

namespace oboe {

/*
 * Create a stream that uses Oboe Audio API.
 */
AudioStreamAAudio::AudioStreamAAudio(const AudioStreamBuilder &builder)
    : AudioStream(builder)
    , mAAudioStream(nullptr) {
    mCallbackThreadEnabled.store(false);
    isSupported();
}

bool AudioStreamAAudio::isSupported() {
    mLibLoader = AAudioLoader::getInstance();
    int openResult = mLibLoader->open();
    return openResult == 0;
}

// Static 'C' wrapper for the error callback method.
// Launch a thread to handle the error.
// That other thread can safely stop, close and delete the stream.
void AudioStreamAAudio::internalErrorCallback(
        AAudioStream *stream,
        void *userData,
        aaudio_result_t error) {
    AudioStreamAAudio *oboeStream = reinterpret_cast<AudioStreamAAudio*>(userData);

    // Prevents deletion of the stream if the app is using AudioStreamBuilder::openSharedStream()
    std::shared_ptr<AudioStream> sharedStream = oboeStream->lockWeakThis();

    // These checks should be enough because we assume that the stream close()
    // will join() any active callback threads and will not allow new callbacks.
    if (oboeStream->wasErrorCallbackCalled()) { // block extra error callbacks
        LOGE("%s() multiple error callbacks called!", __func__);
    } else if (stream != oboeStream->getUnderlyingStream()) {
        LOGW("%s() stream already closed", __func__); // can happen if there are bugs
    } else if (sharedStream) {
        // Handle error on a separate thread using shared pointer.
        std::thread t(oboe_aaudio_error_thread_proc_shared, sharedStream,
                      static_cast<Result>(error));
        t.detach();
    } else {
        // Handle error on a separate thread.
        std::thread t(oboe_aaudio_error_thread_proc, oboeStream,
                      static_cast<Result>(error));
        t.detach();
    }
}

void AudioStreamAAudio::logUnsupportedAttributes() {
    int sdkVersion = getSdkVersion();

    // These attributes are not supported pre Android "P"
    if (sdkVersion < __ANDROID_API_P__) {
        if (mUsage != Usage::Media) {
            LOGW("Usage [AudioStreamBuilder::setUsage()] "
                 "is not supported on AAudio streams running on pre-Android P versions.");
        }

        if (mContentType != ContentType::Music) {
            LOGW("ContentType [AudioStreamBuilder::setContentType()] "
                 "is not supported on AAudio streams running on pre-Android P versions.");
        }

        if (mSessionId != SessionId::None) {
            LOGW("SessionId [AudioStreamBuilder::setSessionId()] "
                 "is not supported on AAudio streams running on pre-Android P versions.");
        }
    }
}

Result AudioStreamAAudio::open() {
    Result result = Result::OK;

    if (mAAudioStream != nullptr) {
        return Result::ErrorInvalidState;
    }

    result = AudioStream::open();
    if (result != Result::OK) {
        return result;
    }

    AAudioStreamBuilder *aaudioBuilder;
    result = static_cast<Result>(mLibLoader->createStreamBuilder(&aaudioBuilder));
    if (result != Result::OK) {
        return result;
    }

    // Do not set INPUT capacity below 4096 because that prevents us from getting a FAST track
    // when using the Legacy data path.
    // If the app requests > 4096 then we allow it but we are less likely to get LowLatency.
    // See internal bug b/80308183 for more details.
    // Fixed in Q but let's still clip the capacity because high input capacity
    // does not increase latency.
    int32_t capacity = mBufferCapacityInFrames;
    constexpr int kCapacityRequiredForFastLegacyTrack = 4096; // matches value in AudioFinger
    if (OboeGlobals::areWorkaroundsEnabled()
            && mDirection == oboe::Direction::Input
            && capacity != oboe::Unspecified
            && capacity < kCapacityRequiredForFastLegacyTrack
            && mPerformanceMode == oboe::PerformanceMode::LowLatency) {
        capacity = kCapacityRequiredForFastLegacyTrack;
        LOGD("AudioStreamAAudio.open() capacity changed from %d to %d for lower latency",
             static_cast<int>(mBufferCapacityInFrames), capacity);
    }
    mLibLoader->builder_setBufferCapacityInFrames(aaudioBuilder, capacity);

    mLibLoader->builder_setChannelCount(aaudioBuilder, mChannelCount);
    mLibLoader->builder_setDeviceId(aaudioBuilder, mDeviceId);
    mLibLoader->builder_setDirection(aaudioBuilder, static_cast<aaudio_direction_t>(mDirection));
    mLibLoader->builder_setFormat(aaudioBuilder, static_cast<aaudio_format_t>(mFormat));
    mLibLoader->builder_setSampleRate(aaudioBuilder, mSampleRate);
    mLibLoader->builder_setSharingMode(aaudioBuilder,
                                       static_cast<aaudio_sharing_mode_t>(mSharingMode));
    mLibLoader->builder_setPerformanceMode(aaudioBuilder,
                                           static_cast<aaudio_performance_mode_t>(mPerformanceMode));

    // These were added in P so we have to check for the function pointer.
    if (mLibLoader->builder_setUsage != nullptr) {
        mLibLoader->builder_setUsage(aaudioBuilder,
                                     static_cast<aaudio_usage_t>(mUsage));
    }

    if (mLibLoader->builder_setContentType != nullptr) {
        mLibLoader->builder_setContentType(aaudioBuilder,
                                           static_cast<aaudio_content_type_t>(mContentType));
    }

    if (mLibLoader->builder_setInputPreset != nullptr) {
        mLibLoader->builder_setInputPreset(aaudioBuilder,
                                           static_cast<aaudio_input_preset_t>(mInputPreset));
    }

    if (mLibLoader->builder_setSessionId != nullptr) {
        mLibLoader->builder_setSessionId(aaudioBuilder,
                                         static_cast<aaudio_session_id_t>(mSessionId));
    }

    // TODO get more parameters from the builder?

    if (mStreamCallback != nullptr) {
        mLibLoader->builder_setDataCallback(aaudioBuilder, oboe_aaudio_data_callback_proc, this);
        mLibLoader->builder_setFramesPerDataCallback(aaudioBuilder, getFramesPerCallback());
        // If the data callback is not being used then the write method will return an error
        // and the app can stop and close the stream.
        mLibLoader->builder_setErrorCallback(aaudioBuilder, internalErrorCallback, this);
    }

    // ============= OPEN THE STREAM ================
    {
        AAudioStream *stream = nullptr;
        result = static_cast<Result>(mLibLoader->builder_openStream(aaudioBuilder, &stream));
        mAAudioStream.store(stream);
    }
    if (result != Result::OK) {
        goto error2;
    }

    // Query and cache the stream properties
    mDeviceId = mLibLoader->stream_getDeviceId(mAAudioStream);
    mChannelCount = mLibLoader->stream_getChannelCount(mAAudioStream);
    mSampleRate = mLibLoader->stream_getSampleRate(mAAudioStream);
    mFormat = static_cast<AudioFormat>(mLibLoader->stream_getFormat(mAAudioStream));
    mSharingMode = static_cast<SharingMode>(mLibLoader->stream_getSharingMode(mAAudioStream));
    mPerformanceMode = static_cast<PerformanceMode>(
            mLibLoader->stream_getPerformanceMode(mAAudioStream));
    mBufferCapacityInFrames = mLibLoader->stream_getBufferCapacity(mAAudioStream);
    mBufferSizeInFrames = mLibLoader->stream_getBufferSize(mAAudioStream);


    // These were added in P so we have to check for the function pointer.
    if (mLibLoader->stream_getUsage != nullptr) {
        mUsage = static_cast<Usage>(mLibLoader->stream_getUsage(mAAudioStream));
    }
    if (mLibLoader->stream_getContentType != nullptr) {
        mContentType = static_cast<ContentType>(mLibLoader->stream_getContentType(mAAudioStream));
    }
    if (mLibLoader->stream_getInputPreset != nullptr) {
        mInputPreset = static_cast<InputPreset>(mLibLoader->stream_getInputPreset(mAAudioStream));
    }
    if (mLibLoader->stream_getSessionId != nullptr) {
        mSessionId = static_cast<SessionId>(mLibLoader->stream_getSessionId(mAAudioStream));
    } else {
        mSessionId = SessionId::None;
    }

    LOGD("AudioStreamAAudio.open() format=%d, sampleRate=%d, capacity = %d",
            static_cast<int>(mFormat), static_cast<int>(mSampleRate),
            static_cast<int>(mBufferCapacityInFrames));

error2:
    mLibLoader->builder_delete(aaudioBuilder);
    LOGD("AudioStreamAAudio.open: AAudioStream_Open() returned %s",
         mLibLoader->convertResultToText(static_cast<aaudio_result_t>(result)));
    return result;
}

Result AudioStreamAAudio::close() {
    // The main reason we have this mutex if to prevent a collision between a call
    // by the application to stop a stream at the same time that an onError callback
    // is being executed because of a disconnect. The close will delete the stream,
    // which could otherwise cause the requestStop() to crash.
    std::lock_guard<std::mutex> lock(mLock);

    AudioStream::close();

    // This will delete the AAudio stream object so we need to null out the pointer.
    AAudioStream *stream = mAAudioStream.exchange(nullptr);
    if (stream != nullptr) {
        return static_cast<Result>(mLibLoader->stream_close(stream));
    } else {
        return Result::ErrorClosed;
    }
}

DataCallbackResult AudioStreamAAudio::callOnAudioReady(AAudioStream *stream,
                                                                 void *audioData,
                                                                 int32_t numFrames) {
    DataCallbackResult result = fireDataCallback(audioData, numFrames);
    if (result == DataCallbackResult::Continue) {
        return result;
    } else {
        if (result == DataCallbackResult::Stop) {
            LOGD("Oboe callback returned DataCallbackResult::Stop");
        } else {
            LOGE("Oboe callback returned unexpected value = %d", result);
        }

        if (getSdkVersion() <= __ANDROID_API_P__) {
            launchStopThread();
            if (isMMapUsed()) {
                return DataCallbackResult::Stop;
            } else {
                // Legacy stream <= API_P cannot be restarted after returning Stop.
                return DataCallbackResult::Continue;
            }
        } else {
            return DataCallbackResult::Stop; // OK >= API_Q
        }
    }
}

Result AudioStreamAAudio::requestStart() {
    std::lock_guard<std::mutex> lock(mLock);
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        // Avoid state machine errors in O_MR1.
        if (getSdkVersion() <= __ANDROID_API_O_MR1__) {
            StreamState state = static_cast<StreamState>(mLibLoader->stream_getState(stream));
            if (state == StreamState::Starting || state == StreamState::Started) {
                // WARNING: On P, AAudio is returning ErrorInvalidState for Output and OK for Input.
                return Result::OK;
            }
        }
        if (mStreamCallback != nullptr) { // Was a callback requested?
            setDataCallbackEnabled(true);
        }
        return static_cast<Result>(mLibLoader->stream_requestStart(stream));
    } else {
        return Result::ErrorClosed;
    }
}

Result AudioStreamAAudio::requestPause() {
    std::lock_guard<std::mutex> lock(mLock);
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        // Avoid state machine errors in O_MR1.
        if (getSdkVersion() <= __ANDROID_API_O_MR1__) {
            StreamState state = static_cast<StreamState>(mLibLoader->stream_getState(stream));
            if (state == StreamState::Pausing || state == StreamState::Paused) {
                return Result::OK;
            }
        }
        return static_cast<Result>(mLibLoader->stream_requestPause(stream));
    } else {
        return Result::ErrorClosed;
    }
}

Result AudioStreamAAudio::requestFlush() {
    std::lock_guard<std::mutex> lock(mLock);
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        // Avoid state machine errors in O_MR1.
        if (getSdkVersion() <= __ANDROID_API_O_MR1__) {
            StreamState state = static_cast<StreamState>(mLibLoader->stream_getState(stream));
            if (state == StreamState::Flushing || state == StreamState::Flushed) {
                return Result::OK;
            }
        }
        return static_cast<Result>(mLibLoader->stream_requestFlush(stream));
    } else {
        return Result::ErrorClosed;
    }
}

Result AudioStreamAAudio::requestStop() {
    std::lock_guard<std::mutex> lock(mLock);
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        // Avoid state machine errors in O_MR1.
        if (getSdkVersion() <= __ANDROID_API_O_MR1__) {
            StreamState state = static_cast<StreamState>(mLibLoader->stream_getState(stream));
            if (state == StreamState::Stopping || state == StreamState::Stopped) {
                return Result::OK;
            }
        }
        return static_cast<Result>(mLibLoader->stream_requestStop(stream));
    } else {
        return Result::ErrorClosed;
    }
}

ResultWithValue<int32_t>   AudioStreamAAudio::write(const void *buffer,
                                     int32_t numFrames,
                                     int64_t timeoutNanoseconds) {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        int32_t result = mLibLoader->stream_write(mAAudioStream, buffer,
                                                  numFrames, timeoutNanoseconds);
        return ResultWithValue<int32_t>::createBasedOnSign(result);
    } else {
        return ResultWithValue<int32_t>(Result::ErrorClosed);
    }
}

ResultWithValue<int32_t>   AudioStreamAAudio::read(void *buffer,
                                 int32_t numFrames,
                                 int64_t timeoutNanoseconds) {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        int32_t result = mLibLoader->stream_read(mAAudioStream, buffer,
                                                 numFrames, timeoutNanoseconds);
        return ResultWithValue<int32_t>::createBasedOnSign(result);
    } else {
        return ResultWithValue<int32_t>(Result::ErrorClosed);
    }
}


// AAudioStream_waitForStateChange() can crash if it is waiting on a stream and that stream
// is closed from another thread.  We do not want to lock the stream for the duration of the call.
// So we call AAudioStream_waitForStateChange() with a timeout of zero so that it will not block.
// Then we can do our own sleep with the lock unlocked.
Result AudioStreamAAudio::waitForStateChange(StreamState currentState,
                                        StreamState *nextState,
                                        int64_t timeoutNanoseconds) {
    Result oboeResult = Result::ErrorTimeout;
    int64_t sleepTimeNanos = 20 * kNanosPerMillisecond; // arbitrary
    aaudio_stream_state_t currentAAudioState = static_cast<aaudio_stream_state_t>(currentState);

    aaudio_result_t result = AAUDIO_OK;
    int64_t timeLeftNanos = timeoutNanoseconds;

    mLock.lock();
    while (true) {
        // Do we still have an AAudio stream? If not then stream must have been closed.
        AAudioStream *stream = mAAudioStream.load();
        if (stream == nullptr) {
            if (nextState != nullptr) {
                *nextState = StreamState::Closed;
            }
            oboeResult = Result::ErrorClosed;
            break;
        }

        // Update and query state change with no blocking.
        aaudio_stream_state_t aaudioNextState;
        result = mLibLoader->stream_waitForStateChange(
                mAAudioStream,
                currentAAudioState,
                &aaudioNextState,
                0); // timeout=0 for non-blocking
        // AAudio will return AAUDIO_ERROR_TIMEOUT if timeout=0 and the state does not change.
        if (result != AAUDIO_OK && result != AAUDIO_ERROR_TIMEOUT) {
            oboeResult = static_cast<Result>(result);
            break;
        }
#if OBOE_FIX_FORCE_STARTING_TO_STARTED
        if (OboeGlobals::areWorkaroundsEnabled()
            && aaudioNextState == static_cast<aaudio_stream_state_t >(StreamState::Starting)) {
            aaudioNextState = static_cast<aaudio_stream_state_t >(StreamState::Started);
        }
#endif // OBOE_FIX_FORCE_STARTING_TO_STARTED
        if (nextState != nullptr) {
            *nextState = static_cast<StreamState>(aaudioNextState);
        }
        if (currentAAudioState != aaudioNextState) { // state changed?
            oboeResult = Result::OK;
            break;
        }

        // Did we timeout or did user ask for non-blocking?
        if (timeLeftNanos <= 0) {
            break;
        }

        // No change yet so sleep.
        mLock.unlock(); // Don't sleep while locked.
        if (sleepTimeNanos > timeLeftNanos) {
            sleepTimeNanos = timeLeftNanos; // last little bit
        }
        AudioClock::sleepForNanos(sleepTimeNanos);
        timeLeftNanos -= sleepTimeNanos;
        mLock.lock();
    }

    mLock.unlock();
    return oboeResult;
}

ResultWithValue<int32_t> AudioStreamAAudio::setBufferSizeInFrames(int32_t requestedFrames) {

    AAudioStream *stream = mAAudioStream.load();

    if (stream != nullptr) {
        int32_t adjustedFrames = requestedFrames;
        if (adjustedFrames > mBufferCapacityInFrames) {
            adjustedFrames = mBufferCapacityInFrames;
        }
        adjustedFrames = QuirksManager::getInstance().clipBufferSize(*this, adjustedFrames);

        int32_t newBufferSize = mLibLoader->stream_setBufferSize(mAAudioStream, adjustedFrames);

        // Cache the result if it's valid
        if (newBufferSize > 0) mBufferSizeInFrames = newBufferSize;

        return ResultWithValue<int32_t>::createBasedOnSign(newBufferSize);

    } else {
        return ResultWithValue<int32_t>(Result::ErrorClosed);
    }
}

StreamState AudioStreamAAudio::getState() const {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        aaudio_stream_state_t aaudioState = mLibLoader->stream_getState(stream);
#if OBOE_FIX_FORCE_STARTING_TO_STARTED
        if (OboeGlobals::areWorkaroundsEnabled()
            && aaudioState == AAUDIO_STREAM_STATE_STARTING) {
            aaudioState = AAUDIO_STREAM_STATE_STARTED;
        }
#endif // OBOE_FIX_FORCE_STARTING_TO_STARTED
        return static_cast<StreamState>(aaudioState);
    } else {
        return StreamState::Closed;
    }
}

int32_t AudioStreamAAudio::getBufferSizeInFrames() {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        mBufferSizeInFrames = mLibLoader->stream_getBufferSize(stream);
    }
    return mBufferSizeInFrames;
}

int32_t AudioStreamAAudio::getFramesPerBurst() {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        mFramesPerBurst = mLibLoader->stream_getFramesPerBurst(stream);
    }
    return mFramesPerBurst;
}

void AudioStreamAAudio::updateFramesRead() {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        mFramesRead = mLibLoader->stream_getFramesRead(stream);
    }
}

void AudioStreamAAudio::updateFramesWritten() {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        mFramesWritten = mLibLoader->stream_getFramesWritten(stream);
    }
}

ResultWithValue<int32_t> AudioStreamAAudio::getXRunCount() const {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        return ResultWithValue<int32_t>::createBasedOnSign(mLibLoader->stream_getXRunCount(stream));
    } else {
        return ResultWithValue<int32_t>(Result::ErrorNull);
    }
}

Result AudioStreamAAudio::getTimestamp(clockid_t clockId,
                                   int64_t *framePosition,
                                   int64_t *timeNanoseconds) {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        if (getState() != StreamState::Started) {
            return Result::ErrorInvalidState;
        }
        return static_cast<Result>(mLibLoader->stream_getTimestamp(stream, clockId,
                                               framePosition, timeNanoseconds));
    } else {
        return Result::ErrorNull;
    }
}

ResultWithValue<double> AudioStreamAAudio::calculateLatencyMillis() {
    AAudioStream *stream = mAAudioStream.load();
    if (stream == nullptr) {
        return ResultWithValue<double>(Result::ErrorClosed);
    }

    // Get the time that a known audio frame was presented.
    int64_t hardwareFrameIndex;
    int64_t hardwareFrameHardwareTime;
    auto result = getTimestamp(CLOCK_MONOTONIC,
                               &hardwareFrameIndex,
                               &hardwareFrameHardwareTime);
    if (result != oboe::Result::OK) {
        return ResultWithValue<double>(static_cast<Result>(result));
    }

    // Get counter closest to the app.
    bool isOutput = (getDirection() == oboe::Direction::Output);
    int64_t appFrameIndex = isOutput ? getFramesWritten() : getFramesRead();

    // Assume that the next frame will be processed at the current time
    using namespace std::chrono;
    int64_t appFrameAppTime =
            duration_cast<nanoseconds>(steady_clock::now().time_since_epoch()).count();

    // Calculate the number of frames between app and hardware
    int64_t frameIndexDelta = appFrameIndex - hardwareFrameIndex;

    // Calculate the time which the next frame will be or was presented
    int64_t frameTimeDelta = (frameIndexDelta * oboe::kNanosPerSecond) / getSampleRate();
    int64_t appFrameHardwareTime = hardwareFrameHardwareTime + frameTimeDelta;

    // The current latency is the difference in time between when the current frame is at
    // the app and when it is at the hardware.
    double latencyNanos = static_cast<double>(isOutput
                          ? (appFrameHardwareTime - appFrameAppTime) // hardware is later
                          : (appFrameAppTime - appFrameHardwareTime)); // hardware is earlier
    double latencyMillis = latencyNanos / kNanosPerMillisecond;

    return ResultWithValue<double>(latencyMillis);
}

bool AudioStreamAAudio::isMMapUsed() {
    AAudioStream *stream = mAAudioStream.load();
    if (stream != nullptr) {
        return mLibLoader->stream_isMMapUsed(stream);
    } else {
        return false;
    }
}

} // namespace oboe
