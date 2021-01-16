/*
 * Copyright (C) 2016 The Android Open Source Project
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

#ifndef OBOE_STREAM_CALLBACK_H
#define OBOE_STREAM_CALLBACK_H

#include "oboe/Definitions.h"

namespace oboe {

class AudioStream;

/**
 * AudioStreamCallback defines a callback interface for:
 *
 * 1) moving data to/from an audio stream using `onAudioReady`
 * 2) being alerted when a stream has an error using `onError*` methods
 *
 */
class AudioStreamCallback {
public:
    virtual ~AudioStreamCallback() = default;

    /**
     * A buffer is ready for processing.
     *
     * For an output stream, this function should render and write numFrames of data
     * in the stream's current data format to the audioData buffer.
     *
     * For an input stream, this function should read and process numFrames of data
     * from the audioData buffer.
     *
     * The audio data is passed through the buffer. So do NOT call read() or
     * write() on the stream that is making the callback.
     *
     * Note that numFrames can vary unless AudioStreamBuilder::setFramesPerCallback()
     * is called.
     *
     * Also note that this callback function should be considered a "real-time" function.
     * It must not do anything that could cause an unbounded delay because that can cause the
     * audio to glitch or pop.
     *
     * These are things the function should NOT do:
     * <ul>
     * <li>allocate memory using, for example, malloc() or new</li>
     * <li>any file operations such as opening, closing, reading or writing</li>
     * <li>any network operations such as streaming</li>
     * <li>use any mutexes or other synchronization primitives</li>
     * <li>sleep</li>
     * <li>oboeStream->stop(), pause(), flush() or close()</li>
     * <li>oboeStream->read()</li>
     * <li>oboeStream->write()</li>
     * </ul>
     *
     * The following are OK to call from the data callback:
     * <ul>
     * <li>oboeStream->get*()</li>
     * <li>oboe::convertToText()</li>
     * <li>oboeStream->setBufferSizeInFrames()</li>
     * </ul>
     *
     * If you need to move data, eg. MIDI commands, in or out of the callback function then
     * we recommend the use of non-blocking techniques such as an atomic FIFO.
     *
     * @param oboeStream pointer to the associated stream
     * @param audioData buffer containing input data or a place to put output data
     * @param numFrames number of frames to be processed
     * @return DataCallbackResult::Continue or DataCallbackResult::Stop
     */
    virtual DataCallbackResult onAudioReady(
            AudioStream *oboeStream,
            void *audioData,
            int32_t numFrames) = 0;

    /**
     * This will be called when an error occurs on a stream or when the stream is disconnected.
     *
     * Note that this will be called on a different thread than the onAudioReady() thread.
     * This thread will be created by Oboe.
     *
     * The underlying stream will already be stopped by Oboe but not yet closed.
     * So the stream can be queried.
     *
     * Do not close or delete the stream in this method because it will be
     * closed after this method returns.
     *
     * @param oboeStream pointer to the associated stream
     * @param error
     */
    virtual void onErrorBeforeClose(AudioStream* /* oboeStream */, Result /* error */) {}

    /**
     * This will be called when an error occurs on a stream or when the stream is disconnected.
     * The underlying AAudio or OpenSL ES stream will already be stopped AND closed by Oboe.
     * So the underlying stream cannot be referenced.
     * But you can still query most parameters.
     *
     * This callback could be used to reopen a new stream on another device.
     * You can safely delete the old AudioStream in this method.
     *
     * @param oboeStream pointer to the associated stream
     * @param error
     */
    virtual void onErrorAfterClose(AudioStream* /* oboeStream */, Result /* error */) {}

};

} // namespace oboe

#endif //OBOE_STREAM_CALLBACK_H
