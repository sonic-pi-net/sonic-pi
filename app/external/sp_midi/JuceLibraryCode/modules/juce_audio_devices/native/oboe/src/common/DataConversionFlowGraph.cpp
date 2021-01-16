/*
 * Copyright (C) 2019 The Android Open Source Project
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

#include <memory>

#include "OboeDebug.h"
#include "DataConversionFlowGraph.h"
#include "SourceFloatCaller.h"
#include "SourceI16Caller.h"

#include <flowgraph/ClipToRange.h>
#include <flowgraph/MonoToMultiConverter.h>
#include <flowgraph/RampLinear.h>
#include <flowgraph/SinkFloat.h>
#include <flowgraph/SinkI16.h>
#include <flowgraph/SinkI24.h>
#include <flowgraph/SourceFloat.h>
#include <flowgraph/SourceI16.h>
#include <flowgraph/SourceI24.h>
#include <flowgraph/SampleRateConverter.h>

using namespace oboe;
using namespace flowgraph;
using namespace resampler;

void DataConversionFlowGraph::setSource(const void *buffer, int32_t numFrames) {
    mSource->setData(buffer, numFrames);
}

static MultiChannelResampler::Quality convertOboeSRQualityToMCR(SampleRateConversionQuality quality) {
    switch (quality) {
        case SampleRateConversionQuality::Fastest:
            return MultiChannelResampler::Quality::Fastest;
        case SampleRateConversionQuality::Low:
            return MultiChannelResampler::Quality::Low;
        default:
        case SampleRateConversionQuality::Medium:
            return MultiChannelResampler::Quality::Medium;
        case SampleRateConversionQuality::High:
            return MultiChannelResampler::Quality::High;
        case SampleRateConversionQuality::Best:
            return MultiChannelResampler::Quality::Best;
    }
}

// Chain together multiple processors.
// Callback Output
//     Use SourceCaller that calls original app callback from the flowgraph.
//     The child callback from FilteredAudioStream read()s from the flowgraph.
// Callback Input
//     Child callback from FilteredAudioStream writes()s to the flowgraph.
//     The output of the flowgraph goes through a BlockWriter to the app callback.
// Blocking Write
//     Write buffer is set on an AudioSource.
//     Data is pulled through the graph and written to the child stream.
// Blocking Read
//     Reads in a loop from the flowgraph Sink to fill the read buffer.
//     A SourceCaller then does a blocking read from the child Stream.
//
Result DataConversionFlowGraph::configure(AudioStream *sourceStream, AudioStream *sinkStream) {

    FlowGraphPortFloatOutput *lastOutput = nullptr;

    bool isOutput = sourceStream->getDirection() == Direction::Output;
    bool isInput = !isOutput;
    mFilterStream = isOutput ? sourceStream : sinkStream;

    AudioFormat sourceFormat = sourceStream->getFormat();
    int32_t sourceChannelCount = sourceStream->getChannelCount();
    int32_t sourceSampleRate = sourceStream->getSampleRate();

    AudioFormat sinkFormat = sinkStream->getFormat();
    int32_t sinkChannelCount = sinkStream->getChannelCount();
    int32_t sinkSampleRate = sinkStream->getSampleRate();

    LOGI("%s() flowgraph converts channels: %d to %d, format: %d to %d, rate: %d to %d, qual = %d",
            __func__,
            sourceChannelCount, sinkChannelCount,
            sourceFormat, sinkFormat,
            sourceSampleRate, sinkSampleRate,
            sourceStream->getSampleRateConversionQuality());

    int32_t framesPerCallback = (sourceStream->getFramesPerCallback() == kUnspecified)
                                ? sourceStream->getFramesPerBurst()
                                : sourceStream->getFramesPerCallback();
    // Source
    // If OUTPUT and using a callback then call back to the app using a SourceCaller.
    // If INPUT and NOT using a callback then read from the child stream using a SourceCaller.
    if ((sourceStream->getCallback() != nullptr && isOutput)
        || (sourceStream->getCallback() == nullptr && isInput)) {
        switch (sourceFormat) {
            case AudioFormat::Float:
                mSourceCaller = std::make_unique<SourceFloatCaller>(sourceChannelCount,
                                                                    framesPerCallback);
                break;
            case AudioFormat::I16:
                mSourceCaller = std::make_unique<SourceI16Caller>(sourceChannelCount,
                                                                  framesPerCallback);
                break;
            default:
                LOGE("%s() Unsupported source caller format = %d", __func__, sourceFormat);
                return Result::ErrorIllegalArgument;
        }
        mSourceCaller->setStream(sourceStream);
        lastOutput = &mSourceCaller->output;
    } else {
        // If OUTPUT and NOT using a callback then write to the child stream using a BlockWriter.
        // If INPUT and using a callback then write to the app using a BlockWriter.
        switch (sourceFormat) {
            case AudioFormat::Float:
                mSource = std::make_unique<SourceFloat>(sourceChannelCount);
                break;
            case AudioFormat::I16:
                mSource = std::make_unique<SourceI16>(sourceChannelCount);
                break;
            default:
                LOGE("%s() Unsupported source format = %d", __func__, sourceFormat);
                return Result::ErrorIllegalArgument;
        }
        if (isInput) {
            // The BlockWriter is after the Sink so use the SinkStream size.
            mBlockWriter.open(framesPerCallback * sinkStream->getBytesPerFrame());
            mAppBuffer = std::make_unique<uint8_t[]>(
                    kDefaultBufferSize * sinkStream->getBytesPerFrame());
        }
        lastOutput = &mSource->output;
    }

    // Sample Rate conversion
    if (sourceSampleRate != sinkSampleRate) {
        mResampler.reset(MultiChannelResampler::make(sourceChannelCount,
                                                     sourceSampleRate,
                                                     sinkSampleRate,
                                                     convertOboeSRQualityToMCR(
                                                             sourceStream->getSampleRateConversionQuality())));
        mRateConverter = std::make_unique<SampleRateConverter>(sourceChannelCount,
                                                               *mResampler.get());
        lastOutput->connect(&mRateConverter->input);
        lastOutput = &mRateConverter->output;
    }

    // Expand the number of channels if required.
    if (sourceChannelCount == 1 && sinkChannelCount > 1) {
        mChannelConverter = std::make_unique<MonoToMultiConverter>(sinkChannelCount);
        lastOutput->connect(&mChannelConverter->input);
        lastOutput = &mChannelConverter->output;
    } else if (sourceChannelCount != sinkChannelCount) {
        LOGW("%s() Channel reduction not supported.", __func__);
        return Result::ErrorUnimplemented; // TODO
    }

    // Sink
    switch (sinkFormat) {
        case AudioFormat::Float:
            mSink = std::make_unique<SinkFloat>(sinkChannelCount);
            break;
        case AudioFormat::I16:
            mSink = std::make_unique<SinkI16>(sinkChannelCount);
            break;
        default:
            LOGE("%s() Unsupported sink format = %d", __func__, sinkFormat);
            return Result::ErrorIllegalArgument;;
    }
    lastOutput->connect(&mSink->input);

    mFramePosition = 0;

    return Result::OK;
}

int32_t DataConversionFlowGraph::read(void *buffer, int32_t numFrames, int64_t timeoutNanos) {
    if (mSourceCaller) {
        mSourceCaller->setTimeoutNanos(timeoutNanos);
    }
    int32_t numRead = mSink->read(mFramePosition, buffer, numFrames);
    mFramePosition += numRead;
    return numRead;
}

// This is similar to pushing data through the flowgraph.
int32_t DataConversionFlowGraph::write(void *inputBuffer, int32_t numFrames) {
    // Put the data from the input at the head of the flowgraph.
    mSource->setData(inputBuffer, numFrames);
    while (true) {
        // Pull and read some data in app format into a small buffer.
        int32_t framesRead = mSink->read(mFramePosition, mAppBuffer.get(), flowgraph::kDefaultBufferSize);
        mFramePosition += framesRead;
        if (framesRead <= 0) break;
        // Write to a block adapter, which will call the destination whenever it has enough data.
        int32_t bytesRead = mBlockWriter.write(mAppBuffer.get(),
                                               framesRead * mFilterStream->getBytesPerFrame());
        if (bytesRead < 0) return bytesRead; // TODO review
    }
    return numFrames;
}

int32_t DataConversionFlowGraph::onProcessFixedBlock(uint8_t *buffer, int32_t numBytes) {
    int32_t numFrames = numBytes / mFilterStream->getBytesPerFrame();
    mCallbackResult = mFilterStream->getCallback()->onAudioReady(mFilterStream, buffer, numFrames);
    // TODO handle STOP from callback, process data remaining in the block adapter
    return numBytes;
}