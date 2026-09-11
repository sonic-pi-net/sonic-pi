//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++
//
// Compiled with -fobjc-arc.

#include "macos.h"

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreGraphics/CoreGraphics.h>
#import <CoreVideo/CoreVideo.h>
#import <CoreMedia/CoreMedia.h>
#import <CoreAudio/CoreAudioTypes.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>
#import <IOKit/pwr_mgt/IOPMLib.h>

#import "capture_target.h"

#include <atomic>
#include <iostream>
#include <sstream>
#include <thread>
#include <vector>

// NSLog for the unified log; std::cout for gui.log, which is always
// readable (see capture_target.mm).
#define RECORDER_LOG(expr) do { \
    std::ostringstream _ss; _ss << expr; \
    NSLog(@"[recorder] %s", _ss.str().c_str()); \
    std::cout << "[recorder] " << _ss.str() << std::endl; \
} while (0)

@interface SonicPiRecorder : NSObject <SCStreamDelegate, SCStreamOutput>
@end

@implementation SonicPiRecorder
{
    SCStream *_stream;
    AVAssetWriter *_writer;
    AVAssetWriterInput *_videoInput;
    AVAssetWriterInput *_audioInput;
    CMAudioFormatDescriptionRef _audioFormat;
    NSWindow *_window;
    NSURL *_fileURL;
    // What the picture shows and how it follows the window (capture_target.h).
    SonicPiCaptureTarget *_target;
    // The file's frame size, fixed at start: AVAssetWriter cannot change
    // it. The crop is the same size in points, so a window resized during
    // the recording is clipped or padded rather than stretched.
    size_t _width, _height;
    CGSize _cropSize;
    // Held for the recording's lifetime to block display dim / sleep.
    IOPMAssertionID _powerAssertion;
    BOOL _showCursor;
    std::atomic<bool> _running;
    // Tri-state writer init flag (Idle → Starting → Started | Failed). See
    // maybeStartWriterAtPTS for the compare_exchange race-free start path.
    std::atomic<uint8_t> _writerStarted;

    // Audio path — only populated when the recorder was started with a
    // non-null shm_audio_buffer slot. The reader pulls master-mix frames
    // from the cross-process shm; the dispatch source pulls on a tight
    // cadence and feeds CMSampleBuffers into _audioInput.
    shm_audio_buffer_reader _audioReader;
    // The tap's live channel count (the device's, up to the ring's ceiling):
    // the recording's channels and the stride of every pulled frame.
    uint32_t _audioChannels;
    dispatch_queue_t _audioQueue;
    dispatch_source_t _audioTimer;
    // Audio session anchor — host-time captured at the first non-empty
    // pull, plus the reader frame count at that moment. Subsequent pulls
    // compute PTS as anchor_pts + (frame_count_since_anchor) / sampleRate.
    // CMTIME_IS_VALID(_audioAnchorPTS) doubles as the "anchor set" flag —
    // all anchor reads/writes happen on _audioQueue so no atomic needed.
    CMTime _audioAnchorPTS;
    uint64_t _audioAnchorFrame;
    // Scratch buffer for pumpAudioOnce — hoisted out of the 5ms hot path
    // so we don't malloc/free 8KB 200×/sec when there's nothing to drain.
    std::vector<float> _audioPullBuf;
}

- (instancetype)initWithWindow:(NSWindow *)window
                       fileURL:(NSURL *)fileURL
                    showCursor:(BOOL)showCursor
                     audioSlot:(shm_audio_buffer *)audioSlot
{
    self = [super init];
    if (!self) return nil;
    _window = window;
    _fileURL = fileURL;
    _showCursor = showCursor;
    _target = [[SonicPiCaptureTarget alloc] initWithWindow:window tag:@"recorder"];
    _running = false;
    _writerStarted = 0;  // kIdle
    _audioReader = shm_audio_buffer_reader(audioSlot);
    _audioChannels = audioSlot ? audioSlot->channels : 0;
    _audioAnchorPTS = kCMTimeInvalid;
    _audioAnchorFrame = 0;
    _powerAssertion = kIOPMNullAssertionID;
    static constexpr uint32_t kPullFrames = 1024;
    _audioPullBuf.resize((size_t)kPullFrames * SHM_AUDIO_CHANNELS);
    return self;
}

- (void)dealloc
{
    if (_audioFormat) {
        CFRelease(_audioFormat);
        _audioFormat = NULL;
    }
}

// AVAssetWriter requires the destination not to exist; remove any leftover.
- (BOOL)prepareWriterWithWidth:(size_t)width height:(size_t)height
{
    NSError *err = nil;
    [[NSFileManager defaultManager] removeItemAtURL:_fileURL error:nil];

    _writer = [[AVAssetWriter alloc] initWithURL:_fileURL
                                        fileType:AVFileTypeQuickTimeMovie
                                           error:&err];
    if (!_writer) {
        RECORDER_LOG("AVAssetWriter init failed: "
                     << [[err localizedDescription] UTF8String]);
        return NO;
    }

    // 1-second fragmented .mov so an interrupted recording stays
    // playable up to the last flushed fragment.
    _writer.movieFragmentInterval = CMTimeMake(1, 1);

    NSDictionary *videoSettings = @{
        AVVideoCodecKey: AVVideoCodecTypeH264,
        AVVideoWidthKey: @(width),
        AVVideoHeightKey: @(height),
        AVVideoCompressionPropertiesKey: @{
            AVVideoAverageBitRateKey: @((width * height * 60 * 4) / 100),
            AVVideoMaxKeyFrameIntervalKey: @60,
            AVVideoExpectedSourceFrameRateKey: @60,
        }
    };
    _videoInput = [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo
                                                     outputSettings:videoSettings];
    _videoInput.expectsMediaDataInRealTime = YES;
    if (![_writer canAddInput:_videoInput]) {
        RECORDER_LOG("can't add video input");
        return NO;
    }
    [_writer addInput:_videoInput];

    // Audio input — fed from the engine's OUT tap on a dispatch timer. AAC stereo 256kbps is the same default the audio Rec
    // button uses for WAV-equivalent quality after encoding.
    if (_audioReader.valid() && _audioChannels > 0) {
        NSDictionary *audioSettings = @{
            AVFormatIDKey:         @(kAudioFormatMPEG4AAC),
            AVNumberOfChannelsKey: @(_audioChannels),
            AVSampleRateKey:       @(SHM_AUDIO_SAMPLE_RATE),
            AVEncoderBitRateKey:   @(256 * 1024),
        };
        _audioInput = [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeAudio
                                                         outputSettings:audioSettings];
        _audioInput.expectsMediaDataInRealTime = YES;
        if (![_writer canAddInput:_audioInput]) {
            RECORDER_LOG("can't add audio input — recording video-only");
            _audioInput = nil;
        } else {
            [_writer addInput:_audioInput];
            AudioStreamBasicDescription asbd = {0};
            asbd.mSampleRate       = SHM_AUDIO_SAMPLE_RATE;
            asbd.mFormatID         = kAudioFormatLinearPCM;
            asbd.mFormatFlags      = kAudioFormatFlagIsFloat |
                                     kAudioFormatFlagIsPacked;
            asbd.mChannelsPerFrame = _audioChannels;
            asbd.mBitsPerChannel   = 32;
            asbd.mFramesPerPacket  = 1;
            asbd.mBytesPerFrame    = _audioChannels * sizeof(float);
            asbd.mBytesPerPacket   = asbd.mBytesPerFrame;
            OSStatus st = CMAudioFormatDescriptionCreate(
                kCFAllocatorDefault, &asbd, 0, NULL, 0, NULL, NULL,
                &_audioFormat);
            if (st != noErr) {
                RECORDER_LOG("CMAudioFormatDescriptionCreate failed: " << st);
                _audioInput = nil;
            }
        }
    }
    return YES;
}

// Build a CMSampleBuffer wrapping `numFrames` of interleaved float audio
// at `frameCount` (frames-from-start-of-tap), then append it to the audio
// input. Returns YES on success.
- (BOOL)appendAudioFrames:(const float *)data
                   frames:(uint32_t)numFrames
            startFrameNum:(uint64_t)frameCount
{
    if (!_audioInput || !_audioFormat || numFrames == 0) return NO;
    if (!_audioInput.isReadyForMoreMediaData) return NO;

    const size_t bytes = (size_t)numFrames * _audioChannels * sizeof(float);
    CMBlockBufferRef block = NULL;
    OSStatus st = CMBlockBufferCreateWithMemoryBlock(
        kCFAllocatorDefault, NULL, bytes, kCFAllocatorDefault, NULL,
        0, bytes, kCMBlockBufferAssureMemoryNowFlag, &block);
    if (st != noErr || !block) {
        RECORDER_LOG("CMBlockBufferCreate failed: " << st);
        return NO;
    }
    st = CMBlockBufferReplaceDataBytes(data, block, 0, bytes);
    if (st != noErr) {
        RECORDER_LOG("CMBlockBufferReplaceDataBytes failed: " << st);
        CFRelease(block);
        return NO;
    }

    // PTS = anchor + (frameCount - anchorFrame) / sampleRate. Anchor is
    // set once at first append (host time captured just before this call).
    CMTime offset = CMTimeMake((int64_t)(frameCount - _audioAnchorFrame),
                               SHM_AUDIO_SAMPLE_RATE);
    CMTime pts = CMTimeAdd(_audioAnchorPTS, offset);

    CMSampleTimingInfo timing = {
        .duration             = CMTimeMake(1, SHM_AUDIO_SAMPLE_RATE),
        .presentationTimeStamp = pts,
        .decodeTimeStamp      = kCMTimeInvalid,
    };

    CMSampleBufferRef sample = NULL;
    st = CMSampleBufferCreateReady(
        kCFAllocatorDefault, block, _audioFormat,
        (CMItemCount)numFrames, 1, &timing, 0, NULL, &sample);
    CFRelease(block);
    if (st != noErr || !sample) {
        RECORDER_LOG("CMSampleBufferCreateReady (audio) failed: " << st);
        return NO;
    }
    BOOL ok = [_audioInput appendSampleBuffer:sample];
    CFRelease(sample);
    if (!ok) {
        RECORDER_LOG("audio appendSampleBuffer failed: "
                     << [[_writer.error localizedDescription] UTF8String]);
    }
    return ok;
}

// Lazily start the AVAssetWriter session at `pts`. Called by whichever
// of the SCK video callback or the audio pull arrives first. Returns
// YES once the writer is in the Writing state.
//
// Concurrency: the SCK video callback runs on Apple's user-initiated
// dispatch queue; the audio pull runs on _audioQueue. A naive
// "if (started) return; else start()" lets both threads enter the start
// path, the second call to startWriting then throws from
// startSessionAtSourceTime: which aborts under ARC. compare_exchange
// elects exactly one thread to run the start path; the loser spins on
// yield until the winner publishes Started or Failed (microseconds;
// startWriting is fast and bounded).
- (BOOL)maybeStartWriterAtPTS:(CMTime)pts
{
    enum : uint8_t { kIdle = 0, kStarting = 1, kStarted = 2, kFailed = 3 };
    uint8_t expected = kIdle;
    if (!_writerStarted.compare_exchange_strong(
            expected, kStarting,
            std::memory_order_acq_rel, std::memory_order_acquire)) {
        while (expected == kStarting) {
            std::this_thread::yield();
            expected = _writerStarted.load(std::memory_order_acquire);
        }
        return expected == kStarted;
    }

    if (![_writer startWriting]) {
        RECORDER_LOG("startWriting failed: "
                     << [[_writer.error localizedDescription] UTF8String]);
        _running = false;
        _writerStarted.store(kFailed, std::memory_order_release);
        return NO;
    }
    [_writer startSessionAtSourceTime:pts];
    _writerStarted.store(kStarted, std::memory_order_release);
    return YES;
}

// Drain whatever new frames are available from the reader and append
// them. Does NOT gate on _running — called both from the running timer
// (via pumpAudioOnce) and from the stop path (one last drain before the
// audio input is marked finished). All calls happen on _audioQueue.
// May start the writer session if no video frame has arrived yet —
// ensures we get a file even if the SCK pipeline is delayed or producing
// nothing (e.g. window occluded).
- (void)drainAudio
{
    if (!_audioInput) return;
    const uint32_t kPullFrames = (uint32_t)(_audioPullBuf.size() / SHM_AUDIO_CHANNELS);
    for (int iter = 0; iter < 8; ++iter) {
        uint64_t gap = 0;
        uint32_t got = _audioReader.pull(_audioPullBuf.data(), kPullFrames, &gap);
        if (gap > 0) {
            RECORDER_LOG("audio reader gap: " << gap << " frames dropped");
        }
        if (got == 0) break;

        // First non-empty pull anchors the audio PTS to host-time-now.
        // Aligns audio to the video stream's CMClockHostTime PTS.
        if (!CMTIME_IS_VALID(_audioAnchorPTS)) {
            _audioAnchorPTS = CMClockGetTime(CMClockGetHostTimeClock());
            _audioAnchorFrame = _audioReader.last_read_position() - got;
        }
        // Start the writer session if video hasn't already. Use the audio
        // anchor PTS — both video and audio share CMClockHostTime so the
        // streams will line up regardless of which one starts the session.
        if (![self maybeStartWriterAtPTS:_audioAnchorPTS]) return;
        const uint64_t startFrame = _audioReader.last_read_position() - got;
        [self appendAudioFrames:_audioPullBuf.data() frames:got startFrameNum:startFrame];
    }
}

- (void)pumpAudioOnce
{
    if (!_running.load()) return;
    [self drainAudio];
}

- (void)startAsync
{
    CGWindowID targetWindowNumber = (CGWindowID)[_window windowNumber];
    if (targetWindowNumber == 0) {
        RECORDER_LOG("window number is 0; cannot capture");
        return;
    }

    [SCShareableContent getShareableContentWithCompletionHandler:^(SCShareableContent * _Nullable content, NSError * _Nullable error) {
        if (error) {
            RECORDER_LOG("SCShareableContent failed: "
                         << [[error localizedDescription] UTF8String]);
            return;
        }

        SCContentFilter *filter = [self->_target filterIn:content];
        if (!filter) {
            RECORDER_LOG("no display in shareable content");
            return;
        }
        CGRect rect = [self->_target sourceRect];
        CGFloat scale = [self->_target scale];
        self->_cropSize = rect.size;
        self->_width  = (size_t)(rect.size.width  * scale);
        self->_height = (size_t)(rect.size.height * scale);
        size_t width = self->_width, height = self->_height;

        if (![self prepareWriterWithWidth:width height:height]) {
            return;
        }

        SCStreamConfiguration *config = [self configuration];

        self->_stream = [[SCStream alloc] initWithFilter:filter
                                            configuration:config
                                                 delegate:self];

        dispatch_queue_t q = dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0);
        NSError *addErr = nil;
        if (![self->_stream addStreamOutput:self
                                       type:SCStreamOutputTypeScreen
                         sampleHandlerQueue:q
                                      error:&addErr]) {
            RECORDER_LOG("add screen output failed: "
                         << [[addErr localizedDescription] UTF8String]);
            self->_stream = nil;
            return;
        }

        [self->_stream startCaptureWithCompletionHandler:^(NSError * _Nullable startErr) {
            if (startErr) {
                RECORDER_LOG("startCapture failed: "
                             << [[startErr localizedDescription] UTF8String]);
                self->_stream = nil;
                return;
            }
            self->_running = true;
            RECORDER_LOG("recording window " << targetWindowNumber
                         << " and the plugin windows over it to "
                         << [[self->_fileURL path] UTF8String]
                         << " (" << width << "x" << height << ")");

            // Follow the window and the bridge from here on (main thread:
            // the window is read on delivery).
            dispatch_async(dispatch_get_main_queue(), ^{
                SCStream *s = self->_stream;
                if (!s) return;
                [self->_target followStream:s configuration:^{ return [self configuration]; }];
            });

            // PreventUserIdleDisplaySleep blocks both display dim and
            // system idle sleep. Released in stopWithCompletion. Lid
            // close on aggressive profiles can still force sleep —
            // fragmented .mov is the backstop.
            IOPMAssertionCreateWithName(
                kIOPMAssertionTypePreventUserIdleDisplaySleep,
                kIOPMAssertionLevelOn,
                CFSTR("Sonic Pi session recording"),
                &self->_powerAssertion);

            // Snap the audio reader to "live now": the tap has been
            // flowing since the engine booted, and the recording is of
            // what happens from this moment on.
            if (self->_audioReader.valid()) {
                self->_audioReader.seek_to_live();
                self->_audioQueue = dispatch_queue_create(
                    "net.sonic-pi.recorder.audio", DISPATCH_QUEUE_SERIAL);
                self->_audioTimer = dispatch_source_create(
                    DISPATCH_SOURCE_TYPE_TIMER, 0, 0, self->_audioQueue);
                // 5ms cadence — audio thread writes a quantum (~2.67ms)
                // per audio block; pulling at 5ms gives us ~2 blocks per
                // tick with comfortable margin against the 1s ring.
                dispatch_source_set_timer(self->_audioTimer,
                    dispatch_time(DISPATCH_TIME_NOW, 0),
                    5 * NSEC_PER_MSEC, 1 * NSEC_PER_MSEC);
                __weak SonicPiRecorder *weakSelf = self;
                dispatch_source_set_event_handler(self->_audioTimer, ^{
                    [weakSelf pumpAudioOnce];
                });
                dispatch_resume(self->_audioTimer);
            }
        }];
    }];
}

- (void)stopWithCompletion:(void (^)(NSError * _Nullable))completion
{
    _running = false;

    // Release the power-management assertion immediately so the OS
    // can sleep/dim again, even if Finalize takes a moment.
    if (_powerAssertion != kIOPMNullAssertionID) {
        IOPMAssertionRelease(_powerAssertion);
        _powerAssertion = kIOPMNullAssertionID;
    }

    // Stop the audio pump first so no more samples land after finalise.
    // The cancel handler is synchronous-on-queue, so a final drain pump
    // is dispatched there before we mark the input finished.
    dispatch_source_t audioTimer = _audioTimer;
    dispatch_queue_t audioQueue = _audioQueue;
    _audioTimer = nil;
    _audioQueue = nil;

    [_target stop];
    SCStream *s = _stream;
    _stream = nil;

    void (^finishWriter)(void) = ^{
        if (!self->_writer || self->_writer.status != AVAssetWriterStatusWriting) {
            if (completion) completion(self->_writer.error);
            return;
        }
        [self->_videoInput markAsFinished];
        if (self->_audioInput) [self->_audioInput markAsFinished];
        [self->_writer finishWritingWithCompletionHandler:^{
            RECORDER_LOG("recording finalised: "
                         << [[self->_fileURL path] UTF8String]
                         << " status=" << (long)self->_writer.status);
            if (completion) completion(self->_writer.error);
        }];
    };

    void (^stopVideo)(void) = ^{
        if (s) {
            [s stopCaptureWithCompletionHandler:^(NSError * _Nullable error) {
                if (error) {
                    RECORDER_LOG("stopCapture error: "
                                 << [[error localizedDescription] UTF8String]);
                }
                finishWriter();
            }];
        } else {
            finishWriter();
        }
    };

    if (audioTimer) {
        dispatch_source_cancel(audioTimer);
        // One last ungated drain on the audio queue picks up any tail
        // frames between the final timer tick and the cancel, then we
        // continue the stop sequence on the same queue.
        dispatch_async(audioQueue, ^{
            [self drainAudio];
            stopVideo();
        });
    } else {
        stopVideo();
    }
}

- (BOOL)isRunning
{
    return _running.load();
}

// The configuration for the initial setup and every live update (cursor
// toggle, window moved). The output size is the file's, fixed; the crop
// follows the window's top-left at that same size in points.
- (SCStreamConfiguration *)configuration
{
    SCStreamConfiguration *config = [[SCStreamConfiguration alloc] init];
    config.pixelFormat = kCVPixelFormatType_32BGRA;
    config.colorSpaceName = kCGColorSpaceSRGB;
    config.queueDepth = 5;
    config.capturesAudio = NO;  // Audio comes from SuperSonic, not SCK.
    config.showsCursor = _showCursor;
    config.minimumFrameInterval = CMTimeMake(1, 60);
    CGRect rect = [_target sourceRect];
    rect.size = _cropSize;
    config.sourceRect = rect;
    config.width  = _width;
    config.height = _height;
    return config;
}

- (void)setShowCursor:(BOOL)showCursor
{
    _showCursor = showCursor;
    SCStream *s = _stream;
    if (!s) return;
    [s updateConfiguration:[self configuration] completionHandler:^(NSError * _Nullable err) {
        if (err) {
            RECORDER_LOG("updateConfiguration failed: "
                         << [[err localizedDescription] UTF8String]);
        }
    }];
}

#pragma mark - SCStreamOutput

- (void)stream:(SCStream *)stream
    didOutputSampleBuffer:(CMSampleBufferRef)sampleBuffer
                   ofType:(SCStreamOutputType)type
{
    if (!_running.load()) return;
    if (!CMSampleBufferIsValid(sampleBuffer)) return;
    if (!CMSampleBufferDataIsReady(sampleBuffer)) return;

    if (type != SCStreamOutputTypeScreen) return;

    // Drop status-only/idle screen buffers. Default-to-Complete on a
    // missing attachment — SCK doesn't always populate it on macOS 26.
    CFArrayRef attachments = CMSampleBufferGetSampleAttachmentsArray(sampleBuffer, false);
    if (attachments && CFArrayGetCount(attachments) > 0) {
        CFDictionaryRef att = (CFDictionaryRef)CFArrayGetValueAtIndex(attachments, 0);
        CFNumberRef statusNumber = (CFNumberRef)CFDictionaryGetValue(att,
            (__bridge CFStringRef)SCStreamFrameInfoStatus);
        SCFrameStatus status = SCFrameStatusComplete;
        if (statusNumber) {
            CFNumberGetValue(statusNumber, kCFNumberIntType, &status);
        }
        if (status != SCFrameStatusComplete) return;
    }

    CMTime pts = CMSampleBufferGetPresentationTimeStamp(sampleBuffer);
    if (![self maybeStartWriterAtPTS:pts]) return;

    if (_videoInput.isReadyForMoreMediaData) {
        [_videoInput appendSampleBuffer:sampleBuffer];
    }
}

#pragma mark - SCStreamDelegate

- (void)stream:(SCStream *)stream didStopWithError:(NSError *)error
{
    RECORDER_LOG("stream stopped: "
                 << (error ? [[error localizedDescription] UTF8String]
                           : "no error"));
    _running = false;
}

@end

// ----------------------------------------------------------------------------
// C++ entry points (declared in macos.h)
// ----------------------------------------------------------------------------

static SonicPiRecorder *gRecorder = nil;

namespace SonicPi {

bool startSessionRecording(void *nsViewPtr, const std::string &filePath,
                           bool showCursor, shm_audio_buffer *audioSlot)
{
    if (!nsViewPtr) {
        RECORDER_LOG("null view pointer");
        return false;
    }

    // Permission preflight — must happen BEFORE any state changes
    // (audio synth /s_new, recorder allocation, menu toggle). SCK's own
    // permission check is async and only fires when getShareableContent
    // resolves, which would leave us "half-started" if the user denies.
    // CGPreflight returns synchronously: true = granted, false = not yet.
    if (!CGPreflightScreenCaptureAccess()) {
        // CGRequest triggers the macOS prompt asynchronously. The result
        // isn't synchronously observable; the user has to enable
        // Sonic Pi in Settings → Privacy & Security → Screen Recording
        // and try again. We deliberately abort now so callers can roll
        // back any pre-flight work (e.g. the audio-out synth /s_new).
        CGRequestScreenCaptureAccess();
        RECORDER_LOG("screen recording permission not granted — aborting; "
                     "user must enable Sonic Pi under Settings → Privacy "
                     "& Security → Screen Recording and retry");
        return false;
    }

    if (gRecorder) {
        [gRecorder stopWithCompletion:nil];
        gRecorder = nil;
    }

    NSView *view = (__bridge NSView *)nsViewPtr;
    NSWindow *window = [view window];
    if (!window) {
        RECORDER_LOG("view has no window");
        return false;
    }

    NSURL *fileURL = [NSURL fileURLWithPath:
        [NSString stringWithUTF8String:filePath.c_str()]];

    gRecorder = [[SonicPiRecorder alloc] initWithWindow:window
                                                fileURL:fileURL
                                             showCursor:showCursor ? YES : NO
                                              audioSlot:audioSlot];
    if (!gRecorder) return false;
    [gRecorder startAsync];
    return true;
}

void stopSessionRecording()
{
    if (gRecorder) {
        [gRecorder stopWithCompletion:nil];
        gRecorder = nil;
    }
}

bool isSessionRecording()
{
    return gRecorder != nil && [gRecorder isRunning];
}

void setRecordShowCursor(bool showCursor)
{
    if (gRecorder) {
        [gRecorder setShowCursor:showCursor ? YES : NO];
    }
}

} // namespace SonicPi
