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
#import <Metal/Metal.h>
#import <CoreVideo/CoreVideo.h>
#import <CoreMedia/CoreMedia.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>

#import "SyphonMetalServer.h"
#import "capture_target.h"

#include <atomic>
#include <iostream>
#include <sstream>

// NSLog goes to the unified system log (visible in Console.app /
// `log show --process "Sonic Pi"`) regardless of whether std::cout has
// been rewired to gui.log. Syphon's own SYPHONLOG also routes to NSLog,
// so all Syphon diagnostics (ours + upstream's) end up in one place.
// Also to std::cout, which IS gui.log: the unified log is not always
// readable (a machine with log collection off shows nothing), and a feed
// that is black or missing a window is diagnosed from what is written here.
#define SYPHONPUB_LOG(expr) do { \
    std::ostringstream _ss; _ss << expr; \
    NSLog(@"[syphon] %s", _ss.str().c_str()); \
    std::cout << "[syphon] " << _ss.str() << std::endl; \
} while (0)

@interface SonicPiSyphonPublisher : NSObject <SCStreamDelegate, SCStreamOutput>
@end

// What is published — the main window's area showing Sonic Pi's and the
// plugin bridge's windows — and how it follows the window is
// SonicPiCaptureTarget's (capture_target.h); this file turns the frames
// into a Syphon server.
@implementation SonicPiSyphonPublisher
{
    SCStream *_stream;
    SyphonMetalServer *_server;
    id<MTLDevice> _device;
    id<MTLCommandQueue> _queue;
    CVMetalTextureCacheRef _textureCache;
    NSWindow *_window;
    NSString *_name;
    std::atomic<bool> _running;
    BOOL _showCursor;
    SonicPiCaptureTarget *_target;
}

- (instancetype)initWithWindow:(NSWindow *)window
                          name:(NSString *)name
                    showCursor:(BOOL)showCursor
{
    self = [super init];
    if (!self) return nil;
    _window = window;
    _name = [name copy];
    _showCursor = showCursor;
    _target = [[SonicPiCaptureTarget alloc] initWithWindow:window tag:@"syphon"];
    _device = MTLCreateSystemDefaultDevice();
    if (!_device) {
        SYPHONPUB_LOG("no Metal device available");
        return nil;
    }
    _queue = [_device newCommandQueue];
    CVReturn r = CVMetalTextureCacheCreate(kCFAllocatorDefault, NULL, _device, NULL, &_textureCache);
    if (r != kCVReturnSuccess) {
        SYPHONPUB_LOG("CVMetalTextureCacheCreate failed: " << r);
        return nil;
    }
    _running = false;
    return self;
}

- (void)dealloc
{
    [_target stop];
    if (_textureCache) {
        CFRelease(_textureCache);
        _textureCache = NULL;
    }
}

#pragma mark - What to capture

- (SCStreamConfiguration *)configuration
{
    SCStreamConfiguration *config = [[SCStreamConfiguration alloc] init];
    config.pixelFormat = kCVPixelFormatType_32BGRA;
    config.colorSpaceName = kCGColorSpaceSRGB;
    config.queueDepth = 5;
    config.capturesAudio = NO;
    // Cursor overlay is opt-in via the View menu. SCK defaults to YES
    // but for VJ use a stray cursor on stage is rarely wanted, so the
    // app default is NO. Live-updateable via -setShowCursor:.
    config.showsCursor = _showCursor;
    CGRect rect = [_target sourceRect];
    CGFloat scale = [_target scale];
    config.sourceRect = rect;
    config.width  = (size_t)(rect.size.width  * scale);
    config.height = (size_t)(rect.size.height * scale);
    config.minimumFrameInterval = CMTimeMake(1, 60);
    return config;
}

- (void)startAsync
{
    CGWindowID targetWindowNumber = (CGWindowID)[_window windowNumber];
    if (targetWindowNumber == 0) {
        SYPHONPUB_LOG("window number is 0; cannot capture");
        return;
    }

    // First call to getShareableContent triggers the screen-recording prompt.
    [SCShareableContent getShareableContentWithCompletionHandler:^(SCShareableContent * _Nullable content, NSError * _Nullable error) {
        if (error) {
            SYPHONPUB_LOG("SCShareableContent failed: "
                          << [[error localizedDescription] UTF8String]);
            return;
        }
        SCContentFilter *filter = [self->_target filterIn:content];
        if (!filter) {
            SYPHONPUB_LOG("no display in shareable content");
            return;
        }
        SCStreamConfiguration *config = [self configuration];

        self->_stream = [[SCStream alloc] initWithFilter:filter
                                            configuration:config
                                                 delegate:self];
        NSError *addErr = nil;
        BOOL added = [self->_stream
            addStreamOutput:self
                       type:SCStreamOutputTypeScreen
         sampleHandlerQueue:dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0)
                      error:&addErr];
        if (!added) {
            SYPHONPUB_LOG("addStreamOutput failed: "
                          << [[addErr localizedDescription] UTF8String]);
            self->_stream = nil;
            return;
        }

        self->_server = [[SyphonMetalServer alloc]
            initWithName:self->_name device:self->_device options:nil];
        if (!self->_server) {
            SYPHONPUB_LOG("SyphonMetalServer init failed");
            self->_stream = nil;
            return;
        }

        [self->_stream startCaptureWithCompletionHandler:^(NSError * _Nullable startErr) {
            if (startErr) {
                SYPHONPUB_LOG("startCapture failed: "
                              << [[startErr localizedDescription] UTF8String]);
                [self->_server stop];
                self->_server = nil;
                self->_stream = nil;
                return;
            }
            self->_running = true;
            SYPHONPUB_LOG("publishing window " << targetWindowNumber
                          << " and the plugin windows over it as '"
                          << [self->_name UTF8String] << "' ("
                          << (size_t)config.width << "x"
                          << (size_t)config.height << ")");
        }];

        // Follow the window and the bridge from here on (main thread: the
        // window is read on delivery). Registered once the stream exists.
        dispatch_async(dispatch_get_main_queue(), ^{
            SCStream *s = self->_stream;
            if (!s) return;
            [self->_target followStream:s configuration:^{ return [self configuration]; }];
        });
    }];
}

- (void)stop
{
    _running = false;
    [_target stop];
    SCStream *s = _stream;
    SyphonMetalServer *sv = _server;
    _stream = nil;
    _server = nil;
    if (s) {
        [s stopCaptureWithCompletionHandler:^(NSError * _Nullable error) {
            if (error) {
                SYPHONPUB_LOG("stop error: "
                              << [[error localizedDescription] UTF8String]);
            }
        }];
    }
    if (sv) {
        [sv stop];
    }
    SYPHONPUB_LOG("stopped");
}

- (BOOL)isRunning
{
    return _running.load();
}

- (void)setShowCursor:(BOOL)showCursor
{
    _showCursor = showCursor;
    SCStream *s = _stream;
    if (!s) return;
    [s updateConfiguration:[self configuration] completionHandler:^(NSError * _Nullable err) {
        if (err) {
            SYPHONPUB_LOG("updateConfiguration failed: "
                          << [[err localizedDescription] UTF8String]);
        }
    }];
}

#pragma mark - SCStreamOutput

- (void)stream:(SCStream *)stream
    didOutputSampleBuffer:(CMSampleBufferRef)sampleBuffer
                   ofType:(SCStreamOutputType)type
{
    if (!_running.load() || type != SCStreamOutputTypeScreen) return;
    if (!CMSampleBufferIsValid(sampleBuffer)) return;

    // SCK can emit status-only buffers (idle / blank) — skip those, but
    // treat a missing/unreadable status attachment as "complete" since
    // SCK doesn't always populate it. Fail-closed here was wrong: it
    // drops every frame when SCK omits the attachment, leaving the
    // IOSurface at its initial clear-color (black).
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

    // Take a strong local copy of the server before any further work so
    // -stop nilling _server on another thread can't tear it out from under
    // us. Skip the per-frame texture allocation entirely if no client is
    // attached — biggest hot-path win when nothing is listening.
    SyphonMetalServer *srv = _server;
    if (!srv || !srv.hasClients) return;

    CVImageBufferRef pixelBuffer = CMSampleBufferGetImageBuffer(sampleBuffer);
    if (!pixelBuffer) return;

    size_t width  = CVPixelBufferGetWidth(pixelBuffer);
    size_t height = CVPixelBufferGetHeight(pixelBuffer);

    CVMetalTextureRef cvTex = NULL;
    CVReturn cvErr = CVMetalTextureCacheCreateTextureFromImage(
        kCFAllocatorDefault, _textureCache, pixelBuffer, NULL,
        MTLPixelFormatBGRA8Unorm, width, height, 0, &cvTex);
    if (cvErr != kCVReturnSuccess || !cvTex) return;

    id<MTLTexture> texture = CVMetalTextureGetTexture(cvTex);
    if (texture) {
        id<MTLCommandBuffer> cb = [_queue commandBuffer];
        NSRect region = NSMakeRect(0, 0, (CGFloat)width, (CGFloat)height);
        // SCK is top-origin, Syphon is bottom-origin (OpenGL convention).
        [srv publishFrameTexture:texture
                 onCommandBuffer:cb
                     imageRegion:region
                         flipped:YES];
        [cb commit];
    }
    CFRelease(cvTex);

    // Without this, the texture cache retains every frame's IOSurface.
    CVMetalTextureCacheFlush(_textureCache, 0);
}

#pragma mark - SCStreamDelegate

- (void)stream:(SCStream *)stream didStopWithError:(NSError *)error
{
    SYPHONPUB_LOG("stream stopped: "
                  << (error ? [[error localizedDescription] UTF8String]
                            : "no error"));
    _running = false;
}

@end

// ----------------------------------------------------------------------------
// C++ entry points (declared in macos.h)
// ----------------------------------------------------------------------------

static SonicPiSyphonPublisher *gPublisher = nil;

namespace SonicPi {

bool startWindowSyphonPublishing(void *nsViewPtr, const std::string &serverName,
                                 bool showCursor)
{
    if (!nsViewPtr) {
        SYPHONPUB_LOG("null view pointer");
        return false;
    }
    if (gPublisher) {
        [gPublisher stop];
        gPublisher = nil;
    }

    NSView *view = (__bridge NSView *)nsViewPtr;
    NSWindow *window = [view window];
    if (!window) {
        SYPHONPUB_LOG("view has no window");
        return false;
    }

    NSString *name = [NSString stringWithUTF8String:serverName.c_str()];
    gPublisher = [[SonicPiSyphonPublisher alloc] initWithWindow:window
                                                           name:name
                                                     showCursor:showCursor ? YES : NO];
    if (!gPublisher) return false;
    [gPublisher startAsync];
    return true;
}

void stopWindowSyphonPublishing()
{
    if (gPublisher) {
        [gPublisher stop];
        gPublisher = nil;
    }
}

bool isSyphonPublishing()
{
    return gPublisher != nil && [gPublisher isRunning];
}

void setSyphonShowCursor(bool showCursor)
{
    if (gPublisher) {
        [gPublisher setShowCursor:showCursor ? YES : NO];
    }
}

} // namespace SonicPi
