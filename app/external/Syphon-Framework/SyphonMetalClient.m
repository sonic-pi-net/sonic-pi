/*
 SyphonMetalClient.m
 Syphon
 
 Copyright 2020-2023 Maxime Touroute & Philippe Chaurand (www.millumin.com),
 bangnoise (Tom Butterworth) & vade (Anton Marini). All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 
 * Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#import "SyphonMetalClient.h"
#import "SyphonSubclassing.h"
#import <os/lock.h>
#import <stdatomic.h>

@implementation SyphonMetalClient
{
    os_unfair_lock  _threadLock;
    id<MTLTexture>  _frame;
    id<MTLDevice>   _device;
    atomic_bool     _frameValid;
}

@dynamic isValid, serverDescription, hasNewFrame;

- (id)initWithServerDescription:(NSDictionary<NSString *, id> *)description
                         device:(id<MTLDevice>)theDevice
                        options:(NSDictionary<NSString *, id> *)options
                   newFrameHandler:(void (^)(SyphonMetalClient *client))handler
{
    self = [super initWithServerDescription:description options:options newFrameHandler:handler];
    if( self )
    {
        _device = theDevice;
        _threadLock = OS_UNFAIR_LOCK_INIT;
        _frame = nil;
        atomic_store(&_frameValid, false);
    }
    return self;
}

- (void)dealloc
{
    [self stop];
}

- (void)stop
{
    os_unfair_lock_lock(&_threadLock);
    atomic_store(&_frameValid, false);
    _frame = nil;
    _device = nil;
    os_unfair_lock_unlock(&_threadLock);
    [super stop];
}

- (void)invalidateFrame
{
    /*
     DO NOT take the lock here, it may already be locked and waiting for the SyphonClientConnectionManager lock
     */
    atomic_store(&_frameValid, false);
}

- (id<MTLTexture>)newFrameImage
{
    [self updateFrameID];

    id<MTLTexture> image = nil;

    os_unfair_lock_lock(&_threadLock);
    if (atomic_load(&_frameValid) == false)
    {
        _frame = nil;

        IOSurfaceRef surface = [self newSurface];
        if (surface != nil)
        {
            MTLTextureDescriptor* descriptor = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatBGRA8Unorm width:IOSurfaceGetWidth(surface) height:IOSurfaceGetHeight(surface) mipmapped:NO];
            _frame = [_device newTextureWithDescriptor:descriptor iosurface:surface plane:0];

            CFRelease(surface);
        }
        
        atomic_store(&_frameValid, true);
    }

    image = _frame;

    os_unfair_lock_unlock(&_threadLock);

    return image;
}

@end
