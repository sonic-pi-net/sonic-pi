/*
   SyphonClientBase.m
   Syphon

    Copyright 2010-2020 bangnoise (Tom Butterworth) & vade (Anton Marini).
    All rights reserved.

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

#import "SyphonClientBase.h"
#import "SyphonServerDirectory.h"
#import "SyphonClientConnectionManager.h"
#import "SyphonPrivate.h"
#import <os/lock.h>

// TODO: name?
static void *SyphonClientServersContext = &SyphonClientServersContext;

@implementation SyphonClientBase {
    os_unfair_lock                  _lock;
    NSUInteger                      _lastFrameID;
    SyphonClientConnectionManager   *_connectionManager;
    NSDictionary<NSString *, id>    *_serverDescription;
    void                            (^_handler)(id);
}

+ (BOOL)automaticallyNotifiesObserversForKey:(NSString *)theKey
{
    if ([theKey isEqualToString:@"serverDescription"])
    {
        return NO;
    }
    else
    {
        return [super automaticallyNotifiesObserversForKey:theKey];
    }
}

- (id)init
{
    // Returns nil
    return [self initWithServerDescription:@{} options:nil newFrameHandler:nil];
}

- (instancetype)initWithServerDescription:(NSDictionary<NSString *, id> *)description
								  options:(nullable NSDictionary<NSString *, id> *)options
						  newFrameHandler:(nullable void (^)(id client))handler
{
    self = [super init];
    if (self)
    {
        _lock = OS_UNFAIR_LOCK_INIT;

        _connectionManager = [[SyphonClientConnectionManager alloc] initWithServerDescription:description];

        _handler = [handler copy]; // copy don't retain
        _serverDescription = description;

        [[SyphonServerDirectory sharedDirectory] addObserver:self
                                                  forKeyPath:@"servers"
                                                     options:NSKeyValueObservingOptionInitial | NSKeyValueObservingOptionNew
                                                     context:SyphonClientServersContext];

        [_connectionManager addInfoClient:(id <SyphonInfoReceiving>)self
                            isFrameClient:handler != nil ? YES : NO];

        NSNumber *dictionaryVersion = [description objectForKey:SyphonServerDescriptionDictionaryVersionKey];
        if (dictionaryVersion == nil
            || [dictionaryVersion unsignedIntValue] > kSyphonDictionaryVersion
            || _connectionManager == nil)
        {
            return nil;
        }
    }
    return self;
}

- (BOOL)isValid
{
    os_unfair_lock_lock(&_lock);
    BOOL result = _connectionManager.isValid;
    os_unfair_lock_unlock(&_lock);
    return result;
}

- (void) dealloc
{
    // Don't call anything in the subclass, it has already been dealloc'd
    [[SyphonServerDirectory sharedDirectory] removeObserver:self forKeyPath:@"servers"];
    [self stopBase];
}

- (void)stop
{
    [self stopBase];
}

- (void)stopBase
{
    os_unfair_lock_lock(&_lock);
    if (_connectionManager)
    {
        [_connectionManager removeInfoClient:(id <SyphonInfoReceiving>)self
                               isFrameClient:_handler != nil ? YES : NO];
        _connectionManager = nil;
    }
    os_unfair_lock_unlock(&_lock);
}

- (void)receiveNewFrame
{
    if (_handler)
    {
        _handler(self);
    }
}

- (void)invalidateFrame
{
    // Nothing for us to do, subclasses will usually override this
}

- (BOOL)hasNewFrame
{
    BOOL result;
    os_unfair_lock_lock(&_lock);
    result = _lastFrameID != _connectionManager.frameID;
    os_unfair_lock_unlock(&_lock);
    return result;
}

- (NSDictionary<NSString *, id> *)serverDescription
{
    os_unfair_lock_lock(&_lock);
    NSDictionary *description = _serverDescription;
    os_unfair_lock_unlock(&_lock);
    return description;
}

#pragma mark Changes
- (void)observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary<NSString *,id> *)change context:(void *)context
{
    if (context == SyphonClientServersContext)
    {
        NSUInteger kind = [change[NSKeyValueChangeKindKey] unsignedIntegerValue];
        if (kind == NSKeyValueChangeSetting || kind == NSKeyValueChangeReplacement)
        {
            NSArray *servers = change[NSKeyValueChangeNewKey];
            NSString *uuid = _serverDescription[SyphonServerDescriptionUUIDKey];
            for (NSDictionary *description in servers) {
                if ([description[SyphonServerDescriptionUUIDKey] isEqualToString:uuid] &&
                    ![_serverDescription isEqualToDictionary:description])
                {
                    [self willChangeValueForKey:@"serverDescription"];
                    NSDictionary *copied = [description copy];
                    os_unfair_lock_lock(&_lock);
                    _serverDescription = copied;
                    os_unfair_lock_unlock(&_lock);
                    [self didChangeValueForKey:@"serverDescription"];
                }
            }
        }
    }
    else
    {
        [super observeValueForKeyPath:keyPath ofObject:object change:change context:context];
    }
}

- (void)updateFrameID
{
    os_unfair_lock_lock(&_lock);
    _lastFrameID = [_connectionManager frameID];
    os_unfair_lock_unlock(&_lock);
}

- (IOSurfaceRef)newSurface
{
    IOSurfaceRef surface;
    [self updateFrameID];
    os_unfair_lock_lock(&_lock);
    surface = [_connectionManager newSurface];
    os_unfair_lock_unlock(&_lock);
    return surface;
}

@end
