/*
    SyphonClientConnectionManager.m
    Syphon

     Copyright 2010-2011 bangnoise (Tom Butterworth) & vade (Anton Marini).
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


#import "SyphonClientConnectionManager.h"
#import "SyphonPrivate.h"
#import "SyphonMessaging.h"
#import <IOSurface/IOSurface.h>
#import <os/lock.h>
#import <stdatomic.h>

#pragma mark Shared Instances

static os_unfair_lock _lookupTableLock = OS_UNFAIR_LOCK_INIT;
static NSMapTable *_lookupTable;

static id SyphonClientPrivateCopyInstance(NSString *uuid)
{
	id result = nil;
	os_unfair_lock_lock(&_lookupTableLock);
	if (uuid) result = [_lookupTable objectForKey:uuid];
	os_unfair_lock_unlock(&_lookupTableLock);
	return result;
}

static void SyphonClientPrivateInsertInstance(id instance, NSString *uuid)
{
	os_unfair_lock_lock(&_lookupTableLock);
	if (uuid)
	{
		if (!_lookupTable) _lookupTable = [[NSMapTable alloc] initWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableWeakMemory capacity:1];
		[_lookupTable setObject:instance forKey:uuid];
	}
	os_unfair_lock_unlock(&_lookupTableLock);
}

static void SyphonClientPrivateRemoveInstance(id instance, NSString *uuid)
{
	os_unfair_lock_lock(&_lookupTableLock);
	if (uuid) [_lookupTable removeObjectForKey:uuid];
	if ([_lookupTable count] == 0)
	{
		_lookupTable = nil;
	}
	os_unfair_lock_unlock(&_lookupTableLock);
}

@interface SyphonClientConnectionManager (Private)
- (void)publishNewFrame;
- (void)setSurfaceID:(IOSurfaceID)surfaceID;
- (IOSurfaceRef)surfaceHavingLock;
- (void)endConnectionHavingLock:(BOOL)hasLock;
- (void)invalidateFramesHavingLock;
@end
@implementation SyphonClientConnectionManager
{
@private
    NSString *_myUUID;
    IOSurfaceID _surfaceID;
    IOSurfaceRef _surface;
    uint32_t _lastSeed;
    NSUInteger _frameID;
    NSString *_serverUUID;
    BOOL _serverActive;
    SyphonMessageReceiver *_connection;
    atomic_int _handlerCount;
    NSHashTable *_infoClients;
    NSHashTable *_frameClients;
    dispatch_queue_t _frameQueue;
    os_unfair_lock _lock;
}

- (id)initWithServerDescription:(NSDictionary<NSString *, id> *)description
{
    self = [super init];
	if (self)
	{
		_serverUUID = [[description objectForKey:SyphonServerDescriptionUUIDKey] copy];
		
		// Return an existing instance for this server if we have one
		id existing = SyphonClientPrivateCopyInstance(_serverUUID);
		if (existing)
		{
			return existing;
		}
		
		NSArray *surfaces = [description objectForKey:SyphonServerDescriptionSurfacesKey];
		BOOL hasIOSurface = NO;
		for (NSDictionary *surface in surfaces)
		{
			if ([[surface objectForKey:SyphonSurfaceType] isEqualToString:SyphonSurfaceTypeIOSurface]) hasIOSurface = YES;
		}
		
		if (!hasIOSurface)
		{
			return nil;
		}
		
		_lock = OS_UNFAIR_LOCK_INIT;
		_myUUID = SyphonCreateUUIDString();
        _serverActive = YES; // Until we know better - SyphonClient has API behaviour depending on this

		SyphonClientPrivateInsertInstance(self, _serverUUID);
	}
	return self;
}

- (void) dealloc
{
	SyphonClientPrivateRemoveInstance(self, _serverUUID);
}

- (void)endConnectionHavingLock:(BOOL)hasLock
{
	SYPHONLOG(@"Ending connection");
	SyphonMessageReceiver *connection;
	// we copy and clear ivars inside the lock, release them outside it
	if (!hasLock) os_unfair_lock_lock(&_lock);
	connection = _connection;
	_connection = nil;
    [self invalidateFramesHavingLock];
	if (!hasLock) os_unfair_lock_unlock(&_lock);
	[connection invalidate];
}

- (void)invalidateServerNotHavingLock
{
    os_unfair_lock_lock(&_lock);
    _serverActive = NO;
    [self endConnectionHavingLock:YES];
    os_unfair_lock_unlock(&_lock);
}

- (void)invalidateFramesHavingLock
{
    if (_surface)
	{
		CFRelease(_surface);
		_surface = NULL;
    }
    for (id <SyphonInfoReceiving> obj in _infoClients) {
        [obj invalidateFrame];
    }
}

- (BOOL)isValid
{
	BOOL result;
	os_unfair_lock_lock(&_lock);
	result = _serverActive;
	os_unfair_lock_unlock(&_lock);
	return result;
}

- (void)addInfoClient:(id <SyphonInfoReceiving>)client isFrameClient:(BOOL)isFrameClient
{
	os_unfair_lock_lock(&_lock);
	if (_infoClients == nil)
    {
        _infoClients = [NSHashTable weakObjectsHashTable];
    }
    [_infoClients addObject:client];
	BOOL shouldSendAdd = NO;
	if (_infoClients.count == 1)
	{
		// set up a connection to receive and deal with messages from the server
        NSSet *classes = [NSSet setWithObjects:[NSString class], [NSNumber class], nil];
        _connection = [[SyphonMessageReceiver alloc] initForName:_myUUID
                                                        protocol:SyphonMessagingProtocolCFMessage
                                                  allowedClasses:classes
                                                         handler:^(id data, uint32_t type) {
			switch (type) {
				case SyphonMessageTypeNewFrame:
					[self publishNewFrame];
					break;
				case SyphonMessageTypeUpdateServerName:
                    // Ignore, handled by SyphonClient from SyphonServerDirectory now
                    // https://github.com/Syphon/Syphon-Framework/issues/34
					break;
				case SyphonMessageTypeUpdateSurfaceID:
					[self setSurfaceID:[(NSNumber *)data unsignedIntValue]];
					break;
				case SyphonMessageTypeRetireServer:
					[self invalidateServerNotHavingLock];
					break;
				default:
					SYPHONLOG(@"Unknown message type #%u received", type);
					break;
			}
		}];
		
		if (_connection != nil)
		{
			shouldSendAdd = YES;
		}
	}
    if (isFrameClient && _frameQueue == nil)
    {
        _frameQueue = dispatch_queue_create([_myUUID cStringUsingEncoding:NSUTF8StringEncoding], 0);
        _frameClients = [NSHashTable weakObjectsHashTable];
    }
	os_unfair_lock_unlock(&_lock);
    if (isFrameClient)
    {
        // only access _frameClients within the queue
        dispatch_sync(_frameQueue, ^{
            [_frameClients addObject:client];
        });
    }
	// We can do this outside the lock because we're not using any protected resources
	if (shouldSendAdd || isFrameClient)
	{
		SyphonMessageSender *sender = [[SyphonMessageSender alloc] initForName:_serverUUID
																	  protocol:SyphonMessagingProtocolCFMessage
														   invalidationHandler:nil];
		
		if (sender == nil)
		{
			SYPHONLOG(@"Failed to create connection to server with uuid:%@", _serverUUID);
			[self invalidateServerNotHavingLock];
		}
        if (shouldSendAdd)
        {
            SYPHONLOG(@"Registering for info updates");
            [sender send:_myUUID ofType:SyphonMessageTypeAddClientForInfo];
        }
        if (isFrameClient && atomic_fetch_add(&_handlerCount, 1) == 0)
        {
            SYPHONLOG(@"Registering for frame updates");
            [sender send:_myUUID ofType:SyphonMessageTypeAddClientForFrames];
        }
	}
}

- (void)removeInfoClient:(id <SyphonInfoReceiving>)client isFrameClient:(BOOL)isFrameClient
{
    if (isFrameClient)
    {
        dispatch_sync(_frameQueue, ^{
            [_frameClients removeObject:client];
        });
    }
	os_unfair_lock_lock(&_lock);
    [_infoClients removeObject:client];
    BOOL shouldSendRemove = _infoClients.count == 0 ? YES : NO;
	if (shouldSendRemove)
	{
        [self endConnectionHavingLock:YES];
	}
	os_unfair_lock_unlock(&_lock);
    if (_serverActive && (shouldSendRemove || isFrameClient))
    {
        // Remove ourself from the server
        SyphonMessageSender *sender = [[SyphonMessageSender alloc] initForName:_serverUUID
                                                                      protocol:SyphonMessagingProtocolCFMessage
                                                           invalidationHandler:nil];

        if (isFrameClient && atomic_fetch_sub(&_handlerCount, 1) == 1)
        {
            SYPHONLOG(@"De-registering for frame updates");
            [sender send:_myUUID ofType:SyphonMessageTypeRemoveClientForFrames];
        }
        if (shouldSendRemove)
        {
            SYPHONLOG(@"De-registering for info updates");
            [sender send:_myUUID ofType:SyphonMessageTypeRemoveClientForInfo];
        }
    }
}

- (NSString*) description
{
	return [NSString stringWithFormat:@"Server UUID: %@", _serverUUID, nil];
}

- (void)publishNewFrame
{
	// This could be dispatch_async WHEN we coalesce incoming messages
	// Just now it's sync so a server can't flood a client (at the cost of blocking servers)
	dispatch_sync(_frameQueue, ^{
		for (id <SyphonFrameReceiving> obj in _frameClients) {
			[obj receiveNewFrame];
		}
	});
}

- (IOSurfaceRef)surfaceHavingLock
{
	if (!_surface)
	{
		// WHOA - This causes a retain.
		_surface = IOSurfaceLookup(_surfaceID);
	}
	return _surface;
}

- (void)setSurfaceID:(IOSurfaceID)surfaceID
{
	os_unfair_lock_lock(&_lock);
	_surfaceID = surfaceID;
	_frameID++; // new surface means a new frame
    [self invalidateFramesHavingLock];
	os_unfair_lock_unlock(&_lock);
}

- (IOSurfaceRef)newSurface
{
    IOSurfaceRef surface;
	os_unfair_lock_lock(&_lock);
    surface = [self surfaceHavingLock];
	os_unfair_lock_unlock(&_lock);
    if (surface) CFRetain(surface);
    return surface;
}

- (NSUInteger)frameID
{
	NSUInteger result;
	os_unfair_lock_lock(&_lock);
	IOSurfaceRef surface = [self surfaceHavingLock];
	if (surface)
	{
		uint32_t seed = IOSurfaceGetSeed(surface);
		if (_lastSeed != seed)
		{
			_frameID++;
			_lastSeed = seed;
		}
	}
	result = _frameID;
	os_unfair_lock_unlock(&_lock);
	return result;
}

@end
