/*
    SyphonServerConnectionManager.m
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

#import "SyphonServerConnectionManager.h"
#import "SyphonPrivate.h"
#import "SyphonMessaging.h"

@interface SyphonServerConnectionManager (Private)
- (void)addInfoClient:(NSString *)clientUUID;
- (void)removeInfoClient:(NSString *)clientUUID;
- (void)addFrameClient:(NSString *)clientUUID;
- (void)removeFrameClient:(NSString *)clientUUID;
- (void)handleDeadConnection;
@end

@implementation SyphonServerConnectionManager {
@private
    SyphonMessageReceiver *_connection;
    NSMutableDictionary<NSString *, SyphonMessageSender *> *_infoClients;
    NSMutableDictionary<NSString *, SyphonMessageSender *> *_frameClients;
    BOOL _alive;
    NSString *_uuid;
    IOSurfaceID _surfaceID;
    SyphonSafeBool _hasClients;
    dispatch_queue_t _queue;
}

+ (BOOL)automaticallyNotifiesObserversForKey:(NSString *)theKey
{
	BOOL automatic;
    if ([theKey isEqualToString:@"hasClients"])
	{
		automatic=NO;
    }
	else
	{
		automatic=[super automaticallyNotifiesObserversForKey:theKey];
    }
    return automatic;
}

+ (NSSet *)keyPathsForValuesAffectingValueForKey:(NSString *)key
{
	if ([key isEqualToString:@"serverDescription"])
	{
		return [NSSet setWithObject:@"name"];
	}
	else
	{
		return [super keyPathsForValuesAffectingValueForKey:key];
	}
}

- (id)init
{
	return [self initWithUUID:nil options:nil];
}

- (id)initWithUUID:(NSString *)uuid options:(NSDictionary<NSString *, id> *)options
{
    self = [super init];
    if (self)
	{
		SyphonSafeBoolSet(&_hasClients, NO);
		_uuid = [uuid copy];
		_infoClients = [[NSMutableDictionary alloc] initWithCapacity:1];
		_frameClients = [[NSMutableDictionary alloc] initWithCapacity:1];
		_queue = dispatch_queue_create([uuid cStringUsingEncoding:NSUTF8StringEncoding], NULL);
	}
	return self;
}

- (void)dealloc
{
	if (_alive)
	{
		[NSException raise:@"SyphonServerConnectionManager" format:@"SyphonServerConnectionManager released while running. Call -stop."];
	}
	SYPHONLOG(@"Releasing SyphonServerConnectionManager for server \"%@\"", _uuid);
}

- (void)setName:(NSString *)serverName
{	
	// Tell connected clients
	dispatch_async(_queue, ^{
        [self->_infoClients enumerateKeysAndObjectsUsingBlock:^(NSString *key, SyphonMessageSender *client, BOOL *stop) {
			[client send:serverName ofType:SyphonMessageTypeUpdateServerName];
		}];
	});
}

- (NSDictionary<NSString *, id<NSCoding>> *)surfaceDescription
{
	return [NSDictionary dictionaryWithObject:SyphonSurfaceTypeIOSurface forKey:SyphonSurfaceType];
}

- (void)addInfoClient:(NSString *)clientUUID
{
	SYPHONLOG(@"Add info client: %@", clientUUID);
	dispatch_async(_queue, ^{
        if (self->_alive && clientUUID)
		{
			SyphonMessageSender *sender = [[SyphonMessageSender alloc] initForName:clientUUID protocol:SyphonMessagingProtocolCFMessage invalidationHandler:^(void){
				[self handleDeadConnection];
			}];
			if (sender)
			{
                NSUInteger countBefore = [self->_infoClients count];
				if (countBefore == 0)
				{
					[self willChangeValueForKey:@"hasClients"];
				}
                if (self->_surfaceID != 0)
				{
                    [sender send:[NSNumber numberWithUnsignedInt:self->_surfaceID] ofType:SyphonMessageTypeUpdateSurfaceID];
				}
                [self->_infoClients setObject:sender forKey:clientUUID];
				if (countBefore == 0)
				{
                    SyphonSafeBoolSet(&self->_hasClients, YES);
					[self didChangeValueForKey:@"hasClients"];
				}
			}
			else
			{
				[self handleDeadConnection];
			}

		}
	});
}

- (void)removeInfoClient:(NSString *)clientUUID
{
	SYPHONLOG(@"Remove info client: %@", clientUUID);
	dispatch_async(_queue, ^{
        if (self->_alive && clientUUID)
		{
            if ([self->_infoClients objectForKey:clientUUID])
			{
                NSUInteger countBefore = [self->_infoClients count];
				if (countBefore == 1)
				{
					[self willChangeValueForKey:@"hasClients"];
				}
				
                [self->_infoClients removeObjectForKey:clientUUID];
				
				if (countBefore == 1)
				{
                    SyphonSafeBoolSet(&self->_hasClients, NO);
					[self didChangeValueForKey:@"hasClients"];
				}
			}
		}
	});
}

- (void)addFrameClient:(NSString *)clientUUID
{
	dispatch_async(_queue, ^{
        if (self->_alive && clientUUID)
		{
			SYPHONLOG(@"Adding frame client: %@", clientUUID);
            SyphonMessageSender *sender = [self->_infoClients objectForKey:clientUUID];
			if (sender == nil)
			{
				SYPHONLOG(@"No info client when frame client added.");
				sender = [[SyphonMessageSender alloc] initForName:clientUUID
														 protocol:SyphonMessagingProtocolCFMessage
											  invalidationHandler:^(void){[self handleDeadConnection];}];
			}
			if (sender)
			{
                [self->_frameClients setObject:sender forKey:clientUUID];
			}
            if (self->_surfaceID != 0)
			{
				// If we have a valid surface
				// then we must have an existing frame
				// so publish it
				[sender send:nil ofType:SyphonMessageTypeNewFrame];
			}
		}
	});
}

- (void)removeFrameClient:(NSString *)clientUUID
{
	SYPHONLOG(@"Removing frame client: %@", clientUUID);
	dispatch_async(_queue, ^{
        if (self->_alive && clientUUID)
		{
            [self->_frameClients removeObjectForKey:clientUUID];
		}
	});
}



#pragma mark Connection handling
- (BOOL)start
{
	SYPHONLOG(@"Start Connection");
	__block BOOL result;
	dispatch_sync(_queue, ^{
		if (!_alive)
		{
            NSSet *classes = [NSSet setWithObjects:[NSString class], nil];
			_connection = [[SyphonMessageReceiver alloc] initForName:_uuid
															protocol:SyphonMessagingProtocolCFMessage
                                                      allowedClasses:classes
															 handler:^(id data, uint32_t type) {
																 switch (type) {
																	 case SyphonMessageTypeAddClientForInfo:
																		 [self addInfoClient:(NSString *)data];
																		 break;
																	 case SyphonMessageTypeRemoveClientForInfo:
																		 [self removeInfoClient:(NSString *)data];
																		 break;
																	 case SyphonMessageTypeAddClientForFrames:
																		 [self addFrameClient:(NSString *)data];
																		 break;
																	 case SyphonMessageTypeRemoveClientForFrames:
																		 [self removeFrameClient:(NSString *)data];
																		 break;
																	 default:
																		 SYPHONLOG(@"Unknown message type %u received.", type);
																		 break;
																 }
															 }];
			
			if(_connection == nil)
			{
				SYPHONLOG(@"Syphon Server: Failed to create connection with UUID, id: %@", _uuid);
				_alive = NO;
			}
			else
			{
				// otherwise it all worked, so lets publish
				SYPHONLOG(@"Created connection with UUID: %@", _uuid);
				_alive = YES;
			}
		}
		result = _alive;
	});
	return result;
}

- (void)stop
{
	SYPHONLOG(@"stopping");
	dispatch_sync(_queue, ^{
		if (_alive)
		{
			// make sure we destroy our connection
			NSUInteger clientCount = [_infoClients count];
			if (clientCount != 0)
			{
				[self willChangeValueForKey:@"hasClients"];
			}
			[_infoClients enumerateKeysAndObjectsUsingBlock:^(NSString *key, SyphonMessageSender *client, BOOL *stop) {
					[client send:nil ofType:SyphonMessageTypeRetireServer];
				}];
			
			[_infoClients removeAllObjects];
			[_frameClients removeAllObjects];
			
			[_connection invalidate];
			_connection = nil;
			
			_alive = NO;
			if (clientCount != 0)
			{
				SyphonSafeBoolSet(&_hasClients, NO);
				[self didChangeValueForKey:@"hasClients"];
			}
		}
	});
}

- (BOOL)hasClients
{
	return SyphonSafeBoolGet(&_hasClients);
}

#pragma mark Serving

- (void)publishNewFrame
{
	dispatch_sync(_queue, ^{
		[_frameClients enumerateKeysAndObjectsUsingBlock:^(NSString *key, SyphonMessageSender *client, BOOL *stop) {
			[client send:nil ofType:SyphonMessageTypeNewFrame];
		}];
	});
}

- (void)setSurfaceID:(IOSurfaceID)newID
{
	dispatch_sync(_queue, ^{
		_surfaceID = newID;
		[_infoClients enumerateKeysAndObjectsUsingBlock:^(NSString * key, SyphonMessageSender * client, BOOL *stop) {
			[client send:[NSNumber numberWithUnsignedInt:newID] ofType:SyphonMessageTypeUpdateSurfaceID];
		}];
	});
}

#pragma mark Notification Handling for NSConnection

- (void)handleDeadConnection
{
	dispatch_async(_queue, ^{
		NSMutableArray<NSString *> *inMemorium = [NSMutableArray arrayWithCapacity:1];
        [self->_infoClients enumerateKeysAndObjectsUsingBlock:^(NSString * key, SyphonMessageSender * client, BOOL *stop) {
			if (!client.isValid)
			{
				[inMemorium addObject:key];
			}
		}];
		[inMemorium enumerateObjectsUsingBlock:^(NSString *obj, NSUInteger idx, BOOL *stop) {
			[self removeInfoClient:obj];
		}];
		[inMemorium removeAllObjects];
        [self->_frameClients enumerateKeysAndObjectsUsingBlock:^(NSString * key, SyphonMessageSender * client, BOOL *stop) {
			if (!client.isValid)
			{
				[inMemorium addObject:key];
			}
		}];
		[inMemorium enumerateObjectsUsingBlock:^(NSString *obj, NSUInteger idx, BOOL *stop) {
			[self removeFrameClient:obj];
		}];
	});
}
@end
