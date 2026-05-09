/*
    SyphonServerDirectory.m
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

#import "SyphonServerDirectory.h"
#import "SyphonPrivate.h"
#import <Cocoa/Cocoa.h>
#import <pthread.h>

#define kSyphonServerDirectoryAnnounceTimeout 6

NSString * const SyphonServerAnnounceNotification = @"SyphonServerAnnounceNotification";
NSString * const SyphonServerUpdateNotification = @"SyphonServerUpdateNotification";
NSString * const SyphonServerRetireNotification = @"SyphonServerRetireNotification";

@interface NSDictionary (SyphonServerDirectoryPimpMyDictionary)
- (NSDictionary *)pimpedVersionForSyphon;
@end

@interface NSArray (SyphonServerDirectoryServerSearch)
- (NSUInteger)indexOfDescriptionForSyphonServerUUID:(NSString *)uuid;
@end

@interface SyphonServerDirectory (Private)
- (id)initOnce;
- (void)requestServerAnnounce;
@end

@implementation SyphonServerDirectory
{
@private
    NSMutableArray<NSDictionary<NSString *, id<NSCoding>> *> *_servers;
    pthread_mutex_t _generalLock;
    pthread_mutex_t _mutateLock;
    NSMutableSet *_pings;
}

+ (BOOL)automaticallyNotifiesObserversForKey:(NSString *)theKey
{
	BOOL automatic;
    if ([theKey isEqualToString:@"servers"])
	{
		automatic=NO;
    }
	else
	{
		automatic=[super automaticallyNotifiesObserversForKey:theKey];
    }
    return automatic;
}

#pragma mark Singleton Instance
+ (void)load
{
	// cause our instantiation so we always post notifications
	[SyphonServerDirectory sharedDirectory];
}

+ (SyphonServerDirectory *)sharedDirectory
{
    static SyphonServerDirectory *sharedDirectory = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedDirectory = [[self alloc] init];
    });
    return sharedDirectory;
}

- (id)init
{
    self = [super init];
    if (self)
	{
		if (pthread_mutex_init(&_generalLock, NULL) != 0
			|| pthread_mutex_init(&_mutateLock, NULL) != 0)
		{
			return nil;
		}
		_servers = [[NSMutableArray alloc] initWithCapacity:4];
		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(handleServerAnnounce:) name:SyphonServerAnnounce object:nil];
		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(handleServerRetire:) name:SyphonServerRetire object:nil];
		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(handleServerUpdate:) name:SyphonServerUpdate object:nil];
		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(handleAccounceRequest:) name:SyphonServerAnnounceRequest object:nil];
		[self requestServerAnnounce];
    }
    return self;
}

- (void)dealloc
{
	// This will never get called as long as we're a singleton object,
	// but maintain it for completeness, and in case we add dealloc on
	// framework unload or something later
	[[NSDistributedNotificationCenter defaultCenter] removeObserver:self];
	pthread_mutex_destroy(&_generalLock);
	pthread_mutex_destroy(&_mutateLock);
}

- (NSArray<NSDictionary<NSString *, id<NSCoding>> *> *)servers
{
	pthread_mutex_lock(&_generalLock);
	NSArray<NSDictionary<NSString *, id<NSCoding>> *> *array = [NSArray arrayWithArray:_servers];
	pthread_mutex_unlock(&_generalLock);
	return array;
}

- (NSArray<NSDictionary<NSString *, id<NSCoding>> *> *)serversMatchingName:(NSString *)name appName:(NSString *)appname
{
	if ([name length] == 0)
	{
		name = nil;
	}
	if ([appname length] == 0)
	{
		appname = nil;
	}
	pthread_mutex_lock(&_generalLock);
	NSIndexSet *indexes = [_servers indexesOfObjectsPassingTest:^(id obj, NSUInteger idx, BOOL *stop) {
		if ((name == nil || [[obj objectForKey:SyphonServerDescriptionNameKey] isEqualToString:name])
			&& (appname == nil || [[obj objectForKey:SyphonServerDescriptionAppNameKey] isEqualToString:appname]))
		{
			return YES;
		} else {
			return NO;
		}
	}];
	NSArray<NSDictionary<NSString *, id<NSCoding>> *> *array = [_servers objectsAtIndexes:indexes];
	pthread_mutex_unlock(&_generalLock);
	return array;
}

- (void)requestServerAnnounce
{
	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:SyphonServerAnnounceRequest object:nil userInfo:nil deliverImmediately:YES];
}

#pragma mark Notification Handling

- (void)handleAccounceRequest:(NSNotification *)aNotification
{
	/*
	 We watch for a global announce request to check the servers we know about are still alive.
	 This could have come from any application.
	 
	 When we get this notification we dispatch a block to be invoked at timeout, and then check that all the
	 servers we know about responded. If they didn't we remove them from our array and post retirement notifications for them.
	 */
	
	pthread_mutex_lock(&_generalLock);
	// If _pings != nil then we're already in the process of doing this, do nothing
	if (_pings == nil)
	{
		_pings = [[NSMutableSet alloc] initWithCapacity:[_servers count]];
		dispatch_time_t when = dispatch_time(DISPATCH_TIME_NOW, NSEC_PER_SEC * kSyphonServerDirectoryAnnounceTimeout);
		dispatch_after(when, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
			// Lock so nobody mutates for the duration
            pthread_mutex_lock(&self->_mutateLock);
			// Lock for access
            pthread_mutex_lock(&self->_generalLock);
			// Get the servers we know about which haven't responded to our announce request
            NSIndexSet *indices = [self->_servers indexesOfObjectsPassingTest:^(id obj, NSUInteger idx, BOOL *stop) {
                if ([self->_pings containsObject:[obj objectForKey:SyphonServerDescriptionUUIDKey]])
				{
					return NO;
				}
				else
				{
					return YES;
				}

			}];
			// Unlock for access as we're either finished or about to post a change, in which case others need access
            pthread_mutex_unlock(&self->_generalLock);
			NSArray *retired = nil;
			if ([indices count] > 0)
			{
				SYPHONLOG(@"Removing servers which didn't respond to an announce request.");
				[self willChange:NSKeyValueChangeRemoval valuesAtIndexes:indices forKey:@"servers"];
				// Lock for access
                pthread_mutex_lock(&self->_generalLock);
				// Save server descriptions for the notifications we will post
                retired = [self->_servers objectsAtIndexes:indices];
				// Make the removal
                [self->_servers removeObjectsAtIndexes:indices];
				// Unlock for access so others can access in response to didChange
                pthread_mutex_unlock(&self->_generalLock);
				[self didChange:NSKeyValueChangeRemoval valuesAtIndexes:indices forKey:@"servers"];
			}
            pthread_mutex_lock(&self->_generalLock);
			// Reset _pings so we will handle the next announce request
            self->_pings = nil;
            pthread_mutex_unlock(&self->_generalLock);
            pthread_mutex_unlock(&self->_mutateLock);
			for (NSDictionary *description in retired) {
				[[NSNotificationCenter defaultCenter] postNotificationName:SyphonServerRetireNotification object:self userInfo:description];
			}
		});		
	}
	pthread_mutex_unlock(&_generalLock);
}

- (void)handleServerAnnounce:(NSNotification *)aNotification
{
	
//	SYPHONLOG(@"new server description: %@", serverInfo);
	
	NSDictionary* serverInfo = [[aNotification userInfo] pimpedVersionForSyphon];
	NSString *uuid = [serverInfo objectForKey:SyphonServerDescriptionUUIDKey];
	// Lock so nobody mutates for the duration
	pthread_mutex_lock(&_mutateLock);
	// Lock for access
	pthread_mutex_lock(&_generalLock);
	NSUInteger index = [_servers indexOfDescriptionForSyphonServerUUID:uuid];
	NSUInteger count = [_servers count];
	// Add the UUID to _pings so we know the server is alive
	if (uuid) [_pings addObject:uuid];
	// Unlock for access, so others can access in response to the willChange
	pthread_mutex_unlock(&_generalLock);
	if (index == NSNotFound)
	{
		NSIndexSet *indexSet = [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(count, 1)];
		[self willChange:NSKeyValueChangeInsertion valuesAtIndexes:indexSet forKey:@"servers"];
		// lock for access
		pthread_mutex_lock(&_generalLock);
		[_servers addObject:serverInfo];
		// unlock for access so others can access in response to the didChange
		pthread_mutex_unlock(&_generalLock);
		[self didChange:NSKeyValueChangeInsertion valuesAtIndexes:indexSet forKey:@"servers"];
		
		[[NSNotificationCenter defaultCenter] postNotificationName:SyphonServerAnnounceNotification object:self userInfo:serverInfo];
	}
	// unlock mutate lock
	pthread_mutex_unlock(&_mutateLock);
}

- (void)handleServerRetire:(NSNotification *)aNotification
{
//	SYPHONLOG(@"retire server description: %@", serverInfo);
	
	NSDictionary* serverInfo = [[aNotification userInfo] pimpedVersionForSyphon];
	NSString *uuid = [serverInfo objectForKey:SyphonServerDescriptionUUIDKey];
	// Lock so nobody mutates for the duration
	pthread_mutex_lock(&_mutateLock);
	// lock for access
	pthread_mutex_lock(&_generalLock);
	NSUInteger index = [_servers indexOfDescriptionForSyphonServerUUID:uuid];
	// unlock for access so others can access in response to willChange
	pthread_mutex_unlock(&_generalLock);
	if(index != NSNotFound)
	{
		NSIndexSet *indexSet = [NSIndexSet indexSetWithIndex:index];
		[self willChange:NSKeyValueChangeRemoval valuesAtIndexes:indexSet forKey:@"servers"];
		// lock for access
		pthread_mutex_lock(&_generalLock);
		[_servers removeObjectAtIndex:index];
		// unlock for access so others can access in response to didChange
		pthread_mutex_unlock(&_generalLock);
		[self didChange:NSKeyValueChangeRemoval valuesAtIndexes:indexSet forKey:@"servers"];
		
		[[NSNotificationCenter defaultCenter] postNotificationName:SyphonServerRetireNotification object:self userInfo:serverInfo];
	}
	// unlock mutate lock
	pthread_mutex_unlock(&_mutateLock);
}

- (void)handleServerUpdate:(NSNotification *)aNotification
{
//	SYPHONLOG(@"updated server description: %@", serverInfo);

	NSDictionary* serverInfo = [[aNotification userInfo] pimpedVersionForSyphon];
	NSString *uuid = [serverInfo objectForKey:SyphonServerDescriptionUUIDKey];
	// Lock so nobody mutates for the duration
	pthread_mutex_lock(&_mutateLock);
	// lock for access
	pthread_mutex_lock(&_generalLock);
	NSUInteger index = [_servers indexOfDescriptionForSyphonServerUUID:uuid];
	// unlock for access so others can access in response to willChange
	pthread_mutex_unlock(&_generalLock);
	if(index != NSNotFound)
	{
		NSIndexSet *indexSet = [NSIndexSet indexSetWithIndex:index];
		[self willChange:NSKeyValueChangeReplacement valuesAtIndexes:indexSet forKey:@"servers"];
		// lock for access
		pthread_mutex_lock(&_generalLock);
		[_servers replaceObjectAtIndex:index withObject:serverInfo];
		// unlock for access so others can access in response to didChange
		pthread_mutex_unlock(&_generalLock);
		[self didChange:NSKeyValueChangeReplacement valuesAtIndexes:indexSet forKey:@"servers"];
				
		[[NSNotificationCenter defaultCenter] postNotificationName:SyphonServerUpdateNotification object:self userInfo:serverInfo];
	}
	pthread_mutex_unlock(&_mutateLock);
}
@end

@implementation NSDictionary (SyphonServerDirectoryPimpMyDictionary)
/*
 
 This all seems a bit silly, and it is probably easier and has no performance penalty to pass the NSImage through the notification.
 NSImage is NSCoding compliant, so that should work and save us a bit of hassle and this bit of code.
  */

- (NSDictionary *)pimpedVersionForSyphon
{
	if ([self objectForKey:SyphonServerDescriptionIconKey] == nil)
	{
		NSString *appName = [self objectForKey:SyphonServerDescriptionAppNameKey];
		NSImage *appImage = nil;
		for(NSRunningApplication* app in [[NSWorkspace sharedWorkspace] runningApplications])
		{
			if([appName isEqualToString:[app localizedName]])
			{
				appImage = [app icon];
			}
		}
		
		if(appImage != nil)
		{
			NSMutableDictionary *newDictionary = [NSMutableDictionary dictionaryWithDictionary:self];
			[newDictionary setObject:appImage forKey:SyphonServerDescriptionIconKey];
			return newDictionary;
		}
	}	
	return self;
}
@end

@implementation NSArray (SyphonServerDirectoryServerSearch)
/*
 
 UUID is the only sure identity, as other members of a dictionary may change, unhelpfully yeilding a NO for isEqual
 
 */
- (NSUInteger)indexOfDescriptionForSyphonServerUUID:(NSString *)uuid
{
	if (uuid == nil)
	{
		return NSNotFound;
	}
	else
	{
		return [self indexOfObjectPassingTest:^(id obj, NSUInteger idx, BOOL *stop) {
			if ([[obj objectForKey:SyphonServerDescriptionUUIDKey] isEqualToString:uuid])
			{
				return YES;
			} else {
				return NO;
			}
		}];
	}
}
@end
