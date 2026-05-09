/*
    SyphonServerDirectory.h
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

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/*!
 @relates SyphonServerDirectory
 The object for this key is a NSString which uniquely identifies a SyphonServer instance. If two dictionaries contain the same string for this key, they represent the same server. This is provided solely to allow you to programmatically determine the identity of a server, and should never be displayed to users in interface elements. This key is not guaranteed to exist in the dictionary.
*/
extern NSString * const SyphonServerDescriptionUUIDKey;

/*!
 @relates SyphonServerDirectory
 The object for this key is a NSString which is the human-readable non-unique name for the SyphonServer. If this string exists and is non-empty, you should use it in interface elements to identify the server, usually in combination with the name of the server's application (see SyphonServerDescriptionAppNameKey). This key is not guaranteed to exist in the dictionary.
*/
extern NSString * const SyphonServerDescriptionNameKey;

/*!
 @relates SyphonServerDirectory
 The object for this key is a NSString with the localized name of the application in which the SyphonServer is running. Use this in combination with the server's name (if present) to identify the server in interface elements.  This key is not guaranteed to exist in the dictionary.
*/
extern NSString * const SyphonServerDescriptionAppNameKey;

/*!
 @relates SyphonServerDirectory
 The object for this key is a NSImage representation of the icon of the application in which the SyphonServer is running. This key is not guaranteed to exist in the dictionary.
*/
extern NSString * const SyphonServerDescriptionIconKey;

/*!
 @relates SyphonServerDirectory
 A new SyphonServer is available on the system. The notification object is the shared SyphonServerDirectory instance. The user info dictionary describes the server and may contain SyphonServerDescription keys.
*/
extern NSString * const SyphonServerAnnounceNotification;

/*!
 @relates SyphonServerDirectory
 An existing SyphonServer instance has changed its description. The notification object is the shared SyphonServerDirectory instance. The user info dictionary describes the server and may contain SyphonServerDescription keys.
*/
extern NSString * const SyphonServerUpdateNotification;

/*!
 @relates SyphonServerDirectory
 A SyphonServer instance will no longer be available.  The notification object is the shared SyphonServerDirectory instance. The user info dictionary describes the retiring server and may contain SyphonServerDescription keys.
*/
extern NSString * const SyphonServerRetireNotification;

/*!
 A server directory provides information on available Syphon servers. Servers are represented by dictionaries. Generally you can expect to find some or all of the keys listed in Constants.
*/

@interface SyphonServerDirectory : NSObject

/*!
 Returns the shared server directory instance. This object is KVO complaint, and can be used to observe changes in server availability, server names and statuses. 
 @returns the shared server instance 
*/
+ (SyphonServerDirectory *)sharedDirectory;

/*!
 `NSArray` of `NSDictionaries` that describe (using the keys above) currently available SyphonServer instances on the system.
*/
@property (readonly) NSArray<NSDictionary<NSString *, id<NSCoding>> *> *servers;

/*! 
 Use this method to discover servers based soley on their name, or application host name. Both parameters are optional. If you do not specify either, all available Syphon servers will be returned.
 @param name Optional (pass `nil` to not specify) Name of the published Syphon server, matches the key value for ``SyphonServerDescriptionNameKey``
 @param appname Optional (pass `nil` to not specify) Application name of the published Syphon server, matches the key value for ``SyphonServerDescriptionAppNameKey``
 @returns An array of dictionaries matching the query you specified. 
*/
- (NSArray<NSDictionary<NSString *, id<NSCoding>> *> *)serversMatchingName:(nullable NSString *)name appName:(nullable NSString *)appname;

@end

NS_ASSUME_NONNULL_END
