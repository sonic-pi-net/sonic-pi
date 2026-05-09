/*
   SyphonServerBase.h
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

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/*!
 @relates SyphonServerBase
 If this key is matched with a NSNumber with a BOOL value YES, then the server will be invisible to other Syphon users. You are then responsible for passing the NSDictionary returned by serverDescription to processes which require it to create a SyphonClient. Default is NO.
 */
extern NSString * const SyphonServerOptionIsPrivate;

@interface SyphonServerBase : NSObject

/*!
 If you implement your own subclass of SyphonServerBase, you must call this designated initializer from your own initializer.

 Creates a new server with the specified human-readable name (which need not be unique) and options. The server will be started immediately. Init may fail and return nil if the server could not be started.

 @param serverName Non-unique human readable server name. This is not required and may be nil, but is usually used by clients in their UI to aid identification.
 @param options A dictionary containing key-value pairs to specify options for the server. Currently supported options are SyphonServerOptionIsPrivate, plus any added by the subclass. See their descriptions for details.
 @returns A newly intialized Syphon server. Nil on failure.
*/
- (instancetype)initWithName:(nullable NSString*)serverName options:(nullable NSDictionary<NSString *, id> *)options NS_DESIGNATED_INITIALIZER;
/*!
 A string representing the name of the server.
 */
@property (strong) NSString* name;

/*!
 A dictionary describing the server. Normally you won't need to access this, however if you created the server as private (using SyphonServerOptionIsPrivate) then you must pass this dictionary to any process in which you wish to create a SyphonClient. You should not rely on the presence of any particular keys in this dictionary.
 */
@property (readonly) NSDictionary<NSString *, id<NSCoding>>* serverDescription;

/*!
 YES if clients are currently attached, NO otherwise. If you generate frames frequently (for instance on a display-link timer), you may choose to test this and only call publishFrameTexture:textureTarget:imageRegion:textureDimensions:flipped: when clients are attached.
 */
@property (readonly) BOOL hasClients;

/*!
 Stops the server instance. Use of this method is optional and releasing all references to the server has the same effect.
 */
- (void)stop;

@end

NS_ASSUME_NONNULL_END
