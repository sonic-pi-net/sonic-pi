/*
   SyphonClientBase.h
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

@interface SyphonClientBase : NSObject
/*!
 Returns a new client instance for the described server. You should check the isValid property after initialization to ensure a connection was made to the server.
 @param description Typically acquired from the shared SyphonServerDirectory, or one of Syphon's notifications.
 @param options Currently ignored. May be nil.
 @param handler A block which is invoked when a new frame becomes available. handler may be nil. This block may be invoked on a thread other than that on which the client was created.
 @returns A newly initialized SyphonClientBase object, or nil if a client could not be created.
*/
- (instancetype)initWithServerDescription:(NSDictionary<NSString *, id> *)description options:(nullable NSDictionary<NSString *, id> *)options newFrameHandler:(nullable void (^)(id client))handler NS_DESIGNATED_INITIALIZER;

/*!
 Returns a dictionary with a description of the server the client is attached to. See SyphonServerDirectory for the keys this dictionary may contain
*/
@property (readonly) NSDictionary<NSString *, id> *serverDescription;

/*!
 A client is valid if it has a working connection to a server. Once this returns NO, the SyphonClient will not yield any further frames.
 */
@property (readonly) BOOL isValid;

/*!
 Stops the client from receiving any further frames from the server. Use of this method is optional and releasing all references to the client has the same effect.

 This method may perform work in the OpenGL context. As with any other OpenGL calls, you must ensure no other threads use those contexts during calls to this method.
 */
- (void)stop;

/*!
 Returns YES if the server has output a new frame since the last time newFrameImage was called for this client, NO otherwise.
*/
@property (readonly) BOOL hasNewFrame;
@end

NS_ASSUME_NONNULL_END
