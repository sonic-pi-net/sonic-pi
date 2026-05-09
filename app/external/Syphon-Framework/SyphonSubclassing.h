/*
   SyphonSubclassing.h
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
#import <Syphon/SyphonServerBase.h>
#import <Syphon/SyphonClientBase.h>

NS_ASSUME_NONNULL_BEGIN

@interface SyphonServerBase (SyphonSubclassing)
/*!
 Subclasses call this to obtain a new IOSurface to draw to. The surface will always be in a BGRA8 format, other formats are not currently supported.
 @param width the width of the IOSurface in pixels
 @param height the height of the IOSurface in pixels
 @param options currently ignored, pass nil
 @returns an existing or new IOSurface sized for the given dimensions - to be released by the caller using CFRelease
 */
- (nullable IOSurfaceRef)newSurfaceForWidth:(size_t)width height:(size_t)height options:(nullable NSDictionary<NSString *, id> *)options;

/*!
 Subclasses may call this to release any current IOSurface
 */
- (void)destroySurface;

/*!
 Subclasses call this to have the server publish a new frame once the subclass has updated the IOSurface.
 */
- (void)publish;

@end

@interface SyphonClientBase (SyphonSubclassing)
/*!
 Subclasses use this method to acquire an IOSurface representing the current output from the server. Subclasses may consider the returned value valid until the next call to -invalidateFrame.

 @returns An IOSurface representing the live output from the server. YOU ARE RESPONSIBLE FOR RELEASING THIS OBJECT using CFRelease() when you
 are finished with it.
 */
- (IOSurfaceRef)newSurface;

/*!
 Subclasses must call this method when dispensing a new frame to allow SyphonClientBase to return a correct value for -hasNewFrame
 */
- (void)updateFrameID;

/*!
 Subclasses override this method to invalidate their output when the server's surface backing changes. Do not call this method directly -
 it will be called for you when necessary.
 */
- (void)invalidateFrame;
@end

NS_ASSUME_NONNULL_END
