/*
 SyphonServerRendererGL.m
 Syphon

 Copyright 2016 bangnoise (Tom Butterworth) & vade (Anton Marini).
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

#import "SyphonServerRendererGL.h"
#import "SyphonIOSurfaceImageLegacy.h"

// These match OpenGL definitions but this class is agnostic
// as to profile so doesn't import GL headers
#define SYPHON_GL_DEPTH_COMPONENT16 0x81A5
#define SYPHON_GL_DEPTH_COMPONENT24 0x81A6
#define SYPHON_GL_DEPTH_COMPONENT32 0x81A7
#define SYPHON_GL_STENCIL_INDEX1    0x8D46
#define SYPHON_GL_STENCIL_INDEX4    0x8D47
#define SYPHON_GL_STENCIL_INDEX8    0x8D48
#define SYPHON_GL_STENCIL_INDEX16   0x8D49

@implementation SyphonServerRendererGL
{
@private
    CGLContextObj _context;
    GLuint  _MSAASampleCount;
    GLenum  _depthBufferFormat;
    GLenum  _stencilBufferFormat;
    GLsizei _width;
    GLsizei _height;
}

- (id)initWithContext:(CGLContextObj)context MSAASampleCount:(GLuint)msc depthBufferResolution:(GLuint)dbr stencilBufferResolution:(GLuint)sbr
{
    self = [super init];
    if (self)
    {
        _context = CGLRetainContext(context);
        _MSAASampleCount = msc;
        if (dbr == 0) _depthBufferFormat = 0;
        else if (dbr < 20) _depthBufferFormat = SYPHON_GL_DEPTH_COMPONENT16;
        else if (dbr < 28) _depthBufferFormat = SYPHON_GL_DEPTH_COMPONENT24;
        else _depthBufferFormat = SYPHON_GL_DEPTH_COMPONENT32;

        // In fact this will almost always be ignored other than to check it is non-zero
        if (sbr == 0) _stencilBufferFormat = 0;
        else if (sbr < 3) _stencilBufferFormat = SYPHON_GL_STENCIL_INDEX1;
        else if (sbr < 6) _stencilBufferFormat = SYPHON_GL_STENCIL_INDEX4;
        else if (sbr < 12) _stencilBufferFormat = SYPHON_GL_STENCIL_INDEX8;
        else _stencilBufferFormat = SYPHON_GL_STENCIL_INDEX16;
    }
    return self;
}

- (void)dealloc
{
    [self beginInContext];
    [self destroySizedResources];
    [self endInContext];
    CGLReleaseContext(_context);
}

- (CGLContextObj)context
{
    return _context;
}

- (GLuint)MSAASampleCount
{
    return _MSAASampleCount;
}

- (GLenum)depthBufferFormat
{
    return _depthBufferFormat;
}

- (GLenum)stencilBufferFormat
{
    return _stencilBufferFormat;
}

- (GLsizei)width
{
    return _width;
}

- (GLsizei)height
{
    return _height;
}

- (void)beginInContext
{

}

- (void)endInContext
{

}

- (BOOL)capabilitiesDidChange
{
    return NO;
}

- (void)destroySizedResources
{

}

- (SyphonOpenGLImage *)newImageForSurface:(IOSurfaceRef)surface
{
    return nil;
}

- (void)setupForBackingTexture:(GLuint)backing width:(GLsizei)width height:(GLsizei)height
{
    _width = width;
    _height = height;
}

- (void)bind
{
    
}

- (void)unbind
{

}

- (void)flush
{

}

- (void)drawFrameTexture:(GLuint)texID textureTarget:(GLenum)target imageRegion:(NSRect)region textureDimensions:(NSSize)size flipped:(BOOL)isFlipped
{

}

@end
