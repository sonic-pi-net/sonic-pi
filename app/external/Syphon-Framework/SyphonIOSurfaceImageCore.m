/*
 SyphonIOSurfaceImageCore.m
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
#import "SyphonIOSurfaceImageCore.h"
#import <OpenGL/gl3.h>

@implementation SyphonIOSurfaceImageCore
{
@private
    CGLContextObj cgl_ctx;
    GLuint _texture;
}

- (id)initWithSurface:(IOSurfaceRef)surface forContext:(CGLContextObj)context
{
    self = [super initWithSurface:surface];
    if (self)
    {
        if (!context)
        {
            return nil;
        }

        cgl_ctx = CGLRetainContext(context);

        CGLContextObj previous = CGLGetCurrentContext();
        if (previous != context)
        {
            CGLSetCurrentContext(context);
        }

        // create the surface backed texture
        glGenTextures(1, &_texture);
#ifdef SYPHON_CORE_RESTORE
        GLint prev;
        glGetIntegerv(GL_TEXTURE_BINDING_RECTANGLE, &prev);
#endif
        glBindTexture(GL_TEXTURE_RECTANGLE, _texture);

        NSSize size = self.textureSize;

        CGLError err = CGLTexImageIOSurface2D(cgl_ctx, GL_TEXTURE_RECTANGLE, GL_RGBA8, size.width, size.height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, surface, 0);
#ifdef SYPHON_CORE_RESTORE
        glBindTexture(GL_TEXTURE_RECTANGLE, prev);
#else
        glBindTexture(GL_TEXTURE_RECTANGLE, 0);
#endif
        if (previous != context)
        {
            CGLSetCurrentContext(previous);
        }

        if(err != kCGLNoError)
        {
            SYPHONLOG(@"Error creating IOSurface texture: %s & %x", CGLErrorString(err), glGetError());
            return nil;
        }
    }
    return self;
}

- (void)dealloc
{
    if (_texture != 0)
    {
        CGLContextObj previous = CGLGetCurrentContext();
        if (previous != cgl_ctx)
        {
            CGLSetCurrentContext(cgl_ctx);
        }
        glDeleteTextures(1, &_texture);
        if (previous != cgl_ctx)
        {
            CGLSetCurrentContext(previous);
        }
    }
    if (cgl_ctx) CGLReleaseContext(cgl_ctx);
}

- (GLuint)textureName
{
    return _texture;
}
@end
