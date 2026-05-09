/*
 SyphonServerRendererCoreGL.m
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

#import "SyphonServerRendererCoreGL.h"
#import "SyphonIOSurfaceImageCore.h"
#import "SyphonServerGLShader.h"
#import "SyphonServerGLVertices.h"
#import <OpenGL/gl3.h>
#import <OpenGL/gl3ext.h> // For glFlushRendererAPPLE()

@implementation SyphonServerRendererCoreGL
{
@private
    GLuint  _depthBuffer;
    GLuint  _stencilBuffer;
    GLuint  _surfaceFBO;
    GLuint  _msaaFBO;
    GLuint  _msaaColorBuffer;
    GLuint  _actualMSAASampleCount;
    CGLContextObj   _prevContext;
    SyphonServerGLShader  *_shader;
    SyphonServerGLVertices *_vertices;
#ifdef SYPHON_CORE_RESTORE
    GLint _previousReadFBO;
    GLint _previousDrawFBO;
#endif
}

- (id)initWithContext:(CGLContextObj)context MSAASampleCount:(GLuint)msc depthBufferResolution:(GLuint)dbr stencilBufferResolution:(GLuint)sbr
{
    self = [super initWithContext:context MSAASampleCount:msc depthBufferResolution:dbr stencilBufferResolution:sbr];
    if (self)
    {
        // TODO: delete this logging
#ifdef SYPHON_CORE_SHARE
#ifdef SYPHON_CORE_RESTORE
#error SYPHON_CORE_SHARE and SYPHON_CORE_RESTORE shouldn't both be defined
#endif
        SYPHONLOG(@"SYPHON_CORE_SHARE");
#elif defined(SYPHON_CORE_RESTORE)
        SYPHONLOG(@"SYPHON_CORE_RESTORE");
#else
        SYPHONLOG(@"Neither SYPHON_CORE_RESTORE nor SYPHON_CORE_SHARE are in use");
#endif
    }
    return self;
}


- (void)beginInContext
{
    assert(!_prevContext);
    _prevContext = CGLGetCurrentContext();
    if (_prevContext)
    {
        CGLRetainContext(_prevContext);
    }
    if (_prevContext != self.context)
    {
        CGLSetCurrentContext(self.context);
    }
}

- (void)endInContext
{
    if (_prevContext != self.context)
    {
        CGLSetCurrentContext(_prevContext);
    }
    if (_prevContext)
    {
        CGLReleaseContext(_prevContext);
        _prevContext = NULL;
    }
}

- (BOOL)capabilitiesDidChange
{
    GLuint newMSAASampleCount = 0;
    BOOL didChange = NO;

    if (self.MSAASampleCount != 0)
    {
        newMSAASampleCount = self.MSAASampleCount;

        GLint maxSamples;
        glGetIntegerv(GL_MAX_SAMPLES, &maxSamples);

        if (newMSAASampleCount > maxSamples) newMSAASampleCount = maxSamples;
    }
    if (newMSAASampleCount != _actualMSAASampleCount)
    {
        didChange = YES;
        _actualMSAASampleCount = newMSAASampleCount;
    }

    return didChange;
}

- (void)destroySizedResources
{
    if(_msaaFBO != 0)
    {
        glDeleteFramebuffers(1, &_msaaFBO);
        _msaaFBO = 0;
    }

    if(_msaaColorBuffer != 0)
    {
        glDeleteRenderbuffers(1, &_msaaColorBuffer);
        _msaaColorBuffer = 0;
    }

    if(_depthBuffer != 0)
    {
        glDeleteRenderbuffers(1, &_depthBuffer);
        _depthBuffer = 0;
    }

    if (_stencilBuffer != 0)
    {
        glDeleteRenderbuffers(1, &_stencilBuffer);
        _stencilBuffer = 0;
    }

    if (_surfaceFBO != 0)
    {
        glDeleteFramebuffers(1, &_surfaceFBO);
        _surfaceFBO = 0;
    }
    [super destroySizedResources];
}

- (SyphonOpenGLImage *)newImageForSurface:(IOSurfaceRef)surface
{
    return [[SyphonIOSurfaceImageCore alloc] initWithSurface:surface forContext:self.context];
}

- (void)setupForBackingTexture:(GLuint)backing width:(GLsizei)width height:(GLsizei)height
{
    [super setupForBackingTexture:backing width:width height:height];
#ifdef SYPHON_CORE_SHARE
    // We are the only user of the context, so needn't set it every frame
    glViewport(0, 0, width, height);
#endif
#ifdef SYPHON_CORE_RESTORE
    // save state
    GLint previousReadFBO;
    GLint previousDrawFBO;
    GLint previousRBO;
    // TODO: capture other state we change
    glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &previousReadFBO);
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &previousDrawFBO);
    glGetIntegerv(GL_RENDERBUFFER_BINDING, &previousRBO);
#endif
    // no error
    GLenum status;
    // If a stencil buffer is requested, always use a combined attachment - support for distinct attachments
    // is optional
    BOOL combineDepthStencil = self.stencilBufferFormat != 0 ? YES : NO;
    if (combineDepthStencil)
    {
        GLenum format = self.depthBufferFormat == GL_DEPTH_COMPONENT32 ? GL_DEPTH32F_STENCIL8 : GL_DEPTH24_STENCIL8;
        _depthBuffer = [self newRenderbufferForInternalFormat:format];
    }
    else
    {
        if (self.depthBufferFormat != 0)
        {
            _depthBuffer = [self newRenderbufferForInternalFormat:self.depthBufferFormat];
        }

        if (self.stencilBufferFormat != 0)
        {
            _stencilBuffer = [self newRenderbufferForInternalFormat:self.stencilBufferFormat];
        }
    }

    if(self.MSAASampleCount > 0)
    {
        // Color MSAA Attachment
        _msaaColorBuffer = [self newRenderbufferForInternalFormat:GL_RGBA];

        // attach color, depth and stencil to our MSAA FBO
        glGenFramebuffers(1, &_msaaFBO);
        glBindFramebuffer(GL_FRAMEBUFFER, _msaaFBO);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, _msaaColorBuffer);
        if (combineDepthStencil)
        {
            glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _depthBuffer);
        }
        else
        {
            if (_depthBuffer != 0)
            {
                glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depthBuffer);
            }
            if (_stencilBuffer != 0)
            {
                glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _stencilBuffer);
            }
        }

        status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        if(status != GL_FRAMEBUFFER_COMPLETE)
        {
            SYPHONLOG(@"SyphonServer: Cannot create MSAA FBO (OpenGL Error %04X), falling back to non-antialiased FBO", status);

            glDeleteFramebuffers(1, &_msaaFBO);
            _msaaFBO = 0;

            glDeleteFramebuffers(1, &_msaaColorBuffer);
            _msaaColorBuffer = 0;

            _actualMSAASampleCount = 0;
        }
    }

    glGenFramebuffers(1, &_surfaceFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, _surfaceFBO);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_RECTANGLE, backing, 0);
    if (_actualMSAASampleCount == 0)
    {
        // If we're not doing MSAA, attach depth and stencil buffers to our FBO
        if (combineDepthStencil)
        {
            glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _depthBuffer);
        }
        else
        {
            if (_depthBuffer != 0)
            {
                glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depthBuffer);
            }
            if (_stencilBuffer != 0)
            {
                glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _stencilBuffer);
            }
        }
    }

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);

    if(status != GL_FRAMEBUFFER_COMPLETE)
    {
        SYPHONLOG(@"SyphonServer: Cannot create FBO (OpenGL Error %04X)", status);
        [self destroySizedResources];
    }
#ifdef SYPHON_CORE_RESTORE
    // restore state
    glBindRenderbuffer(GL_RENDERBUFFER, previousRBO);
    glBindFramebuffer(GL_READ_FRAMEBUFFER, previousReadFBO);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, previousDrawFBO);
    // TODO: restore other saved state
#else
    glBindRenderbuffer(GL_RENDERBUFFER, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
#endif
}

- (GLuint)newRenderbufferForInternalFormat:(GLenum)format
{
    GLuint buffer;
    glGenRenderbuffers(1, &buffer);
    glBindRenderbuffer(GL_RENDERBUFFER, buffer);
    GLenum error = GL_NO_ERROR;
    do {
        // Most cards won't complain as long as the sample count is not more than the maximum they support, but the spec allows
        // them to emit a GL_OUT_OF_MEMORY error if they don't support a particular sample count, so we check for that and attempt
        // to recover by trying a smaller count
        if (error == GL_OUT_OF_MEMORY)
        {
            _actualMSAASampleCount--;
            SYPHONLOG(@"SyphonServer: reducing MSAA sample count due to GL_OUT_OF_MEMORY (now %u)", _actualMSAASampleCount);
        }
        glRenderbufferStorageMultisample(GL_RENDERBUFFER,
                                         _actualMSAASampleCount,
                                         format,
                                         self.width,
                                         self.height);
        error = glGetError();
    } while (error == GL_OUT_OF_MEMORY && _actualMSAASampleCount > 0);
    return buffer;
}

- (void)bind
{
#ifdef SYPHON_CORE_RESTORE
    glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &_previousReadFBO);
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &_previousDrawFBO);
#endif

    if(self.MSAASampleCount)
    {
        glBindFramebuffer(GL_FRAMEBUFFER, _msaaFBO);
    }
    else
    {
        glBindFramebuffer(GL_FRAMEBUFFER, _surfaceFBO);
    }
}

- (void)unbind
{
    // we now have to blit from our MSAA to our IOSurface normal texture
    if(self.MSAASampleCount)
    {
        glBindFramebuffer(GL_READ_FRAMEBUFFER, _msaaFBO);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _surfaceFBO);

        // blit the whole extent from read to draw
        glBlitFramebuffer(0, 0, self.width, self.height, 0, 0, self.width, self.height, GL_COLOR_BUFFER_BIT, GL_NEAREST);
    }

    // flush to make sure IOSurface updates are seen globally.
    glFlushRenderAPPLE();

#ifdef SYPHON_CORE_RESTORE
    // restore state
    glBindFramebuffer(GL_READ_FRAMEBUFFER, _previousReadFBO);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _previousDrawFBO);
#else
    // restore default FBO
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
#endif
}

- (void)flush
{
    glFlush();
}


- (void)drawFrameTexture:(GLuint)texID textureTarget:(GLenum)target imageRegion:(NSRect)region textureDimensions:(NSSize)size flipped:(BOOL)isFlipped
{
    if (_vertices == nil)
    {
        _vertices = [[SyphonServerGLVertices alloc] init];
    }

    if (target != _shader.target)
    {
        _shader = [[SyphonServerGLShader alloc] initForTextureTarget:target];
        [_shader useProgram];
        [_vertices bind];
        GLint vertLoc = _shader.vertexAttribLocation;
        GLint texVertLoc = _shader.textureVertexAttribLocation;
        [_vertices setAttributePointer:vertLoc components:2 stride:4 offset:0];
        [_vertices setAttributePointer:texVertLoc components:2 stride:4 offset:2];
        [_shader endProgram];
        [_vertices unbind];
    }

    GLfloat rx = region.origin.x;
    GLfloat ry = region.origin.y;
    GLfloat rw = region.size.width;
    GLfloat rh = region.size.height;

    if (target == GL_TEXTURE_2D)
    {
        rx /= size.width;
        ry /= size.height;
        rw /= size.width;
        rh /= size.height;
    }

    [_vertices setRegionX:rx
                        Y:ry
                    width:rw
                   height:rh
                  flipped:isFlipped];

    // render to our FBO with an IOSurface backed texture attachment (whew!)
#ifdef SYPHON_CORE_RESTORE
    // TODO: preserve state
    GLint prev;
    if(target == GL_TEXTURE_RECTANGLE)
        glGetIntegerv(GL_TEXTURE_BINDING_RECTANGLE, &prev);
    else
        glGetIntegerv(GL_TEXTURE_BINDING_2D, &prev);
    
#endif
    // Setup OpenGL states
#ifndef SYPHON_CORE_SHARE
    glViewport(0, 0, self.width, self.height);
    // We need to ensure we set this before changing our texture matrix
    glActiveTexture(GL_TEXTURE0);

    // why do we need it ?
    glDisable(GL_BLEND);
#endif
    // dont bother clearing. we dont have any alpha so we just write over the buffer contents. saves us a write.
    glBindTexture(target, texID);

    [_shader useProgram];
    [_vertices bind];
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    [_vertices unbind];
    [_shader endProgram];

#ifdef SYPHON_CORE_RESTORE
    // TODO: restore state
    glBindTexture(target, prev);
#else
    glBindTexture(target, 0);
#endif
}

@end
