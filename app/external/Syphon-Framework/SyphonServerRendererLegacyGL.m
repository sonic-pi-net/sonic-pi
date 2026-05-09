/*
 SyphonServerRendererLegacyGL.m
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

#import "SyphonServerRendererLegacyGL.h"
#import "SyphonIOSurfaceImageLegacy.h"
#import "SyphonOpenGLFunctions.h"
#import <OpenGL/CGLMacro.h>

@implementation SyphonServerRendererLegacyGL
{
@private
    CGLContextObj cgl_ctx;
    BOOL    _combinedDepthStencil;
    GLuint  _depthBuffer;
    GLuint  _stencilBuffer;
    GLuint  _surfaceFBO;
    GLuint  _msaaFBO;
    GLuint  _msaaColorBuffer;
    GLint   _previousReadFBO;
    GLint   _previousDrawFBO;
    GLint   _previousFBO;
    GLuint  _actualMSAASampleCount;
}

- (id)initWithContext:(CGLContextObj)context MSAASampleCount:(GLuint)msc depthBufferResolution:(GLuint)dbr stencilBufferResolution:(GLuint)sbr
{
    self = [super initWithContext:context MSAASampleCount:msc depthBufferResolution:dbr stencilBufferResolution:sbr];
    if (self)
    {
        cgl_ctx = CGLRetainContext(context);
    }
    return self;
}

- (void)dealloc
{
    if (cgl_ctx)
    {
        CGLReleaseContext(cgl_ctx);
    }
}

- (BOOL)capabilitiesDidChange
{
    GLuint newMSAASampleCount = 0;
    BOOL newCombinedDepthStencil = NO;
    BOOL didChange = NO;

    if (self.MSAASampleCount != 0
        && SyphonOpenGLContextSupportsExtension(cgl_ctx, "GL_EXT_framebuffer_multisample"))
    {
            newMSAASampleCount = self.MSAASampleCount;

            GLint maxSamples;
            glGetIntegerv(GL_MAX_SAMPLES_EXT, &maxSamples);

            if (newMSAASampleCount > maxSamples) newMSAASampleCount = maxSamples;
    }
    if (newMSAASampleCount != _actualMSAASampleCount)
    {
        didChange = YES;
        _actualMSAASampleCount = newMSAASampleCount;
    }

    /*
     No current cards support FBOs with seperate depth and stencil buffers, so if both are
     requested, we have to use GL_DEPTH24_STENCIL8.
     If any stencil buffer is requested at all, we also have to use a combi buffer.
     The exception is the software renderer under 10.6, which only works with distinct
     depth and stencil buffers and does not support GL_EXT_packed_depth_stencil.
     */
    if (self.stencilBufferFormat != 0
        && SyphonOpenGLContextSupportsExtension(cgl_ctx, "GL_EXT_packed_depth_stencil"))
    {
        newCombinedDepthStencil = YES;
    }
    if (newCombinedDepthStencil != _combinedDepthStencil)
    {
        didChange = YES;
        _combinedDepthStencil = newCombinedDepthStencil;
    }
    return didChange;
}

- (void)destroySizedResources
{
    if(_msaaFBO != 0)
    {
        glDeleteFramebuffersEXT(1, &_msaaFBO);
        _msaaFBO = 0;
    }

    if(_msaaColorBuffer != 0)
    {
        glDeleteRenderbuffersEXT(1, &_msaaColorBuffer);
        _msaaColorBuffer = 0;
    }

    if(_depthBuffer != 0)
    {
        glDeleteRenderbuffersEXT(1, &_depthBuffer);
        _depthBuffer = 0;
    }

    if (_stencilBuffer != 0)
    {
        glDeleteRenderbuffersEXT(1, &_stencilBuffer);
        _stencilBuffer = 0;
    }

    if (_surfaceFBO != 0)
    {
        glDeleteFramebuffersEXT(1, &_surfaceFBO);
        _surfaceFBO = 0;
    }
    [super destroySizedResources];
}

- (SyphonOpenGLImage *)newImageForSurface:(IOSurfaceRef)surface
{
    return [[SyphonIOSurfaceImageLegacy alloc] initWithSurface:surface forContext:cgl_ctx];
}

- (void)setupForBackingTexture:(GLuint)backing width:(GLsizei)width height:(GLsizei)height
{
    [super setupForBackingTexture:backing width:width height:height];

    // save state
    GLint previousRBO;
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glGetIntegerv(GL_FRAMEBUFFER_BINDING_EXT, &_previousFBO);
    glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING_EXT, &_previousReadFBO);
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING_EXT, &_previousDrawFBO);
    glGetIntegerv(GL_RENDERBUFFER_BINDING_EXT, &previousRBO);

    // no error
    GLenum status;

    if (_combinedDepthStencil == YES)
    {
        _depthBuffer = [self newRenderbufferForInternalFormat:GL_DEPTH24_STENCIL8_EXT];
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
        glGenFramebuffersEXT(1, &_msaaFBO);
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, _msaaFBO);
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_RENDERBUFFER_EXT, _msaaColorBuffer);
        if (_combinedDepthStencil)
        {
            glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER_EXT, _depthBuffer);
        }
        else
        {
            if (_depthBuffer != 0)
            {
                glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, _depthBuffer);
            }
            if (_stencilBuffer != 0)
            {
                glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, _stencilBuffer);
            }
        }

        status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
        if(status != GL_FRAMEBUFFER_COMPLETE_EXT)
        {
            SYPHONLOG(@"SyphonServer: Cannot create MSAA FBO (OpenGL Error %04X), falling back to non-antialiased FBO", status);

            glDeleteFramebuffersEXT(1, &_msaaFBO);
            _msaaFBO = 0;

            glDeleteRenderbuffersEXT(1, &_msaaColorBuffer);
            _msaaColorBuffer = 0;

            _actualMSAASampleCount = 0;
        }
    }

    glGenFramebuffersEXT(1, &_surfaceFBO);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, _surfaceFBO);
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_RECTANGLE_EXT, backing, 0);
    if (_actualMSAASampleCount == 0)
    {
        // If we're not doing MSAA, attach depth and stencil buffers to our FBO
        if (_combinedDepthStencil)
        {
            glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER_EXT, _depthBuffer);
        }
        else
        {
            if (_depthBuffer != 0)
            {
                glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, _depthBuffer);
            }
            if (_stencilBuffer != 0)
            {
                glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, _stencilBuffer);
            }
        }
    }

    status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);

    if(status != GL_FRAMEBUFFER_COMPLETE_EXT)
    {
        SYPHONLOG(@"SyphonServer: Cannot create FBO (OpenGL Error %04X)", status);
        [self destroySizedResources];
    }

    // restore state
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, previousRBO);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, _previousFBO);	
    glBindFramebufferEXT(GL_READ_FRAMEBUFFER_EXT, _previousReadFBO);
    glBindFramebufferEXT(GL_DRAW_FRAMEBUFFER_EXT, _previousDrawFBO);
    glPopAttrib();
}

- (GLuint)newRenderbufferForInternalFormat:(GLenum)format
{
    GLuint buffer;
    glGenRenderbuffersEXT(1, &buffer);
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, buffer);
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
        glRenderbufferStorageMultisampleEXT(GL_RENDERBUFFER_EXT,
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
    glGetIntegerv(GL_FRAMEBUFFER_BINDING_EXT, &_previousFBO);
    glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING_EXT, &_previousReadFBO);
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING_EXT, &_previousDrawFBO);

    if(self.MSAASampleCount)
    {
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, _msaaFBO);
    }
    else
    {
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, _surfaceFBO);
    }
}

- (void)unbind
{
    // we now have to blit from our MSAA to our IOSurface normal texture
    if(self.MSAASampleCount)
    {
        glBindFramebufferEXT(GL_READ_FRAMEBUFFER_EXT, _msaaFBO);
        glBindFramebufferEXT(GL_DRAW_FRAMEBUFFER_EXT, _surfaceFBO);

        // blit the whole extent from read to draw
        glBlitFramebufferEXT(0, 0, self.width, self.height, 0, 0, self.width, self.height, GL_COLOR_BUFFER_BIT, GL_NEAREST);
    }

    // flush to make sure IOSurface updates are seen globally.
    glFlushRenderAPPLE();

    // restore state
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, _previousFBO);
    glBindFramebufferEXT(GL_READ_FRAMEBUFFER_EXT, _previousReadFBO);
    glBindFramebufferEXT(GL_DRAW_FRAMEBUFFER_EXT, _previousDrawFBO);
}

- (void)flush
{
    glFlush();
}

- (void)drawFrameTexture:(GLuint)texID textureTarget:(GLenum)target imageRegion:(NSRect)region textureDimensions:(NSSize)size flipped:(BOOL)isFlipped
{
    // render to our FBO with an IOSurface backed texture attachment (whew!)

    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);
    // Setup OpenGL states
    glViewport(0, 0, self.width,  self.height);

    // We need to ensure we set this before changing our texture matrix
    glActiveTexture(GL_TEXTURE0);
    // ensure we act on the proper client texture as well
    glClientActiveTexture(GL_TEXTURE0);

    glMatrixMode(GL_TEXTURE);
    glPushMatrix();
    glLoadIdentity();

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0, self.width, 0, self.height, -1, 1);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();

    // dont bother clearing. we dont have any alpha so we just write over the buffer contents. saves us a write.
    // via GL_REPLACE TEX_ENV
    glEnable(target);
    glBindTexture(target, texID);

    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glColor4f(1.0, 1.0, 1.0, 1.0);

    // why do we need it ?
    glDisable(GL_BLEND);

    GLfloat tex_coords[8];

    GLfloat texOriginX = region.origin.x;
    GLfloat texOriginY = region.origin.y;
    GLfloat texExtentX = region.size.width + region.origin.x;
    GLfloat texExtentY = region.size.height + region.origin.y;

    if(target == GL_TEXTURE_2D)
    {
        texOriginX /= size.width;
        texOriginY /= size.height;
        texExtentX /= size.width;
        texExtentY /= size.height;
    }

    // X
    tex_coords[0] = texOriginX;
    tex_coords[2] = texOriginX;
    tex_coords[4] = texExtentX;
    tex_coords[6] = texExtentX;

    // Y
    if(!isFlipped)
    {
        tex_coords[1] = texOriginY;
        tex_coords[3] = texExtentY;
        tex_coords[5] = texExtentY;
        tex_coords[7] = texOriginY;
    }
    else
    {
        tex_coords[1] = texExtentY;
        tex_coords[3] = texOriginY;
        tex_coords[5] = texOriginY;
        tex_coords[7] = texExtentY;
    }

    GLfloat verts[] =
    {
        0.0f, 0.0f,
        0.0f, self.height,
        self.width, self.height,
        self.width, 0.0f,
    };

    // Ought to cache the GL_ARRAY_BUFFER_BINDING, GL_ELEMENT_ARRAY_BUFFER_BINDING, set buffer to 0, and reset
    GLint arrayBuffer, elementArrayBuffer;
    glGetIntegerv(GL_ELEMENT_ARRAY_BUFFER_BINDING, &elementArrayBuffer);
    glGetIntegerv(GL_ARRAY_BUFFER_BINDING, &arrayBuffer);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glEnableClientState( GL_TEXTURE_COORD_ARRAY );
    glTexCoordPointer(2, GL_FLOAT, 0, tex_coords );
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, verts );
    glDrawArrays( GL_TRIANGLE_FAN, 0, 4 );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementArrayBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, arrayBuffer);

    glBindTexture(target, 0);

    // Restore OpenGL states
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();

    glMatrixMode(GL_PROJECTION);
    glPopMatrix();


    glMatrixMode(GL_TEXTURE);
    glPopMatrix();

    glPopClientAttrib();
    glPopAttrib();
}

@end
