/*
 SyphonServerGLShader.m
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
#import "SyphonServerGLShader.h"
#import <OpenGL/gl3.h>

static NSString * const vertSource = @"#version 150\n\
in vec2 vertCoord;\
in vec2 texCoord;\
out vec2 fragTexCoord;\
void main() {\
    fragTexCoord = texCoord;\
    gl_Position = vec4(vertCoord, 1.0, 1.0);\
}";

static NSString * const frag2DSource = @"#version 150\n\
uniform sampler2D tex;\
in vec2 fragTexCoord;\
out vec4 color;\
void main() {\
    color = texture(tex, fragTexCoord);\
}";

static NSString * const fragRectSource = @"#version 150\n\
uniform sampler2DRect tex;\
in vec2 fragTexCoord;\
out vec4 color;\
void main() {\
    color = texture(tex, fragTexCoord);\
}";

@implementation SyphonServerGLShader
{
@private
    GLenum _target;
    GLint  _vertexAttrib;
    GLint  _textureVertexAttrib;
}

- (instancetype)initForTextureTarget:(GLenum)target
{
    NSString *fragSource;
    switch (target) {
        case GL_TEXTURE_2D:
            fragSource = frag2DSource;
            break;
        case GL_TEXTURE_RECTANGLE:
            fragSource = fragRectSource;
            break;
        default:
            fragSource = nil;
            break;
    }
    self = [super initWithVertexShader:vertSource fragmentShader:fragSource];
    if (self)
    {
        _target = target;
        // No need to set tex uniform location, it's default 0
        _vertexAttrib = [self getAttributeLocation:@"vertCoord"];
        _textureVertexAttrib = [self getAttributeLocation:@"texCoord"];
    }
    return self;
}

- (GLenum)target
{
    return _target;
}

- (GLint)vertexAttribLocation
{
    return _vertexAttrib;
}

- (GLint)textureVertexAttribLocation
{
    return _textureVertexAttrib;
}

@end
