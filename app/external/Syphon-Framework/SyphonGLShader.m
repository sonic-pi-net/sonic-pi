/*
 SyphonGLShader.m
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

#import "SyphonGLShader.h"
#import <OpenGL/gl3.h>

@implementation SyphonGLShader
{
@private
    GLuint _program;
#ifdef SYPHON_CORE_RESTORE
    GLint _prev;
#endif
}

+ (GLuint)createShader:(NSString *)source ofType:(GLenum)type
{
    const GLchar *cSource = [source cStringUsingEncoding:NSASCIIStringEncoding];

    GLuint shader = glCreateShader(type);

    glShaderSource(shader, 1, &cSource, NULL);
    glCompileShader(shader);

    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);

    if (status == GL_FALSE)
    {
        glDeleteShader(shader);
        shader = 0;
    }
    return shader;
}

- (instancetype)initWithVertexShader:(NSString *)vert fragmentShader:(NSString *)frag
{
    self = [super init];
    if (self)
    {
        GLint status = GL_TRUE;

        GLuint vertShader = [[self class] createShader:vert ofType:GL_VERTEX_SHADER];
        GLuint fragShader = [[self class] createShader:frag ofType:GL_FRAGMENT_SHADER];

        if (vertShader && fragShader)
        {
            _program = glCreateProgram();
            glAttachShader(_program, vertShader);
            glAttachShader(_program, fragShader);

            glLinkProgram(_program);
            glGetProgramiv(_program, GL_LINK_STATUS, &status);
        }
        else
        {
            status = GL_FALSE;
        }

        if (vertShader)
        {
            glDeleteShader(vertShader);
        }
        
        if (fragShader)
        {
            glDeleteShader(fragShader);
        }

        if (status == GL_FALSE)
        {
            return nil;
        }
    }
    return self;
}

- (void)dealloc
{
    if (_program)
    {
        glDeleteProgram(_program);
    }
}

- (void)useProgram
{
#ifdef SYPHON_CORE_RESTORE
    glGetIntegerv(GL_CURRENT_PROGRAM, &_prev);
#endif
    glUseProgram(_program);
}

- (void)endProgram
{
#ifdef SYPHON_CORE_RESTORE
    glUseProgram(_prev);
#else
    glUseProgram(0);
#endif

}

- (GLint)getAttributeLocation:(NSString *)name
{
    return glGetAttribLocation(_program, [name cStringUsingEncoding:NSASCIIStringEncoding]);
}

@end
