/*
 SyphonGLVertices.m
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

#import "SyphonGLVertices.h"
#import <OpenGL/gl3.h>

@implementation SyphonGLVertices
{
@private
    GLuint _vao;
    GLuint _vbo;
#ifdef SYPHON_CORE_RESTORE
    GLint _prevVAO;
#endif
}

- (instancetype)init
{
    self = [super init];
    if (self)
    {
        glGenVertexArrays(1, &_vao);
        glGenBuffers(1, &_vbo);
    }
    return self;
}

- (void)dealloc
{
    if (_vao)
    {
        glDeleteVertexArrays(1, &_vao);
    }
    if (_vbo)
    {
        glDeleteBuffers(1, &_vbo);
    }
}

- (void)setFloats:(GLfloat *)data count:(GLsizei)count
{
#ifdef SYPHON_CORE_RESTORE
    GLint prevVBO;
    glGetIntegerv(GL_ARRAY_BUFFER_BINDING, &prevVBO);
#endif
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, count * sizeof(GLfloat), data, GL_STATIC_DRAW);    
#ifdef SYPHON_CORE_RESTORE
    glBindBuffer(GL_ARRAY_BUFFER, prevVBO);
#else
    glBindBuffer(GL_ARRAY_BUFFER, 0);
#endif
}

- (void)setAttributePointer:(GLint)index components:(GLsizei)components stride:(GLsizei)stride offset:(GLsizei)offset
{
#ifdef SYPHON_CORE_RESTORE
    GLint prevVBO;
    glGetIntegerv(GL_ARRAY_BUFFER_BINDING, &prevVBO);
#endif
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glEnableVertexAttribArray(index);
    glVertexAttribPointer(index, components, GL_FLOAT, GL_FALSE, stride * sizeof(GLfloat), (GLvoid *)(offset * sizeof(GLfloat)));
#ifdef SYPHON_CORE_RESTORE
    glBindBuffer(GL_ARRAY_BUFFER, prevVBO);
#else
    glBindBuffer(GL_ARRAY_BUFFER, 0);
#endif
}

- (void)bind
{
#ifdef SYPHON_CORE_RESTORE
    glGetIntegerv(GL_VERTEX_ARRAY_BINDING, &_prevVAO);
#endif
    glBindVertexArray(_vao);
}

- (void)unbind
{
#ifdef SYPHON_CORE_RESTORE
    glBindVertexArray(_prevVAO);
#else
    glBindVertexArray(0);
#endif
}
@end
