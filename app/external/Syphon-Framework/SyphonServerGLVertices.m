/*
 SyphonServerGLVertices.h
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

#import "SyphonServerGLVertices.h"

@implementation SyphonServerGLVertices
{
@private
    GLfloat _vertices[16];
}

- (void)setRegionX:(GLfloat)x Y:(GLfloat)y width:(GLfloat)width height:(GLfloat)height flipped:(BOOL)isFlipped
{
    GLfloat region[4] = {
        x,
        isFlipped ? height : y,
        width,
        isFlipped ? y : height
    };
    if (_vertices[2] != region[0] || _vertices[3] != region[1] || _vertices[10] != region[2] || _vertices[7] != region[3])
    {
        _vertices[0] = _vertices[1] = _vertices[4]  = _vertices[9] = -1.0;
        _vertices[5] = _vertices[8] = _vertices[12] = _vertices[13] = 1.0;
        _vertices[2]  = _vertices[6]  = region[0];
        _vertices[3]  = _vertices[11] = region[1];
        _vertices[10] = _vertices[14] = region[2];
        _vertices[7]  = _vertices[15] = region[3];
        [self setFloats:_vertices count:16];
    }
}

@end
