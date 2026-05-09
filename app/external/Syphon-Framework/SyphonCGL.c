/*
 SyphonCGL.c
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

#include "SyphonCGL.h"
#include <stddef.h>

bool SyphonOpenGLContextIsLegacy(CGLContextObj context)
{
    CGLPixelFormatObj format = CGLGetPixelFormat(context);
    GLint profile;
    CGLDescribePixelFormat(format, 0, kCGLPFAOpenGLProfile, &profile);
    if (profile == kCGLOGLPVersion_Legacy)
        return true;
    return false;
}

#ifdef SYPHON_CORE_SHARE

CGLContextObj SyphonOpenGLCreateSharedContext(CGLContextObj context)
{
    CGLPixelFormatObj format = CGLGetPixelFormat(context);
    CGLContextObj result;
    CGLError error = CGLCreateContext(format, context, &result);
    if (error == kCGLNoError)
    {
        return result;
    }
    return NULL;
}

#endif
