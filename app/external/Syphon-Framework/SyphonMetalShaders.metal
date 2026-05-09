/*
 SyphonMetalShaders.metal
 Syphon
 
 Copyright 2020-2023 Maxime Touroute & Philippe Chaurand (www.millumin.com),
 bangnoise (Tom Butterworth) & vade (Anton Marini). All rights reserved.
 
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

#include <metal_stdlib>
#include <simd/simd.h>
#include "SyphonServerMetalTypes.h"

using namespace metal;

typedef struct
{
    float4 clipSpacePosition [[position]];
    float2 textureCoordinate;
} RasterizerData;

vertex RasterizerData textureToScreenVertexShader(uint vertexID [[ vertex_id ]],
                                                  constant SYPHONTextureVertex *vertexArray [[ buffer(SYPHONVertexInputIndexVertices) ]],
                                                  constant vector_uint2 *viewportSizePointer  [[ buffer(SYPHONVertexInputIndexViewportSize) ]])
{
    RasterizerData out;
    float2 pixelSpacePosition = vertexArray[vertexID].position.xy;
    float2 viewportSize = float2(*viewportSizePointer);
    out.clipSpacePosition.xy = pixelSpacePosition / (viewportSize / 2.0);
    out.clipSpacePosition.z = 0.0;
    out.clipSpacePosition.w = 1.0;
    out.textureCoordinate = vertexArray[vertexID].textureCoordinate;
    return out;
}

fragment float4 textureToScreenSamplingShader(RasterizerData in [[stage_in]],
                                              texture2d<half> colorTexture [[ texture(SYPHONTextureIndexZero) ]])
{
    constexpr sampler textureSampler (mag_filter::nearest, min_filter::nearest);
    const half4 colorSample = colorTexture.sample(textureSampler, in.textureCoordinate);
    return float4(colorSample);
}
