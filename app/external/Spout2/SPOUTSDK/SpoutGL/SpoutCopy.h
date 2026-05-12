/*

					SpoutCopy.h

		Functions to manage pixel buffer copying

	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	Copyright (c) 2016-2023, Lynn Jarvis. All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, 
	are permitted provided that the following conditions are met:

		1. Redistributions of source code must retain the above copyright notice, 
		   this list of conditions and the following disclaimer.

		2. Redistributions in binary form must reproduce the above copyright notice, 
		   this list of conditions and the following disclaimer in the documentation 
		   and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"	AND ANY 
	EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
	OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE	ARE DISCLAIMED. 
	IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
	INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
	PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
	LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/
#pragma once
#ifndef __spoutCopy__ // standard way as well
#define __spoutCopy__

#include "SpoutCommon.h"
#include <windows.h>
#include <stdio.h> // for debug printf
#include <gl/gl.h> // For OpenGL definitions
#include <intrin.h> // for cpuid (x86) / arch-specific intrinsics
#if defined(_M_IX86) || defined(_M_AMD64)
#  include <emmintrin.h> // for SSE2
#  include <tmmintrin.h> // for SSSE3
#  define SPOUT_HAS_SSE 1
#else
#  define SPOUT_HAS_SSE 0
#endif
#include <cmath> // For compatibility with Clang. PR#81
#include <stdint.h> // for _uint32 etc

class SPOUT_DLLEXP spoutCopy {

	public:

		spoutCopy();
		~spoutCopy();

		// Copy image pixels and select fastest method based on image width
		void CopyPixels(const unsigned char *src, unsigned char *dst,
						unsigned int width, unsigned int height, 
						GLenum glFormat = GL_RGBA, bool bInvert = false) const;

		// Flip a pixel buffer in place
		void FlipBuffer(const unsigned char *src, unsigned char *dst,
						unsigned int width, unsigned int height,
						GLenum glFormat = GL_RGBA) const;

		// Correct for image stride
		void RemovePadding(const unsigned char* source, unsigned char* dest,
			unsigned int width, unsigned int height,
			unsigned int source_stride, GLenum glFormat) const;

		// SSE2 version of memcpy
		void memcpy_sse2(void* dst, const void* src, size_t size) const;

		//
		// RGBA <> RGBA
		//

		// Copy rgba buffers line by line allowing for source pitch using the fastest method
		void rgba2rgba(const void* source, void* dest, unsigned int width, unsigned int height,
			unsigned int sourcePitch, bool bInvert = false) const;

		// Copy rgba buffers line by line allowing for source and destination line pitch
		void rgba2rgba(const void* source, void* dest, unsigned int width, unsigned int height,
			unsigned int sourcePitch, unsigned int destPitch, bool bInvert) const;

		// Copy rgba buffers of differing size
		void rgba2rgbaResample(const void* source, void* dest,
			unsigned int sourceWidth, unsigned int sourceHeight, unsigned int sourcePitch,
			unsigned int destWidth, unsigned int destHeight, bool bInvert = false) const;

		//
		// RGBA <> BGRA
		//

		// Copy rgba to bgra using the fastest method
		void rgba2bgra(const void* rgba_source, void* bgra_dest, unsigned int width, unsigned int height, bool bInvert = false) const;

		// Copy rgba to bgra line by line allowing for source pitch using the fastest method
		void rgba2bgra(const void* rgba_source, void* bgra_dest, unsigned int width, unsigned int height,
			unsigned int sourcePitch, bool bInvert = false) const;

		// Copy rgba to bgra line allowing for source and destination line pitch
		void rgba2bgra(const void* source, void* dest, unsigned int width, unsigned int height,
			unsigned int sourcePitch, unsigned int destPitch, bool bInvert) const;
		
		// Copy bgra to rgba
		void bgra2rgba(const void* bgra_source, void *rgba_dest, unsigned int width, unsigned int height, bool bInvert = false) const;
		
		//
		// RGBA <> RGB, RGBA <> BGR
		//

		// TODO : add RGBA pitch to all functions
		// TODO : avoid redundancy

		// Copy RGBA to RGB allowing for source line pitch
		void rgba2rgb (const void* rgba_source, void* rgb_dest, unsigned int width, unsigned int height,
			unsigned int sourcePitch, bool bInvert = false, bool bMirror = false, bool bSwapRB = false) const;

		// Copy RGBA to BGR allowing for source line pitch
		void rgba2bgr(const void* rgba_source, void* rgb_dest, unsigned int width, unsigned int height,
			unsigned int sourcePitch, bool bInvert = false) const;

		// Copy RGBA to RGB allowing for source and destination pitch
		void rgba2rgbResample(const void* source, void* dest,
			unsigned int sourceWidth, unsigned int sourceHeight, unsigned int sourcePitch,
			unsigned int destWidth, unsigned int destHeight,
			bool bInvert = false, bool bMirror = false, bool bSwapRB = false) const;

		// Copy RGBA to BGR allowing for source and destination pitch
		void rgba2bgrResample(const void* source, void* dest,
			unsigned int sourceWidth, unsigned int sourceHeight, unsigned int sourcePitch,
			unsigned int destWidth, unsigned int destHeight, bool bInvert = false) const;


		// Copy RGB to RGBA allowing for destination pitch
		void rgb2rgba (const void* rgb_source,  void *rgba_dest, unsigned int width, unsigned int height, bool bInvert = false) const;
		
		// Copy RGB to RGBA allowing for destination pitch
		void rgb2rgba(const void *rgb_source, void *rgba_dest,
			unsigned int width, unsigned int height,
			unsigned int dest_pitch, bool bInvert) const;

		// Copy BGR to RGBA
		void bgr2rgba (const void* bgr_source,  void *rgba_dest, unsigned int width, unsigned int height, bool bInvert = false) const;
		
		// Copy BGR to RGBA allowing for destination pitch
		void bgr2rgba(const void *rgb_source, void *rgba_dest,
			unsigned int width, unsigned int height,
			unsigned int dest_pitch, bool bInvert) const;

		//
		// RGB > BGRA
		//


		// Copy RGB to BGRA
		void rgb2bgra (const void* rgb_source,  void *bgra_dest, unsigned int width, unsigned int height, bool bInvert = false) const;
		
		// Copy RGB to BGRA allowing for destination pitch
		void rgb2bgra(const void *rgb_source, void *bgra_dest,
			unsigned int width, unsigned int height,
			unsigned int dest_pitch, bool bInvert) const;

		// Copy RGB to BGRX using SSE
		// Experimental SSE
		void rgb_to_bgrx_sse(unsigned int w, const void* in, void* out) const;

		// Copy BGR to BGRA
		void bgr2bgra (const void* bgr_source,  void *bgra_dest, unsigned int width, unsigned int height, bool bInvert = false) const;
		

		// Copy RGBA to BGR
		void rgba2bgr (const void* rgba_source, void *bgr_dest,  unsigned int width, unsigned int height, bool bInvert = false) const;
		
		// Copy BGRA to RGB
		void bgra2rgb (const void* bgra_source, void *rgb_dest,  unsigned int width, unsigned int height, bool bInvert = false) const;
		
		// Copy BGRA to BGR
		void bgra2bgr (const void* bgra_source, void *bgr_dest,  unsigned int width, unsigned int height, bool bInvert = false) const;



	protected :

		void CheckSSE();
		bool m_bSSE2;
		bool m_bSSE3;
		bool m_bSSSE3;

		void rgba_bgra(const void *rgba_source, void *bgra_dest, unsigned int width, unsigned int height, bool bInvert = false) const;
		void rgba_bgra_sse2(const void *rgba_source, void *bgra_dest, unsigned int width, unsigned int height, bool bInvert = false) const;
		void rgba_bgra_sse3(const void *rgba_source, void *bgra_dest, unsigned int width, unsigned int height, bool bInvert = false) const;

};

#endif
