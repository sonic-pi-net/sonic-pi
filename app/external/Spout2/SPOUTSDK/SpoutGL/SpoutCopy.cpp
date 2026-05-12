/*

	Functions to manage pixel data copy

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
	========================

	20.08.16 - start class
	23.08.16 - finished first version
	10.10.16 - Added SSE3 detection
			   Optimized SSE2 rgba-bgra function
			   Revise rgb2rgba etc.
	11.10.16 - Added SSSE detection and rgba-bgra function
	04.01.17 - Added rgb2bgra, bgr2bgra, bgra2rgb, bgra2bgr
	17.10.18 - Change FlipBuffer to void
	13.11.18 - const changes as per Engel (https://github.com/leadedge/Spout2/pull/26)
	27.11.18 - Add RemovePadding
	17.10.19 - Add rgba2bgrResample and bgra2bgrResample for SpoutCam
	02.02.20 - Add rgba2rgbaResample and row pitch to all resamplers
	22.06.20 - Add void rgba2rgba and rgba2rgb with source pitch
			   Correct const for some functions
	30.06.20 - Use CopyPixels instead of copy loop in rgba2rgba
	09.07.20 - Add rgba2rgb with source stride
	07.09.20 - experimental SSE RGBA to RGB not working
	19.09.20 - Removed experimental SSE RGBA to RGB function
	23.09.20 - CheckSSE - initialize CPUInfo
	06.10.20 - Modifications to rgba2rgb and rgba2rgbResample for SpoutCam mirror and swap red/blue
	24.10.20 - Add rgba2bgra with source pitch
	25.10.20 - Add rgb2rgba with dest pitch
	26.10.20 - Add rgb2bgra with dest pitch
	09.12.20 - Correct movsd line pitch in RemovePadding
	13.03.21 - Change CopyPixels and FlipBuffer to accept GL_LUMINANCE
	09.07.21 - memcpy_sse2 - return for null dst or src
	21.02.22 - use std:: prefix for floor in rgba2rgbResample for Clang compatibility. PR#81
	10.04.22 - Casting changes to avoid Warning C26451 : Arithmetic overflow
	07.10.22 - Corrected missing alpha from rgb2rba and rgb2bgra functions
	24.10.22 - Add experimental rgb_to_bgrx_sse
	28.10.22 - Cleanup / code documentation
	11.12.22 - test for null args in conversion functions
	22.12.22 - Compiler compatibility check
			   Change all {} initializations to "={}"
	02-04-23 - Corrected source pointer increment in rgba2rgba when not inverted

*/

#include "SpoutCopy.h"

//
// Class: spoutCopy
//
// Functions to manage pixel data copy.
//
// Refer to source code for documentation.
//

spoutCopy::spoutCopy() {
	m_bSSE2 = false;
	m_bSSE3 = false;
	m_bSSSE3 = false;
	CheckSSE(); // SSE available - sets m_bSSE2, m_bSSE3, m_bSSSE3
}


spoutCopy::~spoutCopy() {

}

//---------------------------------------------------------
// Function: CopyPixels
// Copy image pixels and select fastest method based on image width.
void spoutCopy::CopyPixels(const unsigned char *source, unsigned char *dest,
	unsigned int width, unsigned int height, 
	GLenum glFormat, bool bInvert) const
{
	unsigned int Size = width*height; // GL_LUMINANCE default

	if (glFormat == GL_RGBA || glFormat == GL_BGRA_EXT)
		Size = width * height * 4;
	else if (glFormat == GL_RGB || glFormat == GL_BGR_EXT)
		Size = width*height * 3;

	if (bInvert) {
		FlipBuffer(source, dest, width, height, glFormat);
	}
	else {
		// Avoid warning C26474 and use implicit cast where possible
		if (width < 320) { // Too small for assembler
			// memcpy(reinterpret_cast<void *>(dest), reinterpret_cast<const void *>(source), Size);
			memcpy(dest, source, Size);
		}
		else if ((Size % 16) == 0 && m_bSSE2) { // 16 byte aligned SSE assembler
			// memcpy_sse2(reinterpret_cast<void *>(dest),	reinterpret_cast<const void *>(source), Size);
			memcpy_sse2(dest, source, Size);
		}
		else if ((Size % 4) == 0) { // 4 byte aligned
			memcpy(dest, source, Size); // was __movsd; ARM64-portable
		}
		else { // Default is standard memcpy
			// memcpy(reinterpret_cast<void *>(dest), reinterpret_cast<const void *>(source), Size);
			memcpy(dest, source, Size);
		}
	}
}

//---------------------------------------------------------
// Function: FlipBuffer
// Flip a pixel buffer in place.
void spoutCopy::FlipBuffer(const unsigned char *src,
	unsigned char *dst,
	unsigned int width,
	unsigned int height,
	GLenum glFormat) const
{
	unsigned int pitch = width; // GL_LUMINANCE default
	if (glFormat == GL_RGBA || glFormat == GL_BGRA_EXT)
		pitch = width * 4; // RGBA format specified
	else if (glFormat == GL_RGB || glFormat == GL_BGR_EXT)
		pitch = width * 3; // RGB format specified

	unsigned int line_s = 0;
	unsigned int line_t = (height - 1)*pitch;

	for (unsigned int y = 0; y<height; y++) {
		// Avoid warning C26474 and use implicit cast where possible
		if (width < 320 || height < 240) // too small for assembler
			memcpy((dst + line_t), (src + line_s), pitch);
			// memcpy(reinterpret_cast<void *>(dst + line_t), reinterpret_cast<const void *>(src + line_s), pitch);
		else if ((pitch % 16) == 0 && m_bSSE2) // use sse assembler function
			memcpy_sse2((dst + line_t), (src + line_s), pitch);
			// memcpy_sse2(reinterpret_cast<void *>(dst + line_t),	reinterpret_cast<const void *>(src + line_s), pitch);
		else if ((pitch % 4) == 0) // use 4 byte move
			memcpy((dst + line_t), (src + line_s), pitch); // was __movsd; ARM64-portable
		else
			memcpy((dst + line_t), (src + line_s), pitch);
			// memcpy(reinterpret_cast<void *>(dst + line_t), reinterpret_cast<const void *>(src + line_s), pitch);
		line_s += pitch;
		line_t -= pitch;
	}

}

//---------------------------------------------------------
// Function: RemovePadding
// Correct for image stride.
void spoutCopy::RemovePadding(const unsigned char *source, unsigned char *dest,
							  unsigned int width, unsigned int height,
							  unsigned int stride, GLenum glFormat) const
{
	// Actual line pitch
	unsigned int pitch = width*4; // default rgba
	if (glFormat == GL_RGB || glFormat == GL_BGR_EXT)
		pitch = width*3; // rgb

	// Remove the padding (stride-pitch)
	for (unsigned int y = 0; y < height; y++) {
		// Avoid warning C26474 and use implicit cast where possible
		if (pitch < 320 || stride < 320) { // too small for assembler
			// memcpy(reinterpret_cast<void *>(dest), reinterpret_cast<const void *>(source), pitch);
			memcpy(dest, source, pitch);
		}
		else if ((pitch % 16) == 0 && (stride % 16) == 0 && m_bSSE2) { // use sse
			// memcpy_sse2(reinterpret_cast<void *>(dest), reinterpret_cast<const void *>(source), pitch);
			memcpy_sse2(dest, source, pitch);
		}
		else if ((pitch % 4) == 0 && (stride % 4) == 0) { // 4 byte move
			memcpy(dest, source, pitch); // was __movsd; ARM64-portable
		}
		else {
			// memcpy(reinterpret_cast<void *>(dest), reinterpret_cast<const void *>(source), pitch);
			memcpy(dest, source, pitch);
		}
		source += stride;
		dest   += pitch;
	}
}

//
// Fast memcpy.
//
// Approx 1.7 times speed of memcpy (0.84 msec per frame 1920x1080).
//
// Original source - William Chan
// (dead link) http://williamchan.ca/portfolio/assembly/ssememcpy/
// See also :
//	http://stackoverflow.com/questions/1715224/very-fast-memcpy-for-image-processing
//	http://www.gamedev.net/topic/502313-special-case---faster-than-memcpy/
// And a more recent comprehensive study :
// Video : https://level1techs.com/video/level1-diagnostic-fixing-our-memcpy-troubles-looking-glass//
// Source : https://github.com/level1wendell/memcpy_sse and others.
//

//---------------------------------------------------------
// Function: memcpy_sse2
// SSE2 version of memcpy
void spoutCopy::memcpy_sse2(void* dst, const void* src, size_t Size) const
{
#if SPOUT_HAS_SSE

	if (!dst || !src)
		return;

	auto pSrc = static_cast<const char *>(src); // Source buffer
	auto pDst = static_cast<char *>(dst); // Destination buffer
	const unsigned int n = (unsigned int)Size >> 7; // Counter = size divided by 128 (8 * 128bit registers)

	__m128i Reg0={};
	__m128i Reg1={};
	__m128i Reg2={};
	__m128i Reg3={};
	__m128i Reg4={};
	__m128i Reg5={};
	__m128i Reg6={};
	__m128i Reg7={};
	for (unsigned int Index = n; Index > 0; --Index) {

		// SSE2 prefetch
		_mm_prefetch(pSrc + 256, _MM_HINT_NTA);
		_mm_prefetch(pSrc + 256 + 64, _MM_HINT_NTA); // ??? TODO : why

		// TODO - research and optimize - minor (0.1msec) speed difference noted
		// _mm_prefetch(pSrc, _MM_HINT_NTA);

		// move data from src to registers
		// 8 x 128 bit (16 bytes each)
		// Increment source pointer by 16 bytes each
		// for a total of 128 bytes per cycle
		Reg0 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc));
		Reg1 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc + 16));
		Reg2 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc + 32));
		Reg3 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc + 48));
		Reg4 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc + 64));
		Reg5 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc + 80));
		Reg6 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc + 96));
		Reg7 = _mm_load_si128(reinterpret_cast<const __m128i *>(pSrc + 112));

		// move data from registers to dest
		// TODO
		// _mm_prefetch((const char*)(pSrc + 256), _MM_HINT_NTA);

		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst), Reg0);
		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst + 16), Reg1);
		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst + 32), Reg2);
		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst + 48), Reg3);
		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst + 64), Reg4);
		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst + 80), Reg5);
		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst + 96), Reg6);
		_mm_stream_si128(reinterpret_cast<__m128i *>(pDst + 112), Reg7);

		pSrc += 128;
		pDst += 128;
	}
#else
	memcpy(dst, src, Size); // ARM64 scalar fallback
#endif
}

//
// Group: RGBA <> RGBA
//

//---------------------------------------------------------
// Function: rgba2rgba
// Copy rgba buffers line by line allowing for source pitch using the fastest method
void spoutCopy::rgba2rgba(const void* rgba_source, void* rgba_dest,
	unsigned int width, unsigned int height, unsigned int sourcePitch, bool bInvert) const
{
	if (!rgba_source || !rgba_dest)
		return;

	for (unsigned int y = 0; y < height; y++) {

		// Start of buffers
		auto source = static_cast<const unsigned __int32*>(rgba_source); // unsigned int = 4 bytes
		auto dest   = static_cast<unsigned __int32*>(rgba_dest);
		if (!source || !dest)
			return;

		// first
		// Casting first avoids warning C26451: Arithmetic overflow with VS2022 code review
		// https://docs.microsoft.com/en-us/visualstudio/code-quality/c26451
		unsigned long YxW            = (unsigned long)(y * width);
		const unsigned long YxSP4    = (unsigned long)(y * sourcePitch / 4);
		const unsigned long InvYxSP4 = (unsigned long)((height - 1 - y) * sourcePitch / 4);

		// Increment to current line
		// pitch is line length in bytes. Divide by 4 to get the width in rgba pixels.
		if (bInvert) {
			source += InvYxSP4;
			dest   += YxW; // dest is not inverted
		}
		else {
			source += YxSP4;
			dest   += YxW;
		}
		// Copy the line as fast as possible
		CopyPixels((const unsigned char*)source, (unsigned char*)dest, width, 1);
	}
}

//---------------------------------------------------------
// Function: rgba2rgba
// Copy rgba buffers line by line allowing for source and destination line pitch
void spoutCopy::rgba2rgba(const void* rgba_source, void* rgba_dest,
	unsigned int width, unsigned int height,
	unsigned int sourcePitch, unsigned int destPitch, bool bInvert) const
{
	if (!rgba_source || !rgba_dest)
		return;

	// For all rows
	for (unsigned int y = 0; y < height; y++) {
		
		// Start of buffers
		auto source = static_cast<const unsigned __int32*>(rgba_source); // unsigned int = 4 bytes
		auto dest   = static_cast<unsigned __int32*>(rgba_dest);
		if (!source || !dest)
			return;

		// Increment to current line
		// Pitch is line length in bytes. Divide by 4 to get the width in rgba pixels.
		if (bInvert) {
			source += (unsigned long)((height - 1 - y)*sourcePitch / 4);
			dest   += (unsigned long)(y * destPitch / 4); // dest is not inverted
		}
		else {
			source += (unsigned long)(y * sourcePitch / 4);
			dest   += (unsigned long)(y * destPitch / 4);
		}
		// Copy the line as fast as possible
		CopyPixels((const unsigned char*)source, (unsigned char*)dest, width, 1);
	}
}

// Adapted from :
// http://tech-algorithm.com/articles/nearest-neighbor-image-scaling/
// http://www.cplusplus.com/forum/general/2615/#msg10482

//---------------------------------------------------------
// Function: rgba2rgbaResample
// Copy rgba buffers of differing size
void spoutCopy::rgba2rgbaResample(const void* source, void* dest,
	unsigned int sourceWidth, unsigned int sourceHeight, unsigned int sourcePitch,
	unsigned int destWidth, unsigned int destHeight, bool bInvert) const
{
	const unsigned char* srcBuffer = (unsigned char*)source; // bgra source
	unsigned char* dstBuffer = (unsigned char*)dest; // bgr dest
	if (!srcBuffer || !dstBuffer)
		return;

	// horizontal and vertical ratios between the original image and the to be scaled image
	const float x_ratio = (float)sourceWidth / (float)destWidth;
	const float y_ratio = (float)sourceHeight / (float)destHeight;
	float px = 0.0f;
	float py = 0.0f;
	unsigned int i =- 0;
	unsigned int j = 0;
	unsigned int pixel = 0;
	int nearestMatch = 0;
	for (i = 0; i < destHeight; i++) {
		for (j = 0; j < destWidth; j++) {
			px = floor((float)j*x_ratio);
			py = floor((float)i*y_ratio);
			if (bInvert)
				pixel = (destHeight - i - 1)*destWidth * 4 + j * 4; // flip vertically
			else
				pixel = i * destWidth * 4 + j * 4;
			nearestMatch = (int)(py*sourcePitch + px * 4);
			dstBuffer[pixel + 0] = srcBuffer[nearestMatch + 0];
			dstBuffer[pixel + 1] = srcBuffer[nearestMatch + 1];
			dstBuffer[pixel + 2] = srcBuffer[nearestMatch + 2];
			dstBuffer[pixel + 3] = srcBuffer[nearestMatch + 3];
		}
	}
}

//
// Group: RGBA <> BGRA
//

//---------------------------------------------------------
// Function: rgba2bgra
// Copy rgba to bgra using the fastest method
void spoutCopy::rgba2bgra(const void* rgba_source, void* bgra_dest,
	unsigned int width, unsigned int height, bool bInvert) const
{
	if (!rgba_source || !bgra_dest)
		return;

	if ((width % 16) == 0) { // 16 byte aligned width
		if (m_bSSE2 && m_bSSSE3) // SSE3 available
			rgba_bgra_sse3(rgba_source, bgra_dest, width, height, bInvert);
		else if (m_bSSE2) // SSE2 available
			rgba_bgra_sse2(rgba_source, bgra_dest, width, height, bInvert);
	}
	else {
		rgba_bgra(rgba_source, bgra_dest, width, height, bInvert);
	}
}

//---------------------------------------------------------
// Function: rgba2bgra
// Copy rgba to bgra line by line allowing for source pitch using the fastest method
void spoutCopy::rgba2bgra(const void *rgba_source, void *bgra_dest,
	unsigned int width, unsigned int height, unsigned int sourcePitch, bool bInvert) const
{
	if (!rgba_source || !bgra_dest)
		return;

	for (unsigned int y = 0; y < height; y++) {

		// Start of buffers
		auto source = static_cast<const unsigned __int32*>(rgba_source); // unsigned int = 4 bytes
		auto dest = static_cast<unsigned __int32*>(bgra_dest);
		if (!source || !dest)
			return;

		// Casting first avoids warning C26451: Arithmetic overflow with VS2022 code review
		// https://docs.microsoft.com/en-us/visualstudio/code-quality/c26451
		unsigned long YxW = (unsigned long)(y * width);

		// Increment to current line.
		if (bInvert) {
			// Pitch is line length in bytes.
			// Divide by 4 to get the line width in rgba pixels.
			source += ((height - 1 - y) * sourcePitch / 4);
			dest += YxW; // dest is not inverted
		}
		else {
			source += (unsigned long)(y * sourcePitch / 4);
			dest += YxW;
		}
		// Copy the line
		if ((width % 16) == 0) { // 16 byte aligned width
			if (m_bSSE2 && m_bSSSE3) // SSE3 available
				rgba_bgra_sse3(source, dest, width, 1, bInvert);
			else if (m_bSSE2) // SSE2 available
				rgba_bgra_sse2(source, dest, width, 1, bInvert);
		}
		else {
			rgba_bgra(source, dest, width, 1, bInvert);
		}
	}
}

//---------------------------------------------------------
// Function: rgba2bgra
// Copy rgba to bgra line by line allowing for source and destination line pitch
void spoutCopy::rgba2bgra(const void* rgba_source, void* bgra_dest,
	unsigned int width, unsigned int height,
	unsigned int sourcePitch, unsigned int destPitch, bool bInvert) const
{
	if (!rgba_source || !bgra_dest)
		return;

	for (unsigned int y = 0; y < height; y++) {

		// Start of buffers
		auto source = static_cast<const unsigned __int32*>(rgba_source); // unsigned int = 4 bytes
		auto dest = static_cast<unsigned __int32*>(bgra_dest);
		if (!source || !dest)
			return;

		// Cast first to avoid warning C26451: Arithmetic overflow
		unsigned long YxDP = (unsigned long)(y * destPitch / 4);

		// Increment to current line.
		// Pitch is line length in bytes.
		// Divide by 4 to get the line width in rgba pixels.
		if (bInvert) {
			source += (unsigned long)((height - 1 - y) * sourcePitch / 4);
			dest += YxDP; // dest is not inverted
		}
		else {
			source += (unsigned long)(y * sourcePitch / 4);
			dest += YxDP;
		}
		// Copy the line
		if ((width % 16) == 0) { // 16 byte aligned width
			if (m_bSSE2 && m_bSSSE3) // SSE3 available
				rgba_bgra_sse3(source, dest, width, 1, bInvert);
			else if (m_bSSE2) // SSE2 available
				rgba_bgra_sse2(source, dest, width, 1, bInvert);
		}
		else {
			rgba_bgra(source, dest, width, 1, bInvert);
		}

	}
}

//---------------------------------------------------------
// Function: bgra2rgba
// Copy bgra to rgba
// Both are swapping red and blue, so use the same function
void spoutCopy::bgra2rgba(const void *bgra_source, void *rgba_dest, unsigned int width, unsigned int height, bool bInvert) const
{
	rgba2bgra(bgra_source, rgba_dest, width, height, bInvert);
}


//
// Group: RGB/BGR <> RGBA/BGRA
//

//---------------------------------------------------------
// Function: rgba2rgb
// Copy RGBA to RGB allowing for source line pitch
void spoutCopy::rgba2rgb(const void* rgba_source, void* rgb_dest,
	unsigned int width, unsigned int height,
	unsigned int rgba_pitch, bool bInvert, bool bMirror, bool bSwapRB) const
{
	// Start of buffers
	auto rgba = static_cast<const unsigned char*>(rgba_source); // rgba/bgra
	auto rgb = static_cast<unsigned char*>(rgb_dest); // rgb/bgr
	if (!rgb || !rgba)
		return;

	// RGB dest does not have padding
	uint64_t rgbsize = (uint64_t)width * (uint64_t)height * 3;
	uint64_t rgbpitch = (uint64_t)width * 3;
	const uint64_t rgba_padding = (uint64_t)rgba_pitch-((uint64_t)width * 4);

	// RGBA source may have padding 
	// Dest and source must be the same dimensions otherwise

	if (bInvert) {
		rgb += rgbsize; // end of rgb buffer
		rgb -= rgbpitch; // beginning of the last rgb line
	}

	// Swap red and blue option
	int ir = 0; int ig = 1; int ib = 2;
	if (bSwapRB) {
		ir = 2; ib = 0;
	}

	unsigned int z = 0;
	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			if (bMirror) {
				z = (width - x - 1)*3;
				*(rgb + z + ir) = *(rgba + 0); // red
				*(rgb + z + ig) = *(rgba + 1); // grn
				*(rgb + z + ib) = *(rgba + 2); // blu
			}
			else {
				z = x * 3;
				*(rgb + z + ir) = *(rgba + 0); // red
				*(rgb + z + ig) = *(rgba + 1); // grn
				*(rgb + z + ib) = *(rgba + 2); // blu
			}
			rgba += 4;
		}
		rgb += (uint64_t)width * 3;
		rgba += rgba_padding;

		if (bInvert)
			rgb -= rgbpitch * 2; // move up a line for invert
	}

} // end rgba2rgb

//---------------------------------------------------------
// Function: rgb2rgba
//
void spoutCopy::rgb2rgba(const void *rgb_source, void *rgba_dest, unsigned int width, unsigned int height, bool bInvert) const
{
	// Start of buffers
	auto rgb = static_cast<const unsigned char*>(rgb_source); // RGB
	auto rgba = static_cast<unsigned char*>(rgba_dest); // RGBA
	if (!rgb || !rgba)
		return;

	const uint64_t rgbsize  = (uint64_t)width * (uint64_t)height * 3;
	const uint64_t rgbpitch = (uint64_t)width * 3;

	if (bInvert) {
		rgb += rgbsize; // end of rgb buffer
		rgb -= rgbpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			// rgb source - rgba dest
			*(rgba + 0) = *(rgb + 0); // red
			*(rgba + 1) = *(rgb + 1); // grn
			*(rgba + 2) = *(rgb + 2); // blu
			*(rgba + 3) = (unsigned char)255; // alpha
			rgb += 3;
			rgba += 4;
		}
		if (bInvert)
			rgb -= rgbpitch * 2L; // move up a line for invert
	}

} // end rgb2rgba

//---------------------------------------------------------
// Function: rgb2rgba
//
void spoutCopy::rgb2rgba(const void *rgb_source, void *rgba_dest,
	unsigned int width, unsigned int height,
	unsigned int dest_pitch, bool bInvert) const
{
	// Start of buffers
	auto rgb = static_cast<const unsigned char*>(rgb_source); // rgb/bgr
	auto rgba = static_cast<unsigned char*>(rgba_dest); // rgba/bgra
	if (!rgb || !rgba)
		return;

	// RGB source does not have padding
	const uint64_t rgbsize      = (uint64_t )width * (uint64_t)height * 3;
	const uint64_t rgbpitch     = (uint64_t)width * 3;
	uint64_t rgba_padding = (uint64_t)dest_pitch - ((uint64_t)width * 4);

	// RGBA dest may have padding 
	// Dest and source must be the same dimensions otherwise

	if (bInvert) {
		rgb += rgbsize; // end of rgb buffer
		rgb -= rgbpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			*(rgba + 0) = *(rgb + 0); // red
			*(rgba + 1) = *(rgb + 1); // grn
			*(rgba + 2) = *(rgb + 2); // blu
			*(rgba + 3) = (unsigned char)255; // alpha
			rgb += 3;
			rgba += 4;
		}
		rgba += rgba_padding;

		if (bInvert)
			rgb -= rgbpitch * 2; // move up a line for invert
	}

} // end rgb2rgba

//---------------------------------------------------------
// Function: bgr2rgba
//
void spoutCopy::bgr2rgba(const void *bgr_source, void *rgba_dest, unsigned int width, unsigned int height, bool bInvert) const
{
	// Start of buffers
	auto bgr = static_cast<const unsigned char*>(bgr_source); // BGR
	auto rgba = static_cast<unsigned char*>(rgba_dest); // RGBA
	if (!bgr || !rgba)
		return;

	const uint64_t bgrsize = (uint64_t)width * (uint64_t)height * 3;
	const uint64_t bgrpitch = (uint64_t)width * 3;

	if (bInvert) {
		bgr += bgrsize; // end of rgb buffer
		bgr -= bgrpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			// bgr source - rgba dest
			*(rgba + 0) = *(bgr + 2); // red
			*(rgba + 1) = *(bgr + 1); // grn
			*(rgba + 2) = *(bgr + 0); // blu
			*(rgba + 3) = (unsigned char)255; // alpha
			bgr += 3;
			rgba += 4;
		}
		if (bInvert)
			bgr -= bgrpitch * 2; // move up a line for invert
	}

} // end bgr2rgba

//---------------------------------------------------------
// Function: bgr2rgba
//
void spoutCopy::bgr2rgba(const void *bgr_source, void *rgba_dest,
	unsigned int width, unsigned int height,
	unsigned int dest_pitch, bool bInvert) const
{
	// Start of buffers
	auto bgr = static_cast<const unsigned char*>(bgr_source); // rgb/bgr
	auto rgba = static_cast<unsigned char*>(rgba_dest); // rgba/bgra
	if (!bgr || !rgba)
		return;

	// BGR buffer dest does not have padding
	const uint64_t bgrsize = (uint64_t)width * (uint64_t)height * 3;
	const uint64_t bgrpitch = (uint64_t)width * 3;
	const uint64_t rgba_padding = (uint64_t)dest_pitch - ((uint64_t)width * 4);

	// RGBA dest may have padding 
	// Dest and source must be the same dimensions otherwise

	if (bInvert) {
		bgr += bgrsize; // end of rgb buffer
		bgr -= bgrpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			*(rgba + 0) = *(bgr + 0); // blu
			*(rgba + 1) = *(bgr + 1); // grn
			*(rgba + 2) = *(bgr + 2); // red
			*(rgba + 3) = (unsigned char)255; // alpha
			bgr += 3;
			rgba += 4;
		}
		rgba += rgba_padding;

		if (bInvert)
			bgr -= bgrpitch * 2; // move up a line for invert
	}

} // end bgr2rgba with dest pitch

//---------------------------------------------------------
// Function: rgb2bgra
//
void spoutCopy::rgb2bgra(const void *rgb_source, void *bgra_dest, unsigned int width, unsigned int height, bool bInvert) const
{
	// Start of buffers
	auto rgb = static_cast<const unsigned char*>(rgb_source); // RGB
	auto bgra = static_cast<unsigned char*>(bgra_dest); // BGRA
	if (!rgb || !bgra)
		return;

	const uint64_t rgbsize = (uint64_t)width * (uint64_t)height * 3;
	const uint64_t rgbpitch = (uint64_t)width * 3;

	if (bInvert) {
		rgb += rgbsize; // end of rgb buffer
		rgb -= rgbpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			// rgb source - bgra dest
			*(bgra + 0) = *(rgb + 2); // blu
			*(bgra + 1) = *(rgb + 1); // grn
			*(bgra + 2) = *(rgb + 0); // red
			*(bgra + 3) = (unsigned char)255; // alpha
			rgb += 3;
			bgra += 4;
		}
		if (bInvert)
			rgb -= rgbpitch * 2; // move up a line for invert
	}

} // end rgb2bgra

//---------------------------------------------------------
// Function: rgb2bgra
//
void spoutCopy::rgb2bgra(const void *rgb_source, void *bgra_dest,
	unsigned int width, unsigned int height,
	unsigned int dest_pitch, bool bInvert) const
{
	// Start of buffers
	auto rgb = static_cast<const unsigned char*>(rgb_source); // rgb/bgr
	auto bgra = static_cast<unsigned char*>(bgra_dest); // rgba/bgra
	if (!rgb || !bgra)
		return;

	// RGB source does not have padding
	const uint64_t rgbsize = (uint64_t)width * (uint64_t)height * 3;
	const uint64_t rgbpitch = (uint64_t)width * 3;
	// BGRA dest may have padding 
	uint64_t bgra_padding = (uint64_t)dest_pitch - ((uint64_t)width * 4);
	// Dest and source must be the same dimensions otherwise

	if (bInvert) {
		rgb += rgbsize; // end of rgb buffer
		rgb -= rgbpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			*(bgra + 0) = *(rgb + 2); // blu
			*(bgra + 1) = *(rgb + 1); // grn
			*(bgra + 2) = *(rgb + 0); // red
			*(bgra + 3) = (unsigned char)255; // alpha
			rgb += 3;
			bgra += 4;
		}
		bgra += bgra_padding;

		if (bInvert)
			rgb -= rgbpitch * 2; // move up a line for invert
	}

} // end rgb2bgra


// Experimental
//
// Testing shows gains of 1 fps
// TODO : exact frame timing
//
// https://exchangetuts.com/fast-vectorized-conversion-from-rgb-to-bgra-1640844423877396
//
// in and out must be 16-byte aligned

//---------------------------------------------------------
// Function: rgb_to_bgrx_sse
// Experimental pending testing
void spoutCopy::rgb_to_bgrx_sse(unsigned int w, const void* inpix, void* outpix) const
{
#if SPOUT_HAS_SSE
	const __m128i* in_vec = static_cast<const __m128i*>(inpix);
	__m128i* out_vec = static_cast<__m128i*>(outpix);

	if (!in_vec || !out_vec)
		return;

	w /= 16;

	while (w-- > 0) {

		/*             0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
		 * in_vec[0]   Ra Ga Ba Rb Gb Bb Rc Gc Bc Rd Gd Bd Re Ge Be Rf
		 * in_vec[1]   Gf Bf Rg Gg Bg Rh Gh Bh Ri Gi Bi Rj Gj Bj Rk Gk
		 * in_vec[2]   Bk Rl Gl Bl Rm Gm Bm Rn Gn Bn Ro Go Bo Rp Gp Bp
		 */
		__m128i in1={};
		__m128i in2={};
		__m128i in3={};
		__m128i out={};

		in1 = in_vec[0];

		out = _mm_shuffle_epi8(in1,
			_mm_set_epi8('\xff', 9, 10, 11, '\xff', 6, 7, 8, '\xff', 3, 4, 5, '\xff', 0, 1, 2));
		out = _mm_or_si128(out,
			_mm_set_epi8('\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0));
		out_vec[0] = out;

		in2 = in_vec[1];

		in1 = _mm_and_si128(in1,
			_mm_set_epi8('\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', 0, 0, 0, 0, 0, 0, 0, 0));
		out = _mm_and_si128(in2,
			_mm_set_epi8(0, 0, 0, 0, 0, 0, 0, 0, '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff'));
		out = _mm_or_si128(out, in1);
		out = _mm_shuffle_epi8(out,
			_mm_set_epi8('\xff', 5, 6, 7, '\xff', 2, 3, 4, '\xff', 15, 0, 1, '\xff', 12, 13, 14));
		out = _mm_or_si128(out,
			_mm_set_epi8('\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0));
		out_vec[1] = out;

		in3 = in_vec[2];
		in_vec += 3;

		in2 = _mm_and_si128(in2,
			_mm_set_epi8('\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', 0, 0, 0, 0, 0, 0, 0, 0));
		out = _mm_and_si128(in3,
			_mm_set_epi8(0, 0, 0, 0, 0, 0, 0, 0, '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff'));
		out = _mm_or_si128(out, in2);
		out = _mm_shuffle_epi8(out,
			_mm_set_epi8('\xff', 1, 2, 3, '\xff', 14, 15, 0, '\xff', 11, 12, 13, '\xff', 8, 9, 10));
		out = _mm_or_si128(out,
			_mm_set_epi8('\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0));
		out_vec[2] = out;

		out = _mm_shuffle_epi8(in3,
			_mm_set_epi8('\xff', 13, 14, 15, '\xff', 10, 11, 12, '\xff', 7, 8, 9, '\xff', 4, 5, 6));
		out = _mm_or_si128(out,
			_mm_set_epi8('\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0, '\xff', 0, 0, 0));
		out_vec[3] = out;

		out_vec += 4;
	}
#else
	(void)w;
	(void)inpix;
	(void)outpix; // ARM64: SSE-only path, not reachable at runtime
#endif
} // end rgb_to_bgrx_sse

//---------------------------------------------------------
// Function: bgr2bgra
//
void spoutCopy::bgr2bgra(const void *bgr_source, void *bgra_dest, unsigned int width, unsigned int height, bool bInvert) const
{
	// Start of buffers
	auto bgr = static_cast<const unsigned char*>(bgr_source); // BGR
	auto bgra = static_cast<unsigned char*>(bgra_dest); // BGRA
	if (!bgr || !bgra)
		return;

	const uint64_t bgrsize = (uint64_t)width * (uint64_t)height * 3;
	const uint64_t bgrpitch = (uint64_t)width * 3;

	if (bInvert) {
		bgr += bgrsize; // end of rgb buffer
		bgr -= bgrpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			// bgr source - bgra dest
			*(bgra + 0) = *(bgr + 0); // blu
			*(bgra + 1) = *(bgr + 1); // grn
			*(bgra + 2) = *(bgr + 2); // red
			*(bgra + 3) = (unsigned char)255; // alpha
			bgr += 3;
			bgra += 4;
		}
		if (bInvert)
			bgr -= bgrpitch * 2; // move up a line for invert
	}

} // end bgr2bgra

//---------------------------------------------------------
// Function: rgba2bgr
//
void spoutCopy::rgba2bgr(const void *rgba_source, void *bgr_dest, unsigned int width, unsigned int height, bool bInvert) const
{

	// Start of buffers
	auto rgba = static_cast<const unsigned char*>(rgba_source); // RGBA
	auto bgr = static_cast<unsigned char*>(bgr_dest); // BGR
	if (!rgba || !bgr)
		return;

	uint64_t bgrsize = (uint64_t)width * (uint64_t)height * 3;
	uint64_t bgrpitch = (uint64_t)width * 3;

	if (bInvert) {
		bgr += bgrsize; // end of rgb buffer
		bgr -= bgrpitch; // beginning of the last bgr line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			// rgba source - bgr dest
			*(bgr + 0) = *(rgba + 2); // blu
			*(bgr + 1) = *(rgba + 1); // grn
			*(bgr + 2) = *(rgba + 0); // red
			bgr += 3;
			rgba += 4;
		}
		if (bInvert)
			bgr -= bgrpitch * 2; // move up a line for invert
	}

} // end rgba2bgr

//---------------------------------------------------------
// Function: rgba2bgr
//
void spoutCopy::rgba2bgr(const void *rgba_source, void *bgr_dest,
	unsigned int width, unsigned int height,
	unsigned int rgba_pitch, bool bInvert) const
{
	// Start of buffers
	auto rgba = static_cast<const unsigned char*>(rgba_source); // rgba/bgra
	auto bgr = static_cast<unsigned char*>(bgr_dest); // rgb/bgr
	if (!rgba || !bgr)
		return;

	// RGB dest does not have padding
	uint64_t rgbsize = (uint64_t)width * (uint64_t)height * 3;
	uint64_t rgbpitch = (uint64_t)width * 3;
	const uint64_t rgba_padding = (uint64_t)rgba_pitch - ((uint64_t)width * 4);

	// RGBA source may have padding 
	// Dest and source must be the same dimensions otherwise
	if (bInvert) {
		bgr += rgbsize; // end of rgb buffer
		bgr -= rgbpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			*(bgr + 2) = *(rgba + 0); // red
			*(bgr + 1) = *(rgba + 1); // grn
			*(bgr + 0) = *(rgba + 2); // blu
			bgr += 3;
			rgba += 4;
		}
		rgba += rgba_padding;

		if (bInvert)
			bgr -= rgbpitch * 2; // move up a line for invert
	}

} // end rgba2bgr

//---------------------------------------------------------
// Function: rgba2rgbResample
//
void spoutCopy::rgba2rgbResample(const void* source, void* dest,
	unsigned int sourceWidth, unsigned int sourceHeight, unsigned int sourcePitch,
	unsigned int destWidth, unsigned int destHeight, bool bInvert, bool bMirror, bool bSwapRB) const
{

	const unsigned char* srcBuffer = (unsigned char*)source; // bgra source
	unsigned char* dstBuffer = (unsigned char*)dest; // bgr dest
	if (!srcBuffer || !dstBuffer)
		return;

	const float x_ratio = (float)sourceWidth / (float)destWidth;
	const float y_ratio = (float)sourceHeight / (float)destHeight;

	// Swap red and blue option
	int ir = 0; int ig = 1; int ib = 2;
	if (bSwapRB) {
		ir = 2; ib = 0;
	}

	float px = 0.0f;
	float py = 0.0f;
	unsigned int i = 0;
	unsigned int j = 0;
	unsigned int pixel = 0;
	int nearestMatch = 0;
	for (i = 0; i < destHeight; i++) {
		for (j = 0; j < destWidth; j++) {
			px = floor((float)j*x_ratio);
			py = floor((float)i*y_ratio);

			if (bMirror) {
				if (bInvert)
					pixel = (destHeight - i - 1)*destWidth * 3 + (destWidth - j - 1) * 3; // flip vertically
				else
					pixel = i * destWidth * 3 + (destWidth - j - 1) * 3;
			}
			else {
				if (bInvert)
					pixel = (destHeight - i - 1)*destWidth * 3 + j * 3; // flip vertically
				else
					pixel = i * destWidth * 3 + j * 3;
			}

			nearestMatch = (int)(py*sourcePitch + px * 4);
			dstBuffer[pixel + ir] = srcBuffer[nearestMatch + 0];
			dstBuffer[pixel + ig] = srcBuffer[nearestMatch + 1];
			dstBuffer[pixel + ib] = srcBuffer[nearestMatch + 2];
		}
	}
}

//---------------------------------------------------------
// Function: rgba2bgrResample
//
void spoutCopy::rgba2bgrResample(const void* source, void* dest,
	unsigned int sourceWidth, unsigned int sourceHeight, unsigned int sourcePitch,
	unsigned int destWidth, unsigned int destHeight, bool bInvert) const
{
	const unsigned char* srcBuffer = (unsigned char*)source; // bgra source
	unsigned char* dstBuffer = (unsigned char*)dest; // bgr dest
	if (!srcBuffer || !dstBuffer)
		return;

	const float x_ratio = (float)sourceWidth / (float)destWidth;
	const float y_ratio = (float)sourceHeight / (float)destHeight;
	float px = 0.0f;
	float py = 0.0f;
	unsigned int i = 0;
	unsigned int j = 0;
	unsigned int pixel = 0;
	int nearestMatch = 0;
	for (i = 0; i < destHeight; i++) {
		for (j = 0; j < destWidth; j++) {
			px = std::floor((float)j*x_ratio);
			py = std::floor((float)i*y_ratio);
			if (bInvert)
				pixel = (destHeight - i - 1)*destWidth * 3 + j * 3; // flip vertically
			else
				pixel = i * destWidth * 3 + j * 3;
			nearestMatch = (int)(py*sourcePitch + px * 4);
			dstBuffer[pixel + 2] = srcBuffer[nearestMatch + 0];
			dstBuffer[pixel + 1] = srcBuffer[nearestMatch + 1];
			dstBuffer[pixel + 0] = srcBuffer[nearestMatch + 2];
		}
	}
}

//---------------------------------------------------------
// Function: bgra2rgb
//
void spoutCopy::bgra2rgb(const void *bgra_source, void *rgb_dest, unsigned int width, unsigned int height, bool bInvert) const
{

	// Start of buffers
	auto bgra = static_cast<const unsigned char*>(bgra_source); // BGRA
	auto rgb = static_cast<unsigned char*>(rgb_dest); // RGB
	if (!bgra || !rgb)
		return;

	uint64_t rgbsize = (uint64_t)width * (uint64_t)height * 3;
	uint64_t rgbpitch = (uint64_t)width * 3;

	if (bInvert) {
		rgb += rgbsize; // end of rgb buffer
		rgb -= rgbpitch; // beginning of the last rgb line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			// bgra source - rgb dest
			*(rgb + 0) = *(bgra + 2); // red
			*(rgb + 1) = *(bgra + 1); // grn
			*(rgb + 2) = *(bgra + 0); // blu
			rgb += 3;
			bgra += 4;
		}
		if (bInvert)
			rgb -= rgbpitch * 2; // move up a line for invert
	}

} // end bgra2rgb

//---------------------------------------------------------
// Function: bgra2bgr
//
void spoutCopy::bgra2bgr(const void *bgra_source, void *bgr_dest, unsigned int width, unsigned int height, bool bInvert) const
{

	// Start of buffers
	auto bgra = static_cast<const unsigned char*>(bgra_source); // BGRA
	auto bgr = static_cast<unsigned char*>(bgr_dest); // BGR
	if (!bgra || !bgr) return;

	uint64_t bgrsize = (uint64_t)width * (uint64_t)height * 3;
	uint64_t bgrpitch = (uint64_t)width * 3;

	if (bInvert) {
		bgr += bgrsize; // end of bgr buffer
		bgr -= bgrpitch; // beginning of the last bgr line
	}

	for (unsigned int y = 0; y < height; y++) {
		for (unsigned int x = 0; x < width; x++) {
			// bgra source - bgr dest
			*(bgr + 2) = *(bgra + 2); // red
			*(bgr + 1) = *(bgra + 1); // grn
			*(bgr + 0) = *(bgra + 0); // blu
			bgr += 3;
			bgra += 4;
		}
		if (bInvert)
			bgr -= bgrpitch * 2; // move up a line for invert
	}
} // end bgra2bgr



//
// Protected
//

//
//					CheckSSE()
//
// https://msdn.microsoft.com/en-us/library/hskdteyh.aspx
//
// static bool SSE2 (void) { return CPU_Rep.f_1_EDX_[26]; }
// SSE2 | [bit 26] EDX
// SSE2 = (cpuid03 & (0x1 << 26))
//
// static bool SSE3 (void) { return CPU_Rep.f_1_ECX_[0]; }
// SSE3 | [bit 0] ECX
// SSE3 = (cpuid02 & (0x1)
//
// static bool SSSE3(void) { return CPU_Rep.f_1_ECX_[9]; }
// SSSE3 | [bit 9] ECX
// SSSE3 = (cpuid02 & (0x1 << 9)
//
// SSE4 not currently used :
//
// SSE4.1 and SSE4.2 are extensions of SSE, SSE2, SSE3, and SSSE3. 
// To check if the processor supports SSE4.1, execute CPUID with EAX = 1 as input.
// If bit 19 of ECX is set, then the processor supports SSE4.1.
// To check if the processor supports SSE4.2 instructions for string / text processing,
// PCMPGTQ, and CRC32, execute CPUID with EAX = 1 as input.
// If bit 20 of ECX is set, then the processor supports these SSE4.2 in structions.
//
// static bool SSE41(void) { return CPU_Rep.f_1_ECX_[19]; }
// SSE41 | [bit 19] ECX
// SSE41 = (cpuid02 & (0x1 << 19))
//
// static bool SSE42(void) { return CPU_Rep.f_1_ECX_[20]; }
// SSE42 | [bit 20] ECX
// SSE42 = (cpuid02 & (0x1 << 20))
//
// EAX - CPUInfo[0]
// EBX - CPUInfo[1]
// ECX - CPUInfo[2]
// EDX - CPUInfo[3]
//
// For intrinsics and SSE : https://software.intel.com/sites/landingpage/IntrinsicsGuide/
//
void spoutCopy::CheckSSE()
{
#if SPOUT_HAS_SSE
	// An array of four integers that contains the information returned
	// in EAX (0), EBX (1), ECX (2), and EDX (3) about supported features of the CPU.
	int CPUInfo[4] ={-1, -1, -1, -1};

	//-- Get number of valid info ids
	__cpuid(CPUInfo, 0);
	const int nIds = CPUInfo[0];

	//-- Get info for id "1"
	if (nIds >= 1) {
		// SSE2 | [bit 26] EDX
		// SSE2 = (cpuid03 & (0x1 << 26))
		__cpuid(CPUInfo, 1); // EAX = 1 for __cpuid
		m_bSSE2 = ((CPUInfo[3] & (0x1 << 26)) || false);
		// SSE3 | [bit 0] ECX
		// SSE3 = (cpuid02 & (0x1)
		m_bSSE3 = ((CPUInfo[2] & (0x1)) || false);
		// SSSE3 | [bit 9] ECX
		// SSSE3 = (cpuid02 & (0x1 << 9)
		m_bSSSE3 = ((CPUInfo[2] & (0x1 << 9)) || false);
	}
#endif // SPOUT_HAS_SSE — ARM64 keeps the false initialisers from the ctor
}


// Copy rgba to bgra without SSE
void spoutCopy::rgba_bgra(const void* rgba_source, void* bgra_dest,
	unsigned int width, unsigned int height, bool bInvert) const
{

	if (!rgba_source)
		return;
	if (!bgra_dest)
		return;

	for (unsigned int y = 0; y < height; y++) {

		// Start of buffer
		auto source = static_cast<const unsigned __int32*>(rgba_source);; // unsigned int = 4 bytes
		auto dest = static_cast<unsigned __int32*>(bgra_dest);
		if (!source || !dest) return;

		// Cast first to avoid warning C26451: Arithmetic overflow
		const unsigned long H1YxW = (unsigned long)((height - 1 - y) * width);
		unsigned long YxW = (unsigned long)(y * width);

		// Increment to current line
		if (bInvert) {
			source += H1YxW;
			dest += YxW; // dest is not inverted
		}
		else {
			source += YxW;
			dest += YxW;
		}

		for (unsigned int x = 0; x < width; x++) {
			const auto rgbapix = source[x];
			dest[x] = (_rotl(rgbapix, 16) & 0x00ff00ff) | (rgbapix & 0xff00ff00);
		}

	}
} // end rgba_bgra


//
// Adapted from : https://searchcode.com/codesearch/view/5070982/
// 
// Copyright (c) 2002-2010 The ANGLE Project Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
//
// https://chromium.googlesource.com/angle/angle/+/master/LICENSE
//
// All instructions SSE2.
//
void spoutCopy::rgba_bgra_sse2(const void* rgba_source, void* bgra_dest, unsigned int width, unsigned int height, bool bInvert) const
{
#if SPOUT_HAS_SSE

	if (!rgba_source)
		return;
	if (!bgra_dest)
		return;

	const __m128i brMask = _mm_set1_epi32(0x00ff00ff); // argb

	for (unsigned int y = 0; y < height; y++) {

		// Start of buffer
		auto source = static_cast<const unsigned __int32*>(rgba_source); // unsigned int = 4 bytes
		auto dest = static_cast<unsigned __int32*>(bgra_dest);
		if (!source || !dest) return;

		// Cast first to avoid warning C26451: Arithmetic overflow
		const unsigned long H1YxW = (unsigned long)((height - 1 - y) * width);
		unsigned long YxW = (unsigned long)(y * width);

		// Increment to current line
		if (bInvert)
			source += H1YxW;
		else
			source += YxW;

		dest += YxW; // dest is not inverted

		// Make output writes aligned
		unsigned int x;
		for (x = 0; ((reinterpret_cast<intptr_t>(&dest[x]) & 15) != 0) && x < width; x++) {
			const auto rgbapix = source[x];
			// rgbapix << 16		: a r g b > g b a r
			//        & 0x00ff00ff  : r g b . > . b . r
			// rgbapix & 0xff00ff00 : a r g b > a . g .
			// result of or			:           a b g r
			dest[x] = (_rotl(rgbapix, 16) & 0x00ff00ff) | (rgbapix & 0xff00ff00);
		}

		for (; x + 3 < width; x += 4) {
			const __m128i sourceData = _mm_loadu_si128(reinterpret_cast<const __m128i*>(&source[x]));
			// Mask out g and a, which don't change
			const __m128i gaComponents = _mm_andnot_si128(brMask, sourceData);
			// Mask out b and r
			const __m128i brComponents = _mm_and_si128(sourceData, brMask);
			// Swap b and r
			const __m128i brSwapped = _mm_shufflehi_epi16(_mm_shufflelo_epi16(brComponents, _MM_SHUFFLE(2, 3, 0, 1)), _MM_SHUFFLE(2, 3, 0, 1));
			const __m128i result = _mm_or_si128(gaComponents, brSwapped);
			_mm_store_si128(reinterpret_cast<__m128i*>(&dest[x]), result);
		}

		// Perform leftover writes
		for (; x < width; x++) {
			const auto rgbapix = source[x];
			dest[x] = (_rotl(rgbapix, 16) & 0x00ff00ff) | (rgbapix & 0xff00ff00);
		}
	}
#else
	(void)rgba_source;
	(void)bgra_dest;
	(void)width;
	(void)height;
	(void)bInvert; // ARM64: unreachable — guarded by m_bSSE2 at the call site
#endif
} // end rgba_bgra_sse2

//
//	Adapted from a Gist snippet by Aur�lien Vall�e (NewbiZ) http://newbiz.github.io/
//
//	https://gist.github.com/NewbiZ/5541524
//
//	Approximately 15% faster than SSE2 function
//
void spoutCopy::rgba_bgra_sse3(const void* rgba_source, void* bgra_dest, unsigned int width, unsigned int height, bool bInvert) const
{
#if SPOUT_HAS_SSE
	// Shuffling mask (RGBA -> BGRA) x 4, in reverse byte order
	static const __m128i m = _mm_set_epi8(15, 12, 13, 14, 11, 8, 9, 10, 7, 4, 5, 6, 3, 0, 1, 2);

	for (unsigned int y = 0; y < height; y++) {

		// Start of buffer
		auto source = static_cast<const unsigned __int32*>(rgba_source); // unsigned int = 4 bytes
		auto dest = static_cast<unsigned __int32*>(bgra_dest);

		// Cast first to avoid warning C26451: Arithmetic overflow
		const unsigned long H1YxW = (unsigned long)((height - 1 - y) * width);
		unsigned long YxW = (unsigned long)(y * width);

		// Increment to current line
		if (bInvert)
			source += H1YxW;
		else
			source += YxW;
		dest += YxW; // dest is not inverted

		// Assert pixels will NOT be aliased here : TODO
		// __m128i* __restrict__ pix = (__m128i*)pixels;
		auto src = reinterpret_cast<const __m128i*>(source);
		auto dst = reinterpret_cast<__m128i*>(dest);

		// Tile the LHS to match 64B cache line size
		const auto srcEnd = reinterpret_cast<const __m128i*>(source + width);
		for (; src < srcEnd; src += 4, dst += 4) {

			__m128i p1 = _mm_load_si128(src); // SSE2
			__m128i p2 = _mm_load_si128(src + 1);
			__m128i p3 = _mm_load_si128(src + 2);
			__m128i p4 = _mm_load_si128(src + 3);

			p1 = _mm_shuffle_epi8(p1, m); // SSSE3
			p2 = _mm_shuffle_epi8(p2, m);
			p3 = _mm_shuffle_epi8(p3, m);
			p4 = _mm_shuffle_epi8(p4, m);

			_mm_store_si128(dst, p1); // SSE2
			_mm_store_si128(dst + 1, p2);
			_mm_store_si128(dst + 2, p3);
			_mm_store_si128(dst + 3, p4);

		}
	}
#else
	(void)rgba_source;
	(void)bgra_dest;
	(void)width;
	(void)height;
	(void)bInvert; // ARM64: unreachable — guarded by m_bSSE2 && m_bSSSE3 at the call site
#endif
} // end rgba_bgra_sse3
