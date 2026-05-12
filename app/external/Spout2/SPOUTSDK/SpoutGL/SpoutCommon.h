//
// Header: SpoutCommon.h
//
// Enables build of the SDK as a DLL.
//
// Includes header for common utilities namespace "SpoutUtils".
//
// Optional _#define legacyOpenGL_ to enable legacy draw functions
//

/*
		Thanks and credit to Malcolm Bechard, the author of this file
		https://github.com/mbechard

		Copyright (c) 2014-2023, Lynn Jarvis. All rights reserved.

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

#ifndef __SpoutCommon__
#define __SpoutCommon__

//
// To build the Spout library as a dll, define
// SPOUT_BUILD_DLL in the preprocessor defines.
// Properties > C++ > Preprocessor > Preprocessor Definitions
//
#if defined(_MSC_VER)
	#if defined(SPOUT_BUILD_DLL)
		#define SPOUT_DLLEXP	__declspec(dllexport)
	#elif defined(SPOUT_IMPORT_DLL)
		#define SPOUT_DLLEXP	__declspec(dllimport)
	#else
		#define SPOUT_DLLEXP
	#endif
#else // _MSC_VER
	#define SPOUT_DLLEXP
#endif // _MSC_VERR

// Common utility functions namespace
#include "SpoutUtils.h"

//
// This definition enables legacy OpenGL rendering code
// used for shared texture Draw functions in SpoutGLDXinterop.cpp
// Not required unless compatibility with OpenGL < 3 is necessary
// Disabled by default for OpenGL 4 compliance
// * Note that the same definition is necessary in SpoutGLextensions.h
//   so that SpoutGLextensions can be used independently of the Spout library.
//
// #define legacyOpenGL
//

//
// Visual Studio code analysis warnings
//

// C++11 scoped (class) enums are not compatible with early compilers (< VS2012 and others).
// The warning is designated "Prefer" and "C" standard unscoped enums are retained for compatibility.
#if defined(_MSC_VER)
#pragma warning(disable:26812) // unscoped enums
#endif


#endif
