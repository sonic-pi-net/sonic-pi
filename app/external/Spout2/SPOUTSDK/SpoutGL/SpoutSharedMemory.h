/*

	SpoutSharedMemory.h
	
	Thanks and credit to Malcolm Bechard the author of this class

	https://github.com/mbechard

	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

#ifndef __SpoutSharedMemory_ // standard way as well
#define __SpoutSharedMemory_

#include "SpoutCommon.h"
#include <windowsx.h>
#include <d3d9.h>
#include <wingdi.h>

using namespace spoututils;

//
// Result of memory segment creation
//
enum SpoutCreateResult {
	SPOUT_CREATE_FAILED = 0,
	SPOUT_CREATE_SUCCESS,
	SPOUT_ALREADY_EXISTS,
	SPOUT_ALREADY_CREATED,
};

class SPOUT_DLLEXP SpoutSharedMemory {

public:

	SpoutSharedMemory();
	~SpoutSharedMemory();

	// Create a new memory segment, or attach to an existing one
	SpoutCreateResult Create(const char* name, int size);

	// Open an existing memory map
	bool Open(const char* name);

	// Close a map
	void Close();

	// Lock an open map and return the buffer
	char* Lock();

	// Unlock a map
	void Unlock();

	// Name of an existing map
	const char* Name();
	
	// Size of an existing map
	int Size();

	// Print map information for debugging
	void Debug();

private:

	char*  m_pBuffer; // Buffer pointer
	HANDLE m_hMap; // Map handle
	HANDLE m_hMutex; // Mutex for map access
	int m_lockCount; // Map access lock count
	char* m_pName; // Map name
	int m_size; // Map size

};

#endif
