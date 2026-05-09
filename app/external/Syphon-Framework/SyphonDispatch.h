/*
	SyphonDispatch.h
	Syphon
 
    Copyright 2010-2011 bangnoise (Tom Butterworth) & vade (Anton Marini).
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

/*
 Syphon Dispatch handles performing a task in the background such that
 
 - Tasks from a single source happen serially
 - Threads are shared where possible
 - Threads calling Syphon Dispatch never block waiting for background tasks
 
 Why not just use dispatch_queues?
 
 - Syphon Dispatch uses fewer threads
 - dispatch_async must copy a block every time it is called, Syphon Dispatch can re-use the same block.
 
*/

/*
 SyphonDispatchSourceRef
	Opaque reference to a dispatch source.
 */
typedef struct SyphonDispatchSource *SyphonDispatchSourceRef;

/*
 SyphonDispatchSourceCreate
	Creates a new dispatch source using the supplied block, which takes no arguments and returns no value.
 */
SyphonDispatchSourceRef SyphonDispatchSourceCreate(void (^block)(void));

/*
 SyphonDispatchSourceSetCompletionBlock
	Sets a block to be invoked after the last reference to the source is released and all firings have been executed.
	The provided block takes no arguments and returns no value.
 */
void SyphonDispatchSourceSetCompletionBlock(SyphonDispatchSourceRef source, void (^block)(void));

/*
 SyphonDispatchSourceRetain
	Retain
 */
SyphonDispatchSourceRef SyphonDispatchSourceRetain(SyphonDispatchSourceRef source);

/*
 SyphonDispatchSourceRelease
	Release
 */
void SyphonDispatchSourceRelease(SyphonDispatchSourceRef source);

/*
 SyphonDispatchSourceFire
	The block passed in at creation time is invoked on a background thread.
 */
void SyphonDispatchSourceFire(SyphonDispatchSourceRef source);
