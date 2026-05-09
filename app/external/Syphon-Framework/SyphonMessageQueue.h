/*
	SyphonMessageQueue.h
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

#import <Foundation/Foundation.h>

/*
 
 SyphonMessageQueue
 
 Queues messages (typically for dequeing and delivery on a seperate thread, and to that end is thread-safe).
 Coalesces pending messages - messages of the same type are removed when another message of that type is queued.
 
 */

@interface SyphonMessageQueue : NSObject

- (void)queue:(NSData *)content ofType:(uint32_t)type;
/*
 - (BOOL)copyAndDequeue:(NSData **)content; type:(uint32_t *)type
	The values of content and type will be set to those of the message from the front of the queue.
	You are responsible for releasing the content NSData if present.
	If no message was queued, the result will be NO.
 */
- (BOOL)copyAndDequeue:(NSData **)content type:(uint32_t *)type;

/*
 - (void *)userInfo
	Attach any custom info to the queue
 */
@property (readwrite, assign) void *userInfo;

@end
