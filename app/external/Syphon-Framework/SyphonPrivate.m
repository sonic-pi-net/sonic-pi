/*
    SyphonConstants.c
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

#import "SyphonPrivate.h"

NSString * const SyphonServerDescriptionDictionaryVersionKey = @"SyphonServerDescriptionDictionaryVersionKey";
NSString * const SyphonServerDescriptionUUIDKey = @"SyphonServerDescriptionUUIDKey";
NSString * const SyphonServerDescriptionNameKey = @"SyphonServerDescriptionNameKey";
NSString * const SyphonServerDescriptionAppNameKey = @"SyphonServerDescriptionAppNameKey";
NSString * const SyphonServerDescriptionIconKey = @"SyphonServerDescriptionIconKey";
NSString * const SyphonServerDescriptionSurfacesKey = @"SyphonServerDescriptionSurfacesKey";

NSString * const SyphonSurfaceType = @"SyphonSurfaceType";
NSString * const SyphonSurfaceTypeIOSurface = @"SyphonSurfaceTypeIOSurface";

NSString * const SyphonServerOptionIsPrivate = @"SyphonServerOptionIsPrivate";
NSString * const SyphonServerOptionAntialiasSampleCount = @"SyphonServerOptionAntialiasSampleCount";
NSString * const SyphonServerOptionDepthBufferResolution = @"SyphonServerOptionDepthBufferResolution";
NSString * const SyphonServerOptionStencilBufferResolution = @"SyphonServerOptionStencilBufferResolution";

NSString *SyphonCreateUUIDString(void)
{
	// generate UUID
	CFUUIDRef	uuidObj = CFUUIDCreate(nil);
	CFStringRef uuid = CFUUIDCreateString(nil, uuidObj);
	CFRelease(uuidObj);
	NSString *result = [[NSString alloc] initWithFormat:@"%@.%@", kSyphonIdentifier, uuid];
	CFRelease(uuid);
	return result;
}

BOOL SyphonSafeBoolGet(SyphonSafeBool *b)
{
	return (*b == 0 ? NO : YES);
}

void SyphonSafeBoolSet(SyphonSafeBool *b, BOOL value)
{
	bool result;
	int32_t new = value ? 1 : 0;
	do {
		int32_t old = *b;
        result = atomic_compare_exchange_strong(b, &old, new);
	} while (!result);
}
