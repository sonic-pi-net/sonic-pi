/*
    SyphonMessageSender.m
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

#import "SyphonMessageSender.h"
#import "SyphonMessaging.h"
#import "SyphonCFMessageSender.h"
//#import "SyphonMachMessageSender.h"

@interface SyphonMessageSender ()
@property (readwrite, atomic) BOOL isValid;
@end

@implementation SyphonMessageSender
{
@private
    NSString *_name;
    void (^_handler)(void);
}

- (id)initForName:(NSString *)name protocol:(NSString *)protocolName invalidationHandler:(void (^)(void))handler;
{
    self = [super init];
	if (self)
	{
		if ([self class] == [SyphonMessageSender class])
		{
            if ([protocolName isEqualToString:SyphonMessagingProtocolCFMessage])
			{
                return [[SyphonCFMessageSender alloc] initForName:name protocol:protocolName invalidationHandler:handler];
            }
			else
			{
			    return nil;
            }        
        }
		else
		{
			// SyphonMessageSender init here
			_handler = [handler copy];
			_name = [name copy];
            _isValid = YES;
		}
	}
	return self;
}


- (NSString *)name
{
	return _name;
}

- (void)send:(id <NSCoding>)payload ofType:(uint32_t)type
{
	// subclasses override this
}

- (void)invalidate
{
    self.isValid = NO;
	if (_handler != nil)
	{
		_handler();
	}
}
@end
