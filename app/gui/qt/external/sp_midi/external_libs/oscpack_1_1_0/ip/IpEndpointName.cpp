/*
	oscpack -- Open Sound Control (OSC) packet manipulation library
    http://www.rossbencina.com/code/oscpack

    Copyright (c) 2004-2013 Ross Bencina <rossb@audiomulch.com>

	Permission is hereby granted, free of charge, to any person obtaining
	a copy of this software and associated documentation files
	(the "Software"), to deal in the Software without restriction,
	including without limitation the rights to use, copy, modify, merge,
	publish, distribute, sublicense, and/or sell copies of the Software,
	and to permit persons to whom the Software is furnished to do so,
	subject to the following conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
	ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
	CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
	WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
	The text above constitutes the entire oscpack license; however, 
	the oscpack developer(s) also make the following non-binding requests:

	Any person wishing to distribute modifications to the Software is
	requested to send the modifications to the original developer so that
	they can be incorporated into the canonical version. It is also 
	requested that these non-binding requests be included whenever the
	above license is reproduced.
*/
#include "IpEndpointName.h"

#include <cstdio>

#include "NetworkingUtils.h"


unsigned long IpEndpointName::GetHostByName( const char *s )
{
	return ::GetHostByName(s);
}


void IpEndpointName::AddressAsString( char *s ) const
{
	if( address == ANY_ADDRESS ){
		std::sprintf( s, "<any>" );
	}else{
		std::sprintf( s, "%d.%d.%d.%d",
				(int)((address >> 24) & 0xFF),
				(int)((address >> 16) & 0xFF),
				(int)((address >> 8) & 0xFF),
				(int)(address & 0xFF) );
	}
}


void IpEndpointName::AddressAndPortAsString( char *s ) const
{
	if( port == ANY_PORT ){
		if( address == ANY_ADDRESS ){
			std::sprintf( s, "<any>:<any>" );
		}else{
			std::sprintf( s, "%d.%d.%d.%d:<any>",
				(int)((address >> 24) & 0xFF),
				(int)((address >> 16) & 0xFF),
				(int)((address >> 8) & 0xFF),
				(int)(address & 0xFF) );
		}
	}else{
		if( address == ANY_ADDRESS ){
			std::sprintf( s, "<any>:%d", port );
		}else{
			std::sprintf( s, "%d.%d.%d.%d:%d",
				(int)((address >> 24) & 0xFF),
				(int)((address >> 16) & 0xFF),
				(int)((address >> 8) & 0xFF),
				(int)(address & 0xFF),
				(int)port );
		}
	}	
}
