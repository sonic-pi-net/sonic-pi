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
#ifndef INCLUDED_OSCPACK_IPENDPOINTNAME_H
#define INCLUDED_OSCPACK_IPENDPOINTNAME_H


class IpEndpointName{
    static unsigned long GetHostByName( const char *s );
public:
    static const unsigned long ANY_ADDRESS = 0xFFFFFFFF;
    static const int ANY_PORT = -1;

    IpEndpointName()
		: address( ANY_ADDRESS ), port( ANY_PORT ) {}
    IpEndpointName( int port_ ) 
		: address( ANY_ADDRESS ), port( port_ ) {}
    IpEndpointName( unsigned long ipAddress_, int port_ ) 
		: address( ipAddress_ ), port( port_ ) {}
    IpEndpointName( const char *addressName, int port_=ANY_PORT )
		: address( GetHostByName( addressName ) )
		, port( port_ ) {}
    IpEndpointName( int addressA, int addressB, int addressC, int addressD, int port_=ANY_PORT )
		: address( ( (addressA << 24) | (addressB << 16) | (addressC << 8) | addressD ) )
		, port( port_ ) {}

	// address and port are maintained in host byte order here
    unsigned long address;
    int port;

    bool IsMulticastAddress() const { return ((address >> 24) & 0xFF) >= 224 && ((address >> 24) & 0xFF) <= 239; }

	enum { ADDRESS_STRING_LENGTH=17 };
	void AddressAsString( char *s ) const;

	enum { ADDRESS_AND_PORT_STRING_LENGTH=23};
	void AddressAndPortAsString( char *s ) const;
};

inline bool operator==( const IpEndpointName& lhs, const IpEndpointName& rhs )
{	
	return (lhs.address == rhs.address && lhs.port == rhs.port );
}

inline bool operator!=( const IpEndpointName& lhs, const IpEndpointName& rhs )
{
	return !(lhs == rhs);
}

#endif /* INCLUDED_OSCPACK_IPENDPOINTNAME_H */
