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

/*
    OscDump prints incoming OSC packets. Unlike the Berkeley dumposc program
    OscDump uses a different printing format which indicates the type of each
    message argument.
*/


#include <iostream>
#include <cstring>
#include <cstdlib>

#if defined(__BORLANDC__) // workaround for BCB4 release build intrinsics bug
namespace std {
using ::__strcmp__;  // avoid error: E2316 '__strcmp__' is not a member of 'std'.
}
#endif

#include "osc/OscReceivedElements.h"
#include "osc/OscPrintReceivedElements.h"

#include "ip/UdpSocket.h"
#include "ip/PacketListener.h"


class OscDumpPacketListener : public PacketListener{
public:
	virtual void ProcessPacket( const char *data, int size, 
			const IpEndpointName& remoteEndpoint )
	{
        (void) remoteEndpoint; // suppress unused parameter warning

		std::cout << osc::ReceivedPacket( data, size );
	}
};

int main(int argc, char* argv[])
{
	if( argc >= 2 && std::strcmp( argv[1], "-h" ) == 0 ){
        std::cout << "usage: OscDump [port]\n";
        return 0;
    }

	int port = 7000;

	if( argc >= 2 )
		port = std::atoi( argv[1] );

	OscDumpPacketListener listener;
    UdpListeningReceiveSocket s(
            IpEndpointName( IpEndpointName::ANY_ADDRESS, port ),
            &listener );

	std::cout << "listening for input on port " << port << "...\n";
	std::cout << "press ctrl-c to end\n";

	s.RunUntilSigInt();

	std::cout << "finishing.\n";	

    return 0;
}


