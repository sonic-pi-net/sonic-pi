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
#include "OscReceiveTest.h"

#include <cstdlib>
#include <cstring>
#include <iostream>

#if defined(__BORLANDC__) // workaround for BCB4 release build intrinsics bug
namespace std {
using ::__strcmp__;  // avoid error: E2316 '__strcmp__' is not a member of 'std'.
}
#endif

#include "osc/OscReceivedElements.h"

#include "ip/UdpSocket.h"
#include "osc/OscPacketListener.h"


namespace osc{

class OscReceiveTestPacketListener : public OscPacketListener{
protected:

    void ProcessMessage( const osc::ReceivedMessage& m, 
            const IpEndpointName& remoteEndpoint )
    {
        (void) remoteEndpoint; // suppress unused parameter warning

        // a more complex scheme involving std::map or some other method of
        // processing address patterns could be used here 
		// (see MessageMappingOscPacketListener.h for example). however, the main
        // purpose of this example is to illustrate and test different argument
        // parsing methods

        try {
            // argument stream, and argument iterator, used in different
            // examples below.
            ReceivedMessageArgumentStream args = m.ArgumentStream();
            ReceivedMessage::const_iterator arg = m.ArgumentsBegin();
            
            if( std::strcmp( m.AddressPattern(), "/test1" ) == 0 ){

                // example #1:
                // parse an expected format using the argument stream interface:
                bool a1;
                osc::int32 a2;
                float a3;
                const char *a4;
                args >> a1 >> a2 >> a3 >> a4 >> osc::EndMessage;

                std::cout << "received '/test1' message with arguments: "
                        << a1 << " " << a2 << " " << a3 << " " << a4 << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/test2" ) == 0 ){

                // example #2:
                // parse an expected format using the argument iterator interface
                // this is a more complicated example of doing the same thing
                // as above.
                bool a1 = (arg++)->AsBool();
                int a2 = (arg++)->AsInt32();
                float a3 = (arg++)->AsFloat();
                const char *a4 = (arg++)->AsString();
                if( arg != m.ArgumentsEnd() )
                    throw ExcessArgumentException();

                std::cout << "received '/test2' message with arguments: "
                         << a1 << " " << a2 << " " << a3 << " " << a4 << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/test3" ) == 0 ){

                // example #3:
                // parse a variable argument format using the argument iterator
                // interface. this is where it is necessary to use
                // argument iterators instead of streams.
                // When messages may contain arguments of varying type, you can
                // use the argument iterator interface to query the types at
                // runtime. this is more flexible that the argument stream
                // interface, which requires each argument to have a fixed type
                
                if( arg->IsBool() ){
                    bool a = (arg++)->AsBoolUnchecked();
                    std::cout << "received '/test3' message with bool argument: "
                        << a << "\n";
                }else if( arg->IsInt32() ){
                    int a = (arg++)->AsInt32Unchecked();
                    std::cout << "received '/test3' message with int32 argument: "
                        << a << "\n";
                }else if( arg->IsFloat() ){
                    float a = (arg++)->AsFloatUnchecked();
                    std::cout << "received '/test3' message with float argument: "
                        << a << "\n";
                }else if( arg->IsString() ){
                    const char *a = (arg++)->AsStringUnchecked();
                    std::cout << "received '/test3' message with string argument: '"
                        << a << "'\n";
                }else{
                    std::cout << "received '/test3' message with unexpected argument type\n";
                }
                
                if( arg != m.ArgumentsEnd() )
                    throw ExcessArgumentException();

                    
            }else if( std::strcmp( m.AddressPattern(), "/no_arguments" ) == 0 ){

                args >> osc::EndMessage;
                std::cout << "received '/no_arguments' message\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_bool" ) == 0 ){

                bool a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_bool' message: " << a << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/nil" ) == 0 ){

                std::cout << "received '/nil' message\n";

            }else if( std::strcmp( m.AddressPattern(), "/inf" ) == 0 ){

                std::cout << "received '/inf' message\n";

            }else if( std::strcmp( m.AddressPattern(), "/an_int" ) == 0 ){

                osc::int32 a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/an_int' message: " << a << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_float" ) == 0 ){

                float a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_float' message: " << a << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_char" ) == 0 ){

                char a;
                args >> a >> osc::EndMessage;
                char s[2] = {0};
                s[0] = a;
                std::cout << "received '/a_char' message: '" << s << "'\n";

            }else if( std::strcmp( m.AddressPattern(), "/an_rgba_color" ) == 0 ){

                osc::RgbaColor a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/an_rgba_color' message: " << a.value << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_midi_message" ) == 0 ){

                osc::MidiMessage a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_midi_message' message: " << a.value << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/an_int64" ) == 0 ){

                osc::int64 a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/an_int64' message: " << a << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_time_tag" ) == 0 ){

                osc::TimeTag a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_time_tag' message: " << a.value << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_double" ) == 0 ){

                double a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_double' message: " << a << "\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_string" ) == 0 ){

                const char *a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_string' message: '" << a << "'\n";

            }else if( std::strcmp( m.AddressPattern(), "/a_symbol" ) == 0 ){

                osc::Symbol a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_symbol' message: '" << a.value << "'\n";

             }else if( std::strcmp( m.AddressPattern(), "/a_blob" ) == 0 ){

                osc::Blob a;
                args >> a >> osc::EndMessage;
                std::cout << "received '/a_blob' message\n";

            }else{
                std::cout << "unrecognised address pattern: "
                        << m.AddressPattern() << "\n";
            }

        }catch( Exception& e ){
            std::cout << "error while parsing message: "
                        << m.AddressPattern() << ": " << e.what() << "\n";
        }
    }    
};


void RunReceiveTest( int port )
{
    osc::OscReceiveTestPacketListener listener;
	UdpListeningReceiveSocket s(
            IpEndpointName( IpEndpointName::ANY_ADDRESS, port ),
            &listener );

	std::cout << "listening for input on port " << port << "...\n";
	std::cout << "press ctrl-c to end\n";

	s.RunUntilSigInt();

	std::cout << "finishing.\n";
}

} // namespace osc

#ifndef NO_OSC_TEST_MAIN

int main(int argc, char* argv[])
{
	if( argc >= 2 && std::strcmp( argv[1], "-h" ) == 0 ){
        std::cout << "usage: OscReceiveTest [port]\n";
        return 0;
    }

	int port = 7000;

	if( argc >= 2 )
		port = std::atoi( argv[1] );

    osc::RunReceiveTest( port );

    return 0;
}

#endif /* NO_OSC_TEST_MAIN */

