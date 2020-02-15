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
#ifndef INCLUDED_OSCPACK_UDPSOCKET_H
#define INCLUDED_OSCPACK_UDPSOCKET_H

#include <cstring> // size_t

#include "NetworkingUtils.h"
#include "IpEndpointName.h"


class PacketListener;
class TimerListener;

class UdpSocket;

class SocketReceiveMultiplexer{
    class Implementation;
    Implementation *impl_;

	friend class UdpSocket;

public:
    SocketReceiveMultiplexer();
    ~SocketReceiveMultiplexer();

	// only call the attach/detach methods _before_ calling Run

    // only one listener per socket, each socket at most once
    void AttachSocketListener( UdpSocket *socket, PacketListener *listener );
    void DetachSocketListener( UdpSocket *socket, PacketListener *listener );

    void AttachPeriodicTimerListener( int periodMilliseconds, TimerListener *listener );
	void AttachPeriodicTimerListener(
            int initialDelayMilliseconds, int periodMilliseconds, TimerListener *listener );
    void DetachPeriodicTimerListener( TimerListener *listener );  

    void Run();      // loop and block processing messages indefinitely
	void RunUntilSigInt();
    void Break();    // call this from a listener to exit once the listener returns
    void AsynchronousBreak(); // call this from another thread or signal handler to exit the Run() state
};


class UdpSocket{
    class Implementation;
    Implementation *impl_;
    
	friend class SocketReceiveMultiplexer::Implementation;
    
public:

	// Ctor throws std::runtime_error if there's a problem
	// initializing the socket.
	UdpSocket();
	virtual ~UdpSocket();

	// Enable broadcast addresses (e.g. x.x.x.255)
	// Sets SO_BROADCAST socket option.
	void SetEnableBroadcast( bool enableBroadcast );

	// Enable multiple listeners for a single port on same 
	// network interface*
	// Sets SO_REUSEADDR (also SO_REUSEPORT on OS X).
	// [*] The exact behavior of SO_REUSEADDR and 
	// SO_REUSEPORT is undefined for some common cases 
	// and may have drastically different behavior on different
	// operating systems.
	void SetAllowReuse( bool allowReuse );


	// The socket is created in an unbound, unconnected state
	// such a socket can only be used to send to an arbitrary
	// address using SendTo(). To use Send() you need to first
	// connect to a remote endpoint using Connect(). To use
	// ReceiveFrom you need to first bind to a local endpoint
	// using Bind().

	// Retrieve the local endpoint name when sending to 'to'
	IpEndpointName LocalEndpointFor( const IpEndpointName& remoteEndpoint ) const;

	// Connect to a remote endpoint which is used as the target
	// for calls to Send()
	void Connect( const IpEndpointName& remoteEndpoint );	
	void Send( const char *data, std::size_t size );
    void SendTo( const IpEndpointName& remoteEndpoint, const char *data, std::size_t size );


	// Bind a local endpoint to receive incoming data. Endpoint
	// can be 'any' for the system to choose an endpoint
	void Bind( const IpEndpointName& localEndpoint );
	bool IsBound() const;

    std::size_t ReceiveFrom( IpEndpointName& remoteEndpoint, char *data, std::size_t size );
};


// convenience classes for transmitting and receiving
// they just call Connect and/or Bind in the ctor.
// note that you can still use a receive socket
// for transmitting etc

class UdpTransmitSocket : public UdpSocket{
public:
	UdpTransmitSocket( const IpEndpointName& remoteEndpoint )
		{ Connect( remoteEndpoint ); }
};


class UdpReceiveSocket : public UdpSocket{
public:
	UdpReceiveSocket( const IpEndpointName& localEndpoint )
		{ Bind( localEndpoint ); }
};


// UdpListeningReceiveSocket provides a simple way to bind one listener
// to a single socket without having to manually set up a SocketReceiveMultiplexer

class UdpListeningReceiveSocket : public UdpSocket{
    SocketReceiveMultiplexer mux_;
    PacketListener *listener_;
public:
	UdpListeningReceiveSocket( const IpEndpointName& localEndpoint, PacketListener *listener )
        : listener_( listener )
    {
        Bind( localEndpoint );
        mux_.AttachSocketListener( this, listener_ );
    }

    ~UdpListeningReceiveSocket()
        { mux_.DetachSocketListener( this, listener_ ); }

    // see SocketReceiveMultiplexer above for the behaviour of these methods...
    void Run() { mux_.Run(); }
	void RunUntilSigInt() { mux_.RunUntilSigInt(); }
    void Break() { mux_.Break(); }
    void AsynchronousBreak() { mux_.AsynchronousBreak(); }
};


#endif /* INCLUDED_OSCPACK_UDPSOCKET_H */
