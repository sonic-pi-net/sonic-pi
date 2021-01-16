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
#ifndef INCLUDED_OSCPACK_MESSAGEMAPPINGOSCPACKETLISTENER_H
#define INCLUDED_OSCPACK_MESSAGEMAPPINGOSCPACKETLISTENER_H

#include <cstring>
#include <map>

#include "OscPacketListener.h"



namespace osc{

template< class T >
class MessageMappingOscPacketListener : public OscPacketListener{
public:
    typedef void (T::*function_type)(const osc::ReceivedMessage&, const IpEndpointName&);

protected:
    void RegisterMessageFunction( const char *addressPattern, function_type f )
    {
        functions_.insert( std::make_pair( addressPattern, f ) );
    }

    virtual void ProcessMessage( const osc::ReceivedMessage& m,
		const IpEndpointName& remoteEndpoint )
    {
        typename function_map_type::iterator i = functions_.find( m.AddressPattern() );
        if( i != functions_.end() )
            (dynamic_cast<T*>(this)->*(i->second))( m, remoteEndpoint );
    }
    
private:
    struct cstr_compare{
        bool operator()( const char *lhs, const char *rhs ) const
            { return std::strcmp( lhs, rhs ) < 0; }
    };

    typedef std::map<const char*, function_type, cstr_compare> function_map_type;
    function_map_type functions_;
};

} // namespace osc

#endif /* INCLUDED_OSCPACK_MESSAGEMAPPINGOSCPACKETLISTENER_H */