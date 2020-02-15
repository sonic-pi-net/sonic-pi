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
#include "OscReceivedElements.h"

#include "OscHostEndianness.h"

#include <cstddef> // ptrdiff_t

namespace osc{


// return the first 4 byte boundary after the end of a str4
// be careful about calling this version if you don't know whether
// the string is terminated correctly.
static inline const char* FindStr4End( const char *p )
{
	if( p[0] == '\0' )    // special case for SuperCollider integer address pattern
		return p + 4;

    p += 3;

    while( *p )
        p += 4;

    return p + 1;
}


// return the first 4 byte boundary after the end of a str4
// returns 0 if p == end or if the string is unterminated
static inline const char* FindStr4End( const char *p, const char *end )
{
    if( p >= end )
        return 0;

	if( p[0] == '\0' )    // special case for SuperCollider integer address pattern
		return p + 4;

    p += 3;
    end -= 1;

    while( p < end && *p )
        p += 4;

    if( *p )
        return 0;
    else
        return p + 1;
}


// round up to the next highest multiple of 4. unless x is already a multiple of 4
static inline uint32 RoundUp4( uint32 x ) 
{
    return (x + 3) & ~((uint32)0x03);
}


static inline int32 ToInt32( const char *p )
{
#ifdef OSC_HOST_LITTLE_ENDIAN
    union{
        osc::int32 i;
        char c[4];
    } u;

    u.c[0] = p[3];
    u.c[1] = p[2];
    u.c[2] = p[1];
    u.c[3] = p[0];

    return u.i;
#else
	return *(int32*)p;
#endif
}


static inline uint32 ToUInt32( const char *p )
{
#ifdef OSC_HOST_LITTLE_ENDIAN
    union{
        osc::uint32 i;
        char c[4];
    } u;

    u.c[0] = p[3];
    u.c[1] = p[2];
    u.c[2] = p[1];
    u.c[3] = p[0];

    return u.i;
#else
	return *(uint32*)p;
#endif
}


static inline int64 ToInt64( const char *p )
{
#ifdef OSC_HOST_LITTLE_ENDIAN
    union{
        osc::int64 i;
        char c[8];
    } u;

    u.c[0] = p[7];
    u.c[1] = p[6];
    u.c[2] = p[5];
    u.c[3] = p[4];
    u.c[4] = p[3];
    u.c[5] = p[2];
    u.c[6] = p[1];
    u.c[7] = p[0];

    return u.i;
#else
	return *(int64*)p;
#endif
}


static inline uint64 ToUInt64( const char *p )
{
#ifdef OSC_HOST_LITTLE_ENDIAN
    union{
        osc::uint64 i;
        char c[8];
    } u;

    u.c[0] = p[7];
    u.c[1] = p[6];
    u.c[2] = p[5];
    u.c[3] = p[4];
    u.c[4] = p[3];
    u.c[5] = p[2];
    u.c[6] = p[1];
    u.c[7] = p[0];

    return u.i;
#else
	return *(uint64*)p;
#endif
}

//------------------------------------------------------------------------------

bool ReceivedPacket::IsBundle() const
{
    return (Size() > 0 && Contents()[0] == '#');
}

//------------------------------------------------------------------------------

bool ReceivedBundleElement::IsBundle() const
{
    return (Size() > 0 && Contents()[0] == '#');
}


osc_bundle_element_size_t ReceivedBundleElement::Size() const
{
    return ToInt32( sizePtr_ );
}

//------------------------------------------------------------------------------

bool ReceivedMessageArgument::AsBool() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == TRUE_TYPE_TAG )
		return true;
	else if( *typeTagPtr_ == FALSE_TYPE_TAG )
		return false;
	else
		throw WrongArgumentTypeException();
}


bool ReceivedMessageArgument::AsBoolUnchecked() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == TRUE_TYPE_TAG )
		return true;
    else
	    return false;
}


int32 ReceivedMessageArgument::AsInt32() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == INT32_TYPE_TAG )
		return AsInt32Unchecked();
	else
		throw WrongArgumentTypeException();
}


int32 ReceivedMessageArgument::AsInt32Unchecked() const
{
#ifdef OSC_HOST_LITTLE_ENDIAN
    union{
        osc::int32 i;
        char c[4];
    } u;

    u.c[0] = argumentPtr_[3];
    u.c[1] = argumentPtr_[2];
    u.c[2] = argumentPtr_[1];
    u.c[3] = argumentPtr_[0];

    return u.i;
#else
	return *(int32*)argument_;
#endif
}


float ReceivedMessageArgument::AsFloat() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == FLOAT_TYPE_TAG )
		return AsFloatUnchecked();
	else
		throw WrongArgumentTypeException();
}


float ReceivedMessageArgument::AsFloatUnchecked() const
{
#ifdef OSC_HOST_LITTLE_ENDIAN
    union{
        float f;
        char c[4];
    } u;

    u.c[0] = argumentPtr_[3];
    u.c[1] = argumentPtr_[2];
    u.c[2] = argumentPtr_[1];
    u.c[3] = argumentPtr_[0];

    return u.f;
#else
	return *(float*)argument_;
#endif
}


char ReceivedMessageArgument::AsChar() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == CHAR_TYPE_TAG )
		return AsCharUnchecked();
	else
		throw WrongArgumentTypeException();
}


char ReceivedMessageArgument::AsCharUnchecked() const
{
    return (char)ToInt32( argumentPtr_ );
}


uint32 ReceivedMessageArgument::AsRgbaColor() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == RGBA_COLOR_TYPE_TAG )
		return AsRgbaColorUnchecked();
	else
		throw WrongArgumentTypeException();
}


uint32 ReceivedMessageArgument::AsRgbaColorUnchecked() const
{
	return ToUInt32( argumentPtr_ );
}


uint32 ReceivedMessageArgument::AsMidiMessage() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == MIDI_MESSAGE_TYPE_TAG )
		return AsMidiMessageUnchecked();
	else
		throw WrongArgumentTypeException();
}


uint32 ReceivedMessageArgument::AsMidiMessageUnchecked() const
{
	return ToUInt32( argumentPtr_ );
}


int64 ReceivedMessageArgument::AsInt64() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == INT64_TYPE_TAG )
		return AsInt64Unchecked();
	else
		throw WrongArgumentTypeException();
}


int64 ReceivedMessageArgument::AsInt64Unchecked() const
{
    return ToInt64( argumentPtr_ );
}


uint64 ReceivedMessageArgument::AsTimeTag() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == TIME_TAG_TYPE_TAG )
		return AsTimeTagUnchecked();
	else
		throw WrongArgumentTypeException();
}


uint64 ReceivedMessageArgument::AsTimeTagUnchecked() const
{
    return ToUInt64( argumentPtr_ );
}


double ReceivedMessageArgument::AsDouble() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == DOUBLE_TYPE_TAG )
		return AsDoubleUnchecked();
	else
		throw WrongArgumentTypeException();
}


double ReceivedMessageArgument::AsDoubleUnchecked() const
{
#ifdef OSC_HOST_LITTLE_ENDIAN
    union{
        double d;
        char c[8];
    } u;

    u.c[0] = argumentPtr_[7];
    u.c[1] = argumentPtr_[6];
    u.c[2] = argumentPtr_[5];
    u.c[3] = argumentPtr_[4];
    u.c[4] = argumentPtr_[3];
    u.c[5] = argumentPtr_[2];
    u.c[6] = argumentPtr_[1];
    u.c[7] = argumentPtr_[0];

    return u.d;
#else
	return *(double*)argument_;
#endif
}


const char* ReceivedMessageArgument::AsString() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == STRING_TYPE_TAG )
		return argumentPtr_;
	else
		throw WrongArgumentTypeException();
}


const char* ReceivedMessageArgument::AsSymbol() const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == SYMBOL_TYPE_TAG )
		return argumentPtr_;
	else
		throw WrongArgumentTypeException();
}


void ReceivedMessageArgument::AsBlob( const void*& data, osc_bundle_element_size_t& size ) const
{
    if( !typeTagPtr_ )
        throw MissingArgumentException();
	else if( *typeTagPtr_ == BLOB_TYPE_TAG )
		AsBlobUnchecked( data, size );
	else
		throw WrongArgumentTypeException();
}


void ReceivedMessageArgument::AsBlobUnchecked( const void*& data, osc_bundle_element_size_t& size ) const
{
    // read blob size as an unsigned int then validate
    osc_bundle_element_size_t sizeResult = (osc_bundle_element_size_t)ToUInt32( argumentPtr_ );
    if( !IsValidElementSizeValue(sizeResult) )
        throw MalformedMessageException("invalid blob size");

    size = sizeResult;
	data = (void*)(argumentPtr_+ osc::OSC_SIZEOF_INT32);
}

std::size_t ReceivedMessageArgument::ComputeArrayItemCount() const
{
    // it is only valid to call ComputeArrayItemCount when the argument is the array start marker
    if( !IsArrayBegin() )
        throw WrongArgumentTypeException();

    std::size_t result = 0;
    unsigned int level = 0;
    const char *typeTag = typeTagPtr_ + 1;

    // iterate through all type tags. note that ReceivedMessage::Init
    // has already checked that the message is well formed.
    while( *typeTag ) {
        switch( *typeTag++ ) {
            case ARRAY_BEGIN_TYPE_TAG:
                level += 1;
                break;

            case ARRAY_END_TYPE_TAG:
                if(level == 0)
                    return result;
                level -= 1;
                break;

            default:
                if( level == 0 ) // only count items at level 0
                    ++result;
        }
    }

    return result;
}

//------------------------------------------------------------------------------

void ReceivedMessageArgumentIterator::Advance()
{
    if( !value_.typeTagPtr_ )
        return;
        
    switch( *value_.typeTagPtr_++ ){
        case '\0':
            // don't advance past end
            --value_.typeTagPtr_;
            break;
            
        case TRUE_TYPE_TAG:
        case FALSE_TYPE_TAG:
        case NIL_TYPE_TAG:
        case INFINITUM_TYPE_TAG:
        
            // zero length
            break;

        case INT32_TYPE_TAG:
        case FLOAT_TYPE_TAG: 					
        case CHAR_TYPE_TAG:
        case RGBA_COLOR_TYPE_TAG:
        case MIDI_MESSAGE_TYPE_TAG:

            value_.argumentPtr_ += 4;
            break;

        case INT64_TYPE_TAG:
        case TIME_TAG_TYPE_TAG:
        case DOUBLE_TYPE_TAG:
				
            value_.argumentPtr_ += 8;
            break;

        case STRING_TYPE_TAG: 
        case SYMBOL_TYPE_TAG:

            // we use the unsafe function FindStr4End(char*) here because all of
            // the arguments have already been validated in
            // ReceivedMessage::Init() below.
            
            value_.argumentPtr_ = FindStr4End( value_.argumentPtr_ );
            break;

        case BLOB_TYPE_TAG:
            {
                // treat blob size as an unsigned int for the purposes of this calculation
                uint32 blobSize = ToUInt32( value_.argumentPtr_ );
                value_.argumentPtr_ = value_.argumentPtr_ + osc::OSC_SIZEOF_INT32 + RoundUp4( blobSize );
            }
            break;

        case ARRAY_BEGIN_TYPE_TAG:
        case ARRAY_END_TYPE_TAG: 

            //    [ Indicates the beginning of an array. The tags following are for
            //        data in the Array until a close brace tag is reached.
            //    ] Indicates the end of an array.

            // zero length, don't advance argument ptr
            break;

        default:    // unknown type tag
            // don't advance
            --value_.typeTagPtr_;
            break;
    }
}

//------------------------------------------------------------------------------

ReceivedMessage::ReceivedMessage( const ReceivedPacket& packet )
    : addressPattern_( packet.Contents() )
{
    Init( packet.Contents(), packet.Size() );
}


ReceivedMessage::ReceivedMessage( const ReceivedBundleElement& bundleElement )
    : addressPattern_( bundleElement.Contents() )
{
    Init( bundleElement.Contents(), bundleElement.Size() );
}


bool ReceivedMessage::AddressPatternIsUInt32() const
{
	return (addressPattern_[0] == '\0');
}


uint32 ReceivedMessage::AddressPatternAsUInt32() const
{
    return ToUInt32( addressPattern_ );
}


void ReceivedMessage::Init( const char *message, osc_bundle_element_size_t size )
{
    if( !IsValidElementSizeValue(size) )
        throw MalformedMessageException( "invalid message size" );

    if( size == 0 )
        throw MalformedMessageException( "zero length messages not permitted" );

    if( !IsMultipleOf4(size) )
        throw MalformedMessageException( "message size must be multiple of four" );

    const char *end = message + size;

    typeTagsBegin_ = FindStr4End( addressPattern_, end );
    if( typeTagsBegin_ == 0 ){
        // address pattern was not terminated before end
        throw MalformedMessageException( "unterminated address pattern" );
    }

    if( typeTagsBegin_ == end ){
        // message consists of only the address pattern - no arguments or type tags.
        typeTagsBegin_ = 0;
        typeTagsEnd_ = 0;
        arguments_ = 0;
            
    }else{
        if( *typeTagsBegin_ != ',' )
            throw MalformedMessageException( "type tags not present" );

        if( *(typeTagsBegin_ + 1) == '\0' ){
            // zero length type tags
            typeTagsBegin_ = 0;
            typeTagsEnd_ = 0;
            arguments_ = 0;

        }else{
            // check that all arguments are present and well formed
                
            arguments_ = FindStr4End( typeTagsBegin_, end );
            if( arguments_ == 0 ){
                throw MalformedMessageException( "type tags were not terminated before end of message" );
            }

            ++typeTagsBegin_; // advance past initial ','
            
            const char *typeTag = typeTagsBegin_;
            const char *argument = arguments_;
            unsigned int arrayLevel = 0;
                        
            do{
                switch( *typeTag ){
                    case TRUE_TYPE_TAG:
                    case FALSE_TYPE_TAG:
                    case NIL_TYPE_TAG:
                    case INFINITUM_TYPE_TAG:
                        // zero length
                        break;

                    //    [ Indicates the beginning of an array. The tags following are for
                    //        data in the Array until a close brace tag is reached.
                    //    ] Indicates the end of an array.
                    case ARRAY_BEGIN_TYPE_TAG:
                        ++arrayLevel;
                        // (zero length argument data)
                        break;

                    case ARRAY_END_TYPE_TAG:
                        --arrayLevel;
                        // (zero length argument data)
                        break;

                    case INT32_TYPE_TAG:
                    case FLOAT_TYPE_TAG:
                    case CHAR_TYPE_TAG:
                    case RGBA_COLOR_TYPE_TAG:
                    case MIDI_MESSAGE_TYPE_TAG:

                        if( argument == end )
                            throw MalformedMessageException( "arguments exceed message size" );
                        argument += 4;
                        if( argument > end )
                            throw MalformedMessageException( "arguments exceed message size" );
                        break;

                    case INT64_TYPE_TAG:
                    case TIME_TAG_TYPE_TAG:
                    case DOUBLE_TYPE_TAG:

                        if( argument == end )
                            throw MalformedMessageException( "arguments exceed message size" );
                        argument += 8;
                        if( argument > end )
                            throw MalformedMessageException( "arguments exceed message size" );
                        break;

                    case STRING_TYPE_TAG: 
                    case SYMBOL_TYPE_TAG:
                    
                        if( argument == end )
                            throw MalformedMessageException( "arguments exceed message size" );
                        argument = FindStr4End( argument, end );
                        if( argument == 0 )
                            throw MalformedMessageException( "unterminated string argument" );
                        break;

                    case BLOB_TYPE_TAG:
                        {
                            if( argument + osc::OSC_SIZEOF_INT32 > end )
                                MalformedMessageException( "arguments exceed message size" );
                                
                            // treat blob size as an unsigned int for the purposes of this calculation
                            uint32 blobSize = ToUInt32( argument );
                            argument = argument + osc::OSC_SIZEOF_INT32 + RoundUp4( blobSize );
                            if( argument > end )
                                MalformedMessageException( "arguments exceed message size" );
                        }
                        break;
                        
                    default:
                        throw MalformedMessageException( "unknown type tag" );
                }

            }while( *++typeTag != '\0' );
            typeTagsEnd_ = typeTag;

            if( arrayLevel !=  0 )
                throw MalformedMessageException( "array was not terminated before end of message (expected ']' end of array tag)" );
        }

        // These invariants should be guaranteed by the above code.
        // we depend on them in the implementation of ArgumentCount()
#ifndef NDEBUG
        std::ptrdiff_t argumentCount = typeTagsEnd_ - typeTagsBegin_;
        assert( argumentCount >= 0 );
        assert( argumentCount <= OSC_INT32_MAX );
#endif
    }
}

//------------------------------------------------------------------------------

ReceivedBundle::ReceivedBundle( const ReceivedPacket& packet )
    : elementCount_( 0 )
{
    Init( packet.Contents(), packet.Size() );
}


ReceivedBundle::ReceivedBundle( const ReceivedBundleElement& bundleElement )
    : elementCount_( 0 )
{
    Init( bundleElement.Contents(), bundleElement.Size() );
}


void ReceivedBundle::Init( const char *bundle, osc_bundle_element_size_t size )
{

    if( !IsValidElementSizeValue(size) )
        throw MalformedBundleException( "invalid bundle size" );

    if( size < 16 )
        throw MalformedBundleException( "packet too short for bundle" );

    if( !IsMultipleOf4(size) )
        throw MalformedBundleException( "bundle size must be multiple of four" );

    if( bundle[0] != '#'
        || bundle[1] != 'b'
        || bundle[2] != 'u'
        || bundle[3] != 'n'
        || bundle[4] != 'd'
        || bundle[5] != 'l'
        || bundle[6] != 'e'
        || bundle[7] != '\0' )
            throw MalformedBundleException( "bad bundle address pattern" );    

    end_ = bundle + size;

    timeTag_ = bundle + 8;

    const char *p = timeTag_ + 8;
        
    while( p < end_ ){
        if( p + osc::OSC_SIZEOF_INT32 > end_ )
            throw MalformedBundleException( "packet too short for elementSize" );

        // treat element size as an unsigned int for the purposes of this calculation
        uint32 elementSize = ToUInt32( p );
        if( (elementSize & ((uint32)0x03)) != 0 )
            throw MalformedBundleException( "bundle element size must be multiple of four" );

        p += osc::OSC_SIZEOF_INT32 + elementSize;
        if( p > end_ )
            throw MalformedBundleException( "packet too short for bundle element" );

        ++elementCount_;
    }

    if( p != end_ )
        throw MalformedBundleException( "bundle contents " );
}


uint64 ReceivedBundle::TimeTag() const
{
    return ToUInt64( timeTag_ );
}


} // namespace osc

