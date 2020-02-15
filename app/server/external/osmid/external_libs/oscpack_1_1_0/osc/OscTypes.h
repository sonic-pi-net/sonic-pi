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
#ifndef INCLUDED_OSCPACK_OSCTYPES_H
#define INCLUDED_OSCPACK_OSCTYPES_H


namespace osc{

// basic types

#if defined(__BORLANDC__) || defined(_MSC_VER)

typedef __int64 int64;
typedef unsigned __int64 uint64;

#elif defined(__x86_64__) || defined(_M_X64)

typedef long int64;
typedef unsigned long uint64;

#else

typedef long long int64;
typedef unsigned long long uint64;

#endif



#if defined(__x86_64__) || defined(_M_X64)

typedef signed int int32;
typedef unsigned int uint32;

#else

typedef signed long int32;
typedef unsigned long uint32;

#endif


enum ValueTypeSizes{
    OSC_SIZEOF_INT32 = 4,
    OSC_SIZEOF_UINT32 = 4,
    OSC_SIZEOF_INT64 = 8,
    OSC_SIZEOF_UINT64 = 8,
};


// osc_bundle_element_size_t is used for the size of bundle elements and blobs
// the OSC spec specifies these as int32 (signed) but we ensure that they 
// are always positive since negative field sizes make no sense.

typedef int32 osc_bundle_element_size_t;

enum {
    OSC_INT32_MAX = 0x7FFFFFFF,

    // Element sizes are specified to be int32, and are always rounded up to nearest 
    // multiple of 4. Therefore their values can't be greater than 0x7FFFFFFC.
    OSC_BUNDLE_ELEMENT_SIZE_MAX = 0x7FFFFFFC
};


inline bool IsValidElementSizeValue( osc_bundle_element_size_t x )
{
    // sizes may not be negative or exceed OSC_BUNDLE_ELEMENT_SIZE_MAX
    return x >= 0 && x <= OSC_BUNDLE_ELEMENT_SIZE_MAX; 
}


inline bool IsMultipleOf4( osc_bundle_element_size_t x )
{
    return (x & ((osc_bundle_element_size_t)0x03)) == 0;
}


enum TypeTagValues {
    TRUE_TYPE_TAG = 'T',
    FALSE_TYPE_TAG = 'F',
    NIL_TYPE_TAG = 'N',
    INFINITUM_TYPE_TAG = 'I',
    INT32_TYPE_TAG = 'i',
    FLOAT_TYPE_TAG = 'f',
    CHAR_TYPE_TAG = 'c',
    RGBA_COLOR_TYPE_TAG = 'r',
    MIDI_MESSAGE_TYPE_TAG = 'm',
    INT64_TYPE_TAG = 'h',
    TIME_TAG_TYPE_TAG = 't',
    DOUBLE_TYPE_TAG = 'd',
    STRING_TYPE_TAG = 's',
    SYMBOL_TYPE_TAG = 'S',
    BLOB_TYPE_TAG = 'b',
    ARRAY_BEGIN_TYPE_TAG = '[',
    ARRAY_END_TYPE_TAG = ']'
};



// i/o manipulators used for streaming interfaces

struct BundleInitiator{
    explicit BundleInitiator( uint64 timeTag_ ) : timeTag( timeTag_ ) {}
    uint64 timeTag;
};

extern BundleInitiator BeginBundleImmediate;

inline BundleInitiator BeginBundle( uint64 timeTag=1 )
{
    return BundleInitiator(timeTag);
}


struct BundleTerminator{
};

extern BundleTerminator EndBundle;

struct BeginMessage{
    explicit BeginMessage( const char *addressPattern_ ) : addressPattern( addressPattern_ ) {}
    const char *addressPattern;
};

struct MessageTerminator{
};

extern MessageTerminator EndMessage;


// osc specific types. they are defined as structs so they can be used
// as separately identifiable types with the streaming operators.

struct NilType{
};

extern NilType OscNil;

#ifndef _OBJC_OBJC_H_
extern NilType Nil; // Objective-C defines Nil. so our Nil is deprecated. use OscNil instead
#endif

struct InfinitumType{
};

extern InfinitumType Infinitum;

struct RgbaColor{
    RgbaColor() {}
    explicit RgbaColor( uint32 value_ ) : value( value_ ) {}
    uint32 value;

    operator uint32() const { return value; }
};


struct MidiMessage{
    MidiMessage() {}
    explicit MidiMessage( uint32 value_ ) : value( value_ ) {}
    uint32 value;

    operator uint32() const { return value; }
};


struct TimeTag{
    TimeTag() {}
    explicit TimeTag( uint64 value_ ) : value( value_ ) {}
    uint64 value;

    operator uint64() const { return value; }
};


struct Symbol{
    Symbol() {}
    explicit Symbol( const char* value_ ) : value( value_ ) {}
    const char* value;

    operator const char *() const { return value; }
};


struct Blob{
    Blob() {}
    explicit Blob( const void* data_, osc_bundle_element_size_t size_ )
            : data( data_ ), size( size_ ) {}
    const void* data;
    osc_bundle_element_size_t size;
};

struct ArrayInitiator{
};

extern ArrayInitiator BeginArray;

struct ArrayTerminator{
};

extern ArrayTerminator EndArray;

} // namespace osc


#endif /* INCLUDED_OSCPACK_OSCTYPES_H */
