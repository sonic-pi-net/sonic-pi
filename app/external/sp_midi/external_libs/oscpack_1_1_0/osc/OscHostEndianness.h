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
#ifndef INCLUDED_OSCPACK_OSCHOSTENDIANNESS_H
#define INCLUDED_OSCPACK_OSCHOSTENDIANNESS_H

/*
    Make sure either OSC_HOST_LITTLE_ENDIAN or OSC_HOST_BIG_ENDIAN is defined

    We try to use preprocessor symbols to deduce the host endianness.

    Alternatively you can define one of the above symbols from the command line.
    Usually you do this with the -D flag to the compiler. e.g.:

    $ g++ -DOSC_HOST_LITTLE_ENDIAN ...
*/

#if defined(OSC_HOST_LITTLE_ENDIAN) || defined(OSC_HOST_BIG_ENDIAN)

// endianness defined on the command line. nothing to do here.

#elif defined(__WIN32__) || defined(WIN32) || defined(WINCE)

// assume that __WIN32__ is only defined on little endian systems

#define OSC_HOST_LITTLE_ENDIAN 1
#undef OSC_HOST_BIG_ENDIAN

#elif defined(__APPLE__)

#if defined(__LITTLE_ENDIAN__)

#define OSC_HOST_LITTLE_ENDIAN 1
#undef OSC_HOST_BIG_ENDIAN

#elif defined(__BIG_ENDIAN__)

#define OSC_HOST_BIG_ENDIAN 1
#undef OSC_HOST_LITTLE_ENDIAN

#endif

#elif defined(__BYTE_ORDER__) && defined(__ORDER_LITTLE_ENDIAN__) && defined(__ORDER_BIG_ENDIAN__)

// should cover gcc and clang

#if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)

#define OSC_HOST_LITTLE_ENDIAN 1
#undef OSC_HOST_BIG_ENDIAN

#elif (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)

#define OSC_HOST_BIG_ENDIAN 1
#undef OSC_HOST_LITTLE_ENDIAN

#endif

#else

// gcc defines __LITTLE_ENDIAN__ and __BIG_ENDIAN__
// for others used here see http://sourceforge.net/p/predef/wiki/Endianness/
#if (defined(__LITTLE_ENDIAN__) && !defined(__BIG_ENDIAN__)) \
    || (defined(__ARMEL__) && !defined(__ARMEB__)) \
    || (defined(__AARCH64EL__) && !defined(__AARCH64EB__)) \
    || (defined(_MIPSEL) && !defined(_MIPSEB)) \
    || (defined(__MIPSEL) && !defined(__MIPSEB)) \
    || (defined(__MIPSEL__) && !defined(__MIPSEB__))

#define OSC_HOST_LITTLE_ENDIAN 1
#undef OSC_HOST_BIG_ENDIAN

#elif (defined(__BIG_ENDIAN__) && !defined(__LITTLE_ENDIAN__)) \
    || (defined(__ARMEB__) && !defined(__ARMEL__)) \
    || (defined(__AARCH64EB__) && !defined(__AARCH64EL__)) \
    || (defined(_MIPSEB) && !defined(_MIPSEL)) \
    || (defined(__MIPSEB) && !defined(__MIPSEL)) \
    || (defined(__MIPSEB__) && !defined(__MIPSEL__))

#define OSC_HOST_BIG_ENDIAN 1
#undef OSC_HOST_LITTLE_ENDIAN

#endif

#endif

#if !defined(OSC_HOST_LITTLE_ENDIAN) && !defined(OSC_HOST_BIG_ENDIAN)

#error please edit OSCHostEndianness.h or define one of {OSC_HOST_LITTLE_ENDIAN, OSC_HOST_BIG_ENDIAN} to configure endianness

#endif

#endif /* INCLUDED_OSCPACK_OSCHOSTENDIANNESS_H */

