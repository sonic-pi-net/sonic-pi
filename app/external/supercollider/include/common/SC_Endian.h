/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
    http://www.audiosynth.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

/* NOTE: This file should declare/define the following functions/macros:

    sc_htonl
    sc_htons
    sc_ntohl
    sc_ntohs

*/

#pragma once

#if defined(__APPLE__)

#    include <machine/endian.h>

#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)

#    include <sys/types.h>
#    include <netinet/in.h>

#elif defined(_WIN32)

#    define LITTLE_ENDIAN 1234
#    define BIG_ENDIAN 4321
#    define BYTE_ORDER LITTLE_ENDIAN

#    define SC_NO_ENDIAN_FUNCTIONS

#elif defined(__linux__)

#    include <endian.h>
#    include <netinet/in.h>

#else

#    error cannot find endianess on this platform

#endif


#ifndef SC_NO_ENDIAN_FUNCTIONS

static inline unsigned int sc_htonl(unsigned int arg) { return htonl(arg); }

static inline unsigned short sc_htons(unsigned short arg) { return htons(arg); }

static inline unsigned int sc_ntohl(unsigned int arg) { return ntohl(arg); }

static inline unsigned short sc_ntohs(unsigned short arg) { return ntohs(arg); }

#else

static inline unsigned int sc_htonl(unsigned int x) {
#    if BYTE_ORDER == LITTLE_ENDIAN
    unsigned char* s = (unsigned char*)&x;
    return (unsigned int)(s[0] << 24 | s[1] << 16 | s[2] << 8 | s[3]);
#    else
    return x;
#    endif
}

static inline unsigned short sc_htons(unsigned short x) {
#    if BYTE_ORDER == LITTLE_ENDIAN
    unsigned char* s = (unsigned char*)&x;
    return (unsigned short)(s[0] << 8 | s[1]);
#    else
    return x;
#    endif
}

static inline unsigned int sc_ntohl(unsigned int x) { return sc_htonl(x); }

static inline unsigned short sc_ntohs(unsigned short x) { return sc_htons(x); }

#endif


#ifndef BYTE_ORDER
#    error BYTE_ORDER undefined, check __FILE__
#endif // BYTE_ORDER

#ifndef BIG_ENDIAN
#    error BIG_ENDIAN undefined, check __FILE__
#endif // BIG_ENDIAN

#ifndef LITTLE_ENDIAN
#    error LITTLE_ENDIAN undefined, check __FILE__
#endif // LITTLE_ENDIAN
