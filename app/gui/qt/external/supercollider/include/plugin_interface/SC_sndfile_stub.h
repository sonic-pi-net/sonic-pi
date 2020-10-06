/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/*
** SC_sndfile_stub.h -- stub of libsndfile definitions, used to preserve binary compatibility when libsndfile
*unavailable
** taken from sndfile.h
**/


/* A SNDFILE* pointer can be passed around much like stdio.h's FILE* pointer. */

typedef struct SNDFILE_tag SNDFILE;


#if (defined(_MSCVER) || defined(_MSC_VER))
typedef __int64_t sf_count_t;
#    define SF_COUNT_MAX 0x7fffffffffffffffi64
#else
typedef off_t sf_count_t;
#    define SF_COUNT_MAX 0x7FFFFFFFFFFFFFFFLL
#endif


/* A pointer to a SF_INFO structure is passed to sf_open_read () and filled in.
** On write, the SF_INFO structure is filled in by the user and passed into
** sf_open_write ().
*/

struct SF_INFO {
    sf_count_t frames; /* Used to be called samples.  Changed to avoid confusion. */
    int samplerate;
    int channels;
    int format;
    int sections;
    int seekable;
};

typedef struct SF_INFO SF_INFO;
