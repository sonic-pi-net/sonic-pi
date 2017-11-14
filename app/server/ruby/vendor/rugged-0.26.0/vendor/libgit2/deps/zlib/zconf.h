/* zconf.h -- configuration of the zlib compression library
 * Copyright (C) 1995-2010 Jean-loup Gailly.
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

/* @(#) $Id$ */

#ifndef ZCONF_H
#define ZCONF_H

#include "../../include/git2/types.h"
#include <stdarg.h>

/* Jeez, don't complain about non-prototype
 * forms, we didn't write zlib */
#if defined(_MSC_VER)
#	pragma warning( disable : 4131 )
#	pragma warning( disable : 4142 ) /* benign redefinition of type */
#endif

/* Maximum value for memLevel in deflateInit2 */
#define MAX_MEM_LEVEL 9

/* Maximum value for windowBits in deflateInit2 and inflateInit2.
 * WARNING: reducing MAX_WBITS makes minigzip unable to extract .gz files
 * created by gzip. (Files created by minigzip can still be extracted by
 * gzip.)
 */
#define MAX_WBITS   15 /* 32K LZ77 window */

#define ZEXTERN extern
#define ZEXPORT
#define ZEXPORTVA
#ifndef FAR
#	define FAR
#endif
#define OF(args)  args
#define Z_ARG(args) args

typedef unsigned char  Byte;  /* 8 bits */
typedef unsigned int   uInt;  /* 16 bits or more */
typedef unsigned long  uLong; /* 32 bits or more */
typedef unsigned long z_crc_t;

typedef Byte  FAR Bytef;
typedef char  FAR charf;
typedef int   FAR intf;
typedef uInt  FAR uIntf;
typedef uLong FAR uLongf;

typedef void const *voidpc;
typedef void FAR   *voidpf;
typedef void       *voidp;

#define z_off_t git_off_t
#define z_off64_t z_off_t
#define z_const const

#endif /* ZCONF_H */
