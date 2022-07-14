/*
  Copyright (C) 2003-2013 Paul Brossier <piem@aubio.org>

  This file is part of aubio.

  aubio is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  aubio is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with aubio.  If not, see <http://www.gnu.org/licenses/>.

*/

#include <aubio.h>

#include "config.h"

#ifdef HAVE_STDIO_H
#include <stdio.h>              // for fprintf
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>             // for exit
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>             // for access
#elif defined(HAVE_WIN_HACKS)
#include <io.h>
#define access _access
#define F_OK   0
#endif
#ifdef HAVE_MATH_H
#include <math.h>               // for isfinite
#endif
#ifdef HAVE_STRING_H
#include <string.h>             // for strcmp
#endif

#ifdef HAVE_C99_VARARGS_MACROS
#ifdef HAVE_DEBUG
#define debug(...)                fprintf (stderr, __VA_ARGS__)
#else
#define debug(...)
#endif
#define verbmsg(...)              if (verbose) fprintf (stderr, __VA_ARGS__)
#define errmsg(...)               fprintf (stderr, __VA_ARGS__)
#define outmsg(...)               fprintf (stdout, __VA_ARGS__)
#else
#ifdef HAVE_DEBUG
#define debug(...)                fprintf (stderr, format , **args)
#else
#define debug(...)
#endif
#define verbmsg(format, args...)  if (verbose) fprintf(stderr, format , ##args)
#define errmsg(format, args...)   fprintf(stderr, format , ##args)
#define outmsg(format, args...)   fprintf(stdout, format , ##args)
#endif

typedef void (aubio_print_func_t) (void);
void send_noteon (smpl_t pitch, smpl_t velo);

/** common process function */
typedef void (*aubio_process_func_t) (fvec_t * input, fvec_t * output);

void process_block (fvec_t *ibuf, fvec_t *obuf);
void process_print (void);

void print_time (uint_t samples);
