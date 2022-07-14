/*
  Copyright (C) 2003-2015 Paul Brossier <piem@aubio.org>

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

/** \mainpage

  \section introduction Introduction

  aubio is a library to extract annotations from audio signals: it provides a
  set of functions that take an input audio signal, and output pitch estimates,
  attack times (onset), beat location estimates, and other annotation tasks.

  \section basics Basics

  All object structures in aubio share the same function prefixes and suffixes:

    - \p new_aubio_foo creates the object \p foo
    - \p aubio_foo_do executes the object \p foo
    - \p del_aubio_foo destroys the object \p foo

  All memory allocation and deallocation take place in the \p new_ and \p del_
  functions. Optionally, more than one \p _do methods are available.
  Additional parameters can be adjusted and observed using:

    - \p aubio_foo_get_param, getter function, gets the value of a parameter
    - \p aubio_foo_set_param, setter function, changes the value of a parameter

  Unless specified in its documentation, no memory operations take place in the
  getter functions. However, memory resizing can take place in setter
  functions.

  \subsection vectors Vectors

  Two basic structures are being used in aubio: ::fvec_t and ::cvec_t. The
  ::fvec_t structures are used to store vectors of floating pointer number.
  ::cvec_t are used to store complex number, as two vectors of norm and phase
  elements.

  Additionally, the ::lvec_t structure can be used to store floating point
  numbers in double precision. They are mostly used to store filter
  coefficients, to avoid instability.

  \subsection objects Available objects

  Here is a list of some of the most common objects for aubio:

  \code

  // fast Fourier transform (FFT)
  aubio_fft_t *fft = new_aubio_fft (winsize);
  // phase vocoder
  aubio_pvoc_t *pv = new_aubio_pvoc (winsize, stepsize);
  // onset detection
  aubio_onset_t *onset = new_aubio_onset (method, winsize, stepsize, samplerate);
  // pitch detection
  aubio_pitch_t *pitch = new_aubio_pitch (method, winsize, stepsize, samplerate);
  // beat tracking
  aubio_tempo_t *tempo = new_aubio_tempo (method, winsize, stepsize, samplerate);

  \endcode

  See the <a href="globals_type.html">list of typedefs</a> for a complete list.

  \subsection example Example

  Here is a simple example that creates an A-Weighting filter and applies it to a
  vector.

  \code

  // set window size, and sampling rate
  uint_t winsize = 1024, sr = 44100;
  // create a vector
  fvec_t *this_buffer = new_fvec (winsize);
  // create the a-weighting filter
  aubio_filter_t *this_filter = new_aubio_filter_a_weighting (sr);

  while (running) {
    // here some code to put some data in this_buffer
    // ...

    // apply the filter, in place
    aubio_filter_do (this_filter, this_buffer);

    // here some code to get some data from this_buffer
    // ...
  }

  // and free the structures
  del_aubio_filter (this_filter);
  del_fvec (this_buffer);

  \endcode

  Several examples of C programs are available in the \p examples/ and \p tests/src
  directories of the source tree.

  Some examples:
  - @ref spectral/test-fft.c
  - @ref spectral/test-phasevoc.c
  - @ref onset/test-onset.c
  - @ref pitch/test-pitch.c
  - @ref tempo/test-tempo.c
  - @ref test-fvec.c
  - @ref test-cvec.c

  \subsection unstable_api Unstable API

  Several more functions are available and used within aubio, but not
  documented here, either because they are not considered useful to the user,
  or because they may need to be changed in the future. However, they can still
  be used by defining AUBIO_UNSTABLE to 1 before including the aubio header:

  \code
  #define AUBIO_UNSTABLE 1
  #include <aubio/aubio.h>
  \endcode

  Future versions of aubio could break API compatibility with these functions
  without warning. If you choose to use functions in AUBIO_UNSTABLE, you are on
  your own.

  \section download Download

  Latest versions, further documentation, examples, wiki, and mailing lists can
  be found at https://aubio.org .

 */

#ifndef AUBIO_H
#define AUBIO_H

/** @file aubio.h Global aubio include file.

  You will want to include this file as:

  @code
    #include <aubio/aubio.h>
  @endcode

  To access headers with unstable prototypes, use:

  @code
    #define AUBIO_UNSTABLE 1
    #include <aubio/aubio.h>
  @endcode

 */

#ifdef __cplusplus
extern "C"
{
#endif

/* in this order */
#include "types.h"
#include "fvec.h"
#include "cvec.h"
#include "lvec.h"
#include "fmat.h"
#include "musicutils.h"
#include "vecutils.h"
#include "temporal/resampler.h"
#include "temporal/filter.h"
#include "temporal/biquad.h"
#include "temporal/a_weighting.h"
#include "temporal/c_weighting.h"
#include "spectral/fft.h"
#include "spectral/dct.h"
#include "spectral/phasevoc.h"
#include "spectral/filterbank.h"
#include "spectral/filterbank_mel.h"
#include "spectral/mfcc.h"
#include "spectral/specdesc.h"
#include "spectral/awhitening.h"
#include "spectral/tss.h"
#include "pitch/pitch.h"
#include "onset/onset.h"
#include "tempo/tempo.h"
#include "notes/notes.h"
#include "io/source.h"
#include "io/sink.h"
#include "synth/sampler.h"
#include "synth/wavetable.h"
#include "utils/parameter.h"
#include "utils/log.h"

#if AUBIO_UNSTABLE
#include "mathutils.h"
#include "io/source_sndfile.h"
#include "io/source_apple_audio.h"
#include "io/source_avcodec.h"
#include "io/source_wavread.h"
#include "io/sink_sndfile.h"
#include "io/sink_apple_audio.h"
#include "io/sink_wavwrite.h"
#include "io/audio_unit.h"
#include "onset/peakpicker.h"
#include "pitch/pitchmcomb.h"
#include "pitch/pitchyin.h"
#include "pitch/pitchyinfft.h"
#include "pitch/pitchyinfast.h"
#include "pitch/pitchschmitt.h"
#include "pitch/pitchfcomb.h"
#include "pitch/pitchspecacf.h"
#include "tempo/beattracking.h"
#include "utils/scale.h"
#include "utils/hist.h"
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
