/*
  Copyright (C) 2004, 2005  Mario Lang <mlang@delysid.org>
  Copyright (C) 2003-2009 Paul Brossier <piem@aubio.org>

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

#include "aubio_priv.h"
#include "fvec.h"
#include "pitch/pitchschmitt.h"

smpl_t aubio_schmittS16LE (aubio_pitchschmitt_t * p, uint_t nframes,
    signed short int *indata);

struct _aubio_pitchschmitt_t
{
  uint_t blockSize;
  uint_t rate;
  signed short int *schmittBuffer;
  signed short int *schmittPointer;
  signed short int *buf;
};

aubio_pitchschmitt_t *
new_aubio_pitchschmitt (uint_t size)
{
  aubio_pitchschmitt_t *p = AUBIO_NEW (aubio_pitchschmitt_t);
  p->blockSize = size;
  p->schmittBuffer = AUBIO_ARRAY (signed short int, p->blockSize);
  p->buf = AUBIO_ARRAY (signed short int, p->blockSize);
  p->schmittPointer = p->schmittBuffer;
  return p;
}

void
aubio_pitchschmitt_do (aubio_pitchschmitt_t * p, const fvec_t * input,
    fvec_t * output)
{
  uint_t j;
  for (j = 0; j < input->length; j++) {
    p->buf[j] = input->data[j] * 32768.;
  }
  output->data[0] = aubio_schmittS16LE (p, input->length, p->buf);
}

smpl_t
aubio_schmittS16LE (aubio_pitchschmitt_t * p, uint_t nframes,
    signed short int *indata)
{
  uint_t i, j;
  uint_t blockSize = p->blockSize;
  signed short int *schmittBuffer = p->schmittBuffer;
  signed short int *schmittPointer = p->schmittPointer;

  smpl_t period = 0., trigfact = 0.6;

  for (i = 0; i < nframes; i++) {
    *schmittPointer++ = indata[i];
    if (schmittPointer - schmittBuffer >= (sint_t) blockSize) {
      sint_t endpoint, startpoint, t1, t2, A1, A2, tc, schmittTriggered;

      schmittPointer = schmittBuffer;

      for (j = 0, A1 = 0, A2 = 0; j < blockSize; j++) {
        if (schmittBuffer[j] > 0 && A1 < schmittBuffer[j])
          A1 = schmittBuffer[j];
        if (schmittBuffer[j] < 0 && A2 < -schmittBuffer[j])
          A2 = -schmittBuffer[j];
      }
      t1 = (sint_t) (A1 * trigfact + 0.5);
      t2 = -(sint_t) (A2 * trigfact + 0.5);
      startpoint = 0;
      for (j = 1; j < blockSize && schmittBuffer[j] <= t1; j++);
      for (     ; j < blockSize - 1 && !(schmittBuffer[j] >= t2 &&
             schmittBuffer[j + 1] < t2); j++);
      startpoint = j;
      schmittTriggered = 0;
      endpoint = startpoint + 1;
      for (j = startpoint, tc = 0; j < blockSize; j++) {
        if (!schmittTriggered) {
          schmittTriggered = (schmittBuffer[j] >= t1);
        } else if (schmittBuffer[j] >= t2 && schmittBuffer[j + 1] < t2) {
          endpoint = j;
          tc++;
          schmittTriggered = 0;
        }
      }
      if ((endpoint > startpoint) && (tc > 0)) {
        period = (smpl_t) (endpoint - startpoint) / tc;
      }
    }
  }

  p->schmittBuffer = schmittBuffer;
  p->schmittPointer = schmittPointer;
  return period;
}

void
del_aubio_pitchschmitt (aubio_pitchschmitt_t * p)
{
  AUBIO_FREE (p->schmittBuffer);
  AUBIO_FREE (p->buf);
  AUBIO_FREE (p);
}
