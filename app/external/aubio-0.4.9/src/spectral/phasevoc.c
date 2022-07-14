/*
  Copyright (C) 2003-2014 Paul Brossier <piem@aubio.org>

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
#include "cvec.h"
#include "mathutils.h"
#include "spectral/fft.h"
#include "spectral/phasevoc.h"

/** phasevocoder internal object */
struct _aubio_pvoc_t {
  uint_t win_s;       /** grain length */
  uint_t hop_s;       /** overlap step */
  aubio_fft_t * fft;  /** fft object */
  fvec_t * data;      /** current input grain, [win_s] frames */
  fvec_t * dataold;   /** memory of past grain, [win_s-hop_s] frames */
  fvec_t * synth;     /** current output grain, [win_s] frames */
  fvec_t * synthold;  /** memory of past grain, [win_s-hop_s] frames */
  fvec_t * w;         /** grain window [win_s] */
  uint_t start;       /** where to start additive synthesis */
  uint_t end;         /** where to end it */
  smpl_t scale;       /** scaling factor for synthesis */
  uint_t end_datasize;  /** size of memory to end */
  uint_t hop_datasize;  /** size of memory to hop_s */
};


/** returns data and dataold slided by hop_s */
static void aubio_pvoc_swapbuffers(aubio_pvoc_t *pv, const fvec_t *new);

/** do additive synthesis from 'old' and 'cur' */
static void aubio_pvoc_addsynth(aubio_pvoc_t *pv, fvec_t * synthnew);

void aubio_pvoc_do(aubio_pvoc_t *pv, const fvec_t * datanew, cvec_t *fftgrain) {
  /* slide  */
  aubio_pvoc_swapbuffers(pv, datanew);
  /* windowing */
  fvec_weight(pv->data, pv->w);
  /* shift */
  fvec_shift(pv->data);
  /* calculate fft */
  aubio_fft_do (pv->fft,pv->data,fftgrain);
}

void aubio_pvoc_rdo(aubio_pvoc_t *pv,cvec_t * fftgrain, fvec_t * synthnew) {
  /* calculate rfft */
  aubio_fft_rdo(pv->fft,fftgrain,pv->synth);
  /* unshift */
  fvec_ishift(pv->synth);
  /* windowing */
  // if overlap = 50%, do not apply window (identity)
  if (pv->hop_s * 2 < pv->win_s) {
    fvec_weight(pv->synth, pv->w);
  }
  /* additive synthesis */
  aubio_pvoc_addsynth(pv, synthnew);
}

aubio_pvoc_t * new_aubio_pvoc (uint_t win_s, uint_t hop_s) {
  aubio_pvoc_t * pv = AUBIO_NEW(aubio_pvoc_t);

  /* if (win_s < 2*hop_s) {
    AUBIO_WRN("Hop size bigger than half the window size!\n");
  } */

  if ((sint_t)hop_s < 1) {
    AUBIO_ERR("pvoc: got hop_size %d, but can not be < 1\n", hop_s);
    goto beach;
  } else if ((sint_t)win_s < 2) {
    AUBIO_ERR("pvoc: got buffer_size %d, but can not be < 2\n", win_s);
    goto beach;
  } else if (win_s < hop_s) {
    AUBIO_ERR("pvoc: hop size (%d) is larger than win size (%d)\n", hop_s, win_s);
    goto beach;
  }

  pv->fft      = new_aubio_fft (win_s);
  if (pv->fft == NULL) {
    goto beach;
  }

  /* remember old */
  pv->data     = new_fvec (win_s);
  pv->synth    = new_fvec (win_s);

  /* new input output */
  if (win_s > hop_s) {
    pv->dataold  = new_fvec  (win_s-hop_s);
    pv->synthold = new_fvec (win_s-hop_s);
  } else {
    pv->dataold  = new_fvec  (1);
    pv->synthold = new_fvec (1);
  }
  pv->w        = new_aubio_window ("hanningz", win_s);

  pv->hop_s    = hop_s;
  pv->win_s    = win_s;

  /* more than 50% overlap, overlap anyway */
  if (win_s < 2 * hop_s) pv->start = 0;
  /* less than 50% overlap, reset latest grain trail */
  else pv->start = win_s - hop_s - hop_s;

  if (win_s > hop_s) pv->end = win_s - hop_s;
  else pv->end = 0;

  pv->end_datasize = pv->end * sizeof(smpl_t);
  pv->hop_datasize = pv->hop_s * sizeof(smpl_t);

  // for reconstruction with 75% overlap
  if (win_s == hop_s * 4) {
    pv->scale = 2./3.;
  } else if (win_s == hop_s * 8) {
    pv->scale = 1./3.;
  } else if (win_s == hop_s * 2) {
    pv->scale = 1.;
  } else {
    pv->scale = .5;
  }

  return pv;

beach:
  AUBIO_FREE (pv);
  return NULL;
}

uint_t aubio_pvoc_set_window(aubio_pvoc_t *pv, const char_t *window) {
  return fvec_set_window(pv->w, (char_t*)window);
}

void del_aubio_pvoc(aubio_pvoc_t *pv) {
  del_fvec(pv->data);
  del_fvec(pv->synth);
  del_fvec(pv->dataold);
  del_fvec(pv->synthold);
  del_fvec(pv->w);
  del_aubio_fft(pv->fft);
  AUBIO_FREE(pv);
}

static void aubio_pvoc_swapbuffers(aubio_pvoc_t *pv, const fvec_t *new)
{
  /* some convenience pointers */
  smpl_t * data = pv->data->data;
  smpl_t * dataold = pv->dataold->data;
  smpl_t * datanew = new->data;
#ifndef HAVE_MEMCPY_HACKS
  uint_t i;
  for (i = 0; i < pv->end; i++)
    data[i] = dataold[i];
  for (i = 0; i < pv->hop_s; i++)
    data[pv->end + i] = datanew[i];
  for (i = 0; i < pv->end; i++)
    dataold[i] = data[i + pv->hop_s];
#else
  memcpy(data, dataold, pv->end_datasize);
  data += pv->end;
  memcpy(data, datanew, pv->hop_datasize);
  data -= pv->end;
  data += pv->hop_s;
  memcpy(dataold, data, pv->end_datasize);
#endif
}

static void aubio_pvoc_addsynth(aubio_pvoc_t *pv, fvec_t *synth_new)
{
  uint_t i;
  /* some convenience pointers */
  smpl_t * synth    = pv->synth->data;
  smpl_t * synthold = pv->synthold->data;
  smpl_t * synthnew = synth_new->data;

  /* put new result in synthnew */
  for (i = 0; i < pv->hop_s; i++)
    synthnew[i] = synth[i] * pv->scale;

  /* no overlap, nothing else to do */
  if (pv->end == 0) return;

  /* add new synth to old one */
  for (i = 0; i < pv->hop_s; i++)
    synthnew[i] += synthold[i];

  /* shift synthold */
  for (i = 0; i < pv->start; i++)
    synthold[i] = synthold[i + pv->hop_s];

  /* erase last frame in synthold */
  for (i = pv->start; i < pv->end; i++)
    synthold[i] = 0.;

  /* additive synth */
  for (i = 0; i < pv->end; i++)
    synthold[i] += synth[i + pv->hop_s] * pv->scale;
}

uint_t aubio_pvoc_get_win(aubio_pvoc_t* pv)
{
  return pv->win_s;
}

uint_t aubio_pvoc_get_hop(aubio_pvoc_t* pv)
{
  return pv->hop_s;
}
