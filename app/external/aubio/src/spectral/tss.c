/*
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

/* default values : alpha=4, beta=3, threshold=0.25 */

#include "aubio_priv.h"
#include "fvec.h"
#include "cvec.h"
#include "mathutils.h"
#include "spectral/tss.h"

struct _aubio_tss_t
{
  smpl_t threshold;
  smpl_t alpha;
  smpl_t beta;
  smpl_t parm;
  smpl_t thrsfact;
  fvec_t *theta1;
  fvec_t *theta2;
  fvec_t *oft1;
  fvec_t *oft2;
  fvec_t *dev;
};

void aubio_tss_do(aubio_tss_t *o, const cvec_t * input,
    cvec_t * trans, cvec_t * stead)
{
  uint_t j;
  uint_t test;
  uint_t nbins     = input->length;
  smpl_t alpha     = o->alpha;
  smpl_t beta      = o->beta;
  smpl_t parm      = o->parm;
  smpl_t * dev    = (smpl_t *)o->dev->data;
  smpl_t * oft1   = (smpl_t *)o->oft1->data;
  smpl_t * oft2   = (smpl_t *)o->oft2->data;
  smpl_t * theta1 = (smpl_t *)o->theta1->data;
  smpl_t * theta2 = (smpl_t *)o->theta2->data;
  /* second phase derivative */
  for (j=0;j<nbins; j++){
    dev[j] = aubio_unwrap2pi(input->phas[j]
        -2.0*theta1[j]+theta2[j]);
    theta2[j] = theta1[j];
    theta1[j] = input->phas[j];

    /* transient analysis */
    test = (ABS(dev[j]) > parm*oft1[j]);
    trans->norm[j] = input->norm[j] * test;
    trans->phas[j] = input->phas[j] * test;

    /* steady state analysis */
    test = (ABS(dev[j]) < parm*oft2[j]);
    stead->norm[j] = input->norm[j] * test;
    stead->phas[j] = input->phas[j] * test;

    /*increase probability for transient */
    test = (trans->norm[j]==0.);
    oft1[j]  = test;
    test = (trans->norm[j]>0.);
    oft1[j] += alpha*test;
    test = (oft1[j]>1. && trans->norm[j]>0.);
    oft1[j] += beta*test;

    /*increase probability for steady states */
    test = (stead->norm[j]==0.);
    oft2[j]  = test;
    test = (stead->norm[j]>0.);
    oft2[j] += alpha*test;
    test = (oft2[j]>1. && stead->norm[j]>0.);
    oft2[j] += beta*test;
  }
}

uint_t aubio_tss_set_threshold(aubio_tss_t *o, smpl_t threshold){
  o->threshold = threshold;
  o->parm = o->threshold * o->thrsfact;
  return AUBIO_OK;
}

aubio_tss_t * new_aubio_tss(uint_t buf_size, uint_t hop_size)
{
  aubio_tss_t * o = AUBIO_NEW(aubio_tss_t);
  uint_t rsize = buf_size/2+1;
  o->threshold = 0.25;
  o->thrsfact = TWO_PI*hop_size/rsize;
  o->alpha = 3.;
  o->beta = 4.;
  o->parm = o->threshold*o->thrsfact;
  o->theta1 = new_fvec(rsize);
  o->theta2 = new_fvec(rsize);
  o->oft1 = new_fvec(rsize);
  o->oft2 = new_fvec(rsize);
  o->dev = new_fvec(rsize);
  return o;
}

void del_aubio_tss(aubio_tss_t *s)
{
  del_fvec(s->theta1);
  del_fvec(s->theta2);
  del_fvec(s->oft1);
  del_fvec(s->oft2);
  del_fvec(s->dev);
  AUBIO_FREE(s);
}

uint_t aubio_tss_set_alpha(aubio_tss_t *o, smpl_t alpha){
  o->alpha = alpha;
  return AUBIO_OK;
}

uint_t aubio_tss_set_beta(aubio_tss_t *o, smpl_t beta){
  o->beta = beta;
  return AUBIO_OK;
}

