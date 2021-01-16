/*
  Copyright (C) 2017 Paul Brossier <piem@aubio.org>

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
#include "spectral/dct.h"

#if defined(HAVE_INTEL_IPP)

#if !HAVE_AUBIO_DOUBLE
#define aubio_IppFloat                 Ipp32f
#define aubio_ippsDCTFwdSpec           IppsDCTFwdSpec_32f
#define aubio_ippsDCTInvSpec           IppsDCTInvSpec_32f
#define aubio_ippsDCTFwdGetSize        ippsDCTFwdGetSize_32f
#define aubio_ippsDCTInvGetSize        ippsDCTInvGetSize_32f
#define aubio_ippsDCTFwdInit           ippsDCTFwdInit_32f
#define aubio_ippsDCTInvInit           ippsDCTInvInit_32f
#define aubio_ippsDCTFwd               ippsDCTFwd_32f
#define aubio_ippsDCTInv               ippsDCTInv_32f
#else /* HAVE_AUBIO_DOUBLE */
#define aubio_IppFloat                 Ipp64f
#define aubio_ippsDCTFwdSpec           IppsDCTFwdSpec_64f
#define aubio_ippsDCTInvSpec           IppsDCTInvSpec_64f
#define aubio_ippsDCTFwdGetSize        ippsDCTFwdGetSize_64f
#define aubio_ippsDCTInvGetSize        ippsDCTInvGetSize_64f
#define aubio_ippsDCTFwdInit           ippsDCTFwdInit_64f
#define aubio_ippsDCTInvInit           ippsDCTInvInit_64f
#define aubio_ippsDCTFwd               ippsDCTFwd_64f
#define aubio_ippsDCTInv               ippsDCTInv_64f
#endif

typedef struct _aubio_dct_ipp_t aubio_dct_ipp_t;

struct _aubio_dct_ipp_t {
  uint_t size;
  Ipp8u* pSpecFwd;
  Ipp8u* pSpecInv;
  Ipp8u* pSpecBuffer;
  Ipp8u* pBuffer;
  aubio_ippsDCTFwdSpec* pFwdDCTSpec;
  aubio_ippsDCTInvSpec* pInvDCTSpec;
};

void del_aubio_dct_ipp (aubio_dct_ipp_t *s);

aubio_dct_ipp_t * new_aubio_dct_ipp (uint_t size) {
  aubio_dct_ipp_t * s = AUBIO_NEW(aubio_dct_ipp_t);

  const IppHintAlgorithm qualityHint = ippAlgHintAccurate; // ippAlgHintFast;
  int pSpecSize, pSpecBufferSize, pBufferSize;
  IppStatus status;

  if ((sint_t)size <= 0) {
    AUBIO_ERR("dct: can only create with sizes greater than 0, requested %d\n",
        size);
    goto beach;
  }

  status = aubio_ippsDCTFwdGetSize(size, qualityHint, &pSpecSize,
      &pSpecBufferSize, &pBufferSize);
  if (status != ippStsNoErr) {
    AUBIO_ERR("dct: failed to initialize dct. IPP error: %d\n", status);
    goto beach;
  }

  //AUBIO_INF("dct: fwd initialized with %d %d %d\n", pSpecSize, pSpecBufferSize,
  //    pBufferSize);

  s->pSpecFwd = ippsMalloc_8u(pSpecSize);
  s->pSpecInv = ippsMalloc_8u(pSpecSize);
  if (pSpecSize > 0) {
    s->pSpecBuffer = ippsMalloc_8u(pSpecBufferSize);
  } else {
    s->pSpecBuffer = NULL;
  }
  s->pBuffer = ippsMalloc_8u(pBufferSize);

  status = aubio_ippsDCTInvGetSize(size, qualityHint, &pSpecSize,
      &pSpecBufferSize, &pBufferSize);
  if (status != ippStsNoErr) {
    AUBIO_ERR("dct: failed to initialize dct. IPP error: %d\n", status);
    goto beach;
  }

  //AUBIO_INF("dct: inv initialized with %d %d %d\n", pSpecSize, pSpecBufferSize,
  //    pBufferSize);

  status = aubio_ippsDCTFwdInit(&(s->pFwdDCTSpec), size, qualityHint, s->pSpecFwd,
      s->pSpecBuffer);
  if (status != ippStsNoErr) {
    AUBIO_ERR("dct: failed to initialize fwd dct. IPP error: %d\n", status);
    goto beach;
  }

  status = aubio_ippsDCTInvInit(&(s->pInvDCTSpec), size, qualityHint, s->pSpecInv,
      s->pSpecBuffer);
  if (status != ippStsNoErr) {
    AUBIO_ERR("dct: failed to initialize inv dct. IPP error: %d\n", status);
    goto beach;
  }

  s->size = size;

  return s;

beach:
  del_aubio_dct_ipp(s);
  return NULL;
}

void del_aubio_dct_ipp(aubio_dct_ipp_t *s) {
  ippFree(s->pSpecFwd);
  ippFree(s->pSpecInv);
  ippFree(s->pSpecBuffer);
  ippFree(s->pBuffer);
  AUBIO_FREE(s);
}

void aubio_dct_ipp_do(aubio_dct_ipp_t *s, const fvec_t *input, fvec_t *output) {

  aubio_ippsDCTFwd((const aubio_IppFloat*)input->data,
      (aubio_IppFloat*)output->data, s->pFwdDCTSpec, s->pBuffer);

}

void aubio_dct_ipp_rdo(aubio_dct_ipp_t *s, const fvec_t *input, fvec_t *output) {

  aubio_ippsDCTInv((const aubio_IppFloat*)input->data,
      (aubio_IppFloat*)output->data, s->pInvDCTSpec, s->pBuffer);

}

#endif //defined(HAVE_INTEL_IPP)
