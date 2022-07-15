#include "aubio_priv.h"
#include "types.h"
#include "fvec.h"
#include "cvec.h"
#include "vecutils.h"

#define AUBIO_OP(OPNAME, OP, TYPE, OBJ) \
void TYPE ## _ ## OPNAME (TYPE ## _t *o) \
{ \
  uint_t j; \
  for (j = 0; j < o->length; j++) { \
    o->OBJ[j] = OP (o->OBJ[j]); \
  } \
}

#define AUBIO_OP_C(OPNAME, OP) \
  AUBIO_OP(OPNAME, OP, fvec, data)

AUBIO_OP_C(exp, EXP)
AUBIO_OP_C(cos, COS)
AUBIO_OP_C(sin, SIN)
AUBIO_OP_C(abs, ABS)
AUBIO_OP_C(sqrt, SQRT)
AUBIO_OP_C(log10, SAFE_LOG10)
AUBIO_OP_C(log, SAFE_LOG)
AUBIO_OP_C(floor, FLOOR)
AUBIO_OP_C(ceil, CEIL)
AUBIO_OP_C(round, ROUND)

void fvec_pow (fvec_t *s, smpl_t power)
{
  uint_t j;
  for (j = 0; j < s->length; j++) {
    s->data[j] = POW(s->data[j], power);
  }
}
