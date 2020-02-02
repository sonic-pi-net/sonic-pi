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

#include "aubio_priv.h"
#include "parameter.h"

#define AUBIO_PARAM_MAX_STEPS 2000
#define AUBIO_PARAM_MIN_STEPS 1

struct _aubio_parameter_t {
  smpl_t current_value;
  smpl_t target_value;
  smpl_t increment;

  smpl_t max_value;
  smpl_t min_value;

  uint_t steps;
};

aubio_parameter_t * new_aubio_parameter (smpl_t min_value, smpl_t max_value, uint_t steps )
{
  aubio_parameter_t * param = AUBIO_NEW(aubio_parameter_t);

  param->min_value = min_value;
  param->max_value = max_value;
  param->steps = steps;

  param->current_value = param->min_value;
  param->target_value = param->current_value;
  param->increment = 0.;

  return param;
}

uint_t aubio_parameter_set_target_value ( aubio_parameter_t * param, smpl_t value )
{
  uint_t err = AUBIO_OK;
  if (value < param->min_value ) {
    param->target_value = param->min_value;
    err = AUBIO_FAIL;
  } else if ( value > param->max_value ) {
    param->target_value = param->max_value;
    err = AUBIO_FAIL;
  } else {
    param->target_value = value;
  }
  param->increment = ( param->target_value - param->current_value ) / param->steps;
  return err;
}

uint_t aubio_parameter_set_current_value ( aubio_parameter_t * param, smpl_t value )
{
  uint_t err = AUBIO_OK;
  if (value < param->min_value ) {
    param->current_value = param->min_value;
    err = AUBIO_FAIL;
  } else if ( value > param->max_value ) {
    param->current_value = param->max_value;
    err = AUBIO_FAIL;
  } else {
    param->current_value = value;
  }
  param->target_value = param->current_value;
  param->increment = 0;
  return err;
}

smpl_t aubio_parameter_get_current_value ( const aubio_parameter_t * s )
{
  return s->current_value;
}

smpl_t aubio_parameter_get_next_value ( aubio_parameter_t * s )
{
  if ( ABS(s->current_value - s->target_value) > ABS(s->increment) ) {
    s->current_value += s->increment;
  } else {
    s->current_value = s->target_value;
  }
  return s->current_value;
}

uint_t aubio_parameter_set_steps ( aubio_parameter_t * param, uint_t steps )
{
  if (steps < AUBIO_PARAM_MIN_STEPS || steps > AUBIO_PARAM_MAX_STEPS) {
    return AUBIO_FAIL;
  }
  param->steps = steps;
  param->increment = ( param->target_value - param->current_value ) / param->steps;
  return AUBIO_OK;
}

uint_t aubio_parameter_get_steps ( const aubio_parameter_t * param )
{
  return param->steps;
}

uint_t aubio_parameter_set_min_value ( aubio_parameter_t * param, smpl_t min_value )
{
  param->min_value = min_value;
  return AUBIO_OK;
}

smpl_t aubio_parameter_get_min_value ( const aubio_parameter_t * param )
{
  return param->min_value;
}

uint_t aubio_parameter_set_max_value ( aubio_parameter_t * param, smpl_t max_value )
{
  param->max_value = max_value;
  return AUBIO_OK;
}

smpl_t aubio_parameter_get_max_value ( const aubio_parameter_t * param )
{
  return param->max_value;
}

void del_aubio_parameter ( aubio_parameter_t * param )
{
  AUBIO_FREE(param);
}
