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

#ifndef AUBIO_PARAMETER_H
#define AUBIO_PARAMETER_H

/** \file

  Parameter with linear interpolation

  This object manages a parameter, with minimum and maximum values, and a
  number of steps to compute linear interpolation between two values.

  \example utils/test-parameter.c

*/

#ifdef __cplusplus
extern "C" {
#endif

/** parameter object */
typedef struct _aubio_parameter_t aubio_parameter_t;

/** create new parameter object

  \param min_value the minimum value of the new parameter
  \param max_value the maximum value of the new parameter
  \param steps the number of steps to interpolate from the old value to the target value

  \return the newly created ::aubio_parameter_t

*/
aubio_parameter_t * new_aubio_parameter(smpl_t min_value, smpl_t max_value, uint_t steps);

/** set target value of the parameter

  \param param parameter, created by ::new_aubio_parameter
  \param value new target value

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_parameter_set_target_value ( aubio_parameter_t * param, smpl_t value );

/** get next parameter

  \param param parameter, created by ::new_aubio_parameter

  \return new interpolated parameter value

*/
smpl_t aubio_parameter_get_next_value ( aubio_parameter_t * param );

/** get current parameter value, without interpolation

  \param param parameter, created by ::new_aubio_parameter

  \return current value

*/
smpl_t aubio_parameter_get_current_value ( const aubio_parameter_t * param );

/** set current parameter value, skipping interpolation

  \param param parameter, created by ::new_aubio_parameter
  \param value new parameter value

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_parameter_set_current_value ( aubio_parameter_t * param, smpl_t value );

/** set number of steps used for interpolation

  \param param parameter, created by ::new_aubio_parameter
  \param steps new number of steps

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_parameter_set_steps ( aubio_parameter_t * param, uint_t steps );

/** get number of steps of this parameter

  \param param parameter, created by ::new_aubio_parameter

  \return number of steps

*/
uint_t aubio_parameter_get_steps ( const aubio_parameter_t * param);

/** set minimum value of this parameter

  \param param parameter, created by ::new_aubio_parameter
  \param min_value new minimum value

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_parameter_set_min_value ( aubio_parameter_t * param, smpl_t min_value );

/** get minimum value of this parameter

  \param param parameter, created by ::new_aubio_parameter

  \return minimum value

*/
smpl_t aubio_parameter_get_min_value ( const aubio_parameter_t * param );

/** set maximum value of this parameter

  \param param parameter, created by ::new_aubio_parameter
  \param max_value new maximum value

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_parameter_set_max_value ( aubio_parameter_t * param, smpl_t max_value );

/** get maximum value of this parameter

  \param param parameter, created by ::new_aubio_parameter

  \return maximum value

*/
smpl_t aubio_parameter_get_max_value ( const aubio_parameter_t * param );

/** destroy ::aubio_parameter_t object

  \param param parameter, created by ::new_aubio_parameter

*/
void del_aubio_parameter( aubio_parameter_t * param );

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PARAMETER_H */
