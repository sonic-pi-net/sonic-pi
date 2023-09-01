/*
  Copyright (C) 2016 Paul Brossier <piem@aubio.org>

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

#ifndef AUBIO_LOG_H
#define AUBIO_LOG_H

#ifdef __cplusplus
extern "C" {
#endif

/** \file

  Logging features

  This file specifies ::aubio_log_set_function and
  ::aubio_log_set_level_function, which let you define one or several custom
  logging functions to redirect warnings and errors from aubio to your
  application. The custom function should have the prototype defined in
  ::aubio_log_function_t.

  After a call to ::aubio_log_set_level_function, ::aubio_log_reset can be used
  to reset each logging functions to the default ones.

  \example utils/test-log.c

*/

/** list of logging levels */
enum aubio_log_level {
  AUBIO_LOG_ERR, /**< critical errors */
  AUBIO_LOG_INF, /**< infos */
  AUBIO_LOG_MSG, /**< general messages */
  AUBIO_LOG_DBG, /**< debug messages */
  AUBIO_LOG_WRN, /**< warnings */
  AUBIO_LOG_LAST_LEVEL, /**< number of valid levels */
};

/** Logging function prototype, to be passed to ::aubio_log_set_function

  \param level log level
  \param message text to log
  \param data optional closure used by the callback

  See @ref utils/test-log.c for an example of logging function.

 */
typedef void (*aubio_log_function_t)(sint_t level, const char_t *message, void
    *data);

/** Set logging function for all levels

  \param fun the function to be used to log, of type ::aubio_log_function_t
  \param data optional closure to be passed to the function (can be NULL if
  nothing to pass)

 */
void aubio_log_set_function(aubio_log_function_t fun, void* data);

/** Set logging function for a given level

  \param level the level for which to set the logging function
  \param fun the function to be used to log, of type ::aubio_log_function_t
  \param data optional closure to be passed to the function (can be NULL if
  nothing to pass)

*/
aubio_log_function_t aubio_log_set_level_function(sint_t level,
    aubio_log_function_t fun, void* data);

/** Reset all logging functions to the default one

 After calling this function, the default logging function will be used to
 print error, warning, normal, and debug messages to `stdout` or `stderr`.

 */
void aubio_log_reset(void);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_LOG_H */
