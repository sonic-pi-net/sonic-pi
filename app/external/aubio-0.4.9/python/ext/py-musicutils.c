#include "aubio-types.h"

PyObject *
Py_aubio_window(PyObject *self, PyObject *args)
{
  char_t *wintype = NULL;
  uint_t winlen = 0;
  fvec_t *window = NULL;

  if (!PyArg_ParseTuple (args, "|sI", &wintype, &winlen)) {
    return NULL;
  }

  window = new_aubio_window(wintype, winlen);
  if (window == NULL) {
    PyErr_SetString (PyExc_ValueError, "failed computing window");
    return NULL;
  }

  return (PyObject *) PyAubio_CFvecToArray(window);
}

PyObject *
Py_aubio_level_lin(PyObject *self, PyObject *args)
{
  PyObject *input;
  fvec_t vec;
  PyObject *level_lin;

  if (!PyArg_ParseTuple (args, "O:level_lin", &input)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  level_lin = PyFloat_FromDouble(aubio_level_lin(&vec));
  if (level_lin == NULL) {
    PyErr_SetString (PyExc_ValueError, "failed computing level_lin");
    return NULL;
  }

  return level_lin;
}

PyObject *
Py_aubio_db_spl(PyObject *self, PyObject *args)
{
  PyObject *input;
  fvec_t vec;
  PyObject *db_spl;

  if (!PyArg_ParseTuple (args, "O:db_spl", &input)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  db_spl = PyFloat_FromDouble(aubio_db_spl(&vec));
  if (db_spl == NULL) {
    PyErr_SetString (PyExc_ValueError, "failed computing db_spl");
    return NULL;
  }

  return db_spl;
}

PyObject *
Py_aubio_silence_detection(PyObject *self, PyObject *args)
{
  PyObject *input;
  fvec_t vec;
  PyObject *silence_detection;
  smpl_t threshold;

  if (!PyArg_ParseTuple (args, "O" AUBIO_NPY_SMPL_CHR ":silence_detection", &input, &threshold)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  silence_detection = PyLong_FromLong(aubio_silence_detection(&vec, threshold));
  if (silence_detection == NULL) {
    PyErr_SetString (PyExc_ValueError, "failed computing silence_detection");
    return NULL;
  }

  return silence_detection;
}

PyObject *
Py_aubio_level_detection(PyObject *self, PyObject *args)
{
  PyObject *input;
  fvec_t vec;
  PyObject *level_detection;
  smpl_t threshold;

  if (!PyArg_ParseTuple (args, "O" AUBIO_NPY_SMPL_CHR ":level_detection", &input, &threshold)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  level_detection = PyFloat_FromDouble(aubio_level_detection(&vec, threshold));
  if (level_detection == NULL) {
    PyErr_SetString (PyExc_ValueError, "failed computing level_detection");
    return NULL;
  }

  return level_detection;
}

PyObject *
Py_aubio_shift(PyObject *self, PyObject *args)
{
  PyObject *input;
  fvec_t vec;

  if (!PyArg_ParseTuple (args, "O:shift", &input)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  fvec_shift(&vec);

  //Py_RETURN_NONE;
  return (PyObject *) PyAubio_CFvecToArray(&vec);
}

PyObject *
Py_aubio_ishift(PyObject *self, PyObject *args)
{
  PyObject *input;
  fvec_t vec;

  if (!PyArg_ParseTuple (args, "O:shift", &input)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  fvec_ishift(&vec);

  //Py_RETURN_NONE;
  return (PyObject *) PyAubio_CFvecToArray(&vec);
}

PyObject*
Py_aubio_hztomel(PyObject *self, PyObject *args, PyObject *kwds)
{
  smpl_t v;
  PyObject *htk = NULL;
  static char *kwlist[] = {"f", "htk", NULL};
  if (!PyArg_ParseTupleAndKeywords(args, kwds, AUBIO_NPY_SMPL_CHR "|O",
        kwlist, &v, &htk))
  {
    return NULL;
  }
  if (htk != NULL && PyObject_IsTrue(htk) == 1)
    return PyFloat_FromDouble(aubio_hztomel_htk(v));
  else
    return PyFloat_FromDouble(aubio_hztomel(v));
}

PyObject*
Py_aubio_meltohz(PyObject *self, PyObject *args, PyObject *kwds)
{
  smpl_t v;
  PyObject *htk = NULL;
  static char *kwlist[] = {"m", "htk", NULL};
  if (!PyArg_ParseTupleAndKeywords(args, kwds, AUBIO_NPY_SMPL_CHR "|O",
        kwlist, &v, &htk))
  {
    return NULL;
  }
  if (htk != NULL && PyObject_IsTrue(htk) == 1)
    return PyFloat_FromDouble(aubio_meltohz_htk(v));
  else
    return PyFloat_FromDouble(aubio_meltohz(v));
}

PyObject*
Py_aubio_hztomel_htk(PyObject *self, PyObject *args)
{
  smpl_t v;
  if (!PyArg_ParseTuple(args, AUBIO_NPY_SMPL_CHR, &v)) {
    return NULL;
  }
  return PyFloat_FromDouble(aubio_hztomel_htk(v));
}

PyObject*
Py_aubio_meltohz_htk(PyObject *self, PyObject *args)
{
  smpl_t v;
  if (!PyArg_ParseTuple(args, AUBIO_NPY_SMPL_CHR, &v)) {
    return NULL;
  }
  return PyFloat_FromDouble(aubio_meltohz_htk(v));
}
