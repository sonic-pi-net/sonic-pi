#include "aubio-types.h"

PyObject *
new_py_fvec(uint_t length) {
    npy_intp dims[] = { length, 1 };
    return PyArray_ZEROS(1, dims, AUBIO_NPY_SMPL, 0);
}

PyObject *
new_py_fmat(uint_t height, uint_t length) {
    npy_intp dims[] = { height, length, 1 };
    return PyArray_ZEROS(2, dims, AUBIO_NPY_SMPL, 0);
}

PyObject *
PyAubio_CFvecToArray (fvec_t * self)
{
  npy_intp dims[] = { self->length, 1 };
  return PyArray_SimpleNewFromData (1, dims, AUBIO_NPY_SMPL, self->data);
}

int
PyAubio_IsValidVector (PyObject * input) {
  npy_intp length;
  if (input == NULL) {
    PyErr_SetString (PyExc_ValueError, "input array is not a python object");
    return 0;
  }
  // parsing input object into a Py_fvec
  if (PyArray_Check(input)) {

    // we got an array, convert it to an fvec
    if (PyArray_NDIM ((PyArrayObject *)input) == 0) {
      PyErr_SetString (PyExc_ValueError, "input array is a scalar");
      return 0;
    } else if (PyArray_NDIM ((PyArrayObject *)input) > 1) {
      PyErr_SetString (PyExc_ValueError,
          "input array has more than one dimensions");
      return 0;
    }

    if (!PyArray_ISFLOAT ((PyArrayObject *)input)) {
      PyErr_SetString (PyExc_ValueError, "input array should be float");
      return 0;
    } else if (PyArray_TYPE ((PyArrayObject *)input) != AUBIO_NPY_SMPL) {
      PyErr_SetString (PyExc_ValueError, "input array should be " AUBIO_NPY_SMPL_STR);
      return 0;
    }

    length = PyArray_SIZE ((PyArrayObject *)input);
    if (length <= 0) {
      PyErr_SetString (PyExc_ValueError, "input array size should be greater than 0");
      return 0;
    }

  } else if (PyObject_TypeCheck (input, &PyList_Type)) {
    PyErr_SetString (PyExc_ValueError, "does not convert from list yet");
    return 0;
  } else {
    PyErr_SetString (PyExc_ValueError, "can only accept vector of float as input");
    return 0;
  }
  return 1;
}

int
PyAubio_ArrayToCFvec (PyObject *input, fvec_t *out) {

  if (!PyAubio_IsValidVector(input)){
    return 0;
  }

  out->length = (uint_t) PyArray_SIZE ((PyArrayObject *)input);
  out->data = (smpl_t *) PyArray_GETPTR1 ((PyArrayObject *)input, 0);
  return 1;
}

PyObject *
PyAubio_CFmatToArray (fmat_t * input)
{
  PyObject *array = NULL;
  uint_t i;
  npy_intp dims[] = { input->length, 1 };
  PyObject *concat = PyList_New (0), *tmp = NULL;
  for (i = 0; i < input->height; i++) {
    tmp = PyArray_SimpleNewFromData (1, dims, AUBIO_NPY_SMPL, input->data[i]);
    PyList_Append (concat, tmp);
    Py_DECREF (tmp);
  }
  array = PyArray_FromObject (concat, AUBIO_NPY_SMPL, 2, 2);
  Py_DECREF (concat);
  return array;
}

int
PyAubio_ArrayToCFmat (PyObject *input, fmat_t *mat) {
  uint_t i, new_height;
  npy_intp length, height;
  if (input == NULL) {
    PyErr_SetString (PyExc_ValueError, "input array is not a python object");
    return 0;
  }
  // parsing input object into a Py_fvec
  if (PyArray_Check(input)) {

    // we got an array, convert it to an fvec
    if (PyArray_NDIM ((PyArrayObject *)input) == 0) {
      PyErr_SetString (PyExc_ValueError, "input array is a scalar");
      return 0;
    } else if (PyArray_NDIM ((PyArrayObject *)input) > 2) {
      PyErr_SetString (PyExc_ValueError,
          "input array has more than two dimensions");
      return 0;
    }

    if (!PyArray_ISFLOAT ((PyArrayObject *)input)) {
      PyErr_SetString (PyExc_ValueError, "input array should be float");
      return 0;
    } else if (PyArray_TYPE ((PyArrayObject *)input) != AUBIO_NPY_SMPL) {
      PyErr_SetString (PyExc_ValueError, "input array should be " AUBIO_NPY_SMPL_STR);
      return 0;
    }

    // no need to really allocate fvec, just its struct member
    length = PyArray_DIM ((PyArrayObject *)input, 1);
    if (length <= 0) {
      PyErr_SetString (PyExc_ValueError, "input array dimension 1 should be greater than 0");
      return 0;
    }
    height = PyArray_DIM ((PyArrayObject *)input, 0);
    if (height <= 0) {
      PyErr_SetString (PyExc_ValueError, "input array dimension 0 should be greater than 0");
      return 0;
    }

  } else if (PyObject_TypeCheck (input, &PyList_Type)) {
    PyErr_SetString (PyExc_ValueError, "can not convert list to fmat");
    return 0;
  } else {
    PyErr_SetString (PyExc_ValueError, "can only accept matrix of float as input");
    return 0;
  }

  new_height = (uint_t)PyArray_DIM ((PyArrayObject *)input, 0);
  if (mat->height != new_height) {
    if (mat->data) {
      free(mat->data);
    }
    mat->data = (smpl_t **)malloc(sizeof(smpl_t*) * new_height);
  }

  mat->height = new_height;
  mat->length = (uint_t)PyArray_DIM ((PyArrayObject *)input, 1);
  for (i=0; i< mat->height; i++) {
    mat->data[i] = (smpl_t*)PyArray_GETPTR1 ((PyArrayObject *)input, i);
  }
  return 1;
}
