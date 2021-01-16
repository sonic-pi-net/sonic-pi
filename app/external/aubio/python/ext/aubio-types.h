#include <Python.h>
#include <structmember.h>

#include "aubio-docstrings.h"
#include "aubio-generated.h"

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION

// define numpy unique symbols for aubio
#define PY_ARRAY_UNIQUE_SYMBOL PYAUBIO_ARRAY_API
#define PY_UFUNC_UNIQUE_SYMBOL PYAUBIO_UFUNC_API

// only import array and ufunc from main module
#ifndef PY_AUBIO_MODULE_MAIN
#define NO_IMPORT_ARRAY
#endif
#include <numpy/arrayobject.h>
#ifndef PY_AUBIO_MODULE_UFUNC
#define NO_IMPORT_UFUNC
#else
#include <numpy/ufuncobject.h>
#endif

//#include <numpy/npy_3kcompat.h>

// import aubio
#define AUBIO_UNSTABLE 1
#ifdef USE_LOCAL_AUBIO
#include "aubio.h"
#else
#include <aubio/aubio.h>
#endif

#define Py_default_vector_length 1024

#define Py_aubio_default_samplerate 44100

#if HAVE_AUBIO_DOUBLE
// 64 bit precision with HAVE_AUBIO_DOUBLE=1
#define AUBIO_NPY_SMPL NPY_DOUBLE
#define AUBIO_NPY_SMPL_STR "float64"
#define AUBIO_NPY_SMPL_CHR "d"
#else
// default is 32 bit precision
#define AUBIO_NPY_SMPL NPY_FLOAT
#define AUBIO_NPY_SMPL_STR "float32"
#define AUBIO_NPY_SMPL_CHR "f"
#endif

#ifndef PATH_MAX
#ifdef MAX_PATH
#define PATH_MAX MAX_PATH
#else
#define PATH_MAX 1024
#endif
#endif

// compat with Python < 2.6
#ifndef Py_TYPE
#define Py_TYPE(ob) (((PyObject*)(ob))->ob_type)
#endif

extern PyTypeObject Py_cvecType;

PyObject * new_py_fvec(uint_t length);
PyObject * new_py_cvec(uint_t length);
PyObject * new_py_fmat(uint_t height, uint_t length);

// defined in aubio-proxy.c
extern int PyAubio_IsValidVector (PyObject *input);

extern PyObject *PyAubio_CFvecToArray (fvec_t * self);
extern int PyAubio_ArrayToCFvec (PyObject * self, fvec_t *out);

extern int PyAubio_PyCvecToCCvec (PyObject *input, cvec_t *i);

extern PyObject *PyAubio_CFmatToArray (fmat_t * self);
extern int PyAubio_ArrayToCFmat (PyObject *input, fmat_t *out);

// hand written wrappers
extern PyTypeObject Py_filterType;

extern PyTypeObject Py_filterbankType;

extern PyTypeObject Py_fftType;

extern PyTypeObject Py_pvocType;

extern PyTypeObject Py_sourceType;

extern PyTypeObject Py_sinkType;
