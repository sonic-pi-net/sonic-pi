#include "aubio-types.h"

static char Py_fft_doc[] = ""
"fft(size=1024)\n"
"\n"
"Compute Fast Fourier Transforms.\n"
"\n"
"Parameters\n"
"----------\n"
"size : int\n"
"    size of the FFT to compute\n"
"\n"
"Example\n"
"-------\n"
">>> x = aubio.fvec(512)\n"
">>> f = aubio.fft(512)\n"
">>> c = f(x); c\n"
"aubio cvec of 257 elements\n"
">>> x2 = f.rdo(c); x2.shape\n"
"(512,)\n"
"";

typedef struct
{
  PyObject_HEAD
  aubio_fft_t * o;
  uint_t win_s;
  // do / rdo input vectors
  fvec_t vecin;
  cvec_t cvecin;
  // do / rdo output results
  PyObject *doout;
  PyObject *rdoout;
} Py_fft;

static PyObject *
Py_fft_new (PyTypeObject * type, PyObject * args, PyObject * kwds)
{
  int win_s = 0;
  Py_fft *self;
  static char *kwlist[] = { "win_s", NULL };

  if (!PyArg_ParseTupleAndKeywords (args, kwds, "|I", kwlist,
          &win_s)) {
    return NULL;
  }

  self = (Py_fft *) type->tp_alloc (type, 0);

  if (self == NULL) {
    return NULL;
  }

  self->win_s = Py_default_vector_length;

  if (win_s > 0) {
    self->win_s = win_s;
  } else if (win_s < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative window size");
    return NULL;
  }

  return (PyObject *) self;
}

static int
Py_fft_init (Py_fft * self, PyObject * args, PyObject * kwds)
{
  self->o = new_aubio_fft (self->win_s);
  if (self->o == NULL) {
    // PyErr_Format(PyExc_RuntimeError, ...) was set above by new_ which called
    // AUBIO_ERR when failing
    return -1;
  }

  self->doout = new_py_cvec(self->win_s);
  self->rdoout = new_py_fvec(self->win_s);

  return 0;
}

static void
Py_fft_del (Py_fft *self, PyObject *unused)
{
  Py_XDECREF(self->doout);
  Py_XDECREF(self->rdoout);
  if (self->o) {
    del_aubio_fft(self->o);
  }
  Py_TYPE(self)->tp_free((PyObject *) self);
}

static PyObject *
Py_fft_do(Py_fft * self, PyObject * args)
{
  PyObject *input;
  cvec_t c_out;

  if (!PyArg_ParseTuple (args, "O", &input)) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &(self->vecin))) {
    return NULL;
  }

  if (self->vecin.length != self->win_s) {
    PyErr_Format(PyExc_ValueError,
                 "input array has length %d, but fft expects length %d",
                 self->vecin.length, self->win_s);
    return NULL;
  }

  Py_INCREF(self->doout);
  if (!PyAubio_PyCvecToCCvec(self->doout, &c_out)) {
    return NULL;
  }
  // compute the function
  aubio_fft_do (self->o, &(self->vecin), &c_out);
  return self->doout;
}

static PyMemberDef Py_fft_members[] = {
  {"win_s", T_INT, offsetof (Py_fft, win_s), READONLY,
    "size of the window"},
  {NULL}
};

static PyObject *
Py_fft_rdo(Py_fft * self, PyObject * args)
{
  PyObject *input;
  fvec_t out;

  if (!PyArg_ParseTuple (args, "O", &input)) {
    return NULL;
  }

  if (!PyAubio_PyCvecToCCvec (input, &(self->cvecin)) ) {
    return NULL;
  }

  if (self->cvecin.length != self->win_s / 2 + 1) {
    PyErr_Format(PyExc_ValueError,
                 "input cvec has length %d, but fft expects length %d",
                 self->cvecin.length, self->win_s / 2 + 1);
    return NULL;
  }

  Py_INCREF(self->rdoout);
  if (!PyAubio_ArrayToCFvec(self->rdoout, &out) ) {
    return NULL;
  }
  // compute the function
  aubio_fft_rdo (self->o, &(self->cvecin), &out);
  return self->rdoout;
}

static PyMethodDef Py_fft_methods[] = {
  {"rdo", (PyCFunction) Py_fft_rdo, METH_VARARGS,
    "synthesis of spectral grain"},
  {NULL}
};

PyTypeObject Py_fftType = {
  PyVarObject_HEAD_INIT (NULL, 0)
  "aubio.fft",
  sizeof (Py_fft),
  0,
  (destructor) Py_fft_del,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  (ternaryfunc)Py_fft_do,
  0,
  0,
  0,
  0,
  Py_TPFLAGS_DEFAULT,
  Py_fft_doc,
  0,
  0,
  0,
  0,
  0,
  0,
  Py_fft_methods,
  Py_fft_members,
  0,
  0,
  0,
  0,
  0,
  0,
  (initproc) Py_fft_init,
  0,
  Py_fft_new,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
};
