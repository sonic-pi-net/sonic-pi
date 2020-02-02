#include "aubio-types.h"

static char Py_filterbank_doc[] = ""
"filterbank(n_filters=40, win_s=1024)\n"
"\n"
"Create a bank of spectral filters. Each instance is a callable\n"
"that holds a matrix of coefficients.\n"
"\n"
"See also :meth:`set_mel_coeffs`, :meth:`set_mel_coeffs_htk`,\n"
":meth:`set_mel_coeffs_slaney`, :meth:`set_triangle_bands`, and\n"
":meth:`set_coeffs`.\n"
"\n"
"Parameters\n"
"----------\n"
"n_filters : int\n"
"    Number of filters to create.\n"
"win_s : int\n"
"    Size of the input spectrum to process.\n"
"\n"
"Examples\n"
"--------\n"
">>> f = aubio.filterbank(128, 1024)\n"
">>> f.set_mel_coeffs(44100, 0, 10000)\n"
">>> c = aubio.cvec(1024)\n"
">>> f(c).shape\n"
"(128, )\n"
"";

static char Py_filterbank_set_triangle_bands_doc[] =""
"set_triangle_bands(freqs, samplerate)\n"
"\n"
"Set triangular bands. The coefficients will be set to triangular\n"
"overlapping windows using the boundaries specified by `freqs`.\n"
"\n"
"`freqs` should contain `n_filters + 2` frequencies in Hz, ordered\n"
"by value, from smallest to largest. The first element should be greater\n"
"or equal to zero; the last element should be smaller or equal to\n"
"`samplerate / 2`.\n"
"\n"
"Parameters\n"
"----------\n"
"freqs: fvec\n"
"    List of frequencies, in Hz.\n"
"samplerate : float\n"
"    Sampling-rate of the expected input.\n"
"\n"
"Example\n"
"-------\n"
">>> fb = aubio.filterbank(n_filters=100, win_s=2048)\n"
">>> samplerate = 44100; freqs = np.linspace(0, 20200, 102)\n"
">>> fb.set_triangle_bands(aubio.fvec(freqs), samplerate)\n"
"";

static char Py_filterbank_set_mel_coeffs_slaney_doc[] = ""
"set_mel_coeffs_slaney(samplerate)\n"
"\n"
"Set coefficients of filterbank to match Slaney's Auditory Toolbox.\n"
"\n"
"The filter coefficients will be set as in Malcolm Slaney's\n"
"implementation. The filterbank should have been created with\n"
"`n_filters = 40`.\n"
"\n"
"This is approximately equivalent to using :meth:`set_mel_coeffs` with\n"
"`fmin = 400./3., fmax = 6853.84`.\n"
"\n"
"Parameters\n"
"----------\n"
"samplerate : float\n"
"    Sampling-rate of the expected input.\n"
"\n"
"References\n"
"----------\n"
"\n"
"Malcolm Slaney, `Auditory Toolbox Version 2, Technical Report #1998-010\n"
"<https://engineering.purdue.edu/~malcolm/interval/1998-010/>`_\n"
"";

static char Py_filterbank_set_mel_coeffs_doc[] = ""
"set_mel_coeffs(samplerate, fmin, fmax)\n"
"\n"
"Set coefficients of filterbank to linearly spaced mel scale.\n"
"\n"
"Parameters\n"
"----------\n"
"samplerate : float\n"
"    Sampling-rate of the expected input.\n"
"fmin : float\n"
"    Lower frequency boundary of the first filter.\n"
"fmax : float\n"
"    Upper frequency boundary of the last filter.\n"
"\n"
"See also\n"
"--------\n"
"hztomel\n"
"";

static char Py_filterbank_set_mel_coeffs_htk_doc[] = ""
"set_mel_coeffs_htk(samplerate, fmin, fmax)\n"
"\n"
"Set coefficients of the filters to be linearly spaced in the HTK mel scale.\n"
"\n"
"Parameters\n"
"----------\n"
"samplerate : float\n"
"    Sampling-rate of the expected input.\n"
"fmin : float\n"
"    Lower frequency boundary of the first filter.\n"
"fmax : float\n"
"    Upper frequency boundary of the last filter.\n"
"\n"
"See also\n"
"--------\n"
"hztomel with `htk=True`\n"
"";

static char Py_filterbank_get_coeffs_doc[] = ""
"get_coeffs()\n"
"\n"
"Get coefficients matrix of filterbank.\n"
"\n"
"Returns\n"
"-------\n"
"array_like\n"
"    Array of shape (n_filters, win_s/2+1) containing the coefficients.\n"
"";

static char Py_filterbank_set_coeffs_doc[] = ""
"set_coeffs(coeffs)\n"
"\n"
"Set coefficients of filterbank.\n"
"\n"
"Parameters\n"
"----------\n"
"coeffs : fmat\n"
"    Array of shape (n_filters, win_s/2+1) containing the coefficients.\n"
"";

static char Py_filterbank_set_power_doc[] = ""
"set_power(power)\n"
"\n"
"Set power applied to input spectrum of filterbank.\n"
"\n"
"Parameters\n"
"----------\n"
"power : float\n"
"    Power to raise input spectrum to before computing the filters.\n"
"";

static char Py_filterbank_get_power_doc[] = ""
"get_power()\n"
"\n"
"Get power applied to filterbank.\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"    Power parameter.\n"
"";

static char Py_filterbank_set_norm_doc[] = ""
"set_norm(norm)\n"
"\n"
"Set norm parameter. If set to `0`, the filters will not be normalized.\n"
"If set to `1`, the filters will be normalized to one. Default to `1`.\n"
"\n"
"This function should be called *before* :meth:`set_triangle_bands`,\n"
":meth:`set_mel_coeffs`, :meth:`set_mel_coeffs_htk`, or\n"
":meth:`set_mel_coeffs_slaney`.\n"
"\n"
"Parameters\n"
"----------\n"
"norm : int\n"
"   `0` to disable, `1` to enable\n"
"";

static char Py_filterbank_get_norm_doc[] = ""
"get_norm()\n"
"\n"
"Get norm parameter of filterbank.\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"    Norm parameter.\n"
"";

typedef struct
{
  PyObject_HEAD
  aubio_filterbank_t * o;
  uint_t n_filters;
  uint_t win_s;
  cvec_t vec;
  fvec_t freqs;
  fmat_t coeffs;
  PyObject *out;
  fvec_t c_out;
} Py_filterbank;

static PyObject *
Py_filterbank_new (PyTypeObject * type, PyObject * args, PyObject * kwds)
{
  int win_s = 0, n_filters = 0;
  Py_filterbank *self;
  static char *kwlist[] = { "n_filters", "win_s", NULL };

  if (!PyArg_ParseTupleAndKeywords (args, kwds, "|II", kwlist,
          &n_filters, &win_s)) {
    return NULL;
  }

  self = (Py_filterbank *) type->tp_alloc (type, 0);

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

  self->n_filters = 40;
  if (n_filters > 0) {
    self->n_filters = n_filters;
  } else if (n_filters < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative number of filters");
    return NULL;
  }

  return (PyObject *) self;
}

static int
Py_filterbank_init (Py_filterbank * self, PyObject * args, PyObject * kwds)
{
  self->o = new_aubio_filterbank (self->n_filters, self->win_s);
  if (self->o == NULL) {
    PyErr_Format(PyExc_RuntimeError, "error creating filterbank with"
        " n_filters=%d, win_s=%d", self->n_filters, self->win_s);
    return -1;
  }
  self->out = new_py_fvec(self->n_filters);

  return 0;
}

static void
Py_filterbank_del (Py_filterbank *self, PyObject *unused)
{
  if (self->o) {
    free(self->coeffs.data);
    del_aubio_filterbank(self->o);
  }
  Py_XDECREF(self->out);
  Py_TYPE(self)->tp_free((PyObject *) self);
}

static PyObject *
Py_filterbank_do(Py_filterbank * self, PyObject * args)
{
  PyObject *input;

  if (!PyArg_ParseTuple (args, "O", &input)) {
    return NULL;
  }

  if (!PyAubio_PyCvecToCCvec(input, &(self->vec) )) {
    return NULL;
  }

  if (self->vec.length != self->win_s / 2 + 1) {
    PyErr_Format(PyExc_ValueError,
                 "input cvec has length %d, but filterbank expects length %d",
                 self->vec.length, self->win_s / 2 + 1);
    return NULL;
  }

  Py_INCREF(self->out);
  if (!PyAubio_ArrayToCFvec(self->out, &(self->c_out))) {
    return NULL;
  }
  // compute the function
  aubio_filterbank_do (self->o, &(self->vec), &(self->c_out));
  return self->out;
}

static PyMemberDef Py_filterbank_members[] = {
  {"win_s", T_INT, offsetof (Py_filterbank, win_s), READONLY,
    "size of the window"},
  {"n_filters", T_INT, offsetof (Py_filterbank, n_filters), READONLY,
    "number of filters"},
  {NULL} /* sentinel */
};

static PyObject *
Py_filterbank_set_triangle_bands (Py_filterbank * self, PyObject *args)
{
  uint_t err = 0;

  PyObject *input;
  smpl_t samplerate;
  if (!PyArg_ParseTuple (args, "O" AUBIO_NPY_SMPL_CHR, &input, &samplerate)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &(self->freqs) )) {
    return NULL;
  }

  err = aubio_filterbank_set_triangle_bands (self->o,
      &(self->freqs), samplerate);
  if (err > 0) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError, "error running set_triangle_bands");
    } else {
      // change the RuntimeError into ValueError
      PyObject *type, *value, *traceback;
      PyErr_Fetch(&type, &value, &traceback);
      Py_XDECREF(type);
      type = PyExc_ValueError;
      Py_XINCREF(type);
      PyErr_Restore(type, value, traceback);
    }
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyObject *
Py_filterbank_set_mel_coeffs_slaney (Py_filterbank * self, PyObject *args)
{
  uint_t err = 0;

  smpl_t samplerate;
  if (!PyArg_ParseTuple (args, AUBIO_NPY_SMPL_CHR, &samplerate)) {
    return NULL;
  }

  err = aubio_filterbank_set_mel_coeffs_slaney (self->o, samplerate);
  if (err > 0) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError, "error running set_mel_coeffs_slaney");
    } else {
      // change the RuntimeError into ValueError
      PyObject *type, *value, *traceback;
      PyErr_Fetch(&type, &value, &traceback);
      Py_XDECREF(type);
      type = PyExc_ValueError;
      Py_XINCREF(type);
      PyErr_Restore(type, value, traceback);
    }
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyObject *
Py_filterbank_set_mel_coeffs (Py_filterbank * self, PyObject *args)
{
  uint_t err = 0;

  smpl_t samplerate;
  smpl_t freq_min;
  smpl_t freq_max;
  if (!PyArg_ParseTuple (args, AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR
        AUBIO_NPY_SMPL_CHR, &samplerate, &freq_min, &freq_max)) {
    return NULL;
  }

  err = aubio_filterbank_set_mel_coeffs (self->o, samplerate,
      freq_min, freq_max);
  if (err > 0) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError, "error running set_mel_coeffs");
    } else {
      // change the RuntimeError into ValueError
      PyObject *type, *value, *traceback;
      PyErr_Fetch(&type, &value, &traceback);
      Py_XDECREF(type);
      type = PyExc_ValueError;
      Py_XINCREF(type);
      PyErr_Restore(type, value, traceback);
    }
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyObject *
Py_filterbank_set_mel_coeffs_htk (Py_filterbank * self, PyObject *args)
{
  uint_t err = 0;

  smpl_t samplerate;
  smpl_t freq_min;
  smpl_t freq_max;
  if (!PyArg_ParseTuple (args, AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR
        AUBIO_NPY_SMPL_CHR, &samplerate, &freq_min, &freq_max)) {
    return NULL;
  }

  err = aubio_filterbank_set_mel_coeffs_htk (self->o, samplerate,
      freq_min, freq_max);
  if (err > 0) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError, "error running set_mel_coeffs_htk");
    } else {
      // change the RuntimeError into ValueError
      PyObject *type, *value, *traceback;
      PyErr_Fetch(&type, &value, &traceback);
      Py_XDECREF(type);
      type = PyExc_ValueError;
      Py_XINCREF(type);
      PyErr_Restore(type, value, traceback);
    }
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyObject *
Py_filterbank_set_coeffs (Py_filterbank * self, PyObject *args)
{
  uint_t err = 0;

  PyObject *input;
  if (!PyArg_ParseTuple (args, "O", &input)) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFmat(input, &(self->coeffs))) {
    return NULL;
  }

  err = aubio_filterbank_set_coeffs (self->o, &(self->coeffs));

  if (err > 0) {
    PyErr_SetString (PyExc_ValueError,
        "error when setting filter coefficients");
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyObject *
Py_filterbank_get_coeffs (Py_filterbank * self, PyObject *unused)
{
  return (PyObject *)PyAubio_CFmatToArray(
      aubio_filterbank_get_coeffs (self->o) );
}

static PyObject *
Py_filterbank_set_power(Py_filterbank *self, PyObject *args)
{
  smpl_t power;

  if (!PyArg_ParseTuple (args, AUBIO_NPY_SMPL_CHR, &power)) {
    return NULL;
  }
  if(aubio_filterbank_set_power (self->o, power)) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError,
          "error running filterbank.set_power");
    } else {
      // change the RuntimeError into ValueError
      PyObject *type, *value, *traceback;
      PyErr_Fetch(&type, &value, &traceback);
      Py_XDECREF(type);
      type = PyExc_ValueError;
      Py_XINCREF(type);
      PyErr_Restore(type, value, traceback);
    }
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyObject *
Py_filterbank_get_power (Py_filterbank * self, PyObject *unused)
{
  smpl_t power = aubio_filterbank_get_power(self->o);
  return (PyObject *)PyFloat_FromDouble (power);
}

static PyObject *
Py_filterbank_set_norm(Py_filterbank *self, PyObject *args)
{
  smpl_t norm;

  if (!PyArg_ParseTuple (args, AUBIO_NPY_SMPL_CHR, &norm)) {
    return NULL;
  }
  if(aubio_filterbank_set_norm (self->o, norm)) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError,
          "error running filterbank.set_power");
    } else {
      // change the RuntimeError into ValueError
      PyObject *type, *value, *traceback;
      PyErr_Fetch(&type, &value, &traceback);
      Py_XDECREF(type);
      type = PyExc_ValueError;
      Py_XINCREF(type);
      PyErr_Restore(type, value, traceback);
    }
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyObject *
Py_filterbank_get_norm (Py_filterbank * self, PyObject *unused)
{
  smpl_t norm = aubio_filterbank_get_norm(self->o);
  return (PyObject *)PyFloat_FromDouble (norm);
}

static PyMethodDef Py_filterbank_methods[] = {
  {"set_triangle_bands", (PyCFunction) Py_filterbank_set_triangle_bands,
    METH_VARARGS, Py_filterbank_set_triangle_bands_doc},
  {"set_mel_coeffs_slaney", (PyCFunction) Py_filterbank_set_mel_coeffs_slaney,
    METH_VARARGS, Py_filterbank_set_mel_coeffs_slaney_doc},
  {"set_mel_coeffs", (PyCFunction) Py_filterbank_set_mel_coeffs,
    METH_VARARGS, Py_filterbank_set_mel_coeffs_doc},
  {"set_mel_coeffs_htk", (PyCFunction) Py_filterbank_set_mel_coeffs_htk,
    METH_VARARGS, Py_filterbank_set_mel_coeffs_htk_doc},
  {"get_coeffs", (PyCFunction) Py_filterbank_get_coeffs,
    METH_NOARGS, Py_filterbank_get_coeffs_doc},
  {"set_coeffs", (PyCFunction) Py_filterbank_set_coeffs,
    METH_VARARGS, Py_filterbank_set_coeffs_doc},
  {"set_power", (PyCFunction) Py_filterbank_set_power,
    METH_VARARGS, Py_filterbank_set_power_doc},
  {"get_power", (PyCFunction) Py_filterbank_get_power,
    METH_NOARGS, Py_filterbank_get_power_doc},
  {"set_norm", (PyCFunction) Py_filterbank_set_norm,
    METH_VARARGS, Py_filterbank_set_norm_doc},
  {"get_norm", (PyCFunction) Py_filterbank_get_norm,
    METH_NOARGS, Py_filterbank_get_norm_doc},
  {NULL}
};

PyTypeObject Py_filterbankType = {
  PyVarObject_HEAD_INIT (NULL, 0)
  "aubio.filterbank",
  sizeof (Py_filterbank),
  0,
  (destructor) Py_filterbank_del,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  (ternaryfunc)Py_filterbank_do,
  0,
  0,
  0,
  0,
  Py_TPFLAGS_DEFAULT,
  Py_filterbank_doc,
  0,
  0,
  0,
  0,
  0,
  0,
  Py_filterbank_methods,
  Py_filterbank_members,
  0,
  0,
  0,
  0,
  0,
  0,
  (initproc) Py_filterbank_init,
  0,
  Py_filterbank_new,
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
