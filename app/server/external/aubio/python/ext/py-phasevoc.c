#include "aubio-types.h"

static char Py_pvoc_doc[] = ""
"pvoc(win_s=512, hop_s=256)\n"
"\n"
"Phase vocoder.\n"
"\n"
"`pvoc` creates callable object implements a phase vocoder [1]_,\n"
"using the tricks detailed in [2]_.\n"
"\n"
"The call function takes one input of type `fvec` and of size\n"
"`hop_s`, and returns a `cvec` of length `win_s//2+1`.\n"
"\n"
"Parameters\n"
"----------\n"
"win_s : int\n"
"  number of channels in the phase-vocoder.\n"
"hop_s : int\n"
"  number of samples expected between each call\n"
"\n"
"Examples\n"
"--------\n"
">>> x = aubio.fvec(256)\n"
">>> pv = aubio.pvoc(512, 256)\n"
">>> pv(x)\n"
"aubio cvec of 257 elements\n"
"\n"
"Default values for hop_s and win_s are provided:\n"
"\n"
">>> pv = aubio.pvoc()\n"
">>> pv.win_s, pv.hop_s\n"
"512, 256\n"
"\n"
"A `cvec` can be resynthesised using `rdo()`:\n"
"\n"
">>> pv = aubio.pvoc(512, 256)\n"
">>> y = aubio.cvec(512)\n"
">>> x_reconstructed = pv.rdo(y)\n"
">>> x_reconstructed.shape\n"
"(256,)\n"
"\n"
"References\n"
"----------\n"
".. [1] James A. Moorer. The use of the phase vocoder in computer music\n"
"   applications. `Journal of the Audio Engineering Society`,\n"
"   26(1/2):42–45, 1978.\n"
".. [2] Amalia de Götzen, Nicolas Bernardini, and Daniel Arfib. Traditional\n"
"   (?) implementations of a phase vocoder: the tricks of the trade.\n"
"   In `Proceedings of the International Conference on Digital Audio\n"
"   Effects` (DAFx-00), pages 37–44, University of Verona, Italy, 2000.\n"
"   (`online version <"
"https://www.cs.princeton.edu/courses/archive/spr09/cos325/Bernardini.pdf"
">`_).\n"
"";


typedef struct
{
  PyObject_HEAD
  aubio_pvoc_t * o;
  uint_t win_s;
  uint_t hop_s;
  fvec_t vecin;
  cvec_t cvecin;
  PyObject *output;
  cvec_t c_output;
  PyObject *routput;
  fvec_t c_routput;
} Py_pvoc;


static PyObject *
Py_pvoc_new (PyTypeObject * type, PyObject * args, PyObject * kwds)
{
  int win_s = 0, hop_s = 0;
  Py_pvoc *self;
  static char *kwlist[] = { "win_s", "hop_s", NULL };

  if (!PyArg_ParseTupleAndKeywords (args, kwds, "|II", kwlist,
          &win_s, &hop_s)) {
    return NULL;
  }

  self = (Py_pvoc *) type->tp_alloc (type, 0);

  if (self == NULL) {
    return NULL;
  }

  self->win_s = Py_default_vector_length;
  self->hop_s = Py_default_vector_length/2;

  if (win_s > 0) {
    self->win_s = win_s;
  } else if (win_s < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative window size");
    return NULL;
  }

  if (hop_s > 0) {
    self->hop_s = hop_s;
  } else if (hop_s < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative hop size");
    return NULL;
  }

  return (PyObject *) self;
}

static int
Py_pvoc_init (Py_pvoc * self, PyObject * args, PyObject * kwds)
{
  self->o = new_aubio_pvoc ( self->win_s, self->hop_s);
  if (self->o == NULL) {
    // PyErr_Format(PyExc_RuntimeError, ...) was set above by new_ which called
    // AUBIO_ERR when failing
    return -1;
  }

  self->output = new_py_cvec(self->win_s);
  self->routput = new_py_fvec(self->hop_s);

  return 0;
}


static void
Py_pvoc_del (Py_pvoc *self, PyObject *unused)
{
  Py_XDECREF(self->output);
  Py_XDECREF(self->routput);
  if (self->o) {
    del_aubio_pvoc(self->o);
  }
  Py_TYPE(self)->tp_free((PyObject *) self);
}


static PyObject *
Py_pvoc_do(Py_pvoc * self, PyObject * args)
{
  PyObject *input;

  if (!PyArg_ParseTuple (args, "O", &input)) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec (input, &(self->vecin) )) {
    return NULL;
  }

  if (self->vecin.length != self->hop_s) {
    PyErr_Format(PyExc_ValueError,
                 "input fvec has length %d, but pvoc expects length %d",
                 self->vecin.length, self->hop_s);
    return NULL;
  }

  Py_INCREF(self->output);
  if (!PyAubio_PyCvecToCCvec (self->output, &(self->c_output))) {
    return NULL;
  }
  // compute the function
  aubio_pvoc_do (self->o, &(self->vecin), &(self->c_output));
  return self->output;
}

static PyMemberDef Py_pvoc_members[] = {
  {"win_s", T_INT, offsetof (Py_pvoc, win_s), READONLY,
    "int: Size of phase vocoder analysis windows, in samples.\n"
    ""},
  {"hop_s", T_INT, offsetof (Py_pvoc, hop_s), READONLY,
    "int: Interval between two analysis, in samples.\n"
    ""},
  { NULL } // sentinel
};

static PyObject *
Py_pvoc_rdo(Py_pvoc * self, PyObject * args)
{
  PyObject *input;
  if (!PyArg_ParseTuple (args, "O", &input)) {
    return NULL;
  }

  if (!PyAubio_PyCvecToCCvec (input, &(self->cvecin) )) {
    return NULL;
  }

  if (self->cvecin.length != self->win_s / 2 + 1) {
    PyErr_Format(PyExc_ValueError,
                 "input cvec has length %d, but pvoc expects length %d",
                 self->cvecin.length, self->win_s / 2 + 1);
    return NULL;
  }

  Py_INCREF(self->routput);
  if (!PyAubio_ArrayToCFvec(self->routput, &(self->c_routput)) ) {
    return NULL;
  }
  // compute the function
  aubio_pvoc_rdo (self->o, &(self->cvecin), &(self->c_routput));
  return self->routput;
}

static PyObject *
Pyaubio_pvoc_set_window (Py_pvoc *self, PyObject *args)
{
  uint_t err = 0;
  char_t *window = NULL;

  if (!PyArg_ParseTuple (args, "s", &window)) {
    return NULL;
  }
  err = aubio_pvoc_set_window (self->o, window);

  if (err > 0) {
    PyErr_SetString (PyExc_ValueError, "error running aubio_pvoc_set_window");
    return NULL;
  }
  Py_RETURN_NONE;
}

static PyMethodDef Py_pvoc_methods[] = {
  {"rdo", (PyCFunction) Py_pvoc_rdo, METH_VARARGS,
    "rdo(fftgrain)\n"
    "\n"
    "Read a new spectral grain and resynthesise the next `hop_s`\n"
    "output samples.\n"
    "\n"
    "Parameters\n"
    "----------\n"
    "fftgrain : cvec\n"
    "    new input `cvec` to synthesize from, should be of size `win_s/2+1`\n"
    "\n"
    "Returns\n"
    "-------\n"
    "fvec\n"
    "    re-synthesised output of shape `(hop_s,)`\n"
    "\n"
    "Example\n"
    "-------\n"
    ">>> pv = aubio.pvoc(2048, 512)\n"
    ">>> out = pv.rdo(aubio.cvec(2048))\n"
    ">>> out.shape\n"
    "(512,)\n"
    ""},
  {"set_window", (PyCFunction) Pyaubio_pvoc_set_window, METH_VARARGS,
    "set_window(window_type)\n"
    "\n"
    "Set window function\n"
    "\n"
    "Parameters\n"
    "----------\n"
    "window_type : str\n"
    "    the window type to use for this phase vocoder\n"
    "\n"
    "Raises\n"
    "------\n"
    "ValueError\n"
    "    If an unknown window type was given.\n"
    "\n"
    "See Also\n"
    "--------\n"
    "window : create a window.\n"
    ""},
  {NULL}
};

PyTypeObject Py_pvocType = {
  PyVarObject_HEAD_INIT (NULL, 0)
  "aubio.pvoc",
  sizeof (Py_pvoc),
  0,
  (destructor) Py_pvoc_del,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  (ternaryfunc)Py_pvoc_do,
  0,
  0,
  0,
  0,
  Py_TPFLAGS_DEFAULT,
  Py_pvoc_doc,
  0,
  0,
  0,
  0,
  0,
  0,
  Py_pvoc_methods,
  Py_pvoc_members,
  0,
  0,
  0,
  0,
  0,
  0,
  (initproc) Py_pvoc_init,
  0,
  Py_pvoc_new,
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
