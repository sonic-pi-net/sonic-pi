#include "aubio-types.h"

typedef struct
{
  PyObject_HEAD
  aubio_filter_t * o;
  uint_t order;
  fvec_t vec;
  PyObject *out;
  fvec_t c_out;
} Py_filter;

static char Py_filter_doc[] = ""
"digital_filter(order=7)\n"
"\n"
"Create a digital filter.\n"
"";

static char Py_filter_set_c_weighting_doc[] = ""
"set_c_weighting(samplerate)\n"
"\n"
"Set filter coefficients to C-weighting.\n"
"\n"
"`samplerate` should be one of 8000, 11025, 16000, 22050, 24000, 32000,\n"
"44100, 48000, 88200, 96000, or 192000. `order` of the filter should be 5.\n"
"\n"
"Parameters\n"
"----------\n"
"samplerate : int\n"
"    Sampling-rate of the input signal, in Hz.\n"
"";

static char Py_filter_set_a_weighting_doc[] = ""
"set_a_weighting(samplerate)\n"
"\n"
"Set filter coefficients to A-weighting.\n"
"\n"
"`samplerate` should be one of 8000, 11025, 16000, 22050, 24000, 32000,\n"
"44100, 48000, 88200, 96000, or 192000. `order` of the filter should be 7.\n"
"\n"
"Parameters\n"
"----------\n"
"samplerate : int\n"
"    Sampling-rate of the input signal.\n"
"";

static char Py_filter_set_biquad_doc[] = ""
"set_biquad(b0, b1, b2, a1, a2)\n"
"\n"
"Set biquad coefficients. `order` of the filter should be 3.\n"
"\n"
"Parameters\n"
"----------\n"
"b0 : float\n"
"    Forward filter coefficient.\n"
"b1 : float\n"
"    Forward filter coefficient.\n"
"b2 : float\n"
"    Forward filter coefficient.\n"
"a1 : float\n"
"    Feedback filter coefficient.\n"
"a2 : float\n"
"    Feedback filter coefficient.\n"
"";

static PyObject *
Py_filter_new (PyTypeObject * type, PyObject * args, PyObject * kwds)
{
  int order= 0;
  Py_filter *self;
  static char *kwlist[] = { "order", NULL };

  if (!PyArg_ParseTupleAndKeywords (args, kwds, "|I", kwlist,
          &order)) {
    return NULL;
  }

  self = (Py_filter *) type->tp_alloc (type, 0);

  if (self == NULL) {
    return NULL;
  }

  self->order = 7;

  if (order > 0) {
    self->order = order;
  } else if (order < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative order");
    return NULL;
  }

  return (PyObject *) self;
}

static int
Py_filter_init (Py_filter * self, PyObject * args, PyObject * kwds)
{
  self->o = new_aubio_filter (self->order);
  if (self->o == NULL) {
    return -1;
  }
  self->out = NULL;
  return 0;
}

static void
Py_filter_del (Py_filter * self)
{
  Py_XDECREF(self->out);
  if (self->o)
    del_aubio_filter (self->o);
  Py_TYPE(self)->tp_free ((PyObject *) self);
}

static PyObject *
Py_filter_do(Py_filter * self, PyObject * args)
{
  PyObject *input;

  if (!PyArg_ParseTuple (args, "O:digital_filter.do", &input)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &(self->vec))) {
    return NULL;
  }

  // initialize output now
  if (self->out == NULL) {
    self->out = new_py_fvec(self->vec.length);
  }

  Py_INCREF(self->out);
  if (!PyAubio_ArrayToCFvec(self->out, &(self->c_out)) ) {
    return NULL;
  }
  // compute the function
  aubio_filter_do_outplace (self->o, &(self->vec), &(self->c_out));
  return self->out;
}

static PyObject *
Py_filter_set_c_weighting (Py_filter * self, PyObject *args)
{
  uint_t err = 0;
  uint_t samplerate;
  if (!PyArg_ParseTuple (args, "I", &samplerate)) {
    return NULL;
  }

  err = aubio_filter_set_c_weighting (self->o, samplerate);
  if (err > 0) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError,
          "error when setting filter to C-weighting");
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
Py_filter_set_a_weighting (Py_filter * self, PyObject *args)
{
  uint_t err = 0;
  uint_t samplerate;
  if (!PyArg_ParseTuple (args, "I", &samplerate)) {
    return NULL;
  }

  err = aubio_filter_set_a_weighting (self->o, samplerate);
  if (err > 0) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError,
          "error when setting filter to A-weighting");
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
Py_filter_set_biquad(Py_filter * self, PyObject *args)
{
  uint_t err = 0;
  lsmp_t b0, b1, b2, a1, a2;
  if (!PyArg_ParseTuple (args, "ddddd", &b0, &b1, &b2, &a1, &a2)) {
    return NULL;
  }

  err = aubio_filter_set_biquad (self->o, b0, b1, b2, a1, a2);
  if (err > 0) {
    if (PyErr_Occurred() == NULL) {
      PyErr_SetString (PyExc_ValueError,
          "error when setting filter with biquad coefficients");
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

static PyMemberDef Py_filter_members[] = {
  // TODO remove READONLY flag and define getter/setter
  {"order", T_INT, offsetof (Py_filter, order), READONLY,
      "order of the filter"},
  {NULL}                        /* Sentinel */
};

static PyMethodDef Py_filter_methods[] = {
  {"set_c_weighting", (PyCFunction) Py_filter_set_c_weighting, METH_VARARGS,
      Py_filter_set_c_weighting_doc},
  {"set_a_weighting", (PyCFunction) Py_filter_set_a_weighting, METH_VARARGS,
      Py_filter_set_a_weighting_doc},
  {"set_biquad", (PyCFunction) Py_filter_set_biquad, METH_VARARGS,
      Py_filter_set_biquad_doc},
  {NULL}
};

PyTypeObject Py_filterType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "aubio.digital_filter",       /* tp_name           */
  sizeof (Py_filter),           /* tp_basicsize      */
  0,                            /* tp_itemsize       */
  (destructor) Py_filter_del,   /* tp_dealloc        */
  0,                            /* tp_print          */
  0,                            /* tp_getattr        */
  0,                            /* tp_setattr        */
  0,                            /* tp_compare        */
  0, //(reprfunc) Py_filter_repr,    /* tp_repr           */
  0,                            /* tp_as_number      */
  0,                            /* tp_as_sequence    */
  0,                            /* tp_as_mapping     */
  0,                            /* tp_hash           */
  (ternaryfunc)Py_filter_do,    /* tp_call           */
  0,                            /* tp_str            */
  0,                            /* tp_getattro       */
  0,                            /* tp_setattro       */
  0,                            /* tp_as_buffer      */
  Py_TPFLAGS_DEFAULT,           /* tp_flags          */
  Py_filter_doc,                /* tp_doc            */
  0,                            /* tp_traverse       */
  0,                            /* tp_clear          */
  0,                            /* tp_richcompare    */
  0,                            /* tp_weaklistoffset */
  0,                            /* tp_iter           */
  0,                            /* tp_iternext       */
  Py_filter_methods,            /* tp_methods        */
  Py_filter_members,            /* tp_members        */
  0,                            /* tp_getset         */
  0,                            /* tp_base           */
  0,                            /* tp_dict           */
  0,                            /* tp_descr_get      */
  0,                            /* tp_descr_set      */
  0,                            /* tp_dictoffset     */
  (initproc) Py_filter_init,    /* tp_init           */
  0,                            /* tp_alloc          */
  Py_filter_new,                /* tp_new            */
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
