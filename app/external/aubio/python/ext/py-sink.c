#include "aubio-types.h"

typedef struct
{
  PyObject_HEAD
  aubio_sink_t * o;
  char_t* uri;
  uint_t samplerate;
  uint_t channels;
  fvec_t write_data;
  fmat_t mwrite_data;
} Py_sink;

static char Py_sink_doc[] = ""
"sink(path, samplerate=44100, channels=1)\n"
"\n"
"Write audio samples to file.\n"
"\n"
"Parameters\n"
"----------\n"
"path : str\n"
"   Pathname of the file to be opened for writing.\n"
"samplerate : int\n"
"   Sampling rate of the file, in Hz.\n"
"channels : int\n"
"   Number of channels to create the file with.\n"
"\n"
"Examples\n"
"--------\n"
"\n"
"Create a new sink at 44100Hz, mono:\n"
"\n"
">>> snk = aubio.sink('out.wav')\n"
"\n"
"Create a new sink at 32000Hz, stereo, write 100 samples into it:\n"
"\n"
">>> snk = aubio.sink('out.wav', samplerate=16000, channels=3)\n"
">>> snk(aubio.fvec(100), 100)\n"
"\n"
"Open a new sink at 48000Hz, stereo, write `1234` samples into it:\n"
"\n"
">>> with aubio.sink('out.wav', samplerate=48000, channels=2) as src:\n"
"...     snk(aubio.fvec(1024), 1024)\n"
"...     snk(aubio.fvec(210), 210)\n"
"...\n"
"\n"
"See also\n"
"--------\n"
"source: read audio samples from a file.\n"
"\n";

static char Py_sink_do_doc[] = ""
"do(vec, write)\n"
"\n"
"Write a single channel vector to sink.\n"
"\n"
"Parameters\n"
"----------\n"
"vec : fvec\n"
"   input vector `(n,)` where `n >= 0`.\n"
"write : int\n"
"   Number of samples to write.\n"
"";

static char Py_sink_do_multi_doc[] = ""
"do_multi(mat, write)\n"
"\n"
"Write a matrix containing vectors from multiple channels to sink.\n"
"\n"
"Parameters\n"
"----------\n"
"mat : numpy.ndarray\n"
"   input matrix of shape `(channels, n)`, where `n >= 0`.\n"
"write : int\n"
"   Number of frames to write.\n"
"";

static char Py_sink_close_doc[] = ""
"close()\n"
"\n"
"Close this sink now.\n"
"\n"
"By default, the sink will be closed before being deleted.\n"
"Explicitly closing a sink can be useful to control the number\n"
"of files simultaneously opened.\n"
"";

static PyObject *
Py_sink_new (PyTypeObject * pytype, PyObject * args, PyObject * kwds)
{
  Py_sink *self;
  char_t* uri = NULL;
  uint_t samplerate = 0;
  uint_t channels = 0;
  static char *kwlist[] = { "uri", "samplerate", "channels", NULL };

  if (!PyArg_ParseTupleAndKeywords (args, kwds, "|sII", kwlist,
          &uri, &samplerate, &channels)) {
    return NULL;
  }

  self = (Py_sink *) pytype->tp_alloc (pytype, 0);

  if (self == NULL) {
    return NULL;
  }

  self->uri = NULL;
  if (uri != NULL) {
    self->uri = (char_t *)malloc(sizeof(char_t) * (strnlen(uri, PATH_MAX) + 1));
    strncpy(self->uri, uri, strnlen(uri, PATH_MAX) + 1);
  }

  self->samplerate = Py_aubio_default_samplerate;
  if (samplerate != 0) {
    self->samplerate = samplerate;
  }

  self->channels = 1;
  if (channels != 0) {
    self->channels = channels;
  }

  return (PyObject *) self;
}

static int
Py_sink_init (Py_sink * self, PyObject * args, PyObject * kwds)
{
  self->o = new_aubio_sink ( self->uri, 0 );
  if (self->o == NULL) {
    // error string was set in new_aubio_sink
    return -1;
  }
  if (aubio_sink_preset_channels(self->o, self->channels) != 0) {
    // error string was set in aubio_sink_preset_channels
    return -1;
  }
  if (aubio_sink_preset_samplerate(self->o, self->samplerate) != 0) {
    // error string was set in aubio_sink_preset_samplerate
    return -1;
  }

  self->samplerate = aubio_sink_get_samplerate ( self->o );
  self->channels = aubio_sink_get_channels ( self->o );

  return 0;
}

static void
Py_sink_del (Py_sink *self, PyObject *unused)
{
  if (self->o) {
    del_aubio_sink(self->o);
    free(self->mwrite_data.data);
  }
  if (self->uri) {
    free(self->uri);
  }
  Py_TYPE(self)->tp_free((PyObject *) self);
}

/* function Py_sink_do */
static PyObject *
Py_sink_do(Py_sink * self, PyObject * args)
{
  /* input vectors python prototypes */
  PyObject * write_data_obj;

  /* input vectors prototypes */
  uint_t write;


  if (!PyArg_ParseTuple (args, "OI", &write_data_obj, &write)) {
    return NULL;
  }

  /* input vectors parsing */
  if (!PyAubio_ArrayToCFvec(write_data_obj, &(self->write_data))) {
    return NULL;
  }


  /* compute _do function */
  aubio_sink_do (self->o, &(self->write_data), write);

  Py_RETURN_NONE;
}

/* function Py_sink_do_multi */
static PyObject *
Py_sink_do_multi(Py_sink * self, PyObject * args)
{
  /* input vectors python prototypes */
  PyObject * write_data_obj;

  /* input vectors prototypes */
  uint_t write;


  if (!PyArg_ParseTuple (args, "OI", &write_data_obj, &write)) {
    return NULL;
  }


  /* input vectors parsing */
  if (!PyAubio_ArrayToCFmat(write_data_obj, &(self->mwrite_data))) {
    return NULL;
  }

  /* compute _do function */
  aubio_sink_do_multi (self->o, &(self->mwrite_data), write);
  Py_RETURN_NONE;
}

static PyMemberDef Py_sink_members[] = {
  {"uri", T_STRING, offsetof (Py_sink, uri), READONLY,
    "str (read-only): Path at which the sink was created."},
  {"samplerate", T_INT, offsetof (Py_sink, samplerate), READONLY,
    "int (read-only): Samplerate at which the sink was created."},
  {"channels", T_INT, offsetof (Py_sink, channels), READONLY,
    "int (read-only): Number of channels with which the sink was created."},
  { NULL } // sentinel
};

static PyObject *
Pyaubio_sink_close (Py_sink *self, PyObject *unused)
{
  aubio_sink_close (self->o);
  Py_RETURN_NONE;
}

static char Pyaubio_sink_enter_doc[] = "";
static PyObject* Pyaubio_sink_enter(Py_sink *self, PyObject *unused) {
  Py_INCREF(self);
  return (PyObject*)self;
}

static char Pyaubio_sink_exit_doc[] = "";
static PyObject* Pyaubio_sink_exit(Py_sink *self, PyObject *unused) {
  return Pyaubio_sink_close(self, unused);
}

static PyMethodDef Py_sink_methods[] = {
  {"do", (PyCFunction) Py_sink_do, METH_VARARGS, Py_sink_do_doc},
  {"do_multi", (PyCFunction) Py_sink_do_multi, METH_VARARGS, Py_sink_do_multi_doc},
  {"close", (PyCFunction) Pyaubio_sink_close, METH_NOARGS, Py_sink_close_doc},
  {"__enter__", (PyCFunction)Pyaubio_sink_enter, METH_NOARGS,
    Pyaubio_sink_enter_doc},
  {"__exit__",  (PyCFunction)Pyaubio_sink_exit, METH_VARARGS,
    Pyaubio_sink_exit_doc},
  {NULL} /* sentinel */
};

PyTypeObject Py_sinkType = {
  PyVarObject_HEAD_INIT (NULL, 0)
  "aubio.sink",
  sizeof (Py_sink),
  0,
  (destructor) Py_sink_del,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  (ternaryfunc)Py_sink_do,
  0,
  0,
  0,
  0,
  Py_TPFLAGS_DEFAULT,
  Py_sink_doc,
  0,
  0,
  0,
  0,
  0,
  0,
  Py_sink_methods,
  Py_sink_members,
  0,
  0,
  0,
  0,
  0,
  0,
  (initproc) Py_sink_init,
  0,
  Py_sink_new,
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
