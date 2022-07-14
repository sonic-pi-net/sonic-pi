#include "aubio-types.h"

typedef struct
{
  PyObject_HEAD
  aubio_source_t * o;
  char_t* uri;
  uint_t samplerate;
  uint_t channels;
  uint_t hop_size;
  uint_t duration;
  PyObject *read_to;
  fvec_t c_read_to;
  PyObject *mread_to;
  fmat_t c_mread_to;
} Py_source;

static char Py_source_doc[] = ""
"source(path, samplerate=0, hop_size=512, channels=0)\n"
"\n"
"Read audio samples from a media file.\n"
"\n"
"`source` open the file specified in `path` and creates a callable\n"
"returning `hop_size` new audio samples at each invocation.\n"
"\n"
"If `samplerate=0` (default), the original sampling rate of `path`\n"
"will be used. Otherwise, the output audio samples will be\n"
"resampled at the desired sampling-rate.\n"
"\n"
"If `channels=0` (default), the original number of channels\n"
"in `path` will be used. Otherwise, the output audio samples\n"
"will be down-mixed or up-mixed to the desired number of\n"
"channels.\n"
"\n"
"If `path` is a URL, a remote connection will be attempted to\n"
"open the resource and stream data from it.\n"
"\n"
"The parameter `hop_size` determines how many samples should be\n"
"read at each consecutive calls.\n"
"\n"
"Parameters\n"
"----------\n"
"path : str\n"
"   pathname (or URL) of the file to be opened for reading\n"
"samplerate : int, optional\n"
"   sampling rate of the file\n"
"hop_size : int, optional\n"
"   number of samples to be read per iteration\n"
"channels : int, optional\n"
"   number of channels of the file\n"
"\n"
"Examples\n"
"--------\n"
"By default, when only `path` is given, the file will be opened\n"
"with its original sampling rate and channel:\n"
"\n"
">>> src = aubio.source('stereo.wav')\n"
">>> src.uri, src.samplerate, src.channels, src.duration\n"
"('stereo.wav', 48000, 2, 86833)\n"
"\n"
"A typical loop to read all samples from a local file could\n"
"look like this:\n"
"\n"
">>> src = aubio.source('stereo.wav')\n"
">>> total_read = 0\n"
">>> while True:\n"
"...     samples, read = src()\n"
"...     # do something with samples\n"
"...     total_read += read\n"
"...     if read < src.hop_size:\n"
"...         break\n"
"...\n"
"\n"
"In a more Pythonic way, it can also look like this:\n"
"\n"
">>> total_read = 0\n"
">>> with aubio.source('stereo.wav') as src:\n"
"...     for frames in src:\n"
"...         total_read += samples.shape[-1]\n"
"...\n"
"\n"
".. rubric:: Basic interface\n"
"\n"
"`source` is a **callable**; its :meth:`__call__` method\n"
"returns a tuple containing:\n"
"\n"
"- a vector of shape `(hop_size,)`, filled with the `read` next\n"
"  samples available, zero-padded if `read < hop_size`\n"
"- `read`, an integer indicating the number of samples read\n"
"\n"
"To read the first `hop_size` samples from the source, simply call\n"
"the instance itself, with no argument:\n"
"\n"
">>> src = aubio.source('song.ogg')\n"
">>> samples, read = src()\n"
">>> samples.shape, read, src.hop_size\n"
"((512,), 512, 512)\n"
"\n"
"The first call returned the slice of samples `[0 : hop_size]`.\n"
"The next call will return samples `[hop_size: 2*hop_size]`.\n"
"\n"
"After several invocations of :meth:`__call__`, when reaching the end\n"
"of the opened stream, `read` might become less than `hop_size`:\n"
"\n"
">>> samples, read = src()\n"
">>> samples.shape, read\n"
"((512,), 354)\n"
"\n"
"The end of the vector `samples` is filled with zeros.\n"
"\n"
"After the end of the stream, `read` will be `0` since no more\n"
"samples are available:\n"
"\n"
">>> samples, read = src()\n"
">>> samples.shape, read\n"
"((512,), 0)\n"
"\n"
"**Note**: when the source has more than one channels, they\n"
"are be down-mixed to mono when invoking :meth:`__call__`.\n"
"To read from each individual channel, see :meth:`__next__`.\n"
"\n"
".. rubric:: ``for`` statements\n"
"\n"
"The `source` objects are **iterables**. This allows using them\n"
"directly in a ``for`` loop, which calls :meth:`__next__` until\n"
"the end of the stream is reached:\n"
"\n"
">>> src = aubio.source('stereo.wav')\n"
">>> for frames in src:\n"
">>>     print (frames.shape)\n"
"...\n"
"(2, 512)\n"
"(2, 512)\n"
"(2, 230)\n"
"\n"
"**Note**: When `next(self)` is called on a source with multiple\n"
"channels, an array of shape `(channels, read)` is returned,\n"
"unlike with :meth:`__call__` which always returns the down-mixed\n"
"channels.\n"
"\n"
"If the file is opened with a single channel, `next(self)` returns\n"
"an array of shape `(read,)`:\n"
"\n"
">>> src = aubio.source('stereo.wav', channels=1)\n"
">>> next(src).shape\n"
"(512,)\n"
"\n"
".. rubric:: ``with`` statements\n"
"\n"
"The `source` objects are **context managers**, which allows using\n"
"them in ``with`` statements:\n"
"\n"
">>> with aubio.source('audiotrack.wav') as source:\n"
"...     n_frames=0\n"
"...     for samples in source:\n"
"...         n_frames += len(samples)\n"
"...     print('read', n_frames, 'samples in', samples.shape[0], 'channels',\n"
"...         'from file \"%%s\"' %% source.uri)\n"
"...\n"
"read 239334 samples in 2 channels from file \"audiotrack.wav\"\n"
"\n"
"The file will be closed before exiting the statement.\n"
"\n"
"See also the methods implementing the context manager,\n"
":meth:`__enter__` and :meth:`__exit__`.\n"
"\n"
".. rubric:: Seeking and closing\n"
"\n"
"At any time, :meth:`seek` can be used to move to any position in\n"
"the file. For instance, to rewind to the start of the stream:\n"
"\n"
">>> src.seek(0)\n"
"\n"
"The opened file will be automatically closed when the object falls\n"
"out of scope and is scheduled for garbage collection.\n"
"\n"
"In some cases, it is useful to manually :meth:`close` a given source,\n"
"for instance to limit the number of simultaneously opened files:\n"
"\n"
">>> src.close()\n"
"\n"
".. rubric:: Input formats\n"
"\n"
"Depending on how aubio was compiled, :class:`source` may or may not\n"
"open certain **files format**. Below are some examples that assume\n"
"support for compressed files and remote urls was compiled in:\n"
"\n"
"- open a local file using its original sampling rate and channels,\n"
"  and with the default hop size:\n"
"\n"
">>> s = aubio.source('sample.wav')\n"
">>> s.uri, s.samplerate, s.channels, s.hop_size\n"
"('sample.wav', 44100, 2, 512)\n"
"\n"
"- open a local compressed audio file, resampling to 32000Hz if needed:\n"
"\n"
">>> s = aubio.source('song.mp3', samplerate=32000)\n"
">>> s.uri, s.samplerate, s.channels, s.hop_size\n"
"('song.mp3', 32000, 2, 512)\n"
"\n"
"- open a local video file, down-mixing and resampling it to 16kHz:\n"
"\n"
">>> s = aubio.source('movie.mp4', samplerate=16000, channels=1)\n"
">>> s.uri, s.samplerate, s.channels, s.hop_size\n"
"('movie.mp4', 16000, 1, 512)\n"
"\n"
"- open a remote resource, with hop_size = 1024:\n"
"\n"
">>> s = aubio.source('https://aubio.org/drum.ogg', hop_size=1024)\n"
">>> s.uri, s.samplerate, s.channels, s.hop_size\n"
"('https://aubio.org/drum.ogg', 48000, 2, 1024)\n"
"\n"
"See Also\n"
"--------\n"
"sink: write audio samples to a file.\n"
"";

static char Py_source_get_samplerate_doc[] = ""
"get_samplerate()\n"
"\n"
"Get sampling rate of source.\n"
"\n"
"Returns\n"
"-------\n"
"int\n"
"    Sampling rate, in Hz.\n"
"";

static char Py_source_get_channels_doc[] = ""
"get_channels()\n"
"\n"
"Get number of channels in source.\n"
"\n"
"Returns\n"
"-------\n"
"int\n"
"    Number of channels.\n"
"";

static char Py_source_do_doc[] = ""
"source.do()\n"
"\n"
"Read vector of audio samples.\n"
"\n"
"If the audio stream in the source has more than one channel,\n"
"the channels will be down-mixed.\n"
"\n"
"Returns\n"
"-------\n"
"samples : numpy.ndarray\n"
"    `fvec` of size `hop_size` containing the new samples.\n"
"read : int\n"
"    Number of samples read from the source, equals to `hop_size`\n"
"    before the end-of-file is reached, less when it is reached,\n"
"    and `0` after.\n"
"\n"
"See Also\n"
"--------\n"
"do_multi\n"
"\n"
"Examples\n"
"--------\n"
">>> src = aubio.source('sample.wav', hop_size=1024)\n"
">>> src.do()\n"
"(array([-0.00123001, -0.00036685,  0.00097106, ..., -0.2031033 ,\n"
"       -0.2025854 , -0.20221856], dtype=" AUBIO_NPY_SMPL_STR "), 1024)\n"
"";

static char Py_source_do_multi_doc[] = ""
"do_multi()\n"
"\n"
"Read multiple channels of audio samples.\n"
"\n"
"If the source was opened with the same number of channels\n"
"found in the stream, each channel will be read individually.\n"
"\n"
"If the source was opened with less channels than the number\n"
"of channels in the stream, only the first channels will be read.\n"
"\n"
"If the source was opened with more channels than the number\n"
"of channel in the original stream, the first channels will\n"
"be duplicated on the additional output channel.\n"
"\n"
"Returns\n"
"-------\n"
"samples : numpy.ndarray\n"
"    NumPy array of shape `(hop_size, channels)` containing the new\n"
"    audio samples.\n"
"read : int\n"
"    Number of samples read from the source, equals to `hop_size`\n"
"    before the end-of-file is reached, less when it is reached,\n"
"    and `0` after.\n"
"\n"
"See Also\n"
"--------\n"
"do\n"
"\n"
"Examples\n"
"--------\n"
">>> src = aubio.source('sample.wav')\n"
">>> src.do_multi()\n"
"(array([[ 0.00668335,  0.0067749 ,  0.00714111, ..., -0.05737305,\n"
"        -0.05856323, -0.06018066],\n"
"       [-0.00842285, -0.0072937 , -0.00576782, ..., -0.09405518,\n"
"        -0.09558105, -0.09725952]], dtype=" AUBIO_NPY_SMPL_STR "), 512)\n"
"";

static char Py_source_close_doc[] = ""
"close()\n"
"\n"
"Close this source now.\n"
"\n"
".. note:: Closing twice a source will **not** raise any exception.\n"
"";

static char Py_source_seek_doc[] = ""
"seek(position)\n"
"\n"
"Seek to position in file.\n"
"\n"
"If the source was not opened with its original sampling-rate,\n"
"`position` corresponds to the position in the re-sampled file.\n"
"\n"
"Parameters\n"
"----------\n"
"position : str\n"
"   position to seek to, in samples\n"
"";

static PyObject *
Py_source_new (PyTypeObject * pytype, PyObject * args, PyObject * kwds)
{
  Py_source *self;
  char_t* uri = NULL;
  uint_t samplerate = 0;
  uint_t hop_size = 0;
  uint_t channels = 0;
  static char *kwlist[] = { "uri", "samplerate", "hop_size", "channels", NULL };

  if (!PyArg_ParseTupleAndKeywords (args, kwds, "|sIII", kwlist,
          &uri, &samplerate, &hop_size, &channels)) {
    return NULL;
  }

  self = (Py_source *) pytype->tp_alloc (pytype, 0);

  if (self == NULL) {
    return NULL;
  }

  self->uri = NULL;
  if (uri != NULL) {
    self->uri = (char_t *)malloc(sizeof(char_t) * (strnlen(uri, PATH_MAX) + 1));
    strncpy(self->uri, uri, strnlen(uri, PATH_MAX) + 1);
  }

  self->samplerate = 0;
  if ((sint_t)samplerate > 0) {
    self->samplerate = samplerate;
  } else if ((sint_t)samplerate < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative value for samplerate");
    return NULL;
  }

  self->hop_size = Py_default_vector_length / 2;
  if ((sint_t)hop_size > 0) {
    self->hop_size = hop_size;
  } else if ((sint_t)hop_size < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative value for hop_size");
    return NULL;
  }

  self->channels = 1;
  if ((sint_t)channels >= 0) {
    self->channels = channels;
  } else if ((sint_t)channels < 0) {
    PyErr_SetString (PyExc_ValueError,
        "can not use negative value for channels");
    return NULL;
  }

  return (PyObject *) self;
}

static int
Py_source_init (Py_source * self, PyObject * args, PyObject * kwds)
{
  self->o = new_aubio_source ( self->uri, self->samplerate, self->hop_size );
  if (self->o == NULL) {
    // PyErr_Format(PyExc_RuntimeError, ...) was set above by new_ which called
    // AUBIO_ERR when failing
    return -1;
  }
  self->samplerate = aubio_source_get_samplerate ( self->o );
  if (self->channels == 0) {
    self->channels = aubio_source_get_channels ( self->o );
  }
  self->duration = aubio_source_get_duration ( self->o );

  self->read_to = new_py_fvec(self->hop_size);
  self->mread_to = new_py_fmat(self->channels, self->hop_size);

  return 0;
}

static void
Py_source_del (Py_source *self, PyObject *unused)
{
  if (self->o) {
    del_aubio_source(self->o);
    free(self->c_mread_to.data);
  }
  if (self->uri) {
    free(self->uri);
  }
  Py_XDECREF(self->read_to);
  Py_XDECREF(self->mread_to);
  Py_TYPE(self)->tp_free((PyObject *) self);
}


/* function Py_source_do */
static PyObject *
Py_source_do(Py_source * self, PyObject * args)
{
  PyObject *outputs;
  uint_t read;
  read = 0;

  Py_INCREF(self->read_to);
  if (!PyAubio_ArrayToCFvec(self->read_to, &(self->c_read_to))) {
    return NULL;
  }
  /* compute _do function */
  aubio_source_do (self->o, &(self->c_read_to), &read);

  if (PyErr_Occurred() != NULL) {
    return NULL;
  }

  outputs = PyTuple_New(2);
  PyTuple_SetItem( outputs, 0, self->read_to );
  PyTuple_SetItem( outputs, 1, (PyObject *)PyLong_FromLong(read));
  return outputs;
}

/* function Py_source_do_multi */
static PyObject *
Py_source_do_multi(Py_source * self, PyObject * args)
{
  PyObject *outputs;
  uint_t read;
  read = 0;

  Py_INCREF(self->mread_to);
  if (!PyAubio_ArrayToCFmat(self->mread_to,  &(self->c_mread_to))) {
    return NULL;
  }
  /* compute _do function */
  aubio_source_do_multi (self->o, &(self->c_mread_to), &read);

  if (PyErr_Occurred() != NULL) {
    return NULL;
  }

  outputs = PyTuple_New(2);
  PyTuple_SetItem( outputs, 0, self->mread_to);
  PyTuple_SetItem( outputs, 1, (PyObject *)PyLong_FromLong(read));
  return outputs;
}

static PyMemberDef Py_source_members[] = {
  {"uri", T_STRING, offsetof (Py_source, uri), READONLY,
    "str (read-only): pathname or URL"},
  {"samplerate", T_INT, offsetof (Py_source, samplerate), READONLY,
    "int (read-only): sampling rate"},
  {"channels", T_INT, offsetof (Py_source, channels), READONLY,
    "int (read-only): number of channels"},
  {"hop_size", T_INT, offsetof (Py_source, hop_size), READONLY,
    "int (read-only): number of samples read per iteration"},
  {"duration", T_INT, offsetof (Py_source, duration), READONLY,
    "int (read-only): total number of frames in the source\n"
    "\n"
    "Can be estimated, for instance if the opened stream is\n"
    "a compressed media or a remote resource.\n"
    "\n"
    "Example\n"
    "-------\n"
    ">>> n = 0\n"
    ">>> src = aubio.source('track1.mp3')\n"
    ">>> for samples in src:\n"
    "...     n += samples.shape[-1]\n"
    "...\n"
    ">>> n, src.duration\n"
    "(9638784, 9616561)\n"
    ""},
  { NULL } // sentinel
};

static PyObject *
Pyaubio_source_get_samplerate (Py_source *self, PyObject *unused)
{
  uint_t tmp = aubio_source_get_samplerate (self->o);
  return (PyObject *)PyLong_FromLong (tmp);
}

static PyObject *
Pyaubio_source_get_channels (Py_source *self, PyObject *unused)
{
  uint_t tmp = aubio_source_get_channels (self->o);
  return (PyObject *)PyLong_FromLong (tmp);
}

static PyObject *
Pyaubio_source_close (Py_source *self, PyObject *unused)
{
  if (aubio_source_close(self->o) != 0) return NULL;
  Py_RETURN_NONE;
}

static PyObject *
Pyaubio_source_seek (Py_source *self, PyObject *args)
{
  uint_t err = 0;

  int position;
  if (!PyArg_ParseTuple (args, "I", &position)) {
    return NULL;
  }

  if (position < 0) {
    PyErr_Format(PyExc_ValueError,
        "error when seeking in source: can not seek to negative value %d",
        position);
    return NULL;
  }

  err = aubio_source_seek(self->o, position);
  if (err != 0) {
    PyErr_SetString (PyExc_ValueError,
        "error when seeking in source");
    return NULL;
  }
  Py_RETURN_NONE;
}

static char Pyaubio_source_enter_doc[] = "";
static PyObject* Pyaubio_source_enter(Py_source *self, PyObject *unused) {
  Py_INCREF(self);
  return (PyObject*)self;
}

static char Pyaubio_source_exit_doc[] = "";
static PyObject* Pyaubio_source_exit(Py_source *self, PyObject *unused) {
  return Pyaubio_source_close(self, unused);
}

static PyObject* Pyaubio_source_iter(PyObject *self) {
  Py_INCREF(self);
  return (PyObject*)self;
}

static PyObject* Pyaubio_source_iter_next(Py_source *self) {
  PyObject *done, *size;
  if (self->channels == 1) {
    done = Py_source_do(self, NULL);
  } else {
    done = Py_source_do_multi(self, NULL);
  }
  if (!PyTuple_Check(done)) {
    PyErr_Format(PyExc_ValueError,
        "error when reading source: not opened?");
    return NULL;
  }
  size = PyTuple_GetItem(done, 1);
  if (size != NULL && PyLong_Check(size)) {
    if (PyLong_AsLong(size) == (long)self->hop_size) {
      PyObject *vec = PyTuple_GetItem(done, 0);
      return vec;
    } else if (PyLong_AsLong(size) > 0) {
      // short read, return a shorter array
      PyObject *vec = PyTuple_GetItem(done, 0);
      // take a copy to prevent resizing internal arrays
      PyArrayObject *shortread = (PyArrayObject*)PyArray_FROM_OTF(vec,
          NPY_NOTYPE, NPY_ARRAY_ENSURECOPY);
      PyArray_Dims newdims;
      PyObject *reshaped;
      newdims.len = PyArray_NDIM(shortread);
      newdims.ptr = PyArray_DIMS(shortread);
      // mono or multiple channels?
      if (newdims.len == 1) {
        newdims.ptr[0] = PyLong_AsLong(size);
      } else {
        newdims.ptr[1] = PyLong_AsLong(size);
      }
      reshaped = PyArray_Newshape(shortread, &newdims, NPY_CORDER);
      Py_DECREF(shortread);
      Py_DECREF(vec);
      return reshaped;
    } else {
      PyErr_SetNone(PyExc_StopIteration);
      return NULL;
    }
  } else {
    PyErr_SetNone(PyExc_StopIteration);
    return NULL;
  }
}

static PyMethodDef Py_source_methods[] = {
  {"get_samplerate", (PyCFunction) Pyaubio_source_get_samplerate,
    METH_NOARGS, Py_source_get_samplerate_doc},
  {"get_channels", (PyCFunction) Pyaubio_source_get_channels,
    METH_NOARGS, Py_source_get_channels_doc},
  {"do", (PyCFunction) Py_source_do,
    METH_NOARGS, Py_source_do_doc},
  {"do_multi", (PyCFunction) Py_source_do_multi,
    METH_NOARGS, Py_source_do_multi_doc},
  {"close", (PyCFunction) Pyaubio_source_close,
    METH_NOARGS, Py_source_close_doc},
  {"seek", (PyCFunction) Pyaubio_source_seek,
    METH_VARARGS, Py_source_seek_doc},
  {"__enter__", (PyCFunction)Pyaubio_source_enter, METH_NOARGS,
    Pyaubio_source_enter_doc},
  {"__exit__",  (PyCFunction)Pyaubio_source_exit, METH_VARARGS,
    Pyaubio_source_exit_doc},
  {NULL} /* sentinel */
};

PyTypeObject Py_sourceType = {
  PyVarObject_HEAD_INIT (NULL, 0)
  "aubio.source",
  sizeof (Py_source),
  0,
  (destructor) Py_source_del,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  (ternaryfunc)Py_source_do,
  0,
  0,
  0,
  0,
  Py_TPFLAGS_DEFAULT,
  Py_source_doc,
  0,
  0,
  0,
  0,
  Pyaubio_source_iter,
  (unaryfunc)Pyaubio_source_iter_next,
  Py_source_methods,
  Py_source_members,
  0,
  0,
  0,
  0,
  0,
  0,
  (initproc) Py_source_init,
  0,
  Py_source_new,
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
