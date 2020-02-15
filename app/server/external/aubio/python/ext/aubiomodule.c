#define PY_AUBIO_MODULE_MAIN
#include "aubio-types.h"
#include "py-musicutils.h"

// this dummy macro is used to convince windows that a string passed as -D flag
// is just that, a string, and not a double.
#define REDEFINESTRING(x) #x
#define DEFINEDSTRING(x) REDEFINESTRING(x)

static char aubio_module_doc[] = "Python module for the aubio library";

static char Py_alpha_norm_doc[] = ""
"alpha_norm(vec, alpha)\n"
"\n"
"Compute `alpha` normalisation factor of vector `vec`.\n"
"\n"
"Parameters\n"
"----------\n"
"vec : fvec\n"
"   input vector\n"
"alpha : float\n"
"   norm factor\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"   p-norm of the input vector, where `p=alpha`\n"
"\n"
"Example\n"
"-------\n"
"\n"
">>> a = aubio.fvec(np.arange(10)); alpha = 2\n"
">>> aubio.alpha_norm(a, alpha), (sum(a**alpha)/len(a))**(1./alpha)\n"
"(5.338539123535156, 5.338539126015656)\n"
"\n"
"Note\n"
"----\n"
"Computed as:\n"
"\n"
".. math::\n"
"  l_{\\alpha} = \n"
"       \\|\\frac{\\sum_{n=0}^{N-1}{{x_n}^{\\alpha}}}{N}\\|^{1/\\alpha}\n"
"";

static char Py_bintomidi_doc[] = ""
"bintomidi(fftbin, samplerate, fftsize)\n"
"\n"
"Convert FFT bin to frequency in midi note, given the sampling rate\n"
"and the size of the FFT.\n"
"\n"
"Parameters\n"
"----------\n"
"fftbin : float\n"
"   input frequency bin\n"
"samplerate : float\n"
"   sampling rate of the signal\n"
"fftsize : float\n"
"   size of the FFT\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"   Frequency converted to midi note.\n"
"\n"
"Example\n"
"-------\n"
"\n"
">>> aubio.bintomidi(10, 44100, 1024)\n"
"68.62871551513672\n"
"";

static char Py_miditobin_doc[] = ""
"miditobin(midi, samplerate, fftsize)\n"
"\n"
"Convert frequency in midi note to FFT bin, given the sampling rate\n"
"and the size of the FFT.\n"
"\n"
"Parameters\n"
"----------\n"
"midi : float\n"
"   input frequency, in midi note\n"
"samplerate : float\n"
"   sampling rate of the signal\n"
"fftsize : float\n"
"   size of the FFT\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"   Frequency converted to FFT bin.\n"
"\n"
"Examples\n"
"--------\n"
"\n"
">>> aubio.miditobin(69, 44100, 1024)\n"
"10.216779708862305\n"
">>> aubio.miditobin(75.08, 32000, 512)\n"
"10.002175331115723\n"
"";

static char Py_bintofreq_doc[] = ""
"bintofreq(fftbin, samplerate, fftsize)\n"
"\n"
"Convert FFT bin to frequency in Hz, given the sampling rate\n"
"and the size of the FFT.\n"
"\n"
"Parameters\n"
"----------\n"
"fftbin : float\n"
"   input frequency bin\n"
"samplerate : float\n"
"   sampling rate of the signal\n"
"fftsize : float\n"
"   size of the FFT\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"   Frequency converted to Hz.\n"
"\n"
"Example\n"
"-------\n"
"\n"
">>> aubio.bintofreq(10, 44100, 1024)\n"
"430.6640625\n"
"";

static char Py_freqtobin_doc[] = ""
"freqtobin(freq, samplerate, fftsize)\n"
"\n"
"Convert frequency in Hz to FFT bin, given the sampling rate\n"
"and the size of the FFT.\n"
"\n"
"Parameters\n"
"----------\n"
"midi : float\n"
"   input frequency, in midi note\n"
"samplerate : float\n"
"   sampling rate of the signal\n"
"fftsize : float\n"
"   size of the FFT\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"   Frequency converted to FFT bin.\n"
"\n"
"Examples\n"
"--------\n"
"\n"
">>> aubio.freqtobin(440, 44100, 1024)\n"
"10.216779708862305\n"
"";

static char Py_zero_crossing_rate_doc[] = ""
"zero_crossing_rate(vec)\n"
"\n"
"Compute zero-crossing rate of `vec`.\n"
"\n"
"Parameters\n"
"----------\n"
"vec : fvec\n"
"   input vector\n"
"\n"
"Returns\n"
"-------\n"
"float\n"
"   Zero-crossing rate.\n"
"\n"
"Example\n"
"-------\n"
"\n"
">>> a = np.linspace(-1., 1., 1000, dtype=aubio.float_type)\n"
">>> aubio.zero_crossing_rate(a), 1/1000\n"
"(0.0010000000474974513, 0.001)\n"
"";

static char Py_min_removal_doc[] = ""
"min_removal(vec)\n"
"\n"
"Remove the minimum value of a vector to each of its element.\n"
"\n"
"Modifies the input vector in-place and returns a reference to it.\n"
"\n"
"Parameters\n"
"----------\n"
"vec : fvec\n"
"   input vector\n"
"\n"
"Returns\n"
"-------\n"
"fvec\n"
"   modified input vector\n"
"\n"
"Example\n"
"-------\n"
"\n"
">>> aubio.min_removal(aubio.fvec(np.arange(1,4)))\n"
"array([0., 1., 2.], dtype=" AUBIO_NPY_SMPL_STR ")\n"
"";

extern void add_ufuncs ( PyObject *m );
extern int generated_types_ready(void);

static PyObject *
Py_alpha_norm (PyObject * self, PyObject * args)
{
  PyObject *input;
  fvec_t vec;
  smpl_t alpha;
  PyObject *result;

  if (!PyArg_ParseTuple (args, "O" AUBIO_NPY_SMPL_CHR ":alpha_norm", &input, &alpha)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  // compute the function
  result = PyFloat_FromDouble(fvec_alpha_norm (&vec, alpha));
  if (result == NULL) {
    return NULL;
  }

  return result;
}

static PyObject *
Py_bintomidi (PyObject * self, PyObject * args)
{
  smpl_t input, samplerate, fftsize;
  smpl_t output;

  if (!PyArg_ParseTuple (args,
        "" AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR,
        &input, &samplerate, &fftsize)) {
    return NULL;
  }

  output = aubio_bintomidi (input, samplerate, fftsize);

  return (PyObject *)PyFloat_FromDouble (output);
}

static PyObject *
Py_miditobin (PyObject * self, PyObject * args)
{
  smpl_t input, samplerate, fftsize;
  smpl_t output;

  if (!PyArg_ParseTuple (args,
        "" AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR,
        &input, &samplerate, &fftsize)) {
    return NULL;
  }

  output = aubio_miditobin (input, samplerate, fftsize);

  return (PyObject *)PyFloat_FromDouble (output);
}

static PyObject *
Py_bintofreq (PyObject * self, PyObject * args)
{
  smpl_t input, samplerate, fftsize;
  smpl_t output;

  if (!PyArg_ParseTuple (args,
        "" AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR,
        &input, &samplerate, &fftsize)) {
    return NULL;
  }

  output = aubio_bintofreq (input, samplerate, fftsize);

  return (PyObject *)PyFloat_FromDouble (output);
}

static PyObject *
Py_freqtobin (PyObject * self, PyObject * args)
{
  smpl_t input, samplerate, fftsize;
  smpl_t output;

  if (!PyArg_ParseTuple (args,
        "" AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR AUBIO_NPY_SMPL_CHR,
        &input, &samplerate, &fftsize)) {
    return NULL;
  }

  output = aubio_freqtobin (input, samplerate, fftsize);

  return (PyObject *)PyFloat_FromDouble (output);
}

static PyObject *
Py_zero_crossing_rate (PyObject * self, PyObject * args)
{
  PyObject *input;
  fvec_t vec;
  PyObject *result;

  if (!PyArg_ParseTuple (args, "O:zero_crossing_rate", &input)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  // compute the function
  result = PyFloat_FromDouble(aubio_zero_crossing_rate (&vec));
  if (result == NULL) {
    return NULL;
  }

  return result;
}

static PyObject *
Py_min_removal(PyObject * self, PyObject * args)
{
  PyObject *input;
  fvec_t vec;

  if (!PyArg_ParseTuple (args, "O:min_removal", &input)) {
    return NULL;
  }

  if (input == NULL) {
    return NULL;
  }

  if (!PyAubio_ArrayToCFvec(input, &vec)) {
    return NULL;
  }

  // compute the function
  fvec_min_removal (&vec);

  // since this function does not return, we could return None
  //Py_RETURN_NONE;
  // however it is convenient to return the modified vector
  return (PyObject *) PyAubio_CFvecToArray(&vec);
  // or even without converting it back to an array
  //Py_INCREF(vec);
  //return (PyObject *)vec;
}

static PyMethodDef aubio_methods[] = {
  {"bintomidi", Py_bintomidi, METH_VARARGS, Py_bintomidi_doc},
  {"miditobin", Py_miditobin, METH_VARARGS, Py_miditobin_doc},
  {"bintofreq", Py_bintofreq, METH_VARARGS, Py_bintofreq_doc},
  {"freqtobin", Py_freqtobin, METH_VARARGS, Py_freqtobin_doc},
  {"alpha_norm", Py_alpha_norm, METH_VARARGS, Py_alpha_norm_doc},
  {"zero_crossing_rate", Py_zero_crossing_rate, METH_VARARGS, Py_zero_crossing_rate_doc},
  {"min_removal", Py_min_removal, METH_VARARGS, Py_min_removal_doc},
  {"level_lin", Py_aubio_level_lin, METH_VARARGS, Py_aubio_level_lin_doc},
  {"db_spl", Py_aubio_db_spl, METH_VARARGS, Py_aubio_db_spl_doc},
  {"silence_detection", Py_aubio_silence_detection, METH_VARARGS, Py_aubio_silence_detection_doc},
  {"level_detection", Py_aubio_level_detection, METH_VARARGS, Py_aubio_level_detection_doc},
  {"window", Py_aubio_window, METH_VARARGS, Py_aubio_window_doc},
  {"shift", Py_aubio_shift, METH_VARARGS, Py_aubio_shift_doc},
  {"ishift", Py_aubio_ishift, METH_VARARGS, Py_aubio_ishift_doc},
  {"hztomel", Py_aubio_hztomel, METH_VARARGS|METH_KEYWORDS, Py_aubio_hztomel_doc},
  {"meltohz", Py_aubio_meltohz, METH_VARARGS|METH_KEYWORDS, Py_aubio_meltohz_doc},
  {"hztomel_htk", Py_aubio_hztomel_htk, METH_VARARGS, Py_aubio_hztomel_htk_doc},
  {"meltohz_htk", Py_aubio_meltohz_htk, METH_VARARGS, Py_aubio_meltohz_htk_doc},
  {NULL, NULL, 0, NULL} /* Sentinel */
};

#if PY_MAJOR_VERSION >= 3
// Python3 module definition
static struct PyModuleDef moduledef = {
   PyModuleDef_HEAD_INIT,
   "_aubio",          /* m_name */
   aubio_module_doc,  /* m_doc */
   -1,                /* m_size */
   aubio_methods,     /* m_methods */
   NULL,              /* m_reload */
   NULL,              /* m_traverse */
   NULL,              /* m_clear */
   NULL,              /* m_free */
};
#endif

void
aubio_log_function(int level, const char *message, void *data)
{
  // remove trailing \n
  char *pos;
  if ((pos=strchr(message, '\n')) != NULL) {
        *pos = '\0';
  }
  // warning or error
  if (level == AUBIO_LOG_ERR) {
    PyErr_Format(PyExc_RuntimeError, "%s", message);
  } else {
    PyErr_WarnEx(PyExc_UserWarning, message, 1);
  }
}

static PyObject *
initaubio (void)
{
  PyObject *m = NULL;
  int err;

  // fvec is defined in __init__.py
  if (   (PyType_Ready (&Py_cvecType) < 0)
      || (PyType_Ready (&Py_filterType) < 0)
      || (PyType_Ready (&Py_filterbankType) < 0)
      || (PyType_Ready (&Py_fftType) < 0)
      || (PyType_Ready (&Py_pvocType) < 0)
      || (PyType_Ready (&Py_sourceType) < 0)
      || (PyType_Ready (&Py_sinkType) < 0)
      // generated objects
      || (generated_types_ready() < 0 )
  ) {
    return m;
  }

#if PY_MAJOR_VERSION >= 3
  m = PyModule_Create(&moduledef);
#else
  m = Py_InitModule3 ("_aubio", aubio_methods, aubio_module_doc);
#endif

  if (m == NULL) {
    return m;
  }

  err = _import_array ();
  if (err != 0) {
    fprintf (stderr,
        "Unable to import Numpy array from aubio module (error %d)\n", err);
  }

  Py_INCREF (&Py_cvecType);
  PyModule_AddObject (m, "cvec", (PyObject *) & Py_cvecType);
  Py_INCREF (&Py_filterType);
  PyModule_AddObject (m, "digital_filter", (PyObject *) & Py_filterType);
  Py_INCREF (&Py_filterbankType);
  PyModule_AddObject (m, "filterbank", (PyObject *) & Py_filterbankType);
  Py_INCREF (&Py_fftType);
  PyModule_AddObject (m, "fft", (PyObject *) & Py_fftType);
  Py_INCREF (&Py_pvocType);
  PyModule_AddObject (m, "pvoc", (PyObject *) & Py_pvocType);
  Py_INCREF (&Py_sourceType);
  PyModule_AddObject (m, "source", (PyObject *) & Py_sourceType);
  Py_INCREF (&Py_sinkType);
  PyModule_AddObject (m, "sink", (PyObject *) & Py_sinkType);

  PyModule_AddStringConstant(m, "float_type", AUBIO_NPY_SMPL_STR);
  PyModule_AddStringConstant(m, "__version__", DEFINEDSTRING(AUBIO_VERSION));

  // add generated objects
  add_generated_objects(m);

  // add ufunc
  add_ufuncs(m);

  aubio_log_set_level_function(AUBIO_LOG_ERR, aubio_log_function, NULL);
  aubio_log_set_level_function(AUBIO_LOG_WRN, aubio_log_function, NULL);
  return m;
}

#if PY_MAJOR_VERSION >= 3
    // Python3 init
    PyMODINIT_FUNC PyInit__aubio(void)
    {
        return initaubio();
    }
#else
    // Python 2 init
    PyMODINIT_FUNC init_aubio(void)
    {
        initaubio();
    }
#endif
