#define PY_AUBIO_MODULE_UFUNC
#include "aubio-types.h"

typedef smpl_t (*aubio_unary_func_t)(smpl_t input);

static void aubio_PyUFunc_d_d(char **args, npy_intp *dimensions,
                            npy_intp* steps, void* data)
{
    npy_intp i;
    npy_intp n = dimensions[0];
    char *in = args[0], *out = args[1];
    npy_intp in_step = steps[0], out_step = steps[1];
    aubio_unary_func_t func = (aubio_unary_func_t)(data);

    for (i = 0; i < n; i++) {
        /*BEGIN main ufunc computation*/
        *((double *)out) = func(*(double *)in);
        /*END main ufunc computation*/

        in += in_step;
        out += out_step;
    }
}

static void aubio_PyUFunc_f_f_As_d_d(char **args, npy_intp *dimensions,
                            npy_intp* steps, void* data)
{
    npy_intp i;
    npy_intp n = dimensions[0];
    char *in = args[0], *out = args[1];
    npy_intp in_step = steps[0], out_step = steps[1];
    aubio_unary_func_t func = (aubio_unary_func_t)(data);

    for (i = 0; i < n; i++) {
        /*BEGIN main ufunc computation*/
        *((float *)out) = func(*(float *)in);
        /*END main ufunc computation*/

        in += in_step;
        out += out_step;
    }
}

static int Py_aubio_unary_n_types = 2;
static int Py_aubio_unary_n_inputs = 1;
static int Py_aubio_unary_n_outputs = 1;
PyUFuncGenericFunction Py_aubio_unary_functions[] = {
  &aubio_PyUFunc_f_f_As_d_d,
  &aubio_PyUFunc_d_d,
  //PyUFunc_f_f_As_d_d, PyUFunc_d_d,
  //PyUFunc_g_g, PyUFunc_OO_O_method,
};

static char Py_aubio_unary_types[] = {
  NPY_FLOAT, NPY_FLOAT,
  NPY_DOUBLE, NPY_DOUBLE,
  //NPY_LONGDOUBLE, NPY_LONGDOUBLE,
  //NPY_OBJECT, NPY_OBJECT,
};

// Note: these docstrings should *not* include the function signatures

static char Py_unwrap2pi_doc[] = ""
"\n"
"Map angle to unit circle :math:`[-\\pi, \\pi[`.\n"
"\n"
"Parameters\n"
"----------\n"
"x : numpy.ndarray\n"
"   input array\n"
"\n"
"Returns\n"
"-------\n"
"numpy.ndarray\n"
"   values clamped to the unit circle :math:`[-\\pi, \\pi[`\n"
"";

static void* Py_unwrap2pi_data[] = {
  (void *)aubio_unwrap2pi,
  (void *)aubio_unwrap2pi,
  //(void *)unwrap2pil,
  //(void *)unwrap2pio,
};

static char Py_freqtomidi_doc[] = ""
"\n"
"Convert frequency `[0; 23000[` to midi `[0; 128[`.\n"
"\n"
"Parameters\n"
"----------\n"
"x : numpy.ndarray\n"
"    Array of frequencies, in Hz.\n"
"\n"
"Returns\n"
"-------\n"
"numpy.ndarray\n"
"    Converted frequencies, in midi note.\n"
"";

static void* Py_freqtomidi_data[] = {
  (void *)aubio_freqtomidi,
  (void *)aubio_freqtomidi,
};

static char Py_miditofreq_doc[] = ""
"\n"
"Convert midi `[0; 128[` to frequency `[0, 23000]`.\n"
"\n"
"Parameters\n"
"----------\n"
"x : numpy.ndarray\n"
"    Array of frequencies, in midi note.\n"
"\n"
"Returns\n"
"-------\n"
"numpy.ndarray\n"
"    Converted frequencies, in Hz\n"
"";

static void* Py_miditofreq_data[] = {
  (void *)aubio_miditofreq,
  (void *)aubio_miditofreq,
};

void add_ufuncs ( PyObject *m )
{
  int err = 0;
  PyObject *dict, *f, *g, *h;

  err = _import_umath ();
  if (err != 0) {
    fprintf (stderr,
        "Unable to import Numpy umath from aubio module (error %d)\n", err);
  }

  dict = PyModule_GetDict(m);
  f = PyUFunc_FromFuncAndData(Py_aubio_unary_functions, Py_unwrap2pi_data, Py_aubio_unary_types,
          Py_aubio_unary_n_types, Py_aubio_unary_n_inputs, Py_aubio_unary_n_outputs,
          PyUFunc_None, "unwrap2pi", Py_unwrap2pi_doc, 0);
  PyDict_SetItemString(dict, "unwrap2pi", f);
  Py_DECREF(f);

  g = PyUFunc_FromFuncAndData(Py_aubio_unary_functions, Py_freqtomidi_data, Py_aubio_unary_types,
          Py_aubio_unary_n_types, Py_aubio_unary_n_inputs, Py_aubio_unary_n_outputs,
          PyUFunc_None, "freqtomidi", Py_freqtomidi_doc, 0);
  PyDict_SetItemString(dict, "freqtomidi", g);
  Py_DECREF(g);

  h = PyUFunc_FromFuncAndData(Py_aubio_unary_functions, Py_miditofreq_data, Py_aubio_unary_types,
          Py_aubio_unary_n_types, Py_aubio_unary_n_inputs, Py_aubio_unary_n_outputs,
          PyUFunc_None, "miditofreq", Py_miditofreq_doc, 0);
  PyDict_SetItemString(dict, "miditofreq", h);
  Py_DECREF(h);
  return;
}
