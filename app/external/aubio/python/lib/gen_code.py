aubiodefvalue = {
    # we have some clean up to do
    'buf_size': 'Py_default_vector_length',
    'win_s': 'Py_default_vector_length',
    'size': 'Py_default_vector_length',
    # and here too
    'hop_size': 'Py_default_vector_length / 2',
    'hop_s': 'Py_default_vector_length / 2',
    # these should be alright
    'samplerate': 'Py_aubio_default_samplerate',
    # now for the non obvious ones
    'n_filters': '40',
    'n_coeffs': '13',
    'nelems': '10',
    'flow': '0.',
    'fhig': '1.',
    'ilow': '0.',
    'ihig': '1.',
    'thrs': '0.5',
    'ratio': '0.5',
    'method': '"default"',
    'uri': '"none"',
    }

member_types = {
        'name': 'type',
        'char_t*': 'T_STRING',
        'uint_t': 'T_INT',
        'smpl_t': 'AUBIO_NPY_SMPL',
        }

pyfromtype_fn = {
        'smpl_t': 'PyFloat_FromDouble',
        'uint_t': 'PyLong_FromLong', # was: 'PyInt_FromLong',
        'fvec_t*': 'PyAubio_CFvecToArray',
        'fmat_t*': 'PyAubio_CFmatToArray',
        }

pytoaubio_fn = {
        'fvec_t*': 'PyAubio_ArrayToCFvec',
        'cvec_t*': 'PyAubio_PyCvecToCCvec',
        #'fmat_t*': 'PyAubio_ArrayToCFmat',
        }

newfromtype_fn = {
        'fvec_t*': 'new_py_fvec',
        'fmat_t*': 'new_py_fmat',
        'cvec_t*': 'new_py_cvec',
        }

delfromtype_fn = {
        'fvec_t*': 'Py_DECREF',
        'fmat_t*': 'Py_DECREF',
        'cvec_t*': 'Py_DECREF',
        }

param_init = {
        'char_t*': 'NULL',
        'uint_t': '0',
        'sint_t': 0,
        'smpl_t': 0.,
        'lsmp_t': 0.,
        }

pyargparse_chars = {
        'smpl_t': 'f', # if not usedouble else 'd',
        'uint_t': 'I',
        'sint_t': 'I',
        'char_t*': 's',
        'fmat_t*': 'O',
        'fvec_t*': 'O',
        'cvec_t*': 'O',
        }

objoutsize = {
        'onset': '1',
        'pitch': '1',
        'notes': '3',
        'wavetable': 'self->hop_size',
        'sampler': 'self->hop_size',
        'mfcc': 'self->n_coeffs',
        'specdesc': '1',
        'tempo': '1',
        'filterbank': 'self->n_filters',
        'tss': 'self->buf_size',
        'dct': 'self->size',
        }

objinputsize = {
        'mfcc': 'self->buf_size / 2 + 1',
        'notes': 'self->hop_size',
        'onset': 'self->hop_size',
        'pitch': 'self->hop_size',
        'sampler': 'self->hop_size',
        'specdesc': 'self->buf_size / 2 + 1',
        'tempo': 'self->hop_size',
        'wavetable': 'self->hop_size',
        'tss': 'self->buf_size / 2 + 1',
        }

def get_name(proto):
    name = proto.replace(' *', '* ').split()[1].split('(')[0]
    name = name.replace('*','')
    if name == '': raise ValueError(proto + "gave empty name")
    return name

def get_return_type(proto):
    import re
    paramregex = re.compile('(\w+ ?\*?).*')
    outputs = paramregex.findall(proto)
    assert len(outputs) == 1
    return outputs[0].replace(' ', '')

def split_type(arg):
    """ arg = 'foo *name' 
        return ['foo*', 'name'] """
    l = arg.split()
    type_arg = {} #'type': l[0], 'name': l[1]}
    type_arg['type'] = " ".join(l[:-1])
    type_arg['name'] = l[-1]
    # fix up type / name
    if type_arg['name'].startswith('*'):
        # ['foo', '*name'] -> ['foo*', 'name']
        type_arg['type'] += '*'
        type_arg['name'] = type_arg['name'][1:]
    if type_arg['type'].endswith(' *'):
        # ['foo *', 'name'] -> ['foo*', 'name']
        type_arg['type'] = type_arg['type'].replace(' *','*')
    if type_arg['type'].startswith('const '):
        # ['foo *', 'name'] -> ['foo*', 'name']
        type_arg['type'] = type_arg['type'].replace('const ','')
    return type_arg

def get_params(proto):
    """ get the list of parameters from a function prototype
    example: proto = "int main (int argc, char ** argv)"
    returns: ['int argc', 'char ** argv']
    """
    import re
    paramregex = re.compile('.*\((.*)\);')
    a = paramregex.findall(proto)[0].split(', ')
    #a = [i.replace('const ', '') for i in a]
    return a

def get_input_params(proto):
    a = get_params(proto)
    return [i.replace('const ', '') for i in a if (i.startswith('const ') or i.startswith('uint_t ') or i.startswith('smpl_t '))]

def get_output_params(proto):
    a = get_params(proto)
    return [i for i in a if not i.startswith('const ')][1:]

def get_params_types_names(proto):
    """ get the list of parameters from a function prototype
    example: proto = "int main (int argc, char ** argv)"
    returns: [['int', 'argc'], ['char **','argv']]
    """
    a = list(map(split_type, get_params(proto)))
    #print proto, a
    #import sys; sys.exit(1)
    return a

class MappedObject(object):

    def __init__(self, prototypes, usedouble = False):
        if usedouble:
            pyargparse_chars['smpl_t'] = 'd'
        self.prototypes = prototypes

        self.shortname = prototypes['shortname']
        self.longname = prototypes['longname']
        self.new_proto = prototypes['new'][0]
        self.del_proto = prototypes['del'][0]
        self.do_proto = prototypes['do'][0]
        self.input_params = get_params_types_names(self.new_proto)
        self.input_params_list = "; ".join(get_input_params(self.new_proto))
        self.outputs = get_params_types_names(self.do_proto)[2:]
        self.do_inputs = [get_params_types_names(self.do_proto)[1]]
        self.do_outputs = get_params_types_names(self.do_proto)[2:]
        struct_output_str = ["PyObject *{0[name]}; {1} c_{0[name]}".format(i, i['type'][:-1]) for i in self.do_outputs]
        if len(self.prototypes['rdo']):
            rdo_outputs = get_params_types_names(prototypes['rdo'][0])[2:]
            struct_output_str += ["PyObject *{0[name]}; {1} c_{0[name]}".format(i, i['type'][:-1]) for i in rdo_outputs]
            self.outputs += rdo_outputs
        self.struct_outputs = ";\n    ".join(struct_output_str)

        #print ("input_params: ", map(split_type, get_input_params(self.do_proto)))
        #print ("output_params", map(split_type, get_output_params(self.do_proto)))

    def gen_code(self):
        out = ""
        try:
            out += self.gen_struct()
            out += self.gen_doc()
            out += self.gen_new()
            out += self.gen_init()
            out += self.gen_del()
            out += self.gen_do()
            if len(self.prototypes['rdo']):
                self.do_proto = self.prototypes['rdo'][0]
                self.do_inputs = [get_params_types_names(self.do_proto)[1]]
                self.do_outputs = get_params_types_names(self.do_proto)[2:]
                out += self.gen_do(method='rdo')
            out += self.gen_memberdef()
            out += self.gen_set()
            out += self.gen_get()
            out += self.gen_methodef()
            out += self.gen_typeobject()
        except Exception as e:
            print ("Failed generating code for", self.shortname)
            raise
        return out

    def gen_struct(self):
        out = """
// {shortname} structure
typedef struct{{
    PyObject_HEAD
    // pointer to aubio object
    {longname} *o;
    // input parameters
    {input_params_list};
    // do input vectors
    {do_inputs_list};
    // output results
    {struct_outputs};
}} Py_{shortname};
"""
        # fmat_t* / fvec_t* / cvec_t* inputs -> full fvec_t /.. struct in Py_{shortname}
        do_inputs_list = "; ".join(get_input_params(self.do_proto)).replace('fvec_t *','fvec_t').replace('fmat_t *', 'fmat_t').replace('cvec_t *', 'cvec_t')
        return out.format(do_inputs_list = do_inputs_list, **self.__dict__)

    def gen_doc(self):
        sig = []
        for p in self.input_params:
            name = p['name']
            defval = aubiodefvalue[name].replace('"','\\\"')
            sig.append("{name}={defval}".format(defval=defval, name=name))
        out = """
#ifndef PYAUBIO_{shortname}_doc
#define PYAUBIO_{shortname}_doc "{shortname}({sig})"
#endif /* PYAUBIO_{shortname}_doc */

static char Py_{shortname}_doc[] = ""
PYAUBIO_{shortname}_doc
"";
"""
        return out.format(sig=', '.join(sig), **self.__dict__)

    def gen_new(self):
        out = """
// new {shortname}
static PyObject *
Py_{shortname}_new (PyTypeObject * pytype, PyObject * args, PyObject * kwds)
{{
    Py_{shortname} *self;
""".format(**self.__dict__)
        params = self.input_params
        for p in params:
            out += """
    {type} {name} = {defval};""".format(defval = param_init[p['type']], **p)
        plist = ", ".join(["\"%s\"" % p['name'] for p in params])
        out += """
    static char *kwlist[] = {{ {plist}, NULL }};""".format(plist = plist)
        argchars = "".join([pyargparse_chars[p['type']] for p in params])
        arglist = ", ".join(["&%s" % p['name'] for p in params])
        out += """
    if (!PyArg_ParseTupleAndKeywords (args, kwds, "|{argchars}", kwlist,
              {arglist})) {{
        return NULL;
    }}
""".format(argchars = argchars, arglist = arglist)
        out += """
    self = (Py_{shortname} *) pytype->tp_alloc (pytype, 0);
    if (self == NULL) {{
        return NULL;
    }}
""".format(**self.__dict__)
        params = self.input_params
        for p in params:
            out += self.check_valid(p)
        out += """
    return (PyObject *)self;
}
"""
        return out

    def check_valid(self, p):
        if p['type'] == 'uint_t':
            return self.check_valid_uint(p)
        if p['type'] == 'char_t*':
            return self.check_valid_char(p)
        else:
            print ("ERROR, no idea how to check %s for validity" % p['type'])

    def check_valid_uint(self, p):
        name = p['name']
        return """
    self->{name} = {defval};
    if ((sint_t){name} > 0) {{
        self->{name} = {name};
    }} else if ((sint_t){name} < 0) {{
        PyErr_SetString (PyExc_ValueError, "can not use negative value for {name}");
        return NULL;
    }}
""".format(defval = aubiodefvalue[name], name = name)

    def check_valid_char(self, p):
        name = p['name']
        return """
    self->{name} = {defval};
    if ({name} != NULL) {{
        self->{name} = {name};
    }}
""".format(defval = aubiodefvalue[name], name = name)

    def gen_init(self):
        out = """
// init {shortname}
static int
Py_{shortname}_init (Py_{shortname} * self, PyObject * args, PyObject * kwds)
{{
""".format(**self.__dict__)
        new_name = get_name(self.new_proto)
        new_params = ", ".join(["self->%s" % s['name'] for s in self.input_params])
        out += """
  self->o = {new_name}({new_params});
""".format(new_name = new_name, new_params = new_params)
        paramchars = "%s"
        paramvals = "self->method"
        out += """
  // return -1 and set error string on failure
  if (self->o == NULL) {{
    PyErr_Format (PyExc_RuntimeError, "failed creating {shortname}");
    return -1;
  }}
""".format(paramchars = paramchars, paramvals = paramvals, **self.__dict__)
        output_create = ""
        for o in self.outputs:
            output_create += """
  self->{name} = {create_fn}({output_size});""".format(name = o['name'], create_fn = newfromtype_fn[o['type']], output_size = objoutsize[self.shortname])
        out += """
  // TODO get internal params after actual object creation?
"""
        out += """
  // create outputs{output_create}
""".format(output_create = output_create)
        out += """
  return 0;
}
"""
        return out

    def gen_memberdef(self):
        out = """
static PyMemberDef Py_{shortname}_members[] = {{
""".format(**self.__dict__)
        for p in get_params_types_names(self.new_proto):
            tmp = "  {{\"{name}\", {ttype}, offsetof (Py_{shortname}, {name}), READONLY, \"TODO documentation\"}},\n"
            pytype = member_types[p['type']]
            out += tmp.format(name = p['name'], ttype = pytype, shortname = self.shortname)
        out += """  {NULL}, // sentinel
};
"""
        return out

    def gen_del(self):
        out = """
// del {shortname}
static void
Py_{shortname}_del  (Py_{shortname} * self, PyObject * unused)
{{""".format(**self.__dict__)
        for input_param in self.do_inputs:
            if input_param['type'] == 'fmat_t *':
                out += """
  free(self->{0[name]}.data);""".format(input_param)
        for o in self.outputs:
            name = o['name']
            del_out = delfromtype_fn[o['type']]
            out += """
  if (self->{name}) {{
    {del_out}(self->{name});
  }}""".format(del_out = del_out, name = name)
        del_fn = get_name(self.del_proto)
        out += """
  if (self->o) {{
    {del_fn}(self->o);
  }}
  Py_TYPE(self)->tp_free((PyObject *) self);
}}
""".format(del_fn = del_fn)
        return out

    def gen_do(self, method = 'do'):
        out = """
// do {shortname}
static PyObject*
Pyaubio_{shortname}_{method}  (Py_{shortname} * self, PyObject * args)
{{""".format(method = method, **self.__dict__)
        input_params = self.do_inputs
        output_params = self.do_outputs
        #print input_params
        #print output_params
        out += """
    PyObject *outputs;"""
        for input_param in input_params:
            out += """
    PyObject *py_{0};""".format(input_param['name'])
        refs = ", ".join(["&py_%s" % p['name'] for p in input_params])
        pyparamtypes = "".join([pyargparse_chars[p['type']] for p in input_params])
        out += """
    if (!PyArg_ParseTuple (args, "{pyparamtypes}", {refs})) {{
        return NULL;
    }}""".format(refs = refs, pyparamtypes = pyparamtypes, **self.__dict__)
        for input_param in input_params:
            out += """

    if (!{pytoaubio}(py_{0[name]}, &(self->{0[name]}))) {{
        return NULL;
    }}""".format(input_param, pytoaubio = pytoaubio_fn[input_param['type']])
        if self.shortname in objinputsize:
            out += """

    if (self->{0[name]}.length != {expected_size}) {{
        PyErr_Format (PyExc_ValueError,
            "input size of {shortname} should be %d, not %d",
            {expected_size}, self->{0[name]}.length);
        return NULL;
    }}""".format(input_param, expected_size = objinputsize[self.shortname], **self.__dict__)
        else:
            out += """

    // TODO: check input sizes"""
        for output_param in output_params:
            out += """

    Py_INCREF(self->{0[name]});
    if (!{pytoaubio}(self->{0[name]}, &(self->c_{0[name]}))) {{
        return NULL;
    }}""".format(output_param, pytoaubio = pytoaubio_fn[output_param['type']])
        do_fn = get_name(self.do_proto)
        inputs = ", ".join(['&(self->'+p['name']+')' for p in input_params])
        c_outputs = ", ".join(["&(self->c_%s)" % p['name'] for p in self.do_outputs])
        outputs = ", ".join(["self->%s" % p['name'] for p in self.do_outputs])
        out += """

    {do_fn}(self->o, {inputs}, {c_outputs});
""".format(
        do_fn = do_fn,
        inputs = inputs, c_outputs = c_outputs,
        )
        if len(self.do_outputs) > 1:
            out += """
    outputs = PyTuple_New({:d});""".format(len(self.do_outputs))
            for i, p in enumerate(self.do_outputs):
                out += """
    PyTuple_SetItem( outputs, {i}, self->{p[name]});""".format(i = i, p = p)
        else:
            out += """
    outputs = self->{p[name]};""".format(p = self.do_outputs[0])
        out += """

    return outputs;
}}
""".format(
        outputs = outputs,
        )
        return out

    def gen_set(self):
        out = """
// {shortname} setters
""".format(**self.__dict__)
        for set_param in self.prototypes['set']:
            params = get_params_types_names(set_param)[1:]
            param = self.shortname.split('_set_')[-1]
            paramdecls = "".join(["""
   {0} {1};""".format(p['type'], p['name']) for p in params])
            method_name = get_name(set_param)
            param = method_name.split('aubio_'+self.shortname+'_set_')[-1]
            refs = ", ".join(["&%s" % p['name'] for p in params])
            paramlist = ", ".join(["%s" % p['name'] for p in params])
            if len(params):
                paramlist = "," + paramlist
            pyparamtypes = ''.join([pyargparse_chars[p['type']] for p in params])
            out += """
static PyObject *
Pyaubio_{shortname}_set_{param} (Py_{shortname} *self, PyObject *args)
{{
  uint_t err = 0;
  {paramdecls}
""".format(param = param, paramdecls = paramdecls, **self.__dict__)

            if len(refs) and len(pyparamtypes):
                out += """

  if (!PyArg_ParseTuple (args, "{pyparamtypes}", {refs})) {{
    return NULL;
  }}
""".format(pyparamtypes = pyparamtypes, refs = refs)

            out += """
  err = aubio_{shortname}_set_{param} (self->o {paramlist});

  if (err > 0) {{
    if (PyErr_Occurred() == NULL) {{
      PyErr_SetString (PyExc_ValueError, "error running aubio_{shortname}_set_{param}");
    }} else {{
      // change the RuntimeError into ValueError
      PyObject *type, *value, *traceback;
      PyErr_Fetch(&type, &value, &traceback);
      Py_XDECREF(type);
      type = PyExc_ValueError;
      Py_XINCREF(type);
      PyErr_Restore(type, value, traceback);
    }}
    return NULL;
  }}
  Py_RETURN_NONE;
}}
""".format(param = param, refs = refs, paramdecls = paramdecls,
        pyparamtypes = pyparamtypes, paramlist = paramlist, **self.__dict__)
        return out

    def gen_get(self):
        out = """
// {shortname} getters
""".format(**self.__dict__)
        for method in self.prototypes['get']:
            params = get_params_types_names(method)
            method_name = get_name(method)
            assert len(params) == 1, \
                "get method has more than one parameter %s" % params
            param = method_name.split('aubio_'+self.shortname+'_get_')[-1]
            paramtype = get_return_type(method)
            ptypeconv = pyfromtype_fn[paramtype]
            out += """
static PyObject *
Pyaubio_{shortname}_get_{param} (Py_{shortname} *self, PyObject *unused)
{{
  {ptype} {param} = aubio_{shortname}_get_{param} (self->o);
  return (PyObject *){ptypeconv} ({param});
}}
""".format(param = param, ptype = paramtype, ptypeconv = ptypeconv,
        **self.__dict__)
        return out

    def gen_methodef(self):
        out = """
static PyMethodDef Py_{shortname}_methods[] = {{""".format(**self.__dict__)
        for m in self.prototypes['set']:
            name = get_name(m)
            shortname = name.replace('aubio_%s_' % self.shortname, '')
            out += """
  {{"{shortname}", (PyCFunction) Py{name},
    METH_VARARGS, ""}},""".format(name = name, shortname = shortname)
        for m in self.prototypes['get']:
            name = get_name(m)
            shortname = name.replace('aubio_%s_' % self.shortname, '')
            out += """
  {{"{shortname}", (PyCFunction) Py{name},
    METH_NOARGS, ""}},""".format(name = name, shortname = shortname)
        for m in self.prototypes['rdo']:
            name = get_name(m)
            shortname = name.replace('aubio_%s_' % self.shortname, '')
            out += """
  {{"{shortname}", (PyCFunction) Py{name},
    METH_VARARGS, ""}},""".format(name = name, shortname = shortname)
        out += """
  {NULL} /* sentinel */
};
"""
        return out

    def gen_typeobject(self):
        return """
PyTypeObject Py_{shortname}Type = {{
  //PyObject_HEAD_INIT (NULL)
  //0,
  PyVarObject_HEAD_INIT (NULL, 0)
  "aubio.{shortname}",
  sizeof (Py_{shortname}),
  0,
  (destructor) Py_{shortname}_del,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  (ternaryfunc)Pyaubio_{shortname}_do,
  0,
  0,
  0,
  0,
  Py_TPFLAGS_DEFAULT,
  Py_{shortname}_doc,
  0,
  0,
  0,
  0,
  0,
  0,
  Py_{shortname}_methods,
  Py_{shortname}_members,
  0,
  0,
  0,
  0,
  0,
  0,
  (initproc) Py_{shortname}_init,
  0,
  Py_{shortname}_new,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
}};
""".format(**self.__dict__)
