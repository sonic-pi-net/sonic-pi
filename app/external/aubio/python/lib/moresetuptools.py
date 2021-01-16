""" A collection of function used from setup.py distutils script """
#
import sys, os, glob, subprocess
import distutils, distutils.command.clean, distutils.dir_util
from gen_external import generate_external, header, output_path

from this_version import get_aubio_version

# inspired from https://gist.github.com/abergmeier/9488990
def add_packages(packages, ext=None, **kw):
    """ use pkg-config to search which of 'packages' are installed """
    flag_map = {
        '-I': 'include_dirs',
        '-L': 'library_dirs',
        '-l': 'libraries'}

    # if a setuptools extension is passed, fill it with pkg-config results
    if ext:
        kw = {'include_dirs': ext.include_dirs,
              'extra_link_args': ext.extra_link_args,
              'library_dirs': ext.library_dirs,
              'libraries': ext.libraries,
             }

    for package in packages:
        print("checking for {:s}".format(package))
        cmd = ['pkg-config', '--libs', '--cflags', package]
        try:
            tokens = subprocess.check_output(cmd)
        except Exception as e:
            print("Running \"{:s}\" failed: {:s}".format(' '.join(cmd), repr(e)))
            continue
        tokens = tokens.decode('utf8').split()
        for token in tokens:
            key = token[:2]
            try:
                arg = flag_map[key]
                value = token[2:]
            except KeyError:
                arg = 'extra_link_args'
                value = token
            kw.setdefault(arg, []).append(value)
    for key, value in iter(kw.items()): # remove duplicated
        kw[key] = list(set(value))
    return kw

def add_local_aubio_header(ext):
    """ use local "src/aubio.h", not <aubio/aubio.h>"""
    ext.define_macros += [('USE_LOCAL_AUBIO', 1)]
    ext.include_dirs += ['src'] # aubio.h

def add_local_aubio_lib(ext):
    """ add locally built libaubio from build/src """
    print("Info: using locally built libaubio")
    ext.library_dirs += [os.path.join('build', 'src')]
    ext.libraries += ['aubio']

def add_local_aubio_sources(ext):
    """ build aubio inside python module instead of linking against libaubio """
    print("Info: libaubio was not installed or built locally with waf, adding src/")
    aubio_sources = sorted(glob.glob(os.path.join('src', '**.c')))
    aubio_sources += sorted(glob.glob(os.path.join('src', '*', '**.c')))
    ext.sources += aubio_sources

def add_local_macros(ext, usedouble = False):
    if usedouble:
        ext.define_macros += [('HAVE_AUBIO_DOUBLE', 1)]
    # define macros (waf puts them in build/src/config.h)
    for define_macro in ['HAVE_STDLIB_H', 'HAVE_STDIO_H',
                         'HAVE_MATH_H', 'HAVE_STRING_H',
                         'HAVE_ERRNO_H', 'HAVE_C99_VARARGS_MACROS',
                         'HAVE_LIMITS_H', 'HAVE_STDARG_H',
                         'HAVE_MEMCPY_HACKS']:
        ext.define_macros += [(define_macro, 1)]

def add_external_deps(ext, usedouble = False):
    # loof for additional packages
    print("Info: looking for *optional* additional packages")
    packages = ['libavcodec', 'libavformat', 'libavutil',
                'libswresample', 'libavresample',
                'sndfile',
                #'fftw3f',
               ]
    # samplerate only works with float
    if usedouble is False:
        packages += ['samplerate']
    else:
        print("Info: not adding libsamplerate in double precision mode")
    add_packages(packages, ext=ext)
    if 'avcodec' in ext.libraries \
            and 'avformat' in ext.libraries \
            and 'avutil' in ext.libraries:
        if 'swresample' in ext.libraries:
            ext.define_macros += [('HAVE_SWRESAMPLE', 1)]
        elif 'avresample' in ext.libraries:
            ext.define_macros += [('HAVE_AVRESAMPLE', 1)]
        if 'swresample' in ext.libraries or 'avresample' in ext.libraries:
            ext.define_macros += [('HAVE_LIBAV', 1)]
    if 'sndfile' in ext.libraries:
        ext.define_macros += [('HAVE_SNDFILE', 1)]
    if 'samplerate' in ext.libraries:
        ext.define_macros += [('HAVE_SAMPLERATE', 1)]
    if 'fftw3f' in ext.libraries:
        ext.define_macros += [('HAVE_FFTW3F', 1)]
        ext.define_macros += [('HAVE_FFTW3', 1)]

    # add accelerate on darwin
    if sys.platform.startswith('darwin'):
        ext.extra_link_args += ['-framework', 'Accelerate']
        ext.define_macros += [('HAVE_ACCELERATE', 1)]
        ext.define_macros += [('HAVE_SOURCE_APPLE_AUDIO', 1)]
        ext.define_macros += [('HAVE_SINK_APPLE_AUDIO', 1)]

    if sys.platform.startswith('win'):
        ext.define_macros += [('HAVE_WIN_HACKS', 1)]

    ext.define_macros += [('HAVE_WAVWRITE', 1)]
    ext.define_macros += [('HAVE_WAVREAD', 1)]

    # TODO: add cblas
    if 0:
        ext.libraries += ['cblas']
        ext.define_macros += [('HAVE_ATLAS_CBLAS_H', 1)]

def add_system_aubio(ext):
    # use pkg-config to find aubio's location
    aubio_version = get_aubio_version()
    add_packages(['aubio = ' + aubio_version], ext)
    if 'aubio' not in ext.libraries:
        print("Info: aubio " + aubio_version + " was not found by pkg-config")
    else:
        print("Info: using system aubio " + aubio_version + " found in " + ' '.join(ext.library_dirs))

def add_libav_on_win(ext):
    """ no pkg-config on windows, simply assume these libs are available """
    ext.libraries += ['avformat', 'avutil', 'avcodec', 'swresample']
    for define_macro in ['HAVE_LIBAV', 'HAVE_SWRESAMPLE']:
        ext.define_macros += [(define_macro, 1)]

class CleanGenerated(distutils.command.clean.clean):
    def run(self):
        if os.path.isdir(output_path):
            distutils.dir_util.remove_tree(output_path)

from distutils.command.build_ext import build_ext as _build_ext
class build_ext(_build_ext):

    user_options = _build_ext.user_options + [
            # The format is (long option, short option, description).
            ('enable-double', None, 'use HAVE_AUBIO_DOUBLE=1 (default: 0)'),
            ]

    def initialize_options(self):
        _build_ext.initialize_options(self)
        self.enable_double = False

    def finalize_options(self):
        _build_ext.finalize_options(self)
        if self.enable_double:
            self.announce(
                    'will generate code for aubio compiled with HAVE_AUBIO_DOUBLE=1',
                    level=distutils.log.INFO)

    def build_extension(self, extension):
        if self.enable_double or 'HAVE_AUBIO_DOUBLE' in os.environ:
            enable_double = True
        else:
            enable_double = False
        # seack for aubio headers and lib in PKG_CONFIG_PATH
        add_system_aubio(extension)
        # the lib was not installed on this system
        if 'aubio' not in extension.libraries:
            # use local src/aubio.h
            if os.path.isfile(os.path.join('src', 'aubio.h')):
                add_local_aubio_header(extension)
            add_local_macros(extension, usedouble=enable_double)
            # look for a local waf build
            if os.path.isfile(os.path.join('build','src', 'fvec.c.1.o')):
                add_local_aubio_lib(extension)
            else:
                # check for external dependencies
                add_external_deps(extension, usedouble=enable_double)
                # force adding libav on windows
                if os.name == 'nt' and ('WITH_LIBAV' in os.environ \
                        or 'CONDA_PREFIX' in os.environ):
                    add_libav_on_win(extension)
                # add libaubio sources and look for optional deps with pkg-config
                add_local_aubio_sources(extension)
        # generate files python/gen/*.c, python/gen/aubio-generated.h
        extension.include_dirs += [ output_path ]
        extension.sources += generate_external(header, output_path, overwrite = False,
                usedouble=enable_double)
        return _build_ext.build_extension(self, extension)
