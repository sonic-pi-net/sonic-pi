#! /usr/bin/python
#
# usage:
#   $ python waf --help
#
# example:
#   $ ./waf distclean configure build
#
# Note: aubio uses the waf build system, which relies on Python. Provided you
# have Python installed, you do *not* need to install anything to build aubio.
# For more info about waf, see http://code.google.com/p/waf/ .

import sys

APPNAME = 'aubio'

from this_version import *

VERSION = get_aubio_version()
LIB_VERSION = get_libaubio_version()

top = '.'
out = 'build'

def add_option_enable_disable(ctx, name, default = None,
        help_str = None, help_disable_str = None):
    if help_str == None:
        help_str = 'enable ' + name + ' support'
    if help_disable_str == None:
        help_disable_str = 'do not ' + help_str
    ctx.add_option('--enable-' + name, action = 'store_true',
            default = default,
            dest = 'enable_' + name.replace('-','_'),
            help = help_str)
    ctx.add_option('--disable-' + name, action = 'store_false',
            #default = default,
            dest = 'enable_' + name.replace('-','_'),
            help = help_disable_str )

def options(ctx):
    ctx.add_option('--build-type', action = 'store',
            default = "release",
            choices = ('debug', 'release'),
            dest = 'build_type',
            help = 'whether to compile with (--build-type=release)' \
                    ' or without (--build-type=debug)' \
                    ' compiler opimizations [default: release]')
    ctx.add_option('--debug', action = 'store_const',
            dest = 'build_type', const = 'debug',
            help = 'build in debug mode (see --build-type)')
    add_option_enable_disable(ctx, 'fftw3f', default = False,
            help_str = 'compile with fftw3f instead of ooura (recommended)',
            help_disable_str = 'do not compile with fftw3f')
    add_option_enable_disable(ctx, 'fftw3', default = False,
            help_str = 'compile with fftw3 instead of ooura',
            help_disable_str = 'do not compile with fftw3')
    add_option_enable_disable(ctx, 'intelipp', default = False,
            help_str = 'use Intel IPP libraries (auto)',
            help_disable_str = 'do not use Intel IPP libraries')
    add_option_enable_disable(ctx, 'complex', default = False,
            help_str ='compile with C99 complex',
            help_disable_str = 'do not use C99 complex (default)' )
    add_option_enable_disable(ctx, 'jack', default = None,
            help_str = 'compile with jack (auto)',
            help_disable_str = 'disable jack support')
    add_option_enable_disable(ctx, 'sndfile', default = None,
            help_str = 'compile with sndfile (auto)',
            help_disable_str = 'disable sndfile')
    add_option_enable_disable(ctx, 'avcodec', default = None,
            help_str = 'compile with libavcodec (auto)',
            help_disable_str = 'disable libavcodec')
    add_option_enable_disable(ctx, 'samplerate', default = None,
            help_str = 'compile with samplerate (auto)',
            help_disable_str = 'disable samplerate')
    add_option_enable_disable(ctx, 'memcpy', default = True,
            help_str = 'use memcpy hacks (default)',
            help_disable_str = 'do not use memcpy hacks')
    add_option_enable_disable(ctx, 'double', default = False,
            help_str = 'compile in double precision mode',
            help_disable_str = 'compile in single precision mode (default)')
    add_option_enable_disable(ctx, 'fat', default = False,
            help_str = 'build fat binaries (darwin only)',
            help_disable_str = 'do not build fat binaries (default)')
    add_option_enable_disable(ctx, 'accelerate', default = None,
            help_str = 'use Accelerate framework (darwin only) (auto)',
            help_disable_str = 'do not use Accelerate framework')
    add_option_enable_disable(ctx, 'apple-audio', default = None,
            help_str = 'use CoreFoundation (darwin only) (auto)',
            help_disable_str = 'do not use CoreFoundation framework')
    add_option_enable_disable(ctx, 'blas', default = False,
            help_str = 'use BLAS acceleration library (no)',
            help_disable_str = 'do not use BLAS library')
    add_option_enable_disable(ctx, 'atlas', default = False,
            help_str = 'use ATLAS acceleration library (no)',
            help_disable_str = 'do not use ATLAS library')
    add_option_enable_disable(ctx, 'wavread', default = True,
            help_str = 'compile with source_wavread (default)',
            help_disable_str = 'do not compile source_wavread')
    add_option_enable_disable(ctx, 'wavwrite', default = True,
            help_str = 'compile with source_wavwrite (default)',
            help_disable_str = 'do not compile source_wavwrite')

    add_option_enable_disable(ctx, 'docs', default = None,
            help_str = 'build documentation (auto)',
            help_disable_str = 'do not build documentation')

    add_option_enable_disable(ctx, 'tests', default = True,
            help_str = 'build tests (true)',
            help_disable_str = 'do not build or run tests')

    add_option_enable_disable(ctx, 'examples', default = True,
            help_str = 'build examples (true)',
            help_disable_str = 'do not build examples')

    ctx.add_option('--with-target-platform', type='string',
            help='set target platform for cross-compilation',
            dest='target_platform')

    ctx.load('compiler_c')
    ctx.load('waf_unit_test')
    ctx.load('gnu_dirs')
    ctx.load('waf_gensyms', tooldir='.')

def configure(ctx):
    target_platform = sys.platform
    if ctx.options.target_platform:
        target_platform = ctx.options.target_platform

    from waflib import Options

    if target_platform=='emscripten':
        ctx.load('c_emscripten')
    else:
        ctx.load('compiler_c')

    ctx.load('waf_unit_test')
    ctx.load('gnu_dirs')
    ctx.load('waf_gensyms', tooldir='.')

    # check for common headers
    ctx.check(header_name='stdlib.h')
    ctx.check(header_name='stdio.h')
    ctx.check(header_name='math.h')
    ctx.check(header_name='string.h')
    ctx.check(header_name='errno.h')
    ctx.check(header_name='limits.h')
    ctx.check(header_name='stdarg.h')
    ctx.check(header_name='getopt.h', mandatory = False)
    ctx.check(header_name='unistd.h', mandatory = False)

    ctx.env['DEST_OS'] = target_platform

    if ctx.options.build_type == "debug":
        ctx.define('DEBUG', 1)
    else:
        ctx.define('NDEBUG', 1)

    if ctx.env.CC_NAME != 'msvc':
        if ctx.options.build_type == "debug":
            # no optimization in debug mode
            ctx.env.prepend_value('CFLAGS', ['-O0'])
        else:
            if target_platform == 'emscripten':
                # -Oz for small js file generation
                ctx.env.prepend_value('CFLAGS', ['-Oz'])
            else:
                # default to -O2 in release mode
                ctx.env.prepend_value('CFLAGS', ['-O2'])
        # enable debug symbols and configure warnings
        ctx.env.prepend_value('CFLAGS', ['-g', '-Wall', '-Wextra'])
    else:
        # enable debug symbols
        ctx.env.CFLAGS += ['/Z7']
        # /FS flag available in msvc >= 12 (2013)
        if 'MSVC_VERSION' in ctx.env and ctx.env.MSVC_VERSION >= 12:
            ctx.env.CFLAGS += ['/FS']
        ctx.env.LINKFLAGS += ['/DEBUG', '/INCREMENTAL:NO']
        # configure warnings
        ctx.env.CFLAGS += ['/W4', '/D_CRT_SECURE_NO_WARNINGS']
        # ignore "possible loss of data" warnings
        ctx.env.CFLAGS += ['/wd4305', '/wd4244', '/wd4245', '/wd4267']
        # ignore "unreferenced formal parameter" warnings
        ctx.env.CFLAGS += ['/wd4100']
        # set optimization level and runtime libs
        if (ctx.options.build_type == "release"):
            ctx.env.CFLAGS += ['/Ox']
            ctx.env.CFLAGS += ['/MD']
        else:
            assert(ctx.options.build_type == "debug")
            ctx.env.CFLAGS += ['/MDd']

    ctx.check_cc(lib='m', uselib_store='M', mandatory=False)

    if target_platform not in ['win32', 'win64']:
        ctx.env.CFLAGS += ['-fPIC']
    else:
        ctx.define('HAVE_WIN_HACKS', 1)
        ctx.env['cshlib_PATTERN'] = 'lib%s.dll'

    if target_platform == 'darwin' and ctx.options.enable_fat:
        ctx.env.CFLAGS += ['-arch', 'i386', '-arch', 'x86_64']
        ctx.env.LINKFLAGS += ['-arch', 'i386', '-arch', 'x86_64']
        MINSDKVER="10.4"
        ctx.env.CFLAGS += [ '-mmacosx-version-min=' + MINSDKVER ]
        ctx.env.LINKFLAGS += [ '-mmacosx-version-min=' + MINSDKVER ]

    if target_platform in [ 'darwin', 'ios', 'iosimulator']:
        if (ctx.options.enable_apple_audio != False):
            ctx.env.FRAMEWORK += ['CoreFoundation', 'AudioToolbox']
            ctx.define('HAVE_SOURCE_APPLE_AUDIO', 1)
            ctx.define('HAVE_SINK_APPLE_AUDIO', 1)
            ctx.msg('Checking for AudioToolbox.framework', 'yes')
        else:
            ctx.msg('Checking for AudioToolbox.framework', 'no (disabled)',
                    color = 'YELLOW')
        if (ctx.options.enable_accelerate != False):
            ctx.define('HAVE_ACCELERATE', 1)
            ctx.env.FRAMEWORK += ['Accelerate']
            ctx.msg('Checking for Accelerate framework', 'yes')
        else:
            ctx.msg('Checking for Accelerate framework', 'no (disabled)',
                    color = 'YELLOW')

    if target_platform in [ 'ios', 'iosimulator' ]:
        MINSDKVER="6.1"
        ctx.env.CFLAGS += ['-std=c99']
        if (ctx.options.enable_apple_audio != False):
            ctx.define('HAVE_AUDIO_UNIT', 1)
            #ctx.env.FRAMEWORK += ['CoreFoundation', 'AudioToolbox']
        if target_platform == 'ios':
            DEVROOT = "/Applications/Xcode.app/Contents"
            DEVROOT += "/Developer/Platforms/iPhoneOS.platform/Developer"
            SDKROOT = "%(DEVROOT)s/SDKs/iPhoneOS.sdk" % locals()
            ctx.env.CFLAGS += [ '-fembed-bitcode' ]
            ctx.env.CFLAGS += [ '-arch', 'arm64' ]
            ctx.env.CFLAGS += [ '-arch', 'armv7' ]
            ctx.env.CFLAGS += [ '-arch', 'armv7s' ]
            ctx.env.LINKFLAGS += [ '-arch', 'arm64' ]
            ctx.env.LINKFLAGS += ['-arch', 'armv7']
            ctx.env.LINKFLAGS += ['-arch', 'armv7s']
            ctx.env.CFLAGS += [ '-miphoneos-version-min=' + MINSDKVER ]
            ctx.env.LINKFLAGS += [ '-miphoneos-version-min=' + MINSDKVER ]
        else:
            DEVROOT = "/Applications/Xcode.app/Contents"
            DEVROOT += "/Developer/Platforms/iPhoneSimulator.platform/Developer"
            SDKROOT = "%(DEVROOT)s/SDKs/iPhoneSimulator.sdk" % locals()
            ctx.env.CFLAGS += [ '-arch', 'i386' ]
            ctx.env.CFLAGS += [ '-arch', 'x86_64' ]
            ctx.env.LINKFLAGS += ['-arch', 'i386']
            ctx.env.LINKFLAGS += ['-arch', 'x86_64']
            ctx.env.CFLAGS += [ '-mios-simulator-version-min=' + MINSDKVER ]
            ctx.env.LINKFLAGS += [ '-mios-simulator-version-min=' + MINSDKVER ]
        ctx.env.CFLAGS += [ '-isysroot' , SDKROOT]
        ctx.env.LINKFLAGS += [ '-isysroot' , SDKROOT]

    if target_platform == 'emscripten':
        if ctx.options.build_type == "debug":
            ctx.env.cshlib_PATTERN = '%s.js'
            ctx.env.LINKFLAGS += ['-s','ASSERTIONS=2']
            ctx.env.LINKFLAGS += ['-s','SAFE_HEAP=1']
            ctx.env.LINKFLAGS += ['-s','ALIASING_FUNCTION_POINTERS=0']
            ctx.env.LINKFLAGS += ['-O0']
        else:
            ctx.env.LINKFLAGS += ['-Oz']
            ctx.env.cshlib_PATTERN = '%s.min.js'

        # doesnt ship file system support in lib
        ctx.env.LINKFLAGS_cshlib += ['-s', 'NO_FILESYSTEM=1']
        # put memory file inside generated js files for easier portability
        ctx.env.LINKFLAGS += ['--memory-init-file', '0']
        ctx.env.cprogram_PATTERN = "%s.js"
        ctx.env.cstlib_PATTERN = '%s.a'

        # tell emscripten functions we want to expose
        from python.lib.gen_external import get_c_declarations, \
                get_cpp_objects_from_c_declarations, \
                get_all_func_names_from_lib, \
                generate_lib_from_c_declarations
        # emscripten can't use double
        c_decls = get_c_declarations(usedouble=False)
        objects = list(get_cpp_objects_from_c_declarations(c_decls))
        # ensure that aubio structs are exported
        objects += ['fvec_t', 'cvec_t', 'fmat_t']
        lib = generate_lib_from_c_declarations(objects, c_decls)
        exported_funcnames = get_all_func_names_from_lib(lib)
        c_mangled_names = ['_' + s for s in exported_funcnames]
        ctx.env.LINKFLAGS_cshlib += ['-s',
                'EXPORTED_FUNCTIONS=%s' % c_mangled_names]

    # check support for C99 __VA_ARGS__ macros
    check_c99_varargs = '''
#include <stdio.h>
#define AUBIO_ERR(...) fprintf(stderr, __VA_ARGS__)
'''

    if ctx.check_cc(fragment = check_c99_varargs,
            type='cstlib',
            msg = 'Checking for C99 __VA_ARGS__ macro',
            mandatory = False):
        ctx.define('HAVE_C99_VARARGS_MACROS', 1)

    # show a message about enable_double status
    if (ctx.options.enable_double == True):
        ctx.msg('Checking for size of smpl_t', 'double')
        ctx.msg('Checking for size of lsmp_t', 'long double')
    else:
        ctx.msg('Checking for size of smpl_t', 'float')
        ctx.msg('Checking for size of lsmp_t', 'double')

    # optionally use complex.h
    if (ctx.options.enable_complex == True):
        ctx.check(header_name='complex.h')
    else:
        ctx.msg('Checking if complex.h is enabled', 'no')

    # check for Intel IPP
    if (ctx.options.enable_intelipp != False):
        has_ipp_headers = ctx.check(header_name=['ippcore.h', 'ippvm.h',
            'ipps.h'], mandatory = False)
        has_ipp_libs = ctx.check(lib=['ippcore', 'ippvm', 'ipps'],
                uselib_store='INTEL_IPP', mandatory = False)
        if (has_ipp_headers and has_ipp_libs):
            ctx.msg('Checking if Intel IPP is available', 'yes')
            ctx.define('HAVE_INTEL_IPP', 1)
            if ctx.env.CC_NAME == 'msvc':
                # force linking multi-threaded static IPP libraries on Windows
                # with msvc
                ctx.define('_IPP_SEQUENTIAL_STATIC', 1)
        else:
            ctx.msg('Checking if Intel IPP is available', 'no')

    # check for fftw3
    if (ctx.options.enable_fftw3 != False or ctx.options.enable_fftw3f != False):
        # one of fftwf or fftw3f
        if (ctx.options.enable_fftw3f != False):
            ctx.check_cfg(package = 'fftw3f',
                    args = '--cflags --libs fftw3f >= 3.0.0',
                    mandatory = ctx.options.enable_fftw3f)
            if (ctx.options.enable_double == True):
                ctx.msg('Warning',
                        'fftw3f enabled, but compiling in double precision!')
        else:
            # fftw3f disabled, take most sensible one according to
            # enable_double
            if (ctx.options.enable_double == True):
                ctx.check_cfg(package = 'fftw3',
                        args = '--cflags --libs fftw3 >= 3.0.0.',
                        mandatory = ctx.options.enable_fftw3)
            else:
                ctx.check_cfg(package = 'fftw3f',
                        args = '--cflags --libs fftw3f >= 3.0.0',
                        mandatory = ctx.options.enable_fftw3)
        ctx.define('HAVE_FFTW3', 1)

    # fftw not enabled, use vDSP, intelIPP or ooura
    if 'HAVE_FFTW3F' in ctx.env.define_key:
        ctx.msg('Checking for FFT implementation', 'fftw3f')
    elif 'HAVE_FFTW3' in ctx.env.define_key:
        ctx.msg('Checking for FFT implementation', 'fftw3')
    elif 'HAVE_ACCELERATE' in ctx.env.define_key:
        ctx.msg('Checking for FFT implementation', 'vDSP')
    elif 'HAVE_INTEL_IPP' in ctx.env.define_key:
        ctx.msg('Checking for FFT implementation', 'Intel IPP')
    else:
        ctx.msg('Checking for FFT implementation', 'ooura')

    # check for libsndfile
    if (ctx.options.enable_sndfile != False):
        ctx.check_cfg(package = 'sndfile',
                args = '--cflags --libs sndfile >= 1.0.4',
                mandatory = ctx.options.enable_sndfile)

    # check for libsamplerate
    if (ctx.options.enable_double):
        if (ctx.options.enable_samplerate):
            ctx.fatal("Could not compile aubio in double precision mode' \
                    ' with libsamplerate")
        else:
            ctx.options.enable_samplerate = False
            ctx.msg('Checking if using samplerate',
                    'no (disabled in double precision mode)', color = 'YELLOW')
    if (ctx.options.enable_samplerate != False):
        ctx.check_cfg(package = 'samplerate',
                args = '--cflags --libs samplerate >= 0.0.15',
                mandatory = ctx.options.enable_samplerate)

    # check for jack
    if (ctx.options.enable_jack != False):
        ctx.check_cfg(package = 'jack',
                args = '--cflags --libs',
                mandatory = ctx.options.enable_jack)

    # check for libav
    if (ctx.options.enable_avcodec != False):
        ctx.check_cfg(package = 'libavcodec',
                args = '--cflags --libs libavcodec >= 54.35.0',
                uselib_store = 'AVCODEC',
                mandatory = ctx.options.enable_avcodec)
        ctx.check_cfg(package = 'libavformat',
                args = '--cflags --libs libavformat >= 52.3.0',
                uselib_store = 'AVFORMAT',
                mandatory = ctx.options.enable_avcodec)
        ctx.check_cfg(package = 'libavutil',
                args = '--cflags --libs libavutil >= 52.3.0',
                uselib_store = 'AVUTIL',
                mandatory = ctx.options.enable_avcodec)
        ctx.check_cfg(package = 'libswresample',
                args = '--cflags --libs libswresample >= 1.2.0',
                uselib_store = 'SWRESAMPLE',
                mandatory = False)
        if 'HAVE_SWRESAMPLE' not in ctx.env:
            ctx.check_cfg(package = 'libavresample',
                    args = '--cflags --libs libavresample >= 1.0.1',
                    uselib_store = 'AVRESAMPLE',
                    mandatory = False)

        msg_check = 'Checking for all libav libraries'
        if 'HAVE_AVCODEC' not in ctx.env:
            ctx.msg(msg_check, 'not found (missing avcodec)', color = 'YELLOW')
        elif 'HAVE_AVFORMAT' not in ctx.env:
            ctx.msg(msg_check, 'not found (missing avformat)', color = 'YELLOW')
        elif 'HAVE_AVUTIL' not in ctx.env:
            ctx.msg(msg_check, 'not found (missing avutil)', color = 'YELLOW')
        elif 'HAVE_SWRESAMPLE' not in ctx.env \
                and 'HAVE_AVRESAMPLE' not in ctx.env:
            resample_missing = 'not found (avresample or swresample required)'
            ctx.msg(msg_check, resample_missing, color = 'YELLOW')
        else:
            ctx.msg(msg_check, 'yes')
            if 'HAVE_SWRESAMPLE' in ctx.env:
                ctx.define('HAVE_SWRESAMPLE', 1)
            elif 'HAVE_AVRESAMPLE' in ctx.env:
                ctx.define('HAVE_AVRESAMPLE', 1)
            ctx.define('HAVE_LIBAV', 1)

    if (ctx.options.enable_wavread != False):
        ctx.define('HAVE_WAVREAD', 1)
    ctx.msg('Checking if using source_wavread',
            ctx.options.enable_wavread and 'yes' or 'no')
    if (ctx.options.enable_wavwrite!= False):
        ctx.define('HAVE_WAVWRITE', 1)
    ctx.msg('Checking if using sink_wavwrite',
            ctx.options.enable_wavwrite and 'yes' or 'no')

    # use BLAS/ATLAS
    if (ctx.options.enable_blas != False):
        ctx.check_cfg(package = 'blas',
                args = '--cflags --libs',
                uselib_store='BLAS', mandatory = ctx.options.enable_blas)
        if 'LIB_BLAS' in ctx.env:
            blas_header = None
            if ctx.env['LIBPATH_BLAS']:
                if 'atlas' in ctx.env['LIBPATH_BLAS'][0]:
                    blas_header = 'atlas/cblas.h'
                elif 'openblas' in ctx.env['LIBPATH_BLAS'][0]:
                    blas_header = 'openblas/cblas.h'
            else:
                blas_header = 'cblas.h'
            ctx.check(header_name = blas_header, mandatory =
                    ctx.options.enable_atlas)

    # use memcpy hacks
    if (ctx.options.enable_memcpy == True):
        ctx.define('HAVE_MEMCPY_HACKS', 1)

    # write configuration header
    ctx.write_config_header('src/config.h')

    # the following defines will be passed as arguments to the compiler
    # instead of being written to src/config.h
    ctx.define('HAVE_CONFIG_H', 1)

    # add some defines used in examples
    ctx.define('AUBIO_PREFIX', ctx.env['PREFIX'])
    ctx.define('PACKAGE', APPNAME)

    # double precision mode
    if (ctx.options.enable_double == True):
        ctx.define('HAVE_AUBIO_DOUBLE', 1)

    if (ctx.options.enable_docs != False):
        # check if txt2man is installed, optional
        try:
          ctx.find_program('txt2man', var='TXT2MAN')
        except ctx.errors.ConfigurationError:
          ctx.to_log('txt2man was not found (ignoring)')

        # check if doxygen is installed, optional
        try:
          ctx.find_program('doxygen', var='DOXYGEN')
        except ctx.errors.ConfigurationError:
          ctx.to_log('doxygen was not found (ignoring)')

        # check if sphinx-build is installed, optional
        try:
          ctx.find_program('sphinx-build', var='SPHINX')
        except ctx.errors.ConfigurationError:
          ctx.to_log('sphinx-build was not found (ignoring)')

def build(bld):
    bld.env['VERSION'] = VERSION
    bld.env['LIB_VERSION'] = LIB_VERSION

    # main source
    bld.recurse('src')

    # add sub directories
    if bld.env['DEST_OS'] not in ['ios', 'iosimulator', 'android']:
        if bld.env['DEST_OS']=='emscripten' and not bld.options.testcmd:
            bld.options.testcmd = 'node %s'
        if bld.options.enable_examples:
            bld.recurse('examples')
        if bld.options.enable_tests:
            bld.recurse('tests')

    # pkg-config template
    bld( source = 'aubio.pc.in' )

    # documentation
    txt2man(bld)
    doxygen(bld)
    sphinx(bld)

    from waflib.Tools import waf_unit_test
    bld.add_post_fun(waf_unit_test.summary)
    bld.add_post_fun(waf_unit_test.set_exit_code)

def txt2man(bld):
    # build manpages from txt files using txt2man
    if bld.env['TXT2MAN']:
        from waflib import TaskGen
        if 'MANDIR' not in bld.env:
            bld.env['MANDIR'] = bld.env['DATAROOTDIR'] + '/man'
        bld.env.VERSION = VERSION
        rule_str = '${TXT2MAN} -t `basename ${TGT} | cut -f 1 -d . | tr a-z A-Z`'
        rule_str += ' -r ${PACKAGE}\\ ${VERSION} -P ${PACKAGE}'
        rule_str += ' -v ${PACKAGE}\\ User\\\'s\\ manual'
        rule_str += ' -s 1 ${SRC} > ${TGT}'
        TaskGen.declare_chain(
                name      = 'txt2man',
                rule      = rule_str,
                ext_in    = '.txt',
                ext_out   = '.1',
                reentrant = False,
                install_path =  '${MANDIR}/man1',
                )
        bld( source = bld.path.ant_glob('doc/*.txt') )

def doxygen(bld):
    # build documentation from source files using doxygen
    if bld.env['DOXYGEN']:
        bld.env.VERSION = VERSION
        rule = '( cat ${SRC[0]} && echo PROJECT_NUMBER=${VERSION}'
        rule += ' && echo OUTPUT_DIRECTORY=%s && echo HTML_OUTPUT=%s )'
        rule += ' | doxygen - > /dev/null'
        rule %= (os.path.abspath(out), 'api')
        bld( name = 'doxygen', rule = rule,
                source = ['doc/web.cfg']
                    + bld.path.find_dir('src').ant_glob('**/*.h'),
                target = bld.path.find_or_declare('api/index.html'),
                cwd = bld.path.find_dir('doc'))
        # evaluate nodes lazily to prevent build directory traversal warnings
        bld.install_files('${DATAROOTDIR}/doc/libaubio-doc/api',
                bld.path.find_or_declare('api').ant_glob('**/*',
                    generator=True), cwd=bld.path.find_or_declare('api'),
                relative_trick=True)

def sphinx(bld):
    # build documentation from source files using sphinx-build
    try:
        import aubio
        has_aubio = True
    except ImportError:
        from waflib import Logs
        Logs.pprint('YELLOW', "Sphinx manual: install aubio first")
        has_aubio = False
    if bld.env['SPHINX'] and has_aubio:
        bld.env.VERSION = VERSION
        rule = '${SPHINX} -b html -D release=${VERSION}' \
                ' -D version=${VERSION} -W -a -q' \
                ' -d %s ' % os.path.join(os.path.abspath(out), 'doctrees')
        rule += ' . %s' % os.path.join(os.path.abspath(out), 'manual')
        bld( name = 'sphinx', rule = rule,
                cwd = bld.path.find_dir('doc'),
                source = bld.path.find_dir('doc').ant_glob('*.rst'),
                target = bld.path.find_or_declare('manual/index.html'))
        # evaluate nodes lazily to prevent build directory traversal warnings
        bld.install_files('${DATAROOTDIR}/doc/libaubio-doc/manual',
                bld.path.find_or_declare('manual').ant_glob('**/*',
                    generator=True), cwd=bld.path.find_or_declare('manual'),
                relative_trick=True)

# register the previous rules as build rules
from waflib.Build import BuildContext

class build_txt2man(BuildContext):
    cmd = 'txt2man'
    fun = 'txt2man'

class build_manpages(BuildContext):
    cmd = 'manpages'
    fun = 'txt2man'

class build_sphinx(BuildContext):
    cmd = 'sphinx'
    fun = 'sphinx'

class build_doxygen(BuildContext):
    cmd = 'doxygen'
    fun = 'doxygen'

def shutdown(bld):
    from waflib import Logs
    if bld.options.target_platform in ['ios', 'iosimulator']:
        msg ='building for %s, contact the author for a commercial license' \
                % bld.options.target_platform
        Logs.pprint('RED', msg)
        msg ='   Paul Brossier <piem@aubio.org>'
        Logs.pprint('RED', msg)

def dist(ctx):
    ctx.excl  = ' **/.waf*'
    ctx.excl += ' **/.git*'
    ctx.excl += ' **/*~ **/*.pyc **/*.swp **/*.swo **/*.swn **/.lock-w*'
    ctx.excl += ' **/build/*'
    ctx.excl += ' doc/_build'
    ctx.excl += ' python/demos_*'
    ctx.excl += ' **/python/gen **/python/build **/python/dist'
    ctx.excl += ' **/python/ext/config.h'
    ctx.excl += ' **/python/lib/aubio/_aubio.so'
    ctx.excl += ' **.egg-info'
    ctx.excl += ' **/.eggs'
    ctx.excl += ' **/.pytest_cache'
    ctx.excl += ' **/.cache'
    ctx.excl += ' **/**.zip **/**.tar.bz2'
    ctx.excl += ' **.tar.bz2**'
    ctx.excl += ' **/doc/full/* **/doc/web/*'
    ctx.excl += ' **/doc/full.cfg'
    ctx.excl += ' **/python/*.db'
    ctx.excl += ' **/python.old/*'
    ctx.excl += ' **/python/*/*.old'
    ctx.excl += ' **/python/lib/aubio/*.so'
    ctx.excl += ' **/python/tests/sounds'
    ctx.excl += ' **/**.asc'
    ctx.excl += ' **/dist*'
    ctx.excl += ' **/.DS_Store'
    ctx.excl += ' **/.travis.yml'
    ctx.excl += ' **/.appveyor.yml'
    ctx.excl += ' **/.circleci/*'
    ctx.excl += ' **/azure-pipelines.yml'
    ctx.excl += ' **/.coverage*'
