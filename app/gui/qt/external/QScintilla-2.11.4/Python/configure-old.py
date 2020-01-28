# This script configures QScintilla for PyQt v3 and/or v4.
#
# Copyright (c) 2019 Riverbank Computing Limited <info@riverbankcomputing.com>
# 
# This file is part of QScintilla.
# 
# This file may be used under the terms of the GNU General Public License
# version 3.0 as published by the Free Software Foundation and appearing in
# the file LICENSE included in the packaging of this file.  Please review the
# following information to ensure the GNU General Public License version 3.0
# requirements will be met: http://www.gnu.org/copyleft/gpl.html.
# 
# If you do not wish to use this file under the terms of the GPL version 3.0
# then you may purchase a commercial license.  For more information contact
# info@riverbankcomputing.com.
# 
# This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
# WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.


import sys
import os
import glob
import optparse


# Import SIP's configuration module so that we have access to the error
# reporting.  Then try and import the configuration modules for both PyQt3 and
# PyQt4.
try:
    import sipconfig
except ImportError:
    sys.stderr.write("Unable to import sipconfig.  Please make sure SIP is installed.\n")
    sys.exit(1)

try:
    import PyQt4.pyqtconfig as pyqt4
except:
    pyqt4 = None

try:
    import pyqtconfig as pyqt3
except:
    pyqt3 = None

if pyqt4 is not None:
    pyqt = pyqt4.Configuration()
    qt_data_dir = pyqt.qt_data_dir
elif pyqt3 is not None:
    pyqt = pyqt3.Configuration()
    qt_data_dir = pyqt.qt_dir
else:
    sipconfig.error("Unable to find either PyQt v3 or v4.")


# Initialise the globals.
sip_min_version = 0x040c00

qsci_define = "QSCINTILLA_DLL"


def create_optparser():
    """Create the parser for the command line.
    """

    def store_abspath(option, opt_str, value, parser):
        setattr(parser.values, option.dest, os.path.abspath(value))

    def store_abspath_dir(option, opt_str, value, parser):
        if not os.path.isdir(value):
            raise optparse.OptionValueError("'%s' is not a directory" % value)
        setattr(parser.values, option.dest, os.path.abspath(value))

    p = optparse.OptionParser(usage="python %prog [options]",
            version="2.11.4")

    p.add_option("-a", "--apidir", action="callback", default=None,
            type="string", metavar="DIR", dest="qscidir",
            callback=store_abspath, help="where QScintilla's API file will be "
            "installed [default: QTDIR/qsci]")
    p.add_option("-c", "--concatenate", action="store_true", default=False,
            dest="concat", help="concatenate the C++ source files")
    p.add_option("-d", "--destdir", action="callback",
            default=pyqt.pyqt_mod_dir, type="string", metavar="DIR",
            dest="qscimoddir", callback=store_abspath, help="where the "
            "QScintilla module will be installed [default: %s]" %
            pyqt.pyqt_mod_dir)
    p.add_option("-j", "--concatenate-split", type="int", default=1,
            metavar="N", dest="split", help="split the concatenated C++ "
            "source files into N pieces [default: 1]")
    p.add_option("-k", "--static", action="store_true", default=False,
            dest="static", help="build the QScintilla module as a static "
            "library")
    p.add_option("-n", action="callback", default=None, type="string",
            metavar="DIR", dest="qsciincdir", callback=store_abspath_dir,
            help="the directory containing the QScintilla Qsci header file "
            "directory [default: %s]" % pyqt.qt_inc_dir)
    p.add_option("--no-docstrings", action="store_true", default=False,
            dest="no_docstrings", help="disable the generation of docstrings")
    p.add_option("-o", action="callback", default=None, type="string",
            metavar="DIR", dest="qscilibdir", callback=store_abspath_dir,
            help="the directory containing the QScintilla library [default: "
            "%s]" % pyqt.qt_lib_dir)
    p.add_option("-p", type="int", default=-1, metavar="3|4", dest="pyqt_major",
            help="specifically configure for PyQt v3 or v4 [default v4, if "
            "found]")
    p.add_option("-r", "--trace", action="store_true", default=False,
            dest="tracing", help="build the QScintilla module with tracing "
            "enabled")
    p.add_option("-s", action="store_true", default=False, dest="not_dll",
            help="QScintilla is a static library and not a DLL (Windows only)")
    p.add_option("-u", "--debug", action="store_true", default=False,
            help="build the QScintilla module with debugging symbols")
    p.add_option("-v", "--sipdir", action="callback", default=None,
            metavar="DIR", dest="qscisipdir", callback=store_abspath,
            type="string", help="where the QScintilla .sip files will be "
            "installed [default: %s]" % pyqt.pyqt_sip_dir)
    p.add_option("-T", "--no-timestamp", action="store_true", default=False,
            dest="no_timestamp", help="suppress timestamps in the header "
            "comments of generated code [default: include timestamps]")

    if sys.platform != 'win32':
        if sys.platform.startswith('linux') or sys.platform == 'darwin':
            pip_default = True
            pip_default_str = "enabled"
        else:
            pip_default = False
            pip_default_str = "disabled"

        p.add_option("--protected-is-public", action="store_true",
                default=pip_default, dest="prot_is_public",
                help="enable building with 'protected' redefined as 'public' "
                        "[default: %s]" % pip_default_str)
        p.add_option("--protected-not-public", action="store_false",
                dest="prot_is_public",
                help="disable building with 'protected' redefined as 'public'")

    return p


def inform_user():
    """Tell the user the option values that are going to be used.
    """
    sipconfig.inform("PyQt %s is being used." % pyqt.pyqt_version_str)
    sipconfig.inform("Qt v%s %s edition is being used." % (sipconfig.version_to_string(pyqt.qt_version), pyqt.qt_edition))
    sipconfig.inform("SIP %s is being used." % pyqt.sip_version_str)

    sipconfig.inform("The QScintilla module will be installed in %s." % opts.qscimoddir)
    sipconfig.inform("The QScintilla API file will be installed in %s." % os.path.join(opts.qscidir, "api", "python"))
    sipconfig.inform("The QScintilla .sip files will be installed in %s." % opts.qscisipdir)

    if opts.no_docstrings:
        sipconfig.inform("The QScintilla module is being built without generated docstrings.")
    else:
        sipconfig.inform("The QScintilla module is being built with generated docstrings.")

    if opts.prot_is_public:
        sipconfig.inform("The QScintilla module is being built with 'protected' redefined as 'public'.")


def check_qscintilla():
    """See if QScintilla can be found and what its version is.
    """
    # Find the QScintilla header files.
    sciglobal = os.path.join(opts.qsciincdir, "Qsci", "qsciglobal.h")

    if os.access(sciglobal, os.F_OK):
        # Get the QScintilla version string.
        _, sciversstr = sipconfig.read_version(sciglobal, "QScintilla", "QSCINTILLA_VERSION", "QSCINTILLA_VERSION_STR")

        if glob.glob(os.path.join(opts.qscilibdir, "*qscintilla2*")):
            # Because we include the Python bindings with the C++ code we can
            # reasonably force the same version to be used and not bother about
            # versioning.
            if sciversstr != "2.11.4":
                sipconfig.error("QScintilla %s is being used but the Python bindings 2.11.4 are being built.  Please use matching versions." % sciversstr)

            sipconfig.inform("QScintilla %s is being used." % sciversstr)
        else:
            sipconfig.error("The QScintilla library could not be found in %s. If QScintilla is installed then use the -o argument to explicitly specify the correct directory." % opts.qscilibdir)
    else:
        sipconfig.error("Qsci/qsciglobal.h could not be found in %s. If QScintilla is installed then use the -n argument to explicitly specify the correct directory." % opts.qsciincdir)


def sip_flags():
    """Return the SIP flags.
    """
    # Get the flags used for the main PyQt module.
    if pyqt.pyqt_version >= 0x040000:
        flags = pyqt.pyqt_sip_flags.split()
    else:
        flags = pyqt.pyqt_qt_sip_flags.split()
        flags.append("-x")
        flags.append("Qsci_Qt4")

    # Generate the API file.
    flags.append("-a")
    flags.append("QScintilla2.api")

    # Add PyQt's .sip files to the search path.
    flags.append("-I")
    flags.append(pyqt.pyqt_sip_dir)

    return flags


def generate_code():
    """Generate the code for the QScintilla module.
    """
    if pyqt.pyqt_version >= 0x040000:
        mname = "Qsci"
    else:
        mname = "qsci"

    sipconfig.inform("Generating the C++ source for the %s module..." % mname)

    # Build the SIP command line.
    argv = ['"' + pyqt.sip_bin + '"']

    argv.extend(sip_flags())

    if opts.no_timestamp:
        argv.append("-T")

    if not opts.no_docstrings:
        argv.append("-o");

    if opts.prot_is_public:
        argv.append("-P");

    if opts.concat:
        argv.append("-j")
        argv.append(str(opts.split))

    if opts.tracing:
        argv.append("-r")

    argv.append("-c")
    argv.append(".")

    buildfile = os.path.join("qsci.sbf")
    argv.append("-b")
    argv.append(buildfile)

    if pyqt.pyqt_version >= 0x040000:
        argv.append("sip/qscimod4.sip")
    else:
        argv.append("sip/qscimod3.sip")

    os.system(" ".join(argv))

    # Check the result.
    if not os.access(buildfile, os.F_OK):
        sipconfig.error("Unable to create the C++ code.")

    # Generate the Makefile.
    sipconfig.inform("Creating the Makefile for the %s module..." % mname)

    if pyqt.pyqt_version >= 0x040000:
        class Makefile(pyqt4.QtGuiModuleMakefile):
            def generate_target_install(self, mfile):
                pyqt4.QtGuiModuleMakefile.generate_target_install(self, mfile)
    else:
        class Makefile(pyqt3.QtModuleMakefile):
            def generate_target_install(self, mfile):
                pyqt3.QtModuleMakefile.generate_target_install(self, mfile)

    installs = []
    sipfiles = []

    for s in glob.glob("sip/*.sip"):
        sipfiles.append(os.path.join("sip", os.path.basename(s)))

    installs.append([sipfiles, os.path.join(opts.qscisipdir, mname)])

    installs.append(("QScintilla2.api", os.path.join(opts.qscidir, "api", "python")))

    # PyQt v4.2 and later can handle MacOS/X universal binaries.
    if pyqt.pyqt_version >= 0x040200:
        makefile = Makefile(
            configuration=pyqt,
            build_file="qsci.sbf",
            install_dir=opts.qscimoddir,
            installs=installs,
            static=opts.static,
            debug=opts.debug,
            universal=pyqt.universal,
            arch=pyqt.arch,
            prot_is_public=opts.prot_is_public,
            deployment_target=pyqt.deployment_target
        )
    else:
        makefile = Makefile(
            configuration=pyqt,
            build_file="qsci.sbf",
            install_dir=opts.qscimoddir,
            installs=installs,
            static=opts.static,
            debug=opts.debug
        )

    if qsci_define:
        makefile.extra_defines.append(qsci_define)

    makefile.extra_include_dirs.append(opts.qsciincdir)
    makefile.extra_lib_dirs.append(opts.qscilibdir)
    makefile.extra_libs.append("qscintilla2")

    makefile.generate()


def main(argv):
    """Create the configuration module module.

    argv is the list of command line arguments.
    """
    global pyqt

    # Check SIP is new enough.
    if ".dev" not in pyqt.sip_version_str and "snapshot" not in pyqt.sip_version_str:
        if pyqt.sip_version < sip_min_version:
            sipconfig.error("This version of QScintilla requires SIP v%s or later" % sipconfig.version_to_string(sip_min_version))

    # Parse the command line.
    global opts

    p = create_optparser()
    opts, args = p.parse_args()

    if args:
        p.print_help()
        sys.exit(2)

    # Provide defaults for platform-specific options.
    if sys.platform == 'win32':
        opts.prot_is_public = False

    if opts.not_dll:
        global qsci_define
        qsci_define = ""

    # Set the version of PyQt explicitly.
    global qt_data_dir

    if opts.pyqt_major == 4:
        if pyqt4 is None:
            sipconfig.error("PyQt v4 was specified with the -p argument but doesn't seem to be installed.")
        else:
            pyqt = pyqt4.Configuration()
            qt_data_dir = pyqt.qt_data_dir
    elif opts.pyqt_major == 3:
        if pyqt3 is None:
            sipconfig.error("PyQt v3 was specified with the -p argument but doesn't seem to be installed.")
        else:
            pyqt = pyqt3.Configuration()
            qt_data_dir = pyqt.qt_dir
    elif opts.pyqt_major >= 0:
        sipconfig.error("Specify either 3 or 4 with the -p argument.")

    # Now we know which version of PyQt to use we can set defaults for those
    # arguments that weren't specified.
    if opts.qscimoddir is None:
        opts.qscimoddir = pyqt.pyqt_mod_dir

    if opts.qsciincdir is None:
        opts.qsciincdir = pyqt.qt_inc_dir

    if opts.qscilibdir is None:
        opts.qscilibdir = pyqt.qt_lib_dir

    if opts.qscisipdir is None:
        opts.qscisipdir = pyqt.pyqt_sip_dir

    if opts.qscidir is None:
        opts.qscidir = os.path.join(qt_data_dir, "qsci")

    # Check for QScintilla.
    check_qscintilla()

    # Tell the user what's been found.
    inform_user()

    # Generate the code.
    generate_code()


###############################################################################
# The script starts here.
###############################################################################

if __name__ == "__main__":
    try:
        main(sys.argv)
    except SystemExit:
        raise
    except:
        sys.stderr.write(
"""An internal error occured.  Please report all the output from the program,
including the following traceback, to support@riverbankcomputing.com.
""")
        raise
