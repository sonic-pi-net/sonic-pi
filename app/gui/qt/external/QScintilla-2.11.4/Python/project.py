# This is the QScintilla build script.
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


import os

from pyqtbuild import PyQtBindings, PyQtProject
from sipbuild import Option


class QScintilla(PyQtProject):
    """ The QScintilla project. """

    def __init__(self):
        """ Initialise the project. """

        super().__init__(sip_files_dir='Python/sip',
                tests_dir='Python/config-tests')

        self.bindings_factories = [Qsci]


class Qsci(PyQtBindings):
    """ The Qsci bindings. """

    def __init__(self, project):
        """ Initialise the bindings. """

        super().__init__(project, 'Qsci', qmake_CONFIG=['qscintilla2'],
                sip_file='qscimod5.sip')

    def apply_user_defaults(self, tool):
        """ Set default values for user options that haven't been set yet. """

        if self.qsci_features_dir is not None:
            os.environ['QMAKEFEATURES'] = os.path.abspath(
                    self.qsci_features_dir)

        if self.qsci_include_dir is not None:
            self.include_dirs.append(os.path.abspath(self.qsci_include_dir))

        if self.qsci_library_dir is not None:
            self.library_dirs.append(os.path.abspath(self.qsci_library_dir))

        super().apply_user_defaults(tool)

    def get_options(self):
        """ Return the list of configurable options. """

        options = super().get_options()

        # The directory containing the features file.
        options.append(
                Option('qsci_features_dir',
                        help="the qscintilla2.prf features file is in DIR",
                        metavar="DIR"))

        # The directory containing the include directory.
        options.append(
                Option('qsci_include_dir',
                        help="the Qsci include file directory is in DIR",
                        metavar="DIR"))

        # The directory containing the library.
        options.append(
                Option('qsci_library_dir',
                        help="the QScintilla library is in DIR",
                        metavar="DIR"))

        return options

    def handle_test_output(self, test_output):
        """ Handle the output from the external test program and return True if
        the bindings are buildable.
        """

        project = self.project

        installed_version = int(test_output[0])
        installed_version_str = test_output[1]

        if project.version != installed_version:
            project.progress(
                    "QScintilla v{0} is required but QScintilla v{1} is "
                    "installed.".format(project.version_str,
                            installed_version_str))
            return False

        return True
