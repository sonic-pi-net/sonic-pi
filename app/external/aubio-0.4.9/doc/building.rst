.. highlight:: bash

.. _building:

Building aubio
==============

.. note::
    To download a prebuilt version of aubio, see :ref:`download`.

aubio uses `waf`_ to configure, compile, and test the source.
A copy of waf is included in aubio tarball, so all you need is a terminal,
a compiler, and a recent version of python installed.

.. note::
    Make sure you have all the :ref:`requirements` you want before building.

Latest release
--------------

The **latest stable release** can be downloaded from https://aubio.org/download::

        $ curl -O http://aubio.org/pub/aubio-<version>.tar.bz2
        $ tar xf aubio-<version>.tar.bz2
        $ cd aubio-<version>/

Git repository
--------------

The **latest git branch** can be obtained with::

        $ git clone git://git.aubio.org/git/aubio
        $ cd aubio/

The following command will fetch the correct `waf`_ version (not included in
aubio's git)::

        $ ./scripts/get_waf.sh

.. note::

  Windows users without `Git Bash`_ installed will want to use the following
  commands instead:

  .. code:: bash

        $ curl -fsS -o waf https://waf.io/waf-1.8.22
        $ curl -fsS -o waf.bat https://raw.githubusercontent.com/waf-project/waf/master/utils/waf.bat


Compiling
---------

To compile the C library, examples programs, and tests, run::

        $ ./waf configure

Check out the available options using ``./waf configure --help``. Once
you are done with configuration, you can start building::

        $ ./waf build

To install the freshly built C library and tools, simply run the following
command::

        $ sudo ./waf install

.. note::
  Windows users should simply run ``waf``, without the leading ``./``. For
  instance:

  .. code:: bash

       $ waf configure build


Running as a user
-----------------

To use aubio without actually installing, for instance if you don't have root
access to install libaubio on your system,

On Linux or macOS, sourcing the script ``scripts/setenv_local.sh`` should help::

       $ source ./scripts/setenv_local.sh

This script sets ``LD_LIBRARY_PATH``, for libaubio, and ``PYTHONPATH`` for the
python module.

On Linux, you should be able to set ``LD_LIBRARY_PATH`` with::

        $ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD/build/src

On Mac OS X, a copy or a symlink can be made in ``~/lib``::

        $ mkdir -p ~/lib
        $ ln -sf $PWD/build/src/libaubio*.dylib ~/lib/

Note on Mac OS X systems older than El Capitan (10.11), the ``DYLD_LIBRARY_PATH``
variable can be set as follows::

        $ export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$PWD/build/src

Cleaning
--------

If you wish to uninstall the files installed by the ``install`` command, use
``uninstall``::

        $ sudo ./waf uninstall

To clean the source directory, use the ``clean`` command::

        $ ./waf clean

To also forget the options previously passed to the last ``./waf configure``
invocation, use the ``distclean`` command::

        $ ./waf distclean

.. _waf: https://waf.io/

.. _Git Bash: https://git-for-windows.github.io/

.. _xcode-frameworks-label:

.. include:: xcode_frameworks.rst

.. include:: android.rst
