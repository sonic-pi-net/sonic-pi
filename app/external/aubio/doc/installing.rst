Installing aubio
================

aubio runs on Linux, Windows, macOS, iOS, Android, and probably a few others
operating systems.

Aubio is available as a C library and as a python module.

Cheat sheet
-----------

- :ref:`get aubio latest source code <building>`::

    # official repo
    git clone https://git.aubio.org/aubio/aubio
    # mirror
    git clone https://github.com/aubio/aubio
    # latest release
    wget https://aubio.org/pub/aubio-<version>.tar.gz


- :ref:`build aubio from source <building>`::

    # 1. simple
    cd aubio
    make

    # 2. step by step
    ./scripts/get_waf.sh
    ./waf configure
    ./waf build
    sudo ./waf install

- :ref:`install python-aubio from source <python-install>`::

    # from git
    pip install git+https://git.aubio.org/aubio/aubio/
    # mirror
    pip install git+https://github.com/aubio/aubio/
    # from latest release
    pip install https://aubio.org/pub/aubio-latest.tar.bz2
    # from pypi
    pip install aubio
    # from source directory
    cd aubio
    pip install -v .

- :ref:`install python-aubio from a pre-compiled binary <python-install>`::

      # conda [osx, linux, win]
      conda install -c conda-forge aubio
      # .deb (debian, ubuntu) [linux]
      sudo apt-get install python3-aubio python-aubio aubio-tools
      # brew [osx]
      brew install aubio --with-python

- :ref:`get a pre-compiled version of libaubio <download>`::

    # .deb (linux) WARNING: old version
    sudo apt-get install aubio-tools

    # python module
    ./setup.py install
    # using pip
    pip install .

- :ref:`check the list of optional dependencies <requirements>`::

    # debian / ubuntu
    dpkg -l libavcodec-dev libavutil-dev libavformat-dev \
            libswresample-dev libavresample-dev \
            libsamplerate-dev libsndfile-dev \
            txt2man doxygen

.. include:: download.rst

.. include:: building.rst

.. include:: requirements.rst
