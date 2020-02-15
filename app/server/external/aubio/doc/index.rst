Welcome
=======

aubio is a collection of algorithms and tools to label and transform music and
sounds. It scans or `listens` to audio signals and attempts to detect musical
events. For instance, when a drum is hit, at which frequency is a note, or at
what tempo is a rhythmic melody.

aubio features include segmenting a sound file before each of its attacks,
performing pitch detection, tapping the beat and producing midi streams from
live audio.

Quick links
===========

* :ref:`python`
* :ref:`manpages`
* :ref:`develop`
* :ref:`building`

.. only:: devel

    .. include:: statuslinks.rst

Project pages
=============

* `Project homepage`_: https://aubio.org
* `aubio on github`_: https://github.com/aubio/aubio
* `aubio on pypi`_: https://pypi.python.org/pypi/aubio
* `Doxygen documentation`_: https://aubio.org/doc/latest/
* `Mailing lists`_: https://lists.aubio.org

.. _Project homepage: https://aubio.org
.. _aubio on github: https://github.com/aubio/aubio
.. _aubio on pypi: https://pypi.python.org/pypi/aubio
.. _Doxygen documentation: https://aubio.org/doc/latest/
.. _Mailing lists: https://lists.aubio.org/

* `Travis Continuous integration page <https://travis-ci.org/aubio/aubio>`_
* `Appveyor Continuous integration page <https://ci.appveyor.com/project/piem/aubio>`_
* `Landscape python code validation <https://landscape.io/github/aubio/aubio/master>`_
* `ReadTheDocs documentation <https://aubio.readthedocs.io/en/latest/>`_

Features
========

aubio provides several algorithms and routines, including:

- several onset detection methods
- different pitch detection methods
- tempo tracking and beat detection
- MFCC (mel-frequency cepstrum coefficients)
- FFT and phase vocoder
- up/down-sampling
- digital filters (low pass, high pass, and more)
- spectral filtering
- transient/steady-state separation
- sound file read and write access
- various mathematics utilities for music applications

The name aubio comes from *audio* with a typo: some errors are likely to be
found in the results.

Content
=======

.. toctree::
   :maxdepth: 2

   installing
   python_module
   python
   cli
   develop
   about
