aubio
=====

[![Travis build status](https://travis-ci.org/aubio/aubio.svg?branch=master)](https://travis-ci.org/aubio/aubio "Travis build status")
[![Appveyor build status](https://img.shields.io/appveyor/ci/piem/aubio/master.svg)](https://ci.appveyor.com/project/piem/aubio "Appveyor build status")
[![Commits since last release](https://img.shields.io/github/commits-since/aubio/aubio/latest.svg)](https://github.com/aubio/aubio "Commits since last release")

[![Documentation](https://readthedocs.org/projects/aubio/badge/?version=latest)](http://aubio.readthedocs.io/en/latest/?badge=latest "Latest documentation")
[![DOI](https://zenodo.org/badge/396389.svg)](https://zenodo.org/badge/latestdoi/396389)

aubio is a library to label music and sounds. It listens to audio signals and
attempts to detect events. For instance, when a drum is hit, at which frequency
is a note, or at what tempo is a rhythmic melody.

Its features include segmenting a sound file before each of its attacks,
performing pitch detection, tapping the beat and producing midi streams from
live audio.

aubio provide several algorithms and routines, including:

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

The name aubio comes from _audio_ with a typo: some errors are likely to be
found in the results.

Python module
-------------

A python module for aubio is provided. For more information on how to use it,
please see the file [`python/README.md`](python/README.md) and the
[manual](https://aubio.org/manual/latest/) .

Tools
-----

The python module comes with the following command line tools:

 - `aubio` extracts informations from sound files
 - `aubiocut` slices sound files at onset or beat timestamps

Additional command line tools are included along with the library:

 - `aubioonset` outputs the time stamp of detected note onsets
 - `aubiopitch` attempts to identify a fundamental frequency, or pitch, for
   each frame of the input sound
 - `aubiomfcc` computes Mel-frequency Cepstrum Coefficients
 - `aubiotrack` outputs the time stamp of detected beats
 - `aubionotes` emits midi-like notes, with an onset, a pitch, and a duration
 - `aubioquiet` extracts quiet and loud regions

Documentation
-------------

  - [manual](https://aubio.org/manual/latest/), generated with sphinx
  - [developer documentation](https://aubio.org/doc/latest/), generated with Doxygen

The latest version of the documentation can be found at:

  https://aubio.org/documentation

Build Instructions
------------------

aubio compiles on Linux, Mac OS X, Windows, Cygwin, and iOS.

To compile aubio, you should be able to simply run:

    make

To compile the python module:

    ./setup.py build

See the [manual](https://aubio.org/manual/latest/) for more information about
[installing aubio](https://aubio.org/manual/latest/installing.html).

Citation
--------

Please use the DOI link above to cite this release in your publications. For
more information, see also the [about
page](https://aubio.org/manual/latest/about.html) in [aubio
manual](https://aubio.org/manual/latest/).

Homepage
--------

The home page of this project can be found at: https://aubio.org/

License
-------

aubio is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Contributing
------------

Patches are welcome: please fork the latest git repository and create a feature
branch. Submitted requests should pass all continuous integration tests.
