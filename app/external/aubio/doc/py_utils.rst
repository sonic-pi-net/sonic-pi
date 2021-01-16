.. default-domain:: py
.. currentmodule:: aubio

Utilities
---------

This section documents various helper functions included in the aubio library.

Note name conversion
....................

.. midiconv.py

.. autofunction:: note2midi

.. autofunction:: midi2note

.. autofunction:: freq2note

.. autofunction:: note2freq

Frequency conversion
....................

.. python/ext/ufuncs.c

.. autofunction:: freqtomidi

.. autofunction:: miditofreq

.. python/ext/py-musicutils.h

.. autofunction:: meltohz

.. autofunction:: hztomel

.. python/ext/aubiomodule.c

.. autofunction:: bintomidi
.. autofunction:: miditobin
.. autofunction:: bintofreq
.. autofunction:: freqtobin

Audio file slicing
..................

.. slicing.py

.. autofunction:: slice_source_at_stamps

Windowing
.........

.. python/ext/py-musicutils.h

.. autofunction:: window

Audio level detection
.....................

.. python/ext/py-musicutils.h

.. autofunction:: level_lin
.. autofunction:: db_spl
.. autofunction:: silence_detection
.. autofunction:: level_detection

Vector utilities
................

.. python/ext/aubiomodule.c

.. autofunction:: alpha_norm
.. autofunction:: zero_crossing_rate
.. autofunction:: min_removal

.. python/ext/py-musicutils.h

.. autofunction:: shift
.. autofunction:: ishift

.. python/ext/ufuncs.c

.. autofunction:: unwrap2pi
