.. _manpages:

Command line tools
==================

The python module comes with the following tools:

 - ``aubio`` estimate and extract descriptors from sound files
 - ``aubiocut`` slices sound files at onset or beat timestamps

More command line tools are included along with the library.

 - ``aubioonset`` outputs the time stamp of detected note onsets
 - ``aubiopitch`` attempts to identify a fundamental frequency, or pitch, for
   each frame of the input sound
 - ``aubiomfcc`` computes Mel-frequency Cepstrum Coefficients
 - ``aubiotrack`` outputs the time stamp of detected beats
 - ``aubionotes`` emits midi-like notes, with an onset, a pitch, and a duration
 - ``aubioquiet`` extracts quiet and loud regions


``aubio``
---------

.. literalinclude:: aubio.txt
   :language: text


``aubiocut``
--------------

.. literalinclude:: aubiocut.txt
   :language: text


``aubioonset``
--------------

.. literalinclude:: aubioonset.txt
   :language: text

``aubiopitch``
--------------

.. literalinclude:: aubiopitch.txt
   :language: text

``aubiomfcc``
--------------

.. literalinclude:: aubiomfcc.txt
   :language: text

``aubiotrack``
--------------

.. literalinclude:: aubiotrack.txt
   :language: text

``aubionotes``
--------------

.. literalinclude:: aubionotes.txt
   :language: text

``aubioquiet``
--------------

.. literalinclude:: aubioquiet.txt
   :language: text


.. include:: cli_features.rst
