.. default-domain:: py
.. currentmodule:: aubio

Examples
--------

Below is a short selection of examples using the aubio module.

Read a sound file
.................

Here is a simple script, :download:`demo_source_simple.py
<../python/demos/demo_source_simple.py>` that reads all the samples from a
media file using :class:`source`:

.. literalinclude:: ../python/demos/demo_source_simple.py
   :language: python

Filter a sound file
...................

Here is another example, :download:`demo_filter.py
<../python/demos/demo_filter.py>`, which applies a filter to a sound file
and writes the filtered signal in another file:

* read audio samples from a file with :class:`source`

* filter them using an `A-weighting <https://en.wikipedia.org/wiki/A-weighting>`_
  filter using :class:`digital_filter`

* write the filtered samples to a new file with :class:`sink`.

.. literalinclude:: ../python/demos/demo_filter.py
   :language: python

More examples
.............

For more examples showing how to use other components of the module, see
the `python demos folder`_.

.. _python demos folder: https://github.com/aubio/aubio/blob/master/python/demos
