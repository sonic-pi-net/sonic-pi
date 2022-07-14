#! /usr/bin/env python

from aubio import miditofreq
from numpy import arange

upsampling = 100.
midi = arange(-10, 148 * upsampling)
midi /= upsampling
freq = miditofreq(midi)

from matplotlib import pyplot as plt

ax = plt.axes()
ax.semilogy(midi, freq, '.')
ax.set_xlabel('midi note')
ax.set_ylabel('frequency (Hz)')
plt.show()
