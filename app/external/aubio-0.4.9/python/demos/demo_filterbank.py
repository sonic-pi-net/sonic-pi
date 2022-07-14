#! /usr/bin/env python

"""Create a filterbank from a list of frequencies.

This demo uses `aubio.filterbank.set_triangle_bands` to build a set of
triangular filters from a list of frequencies.

The filterbank coefficients are then modified before being displayed."""

import aubio
import numpy as np
import matplotlib.pyplot as plt

# sampling rate and size of the fft
samplerate = 48000
win_s = 2048

# define a list of custom frequency
freq_list = [60, 80, 200, 400, 800, 1600, 3200, 6400, 12800, 24000]
# number of filters to create
n_filters = len(freq_list) - 2

# create a new filterbank
f = aubio.filterbank(n_filters, win_s)
freqs = aubio.fvec(freq_list)
f.set_triangle_bands(freqs, samplerate)

# get the coefficients from the filterbank
coeffs = f.get_coeffs()
# apply a gain to fifth band
coeffs[4] *= 6.
# load the modified coeffs into the filterbank
f.set_coeffs(coeffs)

# display the band gains in a loglog plot
freqs = np.vstack([np.arange(win_s // 2 + 1) * samplerate / win_s] * n_filters)
plt.title('filterbank built from a list of frequencies\n'
          'The 5th band has been amplified by a factor 6.')
plt.loglog(freqs.T, f.get_coeffs().T, '.-')
plt.xlim([50, samplerate/2])
plt.ylim([1.0e-6, 2.0e-2])
plt.xlabel('log frequency (Hz)')
plt.ylabel('log amplitude')
plt.show()
