#! /usr/bin/env python

from aubio import filterbank, fvec
from pylab import loglog, show, subplot, xlim, ylim, xlabel, ylabel, title
from numpy import vstack, arange

win_s = 2048
samplerate = 48000

freq_list = [60, 80, 200, 400, 800, 1600, 3200, 6400, 12800, 24000]
n_filters = len(freq_list) - 2

f = filterbank(n_filters, win_s)
freqs = fvec(freq_list)
f.set_triangle_bands(freqs, samplerate)

subplot(211)
title('Examples of filterbank built with set_triangle_bands and set_coeffs')
times = vstack([arange(win_s // 2 + 1) * samplerate / win_s] * n_filters)
loglog(times.T, f.get_coeffs().T, '.-')
xlim([50, samplerate/2])
ylim([1.0e-6, 2.0e-2])
ylabel('Amplitude')

## build a new filterbank

freq_list = [60, 80, 200, 400, 800, 1200, 1600, 3200, 6400, 10000, 15000, 24000]
n_filters = len(freq_list) - 2

f = filterbank(n_filters, win_s)
freqs = fvec(freq_list)
f.set_triangle_bands(freqs, samplerate)

coeffs = f.get_coeffs()
coeffs[4] *= 5.

f.set_coeffs(coeffs)

subplot(212)
times = vstack([arange(win_s // 2 + 1) * samplerate / win_s] * n_filters)
loglog(times.T, f.get_coeffs().T, '.-')
xlim([50, samplerate/2])
ylim([1.0e-6, 2.0e-2])
xlabel('Frequency (Hz)')
ylabel('Amplitude')

show()
