#! /usr/bin/env python

from aubio import filterbank
from numpy import arange, vstack

win_s = 8192
samplerate = 16000

f = filterbank(40, win_s)
f.set_mel_coeffs_slaney(samplerate)

from pylab import loglog, title, show, xlim, ylim, xlabel, ylabel
xlim([0,samplerate / 2])
times = vstack([arange(win_s // 2 + 1) * samplerate / win_s] * 40)
loglog(times.T, f.get_coeffs().T, '.-')
title('Mel frequency bands coefficients')
xlim([100, 7500])
ylim([1.0e-3, 2.0e-2])
xlabel('Frequency (Hz)')
ylabel('Amplitude')
show()
