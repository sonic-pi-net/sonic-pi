#! /usr/bin/env python
# -*- coding: utf8 -*-

""" Pure python implementation of the sum of squared difference

    sqd_yin: original sum of squared difference [0]
        d_t(tau) = x ⊗ kernel
    sqd_yinfast: sum of squared diff using complex domain [0]
    sqd_yinfftslow: tappered squared diff [1]
    sqd_yinfft: modified squared diff using complex domain [1]

[0]:http://audition.ens.fr/adc/pdf/2002_JASA_YIN.pdf
[1]:https://aubio.org/phd/
"""

import sys
import numpy as np
import matplotlib.pyplot as plt

def sqd_yin(samples):
    """ compute original sum of squared difference

    Brute-force computation (cost o(N**2), slow)."""
    B = len(samples)
    W = B//2
    yin = np.zeros(W)
    for j in range(W):
        for tau in range(1, W):
            yin[tau] += (samples[j] - samples[j+tau])**2
    return yin

def sqd_yinfast(samples):
    """ compute approximate sum of squared difference

    Using complex convolution (fast, cost o(n*log(n)) )"""
    # yin_t(tau) = (r_t(0) + r_(t+tau)(0)) - 2r_t(tau)
    B = len(samples)
    W = B//2
    yin = np.zeros(W)
    sqdiff = np.zeros(W)
    kernel = np.zeros(B)
    # compute r_(t+tau)(0)
    squares = samples**2
    for tau in range(W):
        sqdiff[tau] = squares[tau:tau+W].sum()
    # add r_t(0)
    sqdiff += sqdiff[0]
    # compute r_t(tau) using kernel convolution in complex domain
    samples_fft = np.fft.fft(samples)
    kernel[1:W+1] = samples[W-1::-1] # first half, reversed
    kernel_fft = np.fft.fft(kernel)
    r_t_tau = np.fft.ifft(samples_fft * kernel_fft).real[W:]
    # compute yin_t(tau)
    yin = sqdiff - 2 * r_t_tau
    return yin

def sqd_yintapered(samples):
    """ compute tappered sum of squared difference

    Brute-force computation (cost o(N**2), slow)."""
    B = len(samples)
    W = B//2
    yin = np.zeros(W)
    for tau in range(1, W):
        for j in range(W - tau):
            yin[tau] += (samples[j] - samples[j+tau])**2
    return yin

def sqd_yinfft(samples):
    """ compute yinfft modified sum of squared differences

    Very fast, improved performance in transients.

    FIXME: biased."""
    B = len(samples)
    W = B//2
    yin = np.zeros(W)
    def hanningz(W):
        return .5 * (1. - np.cos(2. * np.pi * np.arange(W) / W))
    #win = np.ones(B)
    win = hanningz(B)
    sqrmag = np.zeros(B)
    fftout = np.fft.fft(win*samples)
    sqrmag[0] = fftout[0].real**2
    for l in range(1, W):
        sqrmag[l] = fftout[l].real**2 + fftout[l].imag**2
        sqrmag[B-l] = sqrmag[l]
    sqrmag[W] = fftout[W].real**2
    fftout = np.fft.fft(sqrmag)
    sqrsum = 2.*sqrmag[:W + 1].sum()
    yin[0] = 0
    yin[1:] = sqrsum - fftout.real[1:W]
    return yin / B

def cumdiff(yin):
    """ compute the cumulative mean normalized difference """
    W = len(yin)
    yin[0] = 1.
    cumsum = 0.
    for tau in range(1, W):
        cumsum += yin[tau]
        if cumsum != 0:
            yin[tau] *= tau/cumsum
        else:
            yin[tau] = 1
    return yin

def compute_all(x):
    import time
    now = time.time()

    yin     = sqd_yin(x)
    t1 = time.time()
    print ("yin took %.2fms" % ((t1-now) * 1000.))

    yinfast = sqd_yinfast(x)
    t2 = time.time()
    print ("yinfast took: %.2fms" % ((t2-t1) * 1000.))

    yintapered = sqd_yintapered(x)
    t3 = time.time()
    print ("yintapered took: %.2fms" % ((t3-t2) * 1000.))

    yinfft  = sqd_yinfft(x)
    t4 = time.time()
    print ("yinfft took: %.2fms" % ((t4-t3) * 1000.))

    return yin, yinfast, yintapered, yinfft

def plot_all(yin, yinfast, yintapered, yinfft):
    fig, axes = plt.subplots(nrows=2, ncols=2, sharex=True, sharey='col')

    axes[0, 0].plot(yin, label='yin')
    axes[0, 0].plot(yintapered, label='yintapered')
    axes[0, 0].set_ylim(bottom=0)
    axes[0, 0].legend()
    axes[1, 0].plot(yinfast, '-', label='yinfast')
    axes[1, 0].plot(yinfft, label='yinfft')
    axes[1, 0].legend()

    axes[0, 1].plot(cumdiff(yin), label='yin')
    axes[0, 1].plot(cumdiff(yintapered), label='yin tapered')
    axes[0, 1].set_ylim(bottom=0)
    axes[0, 1].legend()
    axes[1, 1].plot(cumdiff(yinfast), '-', label='yinfast')
    axes[1, 1].plot(cumdiff(yinfft), label='yinfft')
    axes[1, 1].legend()

    fig.tight_layout()

testfreqs = [441., 800., 10000., 40.]

if len(sys.argv) > 1:
    testfreqs = map(float,sys.argv[1:])

for f in testfreqs:
    print ("Comparing yin implementations for sine wave at %.fHz" % f)
    samplerate = 44100.
    win_s = 4096

    x = np.cos(2.*np.pi * np.arange(win_s) * f / samplerate)

    n_times = 1#00
    for n in range(n_times):
        yin, yinfast, yinfftslow, yinfft = compute_all(x)
    if 0: # plot difference
        plt.plot(yin-yinfast)
        plt.tight_layout()
        plt.show()
    if 1:
        plt.plot(yinfftslow-yinfft)
        plt.tight_layout()
        plt.show()
    plot_all(yin, yinfast, yinfftslow, yinfft)
plt.show()
