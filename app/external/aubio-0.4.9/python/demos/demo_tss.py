#! /usr/bin/env python

import sys
from aubio import source, sink, pvoc, tss

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s <inputfile> <outputfile_transient> <outputfile_steady>' % sys.argv[0])
        sys.exit(1)

    samplerate = 44100
    win_s = 1024       # fft size
    hop_s = win_s // 8 # block size

    f = source(sys.argv[1], samplerate, hop_s)
    g = sink(sys.argv[2], samplerate)
    h = sink(sys.argv[3], samplerate)

    pva = pvoc(win_s, hop_s)    # a phase vocoder
    pvb = pvoc(win_s, hop_s)    # another phase vocoder
    t = tss(win_s, hop_s)       # transient steady state separation

    t.set_threshold(0.01)
    t.set_alpha(3.)
    t.set_beta(4.)

    read = hop_s

    while read:
        samples, read = f()               # read file
        spec = pva(samples)               # compute spectrum
        trans_spec, stead_spec = t(spec)  # transient steady-state separation
        transients = pva.rdo(trans_spec)  # overlap-add synthesis of transients
        steadstate = pvb.rdo(stead_spec)  # overlap-add synthesis of steady states
        g(transients, read)               # write transients to output
        h(steadstate, read)               # write steady states to output

    del f, g, h                           # finish writing the files now
    sys.exit(0)

    from demo_spectrogram import get_spectrogram
    from pylab import subplot, show
    subplot(311)
    get_spectrogram(sys.argv[1])
    subplot(312)
    get_spectrogram(sys.argv[2])
    subplot(313)
    get_spectrogram(sys.argv[3])
    show()
