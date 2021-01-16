#! /usr/bin/env python

import sys
from aubio import source, sink, pvoc
from numpy import arange, exp, hstack, zeros, cos
from math import pi

def gauss(size):
    return exp(- 1.0 / (size * size) * pow(2.0* arange(size) - 1. *size, 2.));

def hanningz(size):
    return 0.5 * (1. - cos(2.*pi*arange(size) / size))

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s <inputfile> <outputfile>' % sys.argv[0])
        sys.exit(1)
    samplerate = 0 
    if len(sys.argv) > 3: samplerate = int(sys.argv[3])
    f = source(sys.argv[1], samplerate, 256)
    samplerate = f.samplerate
    g = sink(sys.argv[2], samplerate)

    win_s = 512 # fft size
    hop_s = win_s // 2 # hop size
    pv = pvoc(win_s, hop_s) # phase vocoder

    # spectral weighting vector
    spec_weight = hstack ( [
        .8 * hanningz(80)[40:],
        zeros( 50 ),
        1.3 * hanningz(100),
        zeros (win_s // 2 + 1 - 40 - 50 - 100),
        ] )

    if 0:
        from pylab import plot, show
        plot(spec_weight) 
        show()

    total_frames, read = 0, hop_s
    while read:
        # get new samples
        samples, read = f()
        # compute spectrum
        spectrum = pv(samples)
        # apply weight to spectral amplitudes
        spectrum.norm *= spec_weight
        # resynthesise modified samples
        new_samples = pv.rdo(spectrum)
        # write to output
        g(new_samples, read)
        total_frames += read

    duration = total_frames / float(samplerate)
    print("read {:.3f}s from {:s}".format(duration, f.uri))
