#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys, os
import numpy as np
from aubio import fvec, sink, float_type

if __name__ == '__main__':
    if len(sys.argv) < 1:
        print('usage: %s' % sys.argv[0])
        sys.exit(1)

    samplerate = 44100
    hop_size = 256

    # create python/tests/sounds if needed
    output_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    output_dir = os.path.join(output_dir, 'tests', 'sounds')
    if not os.path.isdir(output_dir):
        os.makedirs(output_dir)

    filenames = ['44100Hz_1f_silence.wav',
                 '22050Hz_5s_brownnoise.wav',
                 '32000Hz_127f_sine440.wav',
                ]
    samplerates = [44100, 22050, 32000]
    durations = [1, 5*22050, 127]

    for fname, samplerate, duration in zip(filenames, samplerates, durations):
        output_name = os.path.join(output_dir, fname)
        g = sink(output_name, samplerate)
        total_frames = 0
        while total_frames < duration:
            write = min(hop_size, duration - total_frames)
            if 'brownnoise' in fname:
                vec = np.random.rand(write).astype(float_type) * 2. - 1.
            elif 'sine' in fname:
                freq = 440
                t = np.arange(write).astype(float_type) + total_frames
                vec = np.sin(2. * np.pi * freq * t / float(samplerate))
            else:
                # silence
                vec = fvec(write)
            g(vec, write)
            total_frames += write
        outstr = "wrote {:2f}s".format(total_frames / float(samplerate))
        outstr += " ({:d} frames".format(total_frames)
        outstr += " at {:d}Hz)".format(g.samplerate)
        outstr += " to {:s}".format(g.uri)
        print(outstr)
