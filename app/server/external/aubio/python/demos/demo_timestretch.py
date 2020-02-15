#! /usr/bin/env python

# Implementation of the timescale algorithm according to Dan Ellis, *A Phase
# Vocoder in Matlab*.  http://www.ee.columbia.edu/~dpwe/resources/matlab/pvoc/

# This file follows the original implementation, with analysis in a first pass,
# and synthesis in a second pass.

import sys
from aubio import source, sink, pvoc, cvec
from aubio import unwrap2pi, float_type
import numpy as np

win_s = 1024
hop_s = win_s // 8 # 87.5 % overlap

warmup = win_s // hop_s - 1

if len(sys.argv) < 3:
    print("Usage: {:s} <source_filename> <output_filename> <rate> [samplerate]".format(sys.argv[0]))
    print("""Examples:
    # twice faster
    {0} track_01.mp3 track_01_faster.wav 2.0
    # twice slower
    {0} track_02.flac track_02_slower.wav 0.5
    # one and a half time faster, resampling first the input to 22050
    {0} track_02.flac track_02_slower.wav 1.5 22050""".format(sys.argv[0]))
    sys.exit(1)

source_filename = sys.argv[1]
output_filename = sys.argv[2]
rate = float(sys.argv[3])

samplerate = 0 if len(sys.argv) < 5 else int(sys.argv[4])
source_in = source(source_filename, samplerate, hop_s)
samplerate = source_in.samplerate
p = pvoc(win_s, hop_s)

# allocate memory to store norms and phases
n_blocks = source_in.duration // hop_s + 1
# adding an empty frame at end of spectrogram
norms  = np.zeros((n_blocks + 1, win_s // 2 + 1), dtype = float_type)
phases = np.zeros((n_blocks + 1, win_s // 2 + 1), dtype = float_type)

block_read = 0
while True:
    # read from source
    samples, read = source_in()
    # compute fftgrain
    spec = p(samples)
    # store current grain
    norms[block_read] = spec.norm
    phases[block_read] = spec.phas
    # until end of file
    if read < hop_s: break
    # increment block counter
    block_read += 1

# just to make sure
#source_in.close()

sink_out = sink(output_filename, samplerate)

# interpolated time steps (j = alpha * i)
steps = np.arange(0, n_blocks, rate, dtype = float_type)
# initial phase
phas_acc = phases[0]
# excepted phase advance in each bin
phi_advance = np.linspace(0, np.pi * hop_s, win_s / 2 + 1).astype (float_type)

new_grain = cvec(win_s)

for (t, step) in enumerate(steps):

    frac = 1. - np.mod(step, 1.0)
    # get pair of frames
    t_norms = norms[int(step):int(step+2)]
    t_phases = phases[int(step):int(step+2)]

    # compute interpolated frame
    new_grain.norm = frac * t_norms[0] + (1. - frac) * t_norms[1]
    new_grain.phas = phas_acc
    #print t, step, new_grain.norm
    #print t, step, phas_acc

    # psola
    samples = p.rdo(new_grain)
    if t > warmup: # skip the first few frames to warm up phase vocoder
        # write to sink
        sink_out(samples, hop_s)

    # calculate phase advance
    dphas = t_phases[1] - t_phases[0] - phi_advance
    # unwrap angle to [-pi; pi]
    dphas = unwrap2pi(dphas)
    # cumulate phase, to be used for next frame
    phas_acc += phi_advance + dphas

for t in range(warmup + 1): # purge the last frames from the phase vocoder
    new_grain.norm[:] = 0
    new_grain.phas[:] = 0
    samples = p.rdo(new_grain)
    sink_out(samples, read if t == warmup else hop_s)

# just to make sure
#sink_out.close()

format_out = "read {:d} blocks from {:s} at {:d}Hz and rate {:f}, wrote {:d} blocks to {:s}"
print (format_out.format(block_read, source_filename, samplerate, rate,
    len(steps), output_filename))
