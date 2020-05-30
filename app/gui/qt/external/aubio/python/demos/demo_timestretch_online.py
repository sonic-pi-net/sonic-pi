#! /usr/bin/env python

# Implementation of the timescale algorithm according to Dan Ellis, *A Phase
# Vocoder in Matlab*.  http://www.ee.columbia.edu/~dpwe/resources/matlab/pvoc/

# This file performs both analysis and synthesis in a single pass. See also
# `demo_timestretch.py` for a version following the original implementation.

import sys
from aubio import source, sink, pvoc, cvec
from aubio import unwrap2pi, float_type
import numpy as np

win_s = 512
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

sink_out = sink(output_filename, samplerate)

# excepted phase advance in each bin
phi_advance = np.linspace(0, np.pi * hop_s, win_s / 2 + 1).astype (float_type)

old_grain = cvec(win_s)
new_grain = cvec(win_s)

block_read = 0
interp_read = 0
interp_block = 0
while True:

    samples, read = source_in()
    cur_grain = p(samples)

    if block_read == 1:
        phas_acc = old_grain.phas

    #print "block_read", block_read
    while True and (block_read > 0):
        if interp_read >= block_read:
            break
        #print "`--- interp_block:", interp_block,
        #print 'at orig_block', interp_read, '<- from', block_read - 1, block_read,
        #print 'old_grain', old_grain, 'cur_grain', cur_grain
        # time to compute interp grain
        frac = 1. - np.mod(interp_read, 1.0)

        # compute interpolated frame
        new_grain.norm = frac * old_grain.norm + (1. - frac) * cur_grain.norm
        new_grain.phas = phas_acc

        # psola
        samples = p.rdo(new_grain)
        if interp_read > warmup: # skip the first frames to warm up phase vocoder
            # write to sink
            sink_out(samples, hop_s)

        # calculate phase advance
        dphas = cur_grain.phas - old_grain.phas - phi_advance
        # unwrap angle to [-pi; pi]
        dphas = unwrap2pi(dphas)
        # cumulate phase, to be used for next frame
        phas_acc += phi_advance + dphas

        # prepare for next interp block
        interp_block += 1
        interp_read = interp_block * rate
        if interp_read >= block_read:
            break

    # copy cur_grain to old_grain
    old_grain.norm = np.copy(cur_grain.norm)
    old_grain.phas = np.copy(cur_grain.phas)

    # until end of file
    if read < hop_s: break
    # increment block counter
    block_read += 1

for t in range(warmup + 2): # purge the last frames from the phase vocoder
    new_grain.norm[:] = 0
    new_grain.phas[:] = 0
    samples = p.rdo(new_grain)
    sink_out(samples, read if t == warmup + 1 else hop_s)

# just to make sure
source_in.close()
sink_out.close()

format_out = "read {:d} blocks from {:s} at {:d}Hz and rate {:f}, wrote {:d} blocks to {:s}"
print (format_out.format(block_read, source_filename, samplerate, rate,
    interp_block, output_filename))
