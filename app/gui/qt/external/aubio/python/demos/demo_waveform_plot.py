#! /usr/bin/env python

import sys
from aubio import source
from numpy import zeros, hstack

def get_waveform_plot(filename, samplerate = 0, block_size = 4096, ax = None, downsample = 2**4):
    import matplotlib.pyplot as plt
    if not ax:
        fig = plt.figure()
        ax = fig.add_subplot(111)
    hop_s = block_size

    allsamples_max = zeros(0,)
    downsample = downsample  # to plot n samples / hop_s

    a = source(filename, samplerate, hop_s)            # source file
    if samplerate == 0: samplerate = a.samplerate

    total_frames = 0
    while True:
        samples, read = a()
        # keep some data to plot it later
        new_maxes = (abs(samples.reshape(hop_s//downsample, downsample))).max(axis=0)
        allsamples_max = hstack([allsamples_max, new_maxes])
        total_frames += read
        if read < hop_s: break
    allsamples_max = (allsamples_max > 0) * allsamples_max
    allsamples_max_times = [ ( float (t) / downsample ) * hop_s for t in range(len(allsamples_max)) ]

    ax.plot(allsamples_max_times,  allsamples_max, '-b')
    ax.plot(allsamples_max_times, -allsamples_max, '-b')
    ax.axis(xmin = allsamples_max_times[0], xmax = allsamples_max_times[-1])

    set_xlabels_sample2time(ax, allsamples_max_times[-1], samplerate)
    return ax

def set_xlabels_sample2time(ax, latest_sample, samplerate):
    ax.axis(xmin = 0, xmax = latest_sample)
    if latest_sample / float(samplerate) > 60:
        ax.set_xlabel('time (mm:ss)')
        ax.set_xticklabels([ "%02d:%02d" % (t/float(samplerate)/60, (t/float(samplerate))%60) for t in ax.get_xticks()[:-1]], rotation = 50)
    else:
        ax.set_xlabel('time (ss.mm)')
        ax.set_xticklabels([ "%02d.%02d" % (t/float(samplerate), 100*((t/float(samplerate))%1) ) for t in ax.get_xticks()[:-1]], rotation = 50)


if __name__ == '__main__':
    import matplotlib.pyplot as plt
    if len(sys.argv) < 2:
        print("Usage: %s <filename>" % sys.argv[0])
    else:
        for soundfile in sys.argv[1:]:
            get_waveform_plot(soundfile)
            # display graph
            plt.show()
