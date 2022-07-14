#! /usr/bin/env python

import sys
from aubio import source, pvoc, filterbank
from numpy import vstack, zeros

win_s = 512                 # fft size
hop_s = win_s // 4          # hop size

if len(sys.argv) < 2:
    print("Usage: %s <filename> [samplerate]" % sys.argv[0])
    sys.exit(1)

filename = sys.argv[1]

samplerate = 0
if len( sys.argv ) > 2: samplerate = int(sys.argv[2])

s = source(filename, samplerate, hop_s)
samplerate = s.samplerate

pv = pvoc(win_s, hop_s)

f = filterbank(40, win_s)
f.set_mel_coeffs_slaney(samplerate)

energies = zeros((40,))
o = {}

total_frames = 0
downsample = 2

while True:
    samples, read = s()
    fftgrain = pv(samples)
    new_energies = f(fftgrain)
    timestr = '%f' % (total_frames / float(samplerate) )
    print('{:s} {:s}'.format(timestr, ' '.join(['%f' % b for b in new_energies])))
    energies = vstack( [energies, new_energies] )
    total_frames += read
    if read < hop_s: break

if 1:
    print("done computing, now plotting")
    import matplotlib.pyplot as plt
    from demo_waveform_plot import get_waveform_plot
    from demo_waveform_plot import set_xlabels_sample2time
    fig = plt.figure()
    plt.rc('lines',linewidth='.8')
    wave = plt.axes([0.1, 0.75, 0.8, 0.19])
    get_waveform_plot(filename, samplerate, block_size = hop_s, ax = wave )
    wave.yaxis.set_visible(False)
    wave.xaxis.set_visible(False)

    n_plots = len(energies.T)
    all_desc_times = [ x * hop_s  for x in range(len(energies)) ]
    for i, band in enumerate(energies.T):
        ax = plt.axes ( [0.1, 0.75 - ((i+1) * 0.65 / n_plots),  0.8, 0.65 / n_plots], sharex = wave )
        ax.plot(all_desc_times, band, '-', label = 'band %d' % i)
        #ax.set_ylabel(method, rotation = 0)
        ax.xaxis.set_visible(False)
        ax.yaxis.set_visible(False)
        ax.axis(xmax = all_desc_times[-1], xmin = all_desc_times[0])
        ax.annotate('band %d' % i, xy=(-10, 0),  xycoords='axes points',
                horizontalalignment='right', verticalalignment='bottom',
                size = 'xx-small',
                )
    set_xlabels_sample2time( ax, all_desc_times[-1], samplerate) 
    #plt.ylabel('spectral descriptor value')
    ax.xaxis.set_visible(True)
    plt.show()
