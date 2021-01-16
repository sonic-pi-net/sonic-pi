#! /usr/bin/env python

import numpy as np
import aubio

def build_sinusoid(length, freqs, samplerate):
    return np.sin( 2. * np.pi * np.arange(length) * freqs / samplerate).astype(aubio.float_type)

def run_pitch(p, input_vec):
    cands = []
    for vec_slice in input_vec.reshape((-1, p.hop_size)):
        a = p(vec_slice)[0]
        cands.append(a)
    return cands

methods = ['default', 'schmitt', 'fcomb', 'mcomb', 'yin', 'yinfft']

cands = {}
buf_size = 2048
hop_size = 512
samplerate = 44100
sin_length = (samplerate * 10) % 512 * 512
freqs = np.zeros(sin_length)

partition = sin_length // 8
pointer = 0

pointer += partition
freqs[pointer: pointer + partition] = 440

pointer += partition
pointer += partition
freqs[ pointer : pointer + partition ] = 740

pointer += partition
freqs[ pointer : pointer + partition ] = 1480

pointer += partition
pointer += partition
freqs[ pointer : pointer + partition ] = 400 + 5 * np.random.random(sin_length//8)

a = build_sinusoid(sin_length, freqs, samplerate)

for method in methods:
    p = aubio.pitch(method, buf_size, hop_size, samplerate)
    cands[method] = run_pitch(p, a)
    print(method)
    print(cands[method])

print("done computing")

if 1:
    import matplotlib.pyplot as plt

    # times
    ramp = np.arange(0, sin_length / hop_size).astype('float') * hop_size / samplerate

    # plot each result
    for method in methods:
        plt.plot(ramp, cands[method], '.-', label=method)

    # plot ground truth
    ramp = np.arange(0, sin_length).astype('float') / samplerate
    plt.plot(ramp, freqs, ':', label = 'ground truth')

    plt.legend(loc='upper left')

    plt.xlabel('time (s)')
    plt.ylabel('frequency (Hz)')
    plt.ylim([0,2000])
    plt.show()
