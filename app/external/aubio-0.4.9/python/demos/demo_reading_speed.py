#! /usr/bin/env python
# -*- coding: utf-8 -*-

"""

Compare the speed of several methods for reading and loading a sound file.

Optionally, this file can make use of the following packages:

    - audioread     https://github.com/beetbox/audioread
    - scipy         https://scipy.org
    - librosa       https://github.com/bmcfee/librosa
    - pydub         https://github.com/jiaaro/pydub

Uncomment the function names below and send us your speed results!

"""


test_functions = [
            "read_file_aubio",
            "load_file_aubio",
            #"load_file_scipy",
            #"load_file_scipy_mmap",
            #"read_file_audioread",
            #"load_file_librosa",
            #"read_file_pydub",
            #"load_file_pydub",
            ]


import numpy as np

def read_file_audioread(filename):
    import audioread
    # taken from librosa.util.utils
    def convert_buffer_to_float(buf, n_bytes = 2, dtype = np.float32):
        # Invert the scale of the data
        scale = 1./float(1 << ((8 * n_bytes) - 1))
        # Construct the format string
        fmt = '<i{:d}'.format(n_bytes)
        # Rescale and format the data buffer
        out = scale * np.frombuffer(buf, fmt).astype(dtype)
        return out

    with audioread.audio_open(filename) as f:
        total_frames = 0
        for buf in f:
            samples = convert_buffer_to_float(buf)
            samples = samples.reshape(f.channels, -1)
            total_frames += samples.shape[1]
        return total_frames, f.samplerate

def load_file_librosa(filename):
    import librosa
    y, sr = librosa.load(filename, sr = None)
    #print y.mean(), y.shape
    return len(y), sr

def load_file_scipy(filename):
    import scipy.io.wavfile
    sr, y = scipy.io.wavfile.read(filename)
    y = y.astype('float32') / 32767
    #print y.mean(), y.shape
    return len(y), sr

def load_file_scipy_mmap(filename):
    import scipy.io.wavfile
    sr, y = scipy.io.wavfile.read(filename, mmap = True)
    #print y.mean(), y.shape
    return len(y), sr

def read_file_pydub(filename):
    from pydub import AudioSegment
    song = AudioSegment.from_file(filename)
    song.get_array_of_samples()
    return song.frame_count(), song.frame_rate

def load_file_pydub(filename):
    from pydub import AudioSegment
    song = AudioSegment.from_file(filename)
    y = np.asarray(song.get_array_of_samples(), dtype = 'float32')
    y = y.reshape(song.channels, -1) / 32767.
    return song.frame_count(), song.frame_rate

def read_file_aubio(filename):
    import aubio
    f = aubio.source(filename, hop_size = 1024)
    total_frames = 0
    while True:
        _, read = f()
        total_frames += read
        if read < f.hop_size: break
    return total_frames, f.samplerate

def load_file_aubio(filename):
    import aubio
    f = aubio.source(filename, hop_size = 1024)
    y = np.zeros(f.duration, dtype = aubio.float_type)
    total_frames = 0
    while True:
        samples, read = f()
        y[total_frames:total_frames + read] = samples[:read]
        total_frames += read
        if read < f.hop_size: break
    assert len(y) == total_frames
    #print y.mean(), y.shape
    return total_frames, f.samplerate

def test_speed(function, filename):
    times = []
    for _ in range(10):
        start = time.time()
        try:
            total_frames, samplerate = function(filename)
        except ImportError as e:
            print ("error: failed importing {:s}".format(e))
            return
        elapsed = time.time() - start
        #print ("{:5f} ".format(elapsed)),
        times.append(elapsed)

    #print
    times = np.array(times)
    duration_min = int(total_frames/float(samplerate) // 60)
    str_format = '{:25s} took {:5f} seconds avg (±{:5f}) to run on a ~ {:d} minutes long file'
    print (str_format.format(function.__name__, times.mean(), times.std(), duration_min ))

if __name__ == '__main__':
    import sys, time
    if len(sys.argv) < 2:
        print ("not enough arguments")
        sys.exit(1)
    filename = sys.argv[1]

    for f in test_functions:
        # get actual function from globals
        test_function = globals()[f]
        test_speed(test_function, filename)
