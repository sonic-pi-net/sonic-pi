#! /usr/bin/env python

def play_source(source_path):
    """Play an audio file using pysoundcard."""

    from aubio import source
    from pysoundcard import Stream
    
    hop_size = 256
    f = source(source_path, hop_size = hop_size)
    samplerate = f.samplerate

    s = Stream(samplerate = samplerate, blocksize = hop_size)
    s.start()
    read = 0
    while 1:
        vec, read = f()
        s.write(vec)
        if read < hop_size: break
    s.stop()

if __name__ == '__main__':
    import sys
    play_source(sys.argv[1])
