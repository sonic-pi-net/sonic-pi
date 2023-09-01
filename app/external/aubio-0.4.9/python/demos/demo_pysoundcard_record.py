#! /usr/bin/env python

def record_sink(sink_path):
    """Record an audio file using pysoundcard."""

    from aubio import sink
    from pysoundcard import Stream

    hop_size = 256
    duration = 5 # in seconds
    s = Stream(blocksize = hop_size, channels = 1)
    g = sink(sink_path, samplerate = int(s.samplerate))

    s.start()
    total_frames = 0
    try:
        while total_frames < duration * s.samplerate:
            vec = s.read(hop_size)
            # mix down to mono
            mono_vec = vec.sum(-1) / float(s.channels[0])
            g(mono_vec, hop_size)
            total_frames += hop_size
    except KeyboardInterrupt:
        duration = total_frames / float(s.samplerate)
        print("stopped after %.2f seconds" % duration)
    s.stop()

if __name__ == '__main__':
    import sys
    record_sink(sys.argv[1])
