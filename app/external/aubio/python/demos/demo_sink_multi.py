#! /usr/bin/env python

import sys
from aubio import source, sink

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('usage: %s <inputfile> <outputfile> [samplerate] [hop_size]' % sys.argv[0])
        sys.exit(1)

    if len(sys.argv) > 3: samplerate = int(sys.argv[3])
    else: samplerate = 0
    if len(sys.argv) > 4: hop_size = int(sys.argv[4])
    else: hop_size = 256

    f = source(sys.argv[1], samplerate, hop_size)
    if samplerate == 0: samplerate = f.samplerate
    g = sink(sys.argv[2], samplerate, f.channels)

    total_frames, read = 0, hop_size
    while read:
        vec, read = f.do_multi()
        g.do_multi(vec, read)
        total_frames += read
    outstr = "wrote %.2fs" % (total_frames / float(samplerate))
    outstr += " (%d frames in" % total_frames
    outstr += " %d blocks" % (total_frames // f.hop_size)
    outstr += " of %d channels" % f.channels
    outstr += " at %dHz)" % f.samplerate
    outstr += " from " + f.uri
    outstr += " to " + g.uri
    print(outstr)
