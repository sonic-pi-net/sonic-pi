#! /usr/bin/env python

import sys
import os.path
from aubio import source, sink

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('usage: %s <inputfile> <duration>' % sys.argv[0])
        sys.exit(1)
    source_file = sys.argv[1]
    duration = float(sys.argv[2])
    source_base_name, source_ext = os.path.splitext(os.path.basename(source_file))

    hopsize = 256
    slice_n, total_frames_written, read = 0, 0, hopsize

    def new_sink_name(source_base_name, slice_n, duration = duration):
        return source_base_name + '_%02.3f' % (slice_n*duration) + '.wav'

    f = source(source_file, 0, hopsize)
    samplerate = f.samplerate
    g = sink(new_sink_name(source_base_name, slice_n), samplerate)

    #print "new slice:", slice_n, 0, "+", 0, "=", 0
    while read == hopsize:
        vec, read = f()
        start_of_next_region = int(duration * samplerate * (slice_n + 1))
        remaining = start_of_next_region - total_frames_written
        # number of samples remaining is less than what we got
        if remaining <= read:
            # write remaining samples from current region
            g(vec[0:remaining], remaining)
            # close this file
            del g
            #print "new slice", slice_n, total_frames_written, "+", remaining, "=", start_of_next_region
            slice_n += 1
            # create a new file for the new region
            g = sink(new_sink_name(source_base_name, slice_n), samplerate)
            # write the remaining samples in the new file
            g(vec[remaining:read], read - remaining)
        else:
            g(vec[0:read], read)
        total_frames_written += read
    total_duration = total_frames_written / float(samplerate)
    slice_n += 1
    outstr = 'created %(slice_n)s slices from %(source_base_name)s%(source_ext)s' % locals()
    outstr += ' (total duration %(total_duration).2fs)' % locals()
    print(outstr)
    # close source and sink files
    del f, g
