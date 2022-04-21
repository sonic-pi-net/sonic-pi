#! /usr/bin/env python

import sys
import os.path
import aubio


def apply_filter(path, target):
    # open input file, get its samplerate
    s = aubio.source(path)
    samplerate = s.samplerate

    # create an A-weighting filter
    f = aubio.digital_filter(7)
    f.set_a_weighting(samplerate)

    # create output file
    o = aubio.sink(target, samplerate)

    total_frames = 0
    while True:
        # read from source
        samples, read = s()
        # filter samples
        filtered_samples = f(samples)
        # write to sink
        o(filtered_samples, read)
        # count frames read
        total_frames += read
        # end of file reached
        if read < s.hop_size:
            break

    # print some info
    duration = total_frames / float(samplerate)
    input_str = "input: {:s} ({:.2f} s, {:d} Hz)"
    output_str = "output: {:s}, A-weighting filtered ({:d} frames total)"
    print(input_str.format(s.uri, duration, samplerate))
    print(output_str.format(o.uri, total_frames))

if __name__ == '__main__':
    usage = "{:s} <input_file> [output_file]".format(sys.argv[0])
    if not 1 < len(sys.argv) < 4:
        print(usage)
        sys.exit(1)
    if len(sys.argv) < 3:
        input_path = sys.argv[1]
        basename = os.path.splitext(os.path.basename(input_path))[0] + ".wav"
        output_path = "filtered_" + basename
    else:
        input_path, output_path = sys.argv[1:]
    # run function
    apply_filter(input_path, output_path)
