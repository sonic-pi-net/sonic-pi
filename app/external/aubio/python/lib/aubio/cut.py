#! /usr/bin/env python

""" this file was written by Paul Brossier
  it is released under the GNU/GPL license.
"""

import sys
from aubio.cmd import AubioArgumentParser, _cut_slice

def aubio_cut_parser():
    parser = AubioArgumentParser()
    parser.add_input()
    parser.add_argument("-O", "--onset-method",
            action="store", dest="onset_method", default='default',
            metavar = "<onset_method>",
            help="onset detection method [default=default] \
                    complexdomain|hfc|phase|specdiff|energy|kl|mkl")
    # cutting methods
    parser.add_argument("-b", "--beat",
            action="store_true", dest="beat", default=False,
            help="slice at beat locations")
    """
    parser.add_argument("-S", "--silencecut",
            action="store_true", dest="silencecut", default=False,
            help="use silence locations")
    parser.add_argument("-s", "--silence",
            metavar = "<value>",
            action="store", dest="silence", default=-70,
            help="silence threshold [default=-70]")
            """
    # algorithm parameters
    parser.add_buf_hop_size()
    parser.add_argument("-t", "--threshold", "--onset-threshold",
            metavar = "<threshold>", type=float,
            action="store", dest="threshold", default=0.3,
            help="onset peak picking threshold [default=0.3]")
    parser.add_argument("-c", "--cut",
            action="store_true", dest="cut", default=False,
            help="cut input sound file at detected labels")
    parser.add_minioi()

    """
    parser.add_argument("-D", "--delay",
            action = "store", dest = "delay", type = float,
            metavar = "<seconds>", default=0,
            help="number of seconds to take back [default=system]\
                    default system delay is 3*hopsize/samplerate")
    parser.add_argument("-C", "--dcthreshold",
            metavar = "<value>",
            action="store", dest="dcthreshold", default=1.,
            help="onset peak picking DC component [default=1.]")
    parser.add_argument("-L", "--localmin",
            action="store_true", dest="localmin", default=False,
            help="use local minima after peak detection")
    parser.add_argument("-d", "--derivate",
            action="store_true", dest="derivate", default=False,
            help="derivate onset detection function")
    parser.add_argument("-z", "--zerocross",
            metavar = "<value>",
            action="store", dest="zerothres", default=0.008,
            help="zero-crossing threshold for slicing [default=0.00008]")
    # plotting functions
    parser.add_argument("-p", "--plot",
            action="store_true", dest="plot", default=False,
            help="draw plot")
    parser.add_argument("-x", "--xsize",
            metavar = "<size>",
            action="store", dest="xsize", default=1.,
            type=float, help="define xsize for plot")
    parser.add_argument("-y", "--ysize",
            metavar = "<size>",
            action="store", dest="ysize", default=1.,
            type=float, help="define ysize for plot")
    parser.add_argument("-f", "--function",
            action="store_true", dest="func", default=False,
            help="print detection function")
    parser.add_argument("-n", "--no-onsets",
            action="store_true", dest="nplot", default=False,
            help="do not plot detected onsets")
    parser.add_argument("-O", "--outplot",
            metavar = "<output_image>",
            action="store", dest="outplot", default=None,
            help="save plot to output.{ps,png}")
    parser.add_argument("-F", "--spectrogram",
            action="store_true", dest="spectro", default=False,
            help="add spectrogram to the plot")
    """
    parser.add_slicer_options()
    parser.add_verbose_help()
    return parser


def _cut_analyze(options):
    hopsize = options.hop_size
    bufsize = options.buf_size
    samplerate = options.samplerate
    source_uri = options.source_uri

    # analyze pass
    from aubio import onset, tempo, source

    s = source(source_uri, samplerate, hopsize)
    if samplerate == 0:
        samplerate = s.samplerate
        options.samplerate = samplerate

    if options.beat:
        o = tempo(options.onset_method, bufsize, hopsize,
                samplerate=samplerate)
    else:
        o = onset(options.onset_method, bufsize, hopsize,
                samplerate=samplerate)
        if options.minioi:
            if options.minioi.endswith('ms'):
                o.set_minioi_ms(int(options.minioi[:-2]))
            elif options.minioi.endswith('s'):
                o.set_minioi_s(int(options.minioi[:-1]))
            else:
                o.set_minioi(int(options.minioi))
    o.set_threshold(options.threshold)

    timestamps = []
    total_frames = 0
    while True:
        samples, read = s()
        if o(samples):
            timestamps.append(o.get_last())
            if options.verbose:
                print("%.4f" % o.get_last_s())
        total_frames += read
        if read < hopsize:
            break
    del s
    return timestamps, total_frames

def main():
    parser = aubio_cut_parser()
    options = parser.parse_args()
    if not options.source_uri and not options.source_uri2:
        sys.stderr.write("Error: no file name given\n")
        parser.print_help()
        sys.exit(1)
    elif options.source_uri2 is not None:
        options.source_uri = options.source_uri2

    # analysis
    timestamps, total_frames = _cut_analyze(options)

    # print some info
    duration = float(total_frames) / float(options.samplerate)
    base_info = '%(source_uri)s' % {'source_uri': options.source_uri}
    base_info += ' (total %(duration).2fs at %(samplerate)dHz)\n' % \
            {'duration': duration, 'samplerate': options.samplerate}

    info = "found %d timestamps in " % len(timestamps)
    info += base_info
    sys.stderr.write(info)

    if options.cut:
        _cut_slice(options, timestamps)
        info = "created %d slices from " % len(timestamps)
        info += base_info
        sys.stderr.write(info)
