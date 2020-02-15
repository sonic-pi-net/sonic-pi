"""utility routines to slice sound files at given timestamps"""

import os
from aubio import source, sink

_max_timestamp = 1e120


def slice_source_at_stamps(source_file, timestamps, timestamps_end=None,
                           output_dir=None, samplerate=0, hopsize=256,
                           create_first=False):
    """Slice a sound file at given timestamps.

    This function reads `source_file` and creates slices, new smaller
    files each starting at `t` in `timestamps`, a list of integer
    corresponding to time locations in `source_file`, in samples.

    If `timestamps_end` is unspecified, the slices will end at
    `timestamps_end[n] = timestamps[n+1]-1`, or the end of file.
    Otherwise, `timestamps_end` should be a list with the same length
    as `timestamps` containing the locations of the end of each slice.

    If `output_dir` is unspecified, the new slices will be written in
    the current directory. If `output_dir` is a string, new slices
    will be written in `output_dir`, after creating the directory if
    required.

    The default `samplerate` is 0, meaning the original sampling rate
    of `source_file` will be used. When using a sampling rate
    different to the one of the original files, `timestamps` and
    `timestamps_end` should be expressed in the re-sampled signal.

    The `hopsize` parameter simply tells :class:`source` to use this
    hopsize and does not change the output slices.

    If `create_first` is True and `timestamps` does not start with `0`, the
    first slice from `0` to `timestamps[0] - 1` will be automatically added.

    Parameters
    ----------
    source_file : str
        path of the resource to slice
    timestamps : :obj:`list` of :obj:`int`
        time stamps at which to slice, in samples
    timestamps_end : :obj:`list` of :obj:`int` (optional)
        time stamps at which to end the slices
    output_dir : str (optional)
        output directory to write the slices to
    samplerate : int (optional)
        samplerate to read the file at
    hopsize : int (optional)
        number of samples read from source per iteration
    create_first : bool (optional)
        always create the slice at the start of the file

    Examples
    --------
    Create two slices: the first slice starts at the beginning of the
    input file `loop.wav` and lasts exactly one second, starting at
    sample `0` and ending at sample `44099`; the second slice starts
    at sample `44100` and lasts until the end of the input file:

    >>> aubio.slice_source_at_stamps('loop.wav', [0, 44100])

    Create one slice, from 1 second to 2 seconds:

    >>> aubio.slice_source_at_stamps('loop.wav', [44100], [44100 * 2 - 1])

    Notes
    -----
    Slices may be overlapping. If `timestamps_end` is `1` element
    shorter than `timestamps`, the last slice will end at the end of
    the file.
    """

    if not timestamps:
        raise ValueError("no timestamps given")

    if timestamps[0] != 0 and create_first:
        timestamps = [0] + timestamps
        if timestamps_end is not None:
            timestamps_end = [timestamps[1] - 1] + timestamps_end

    if timestamps_end is not None:
        if len(timestamps_end) == len(timestamps) - 1:
            timestamps_end = timestamps_end + [_max_timestamp]
        elif len(timestamps_end) != len(timestamps):
            raise ValueError("len(timestamps_end) != len(timestamps)")
    else:
        timestamps_end = [t - 1 for t in timestamps[1:]] + [_max_timestamp]

    regions = list(zip(timestamps, timestamps_end))

    source_base_name, _ = os.path.splitext(os.path.basename(source_file))
    if output_dir is not None:
        if not os.path.isdir(output_dir):
            os.makedirs(output_dir)
        source_base_name = os.path.join(output_dir, source_base_name)

    def _new_sink_name(source_base_name, timestamp, samplerate):
        # create name based on a timestamp in samples, converted in seconds
        timestamp_seconds = timestamp / float(samplerate)
        return source_base_name + "_%011.6f" % timestamp_seconds + '.wav'

    # open source file
    _source = source(source_file, samplerate, hopsize)
    samplerate = _source.samplerate

    total_frames = 0
    slices = []

    while True:
        # get hopsize new samples from source
        vec, read = _source.do_multi()
        # if the total number of frames read will exceed the next region start
        while regions and total_frames + read >= regions[0][0]:
            # get next region
            start_stamp, end_stamp = regions.pop(0)
            # create a name for the sink
            new_sink_path = _new_sink_name(source_base_name, start_stamp,
                                           samplerate)
            # create its sink
            _sink = sink(new_sink_path, samplerate, _source.channels)
            # create a dictionary containing all this
            new_slice = {'start_stamp': start_stamp, 'end_stamp': end_stamp,
                         'sink': _sink}
            # append the dictionary to the current list of slices
            slices.append(new_slice)

        for current_slice in slices:
            start_stamp = current_slice['start_stamp']
            end_stamp = current_slice['end_stamp']
            _sink = current_slice['sink']
            # sample index to start writing from new source vector
            start = max(start_stamp - total_frames, 0)
            # number of samples yet to written be until end of region
            remaining = end_stamp - total_frames + 1
            # not enough frames remaining, time to split
            if remaining < read:
                if remaining > start:
                    # write remaining samples from current region
                    _sink.do_multi(vec[:, start:remaining], remaining - start)
                    # close this file
                    _sink.close()
            elif read > start:
                # write all the samples
                _sink.do_multi(vec[:, start:read], read - start)
        total_frames += read
        # remove old slices
        slices = list(filter(lambda s: s['end_stamp'] > total_frames,
                             slices))
        if read < hopsize:
            break
