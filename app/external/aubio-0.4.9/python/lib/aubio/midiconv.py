# -*- coding: utf-8 -*-
""" utilities to convert midi note number to and from note names """

import sys
from ._aubio import freqtomidi, miditofreq

__all__ = ['note2midi', 'midi2note', 'freq2note', 'note2freq']

py3 = sys.version_info[0] == 3
if py3:
    str_instances = str
    int_instances = int
else:
    str_instances = (str, unicode)
    int_instances = (int, long)


def note2midi(note):
    """Convert note name to midi note number.

    Input string `note` should be composed of one note root
    and one octave, with optionally one modifier in between.

    List of valid components:

    - note roots: `C`, `D`, `E`, `F`, `G`, `A`, `B`,
    - modifiers: `b`, `#`, as well as unicode characters
      `𝄫`, `♭`, `♮`, `♯` and `𝄪`,
    - octave numbers: `-1` -> `11`.

    Parameters
    ----------
    note : str
        note name

    Returns
    -------
    int
        corresponding midi note number

    Examples
    --------
    >>> aubio.note2midi('C#4')
    61
    >>> aubio.note2midi('B♭5')
    82

    Raises
    ------
    TypeError
        If `note` was not a string.
    ValueError
        If an error was found while converting `note`.

    See Also
    --------
    midi2note, freqtomidi, miditofreq
    """
    _valid_notenames = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7,
                        'A': 9, 'B': 11}
    _valid_modifiers = {
            u'𝄫': -2,                         # double flat
            u'♭': -1, 'b': -1, '\u266d': -1,  # simple flat
            u'♮': 0, '\u266e': 0, None: 0,    # natural
            '#': +1, u'♯': +1, '\u266f': +1,  # sharp
            u'𝄪': +2,                         # double sharp
            }
    _valid_octaves = range(-1, 10)
    if not isinstance(note, str_instances):
        msg = "a string is required, got {:s} ({:s})"
        raise TypeError(msg.format(str(type(note)), repr(note)))
    if len(note) not in range(2, 5):
        msg = "string of 2 to 4 characters expected, got {:d} ({:s})"
        raise ValueError(msg.format(len(note), note))
    notename, modifier, octave = [None] * 3

    if len(note) == 4:
        notename, modifier, octave_sign, octave = note
        octave = octave_sign + octave
    elif len(note) == 3:
        notename, modifier, octave = note
        if modifier == '-':
            octave = modifier + octave
            modifier = None
    else:
        notename, octave = note

    notename = notename.upper()
    octave = int(octave)

    if notename not in _valid_notenames:
        raise ValueError("%s is not a valid note name" % notename)
    if modifier not in _valid_modifiers:
        raise ValueError("%s is not a valid modifier" % modifier)
    if octave not in _valid_octaves:
        raise ValueError("%s is not a valid octave" % octave)

    midi = (octave + 1) * 12 + _valid_notenames[notename] \
                             + _valid_modifiers[modifier]
    if midi > 127:
        raise ValueError("%s is outside of the range C-2 to G8" % note)
    return midi


def midi2note(midi):
    """Convert midi note number to note name.

    Parameters
    ----------
    midi : int [0, 128]
        input midi note number

    Returns
    -------
    str
        note name

    Examples
    --------
    >>> aubio.midi2note(70)
    'A#4'
    >>> aubio.midi2note(59)
    'B3'

    Raises
    ------
    TypeError
        If `midi` was not an integer.
    ValueError
        If `midi` is out of the range `[0, 128]`.

    See Also
    --------
    note2midi, miditofreq, freqtomidi
    """
    if not isinstance(midi, int_instances):
        raise TypeError("an integer is required, got %s" % midi)
    if midi not in range(0, 128):
        msg = "an integer between 0 and 127 is excepted, got {:d}"
        raise ValueError(msg.format(midi))
    _valid_notenames = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#',
                        'A', 'A#', 'B']
    return _valid_notenames[midi % 12] + str(int(midi / 12) - 1)


def freq2note(freq):
    """Convert frequency in Hz to nearest note name.

    Parameters
    ----------
    freq : float [0, 23000[
        input frequency, in Hz

    Returns
    -------
    str
        name of the nearest note

    Example
    -------
    >>> aubio.freq2note(440)
    'A4'
    >>> aubio.freq2note(220.1)
    'A3'
    """
    nearest_note = int(freqtomidi(freq) + .5)
    return midi2note(nearest_note)


def note2freq(note):
    """Convert note name to corresponding frequency, in Hz.

    Parameters
    ----------
    note : str
        input note name

    Returns
    -------
    freq : float [0, 23000[
        frequency, in Hz

    Example
    -------
    >>> aubio.note2freq('A4')
    440
    >>> aubio.note2freq('A3')
    220.1
    """
    midi = note2midi(note)
    return miditofreq(midi)
