#! /usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from aubio import note2midi, freq2note, note2freq, float_type
from numpy.testing import TestCase
from _tools import parametrize, assert_raises, skipTest

list_of_known_notes = (
        ( 'C-1', 0 ),
        ( 'C#-1', 1 ),
        ( 'd2', 38 ),
        ( 'C3', 48 ),
        ( 'B3', 59 ),
        ( 'B#3', 60 ),
        ( 'C♯4', 61 ),
        ( 'A4', 69 ),
        ( 'A#4', 70 ),
        ( 'A♯4', 70 ),
        ( 'A\u266f4', 70 ),
        ( 'Bb4', 70 ),
        ( 'B♭4', 70 ),
        ( 'B\u266d4', 70 ),
        ( 'G8', 115 ),
        ( 'G♯8', 116 ),
        ( 'G9', 127 ),
        ( 'A♮2', 45 ),
        )

list_of_known_notes_with_unicode_issues = (
        ('C𝄪4', 62 ),
        ('E𝄫4', 62 ),
        )

list_of_unknown_notes = (
        ( 'G\udd2a2' ),
        ( 'B\ufffd2' ),
        ( 'B\u266e\u266e2' ),
        ( 'B\u266f\u266d3' ),
        ( 'B33' ),
        ( 'C.3' ),
        ( 'A' ),
        ( '2' ),
        )

class Test_note2midi_good_values(object):

    @parametrize('note, midi', list_of_known_notes)
    def test_note2midi_known_values(self, note, midi):
        " known values are correctly converted "
        assert note2midi(note) == midi

    @parametrize('note, midi', list_of_known_notes_with_unicode_issues)
    def test_note2midi_known_values_with_unicode_issues(self, note, midi):
        " difficult values are correctly converted unless expected failure "
        try:
            assert note2midi(note) == midi
        except UnicodeEncodeError as e:
            # platforms with decoding failures include:
            # - osx: python <= 2.7.10
            # - win: python <= 2.7.12
            import sys
            strmsg = "len(u'\\U0001D12A') != 1, expected decoding failure"
            strmsg += " | upgrade to Python 3 to fix"
            strmsg += " | {:s} | {:s} {:s}"
            if len('\U0001D12A') != 1 and sys.version[0] == '2':
                skipTest(strmsg.format(repr(e), sys.platform, sys.version))
            else:
                raise

class note2midi_wrong_values(TestCase):

    def test_note2midi_missing_octave(self):
        " fails when passed only one character"
        self.assertRaises(ValueError, note2midi, 'C')

    def test_note2midi_wrong_modifier(self):
        " fails when passed a note with an invalid modifier "
        self.assertRaises(ValueError, note2midi, 'C.1')

    def test_note2midi_another_wrong_modifier_again(self):
        " fails when passed a note with a invalid note name "
        self.assertRaises(ValueError, note2midi, 'CB-3')

    def test_note2midi_wrong_octave(self):
        " fails when passed a wrong octave number "
        self.assertRaises(ValueError, note2midi, 'CBc')

    def test_note2midi_out_of_range(self):
        " fails when passed a note out of range"
        self.assertRaises(ValueError, note2midi, 'A9')

    def test_note2midi_wrong_note_name(self):
        " fails when passed a note with a wrong name"
        self.assertRaises(ValueError, note2midi, 'W9')

    def test_note2midi_low_octave(self):
        " fails when passed a note with a too low octave"
        self.assertRaises(ValueError, note2midi, 'C-9')

    def test_note2midi_wrong_data_type(self):
        " fails when passed a non-string value "
        self.assertRaises(TypeError, note2midi, 123)

    def test_note2midi_wrong_data_too_long(self):
        " fails when passed a note with a note name longer than expected"
        self.assertRaises(ValueError, note2midi, 'CB+-3')

class Test_note2midi_unknown_values(object):

    @parametrize('note', list_of_unknown_notes)
    def test_note2midi_unknown_values(self, note):
        " unknown values throw out an error "
        assert_raises(ValueError, note2midi, note)

class freq2note_simple_test(TestCase):

    def test_freq2note_above(self):
        " make sure freq2note(441) == A4 "
        self.assertEqual("A4", freq2note(441))

    def test_freq2note_under(self):
        " make sure freq2note(439) == A4 "
        self.assertEqual("A4", freq2note(439))

class note2freq_simple_test(TestCase):

    def test_note2freq(self):
        " make sure note2freq('A3') == 220"
        self.assertEqual(220, note2freq("A3"))

    def test_note2freq_under(self):
        " make sure note2freq(A4) == 440"
        if float_type == 'float32':
            self.assertEqual(440, note2freq("A4"))
        else:
            self.assertLess(abs(note2freq("A4")-440), 1.e-12)

if __name__ == '__main__':
    from _tools import run_module_suite
    run_module_suite()
