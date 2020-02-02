#! /usr/bin/env python
# -*- coding: utf-8 -*-

from aubio import midi2note
from _tools import parametrize, assert_raises

list_of_known_midis = (
        ( 0, 'C-1' ),
        ( 1, 'C#-1' ),
        ( 38, 'D2' ),
        ( 48, 'C3' ),
        ( 59, 'B3' ),
        ( 60, 'C4' ),
        ( 127, 'G9' ),
        )

class Test_midi2note_good_values(object):

    @parametrize('midi, note', list_of_known_midis)
    def test_midi2note_known_values(self, midi, note):
        " known values are correctly converted "
        assert midi2note(midi) == (note)

class Test_midi2note_wrong_values(object):

    def test_midi2note_negative_value(self):
        " fails when passed a negative value "
        assert_raises(ValueError, midi2note, -2)

    def test_midi2note_large(self):
        " fails when passed a value greater than 127 "
        assert_raises(ValueError, midi2note, 128)

    def test_midi2note_floating_value(self):
        " fails when passed a floating point "
        assert_raises(TypeError, midi2note, 69.2)

    def test_midi2note_character_value(self):
        " fails when passed a value that can not be transformed to integer "
        assert_raises(TypeError, midi2note, "a")

if __name__ == '__main__':
    from _tools import run_module_suite
    run_module_suite()
