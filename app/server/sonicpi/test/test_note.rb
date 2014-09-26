#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require 'test/unit'
require_relative "../lib/sonicpi/note"

module SonicPi
  class NoteTester < Test::Unit::TestCase

    def test_resolution_of_nil
      assert_equal(nil, Note.resolve_midi_note(nil))
    end

    def test_resolution_of_numbers
      assert_equal(60, Note.resolve_midi_note(60))
      assert_equal(60.2, Note.resolve_midi_note(60.2))
    end

    def test_resolution_of_symbols
      assert_equal(12, Note.resolve_midi_note(:C, 0))
      assert_equal(0, Note.resolve_midi_note(:C, -1))
      assert_equal(60, Note.resolve_midi_note(:C, 4))
      assert_equal(60, Note.resolve_midi_note(:c, 4))
      assert_equal(61, Note.resolve_midi_note(:Cs, 4))
      assert_equal(61, Note.resolve_midi_note(:cs, 4))
      assert_equal(60, Note.resolve_midi_note(:C4))
      assert_equal(61, Note.resolve_midi_note(:Cs4))
      assert_equal(61, Note.resolve_midi_note(:cs4))
      assert_equal(60, Note.resolve_midi_note(:C4, 4))
      assert_equal(60, Note.resolve_midi_note(:C6, 4))
    end

    def test_resolution_of_strings
      assert_equal(12, Note.resolve_midi_note("C", 0))
      assert_equal(0, Note.resolve_midi_note("C", -1))
      assert_equal(60, Note.resolve_midi_note("C", 4))
      assert_equal(60, Note.resolve_midi_note("c", 4))
      assert_equal(61, Note.resolve_midi_note("Cs", 4))
      assert_equal(61, Note.resolve_midi_note("cs", 4))
      assert_equal(61, Note.resolve_midi_note("Cs4"))
      assert_equal(61, Note.resolve_midi_note("cs4"))
      assert_equal(60, Note.resolve_midi_note("C4"))
      assert_equal(60, Note.resolve_midi_note("C4", 4))
      assert_equal(60, Note.resolve_midi_note("C6", 4))
    end

    def test_resolution_of_name
      assert_equal(:C, Note.resolve_note_name(60))
      assert_equal(:C, Note.resolve_note_name(:C4))
      assert_equal(:C, Note.resolve_note_name(:C))
      assert_equal(:C, Note.resolve_note_name(:C, 4))
      assert_equal(:C, Note.resolve_note_name(:C4, 4))
    end

    def test_init_c4
      n = Note.new(:C4)
      assert_equal(4, n.octave)
      assert_equal(:C, n.pitch_class)
      assert_equal(0, n.interval)
      assert_equal(60, n.midi_note)
    end

    def test_init_c_4
      n = Note.new(:C, 4)
      assert_equal(4, n.octave)
      assert_equal(:C, n.pitch_class)
      assert_equal(0, n.interval)
      assert_equal(60, n.midi_note)
    end

    def test_init_Eb3
      n = Note.new(:Eb3)
      assert_equal(3, n.octave)
      assert_equal(:Eb, n.pitch_class)
      assert_equal(3, n.interval)
      assert_equal(51, n.midi_note)
    end

    def test_init_EB3
      n = Note.new(:EB3)
      assert_equal(3, n.octave)
      assert_equal(:Eb, n.pitch_class)
      assert_equal(3, n.interval)
      assert_equal(51, n.midi_note)
    end

    def test_init_EF3
      n = Note.new(:EF3)
      assert_equal(3, n.octave)
      assert_equal(:Eb, n.pitch_class)
      assert_equal(3, n.interval)
      assert_equal(51, n.midi_note)
    end

    def test_init_Fs_7
      n = Note.new(:Fs, 7)
      assert_equal(7, n.octave)
      assert_equal(:Fs, n.pitch_class)
      assert_equal(6, n.interval)
      assert_equal(102, n.midi_note)
    end

    def test_init_Fs3_7
      # The 3 in Fs3 should be overridden by
      # the explicit octave value 7
      n = Note.new(:Fs3, 7)
      assert_equal(7, n.octave)
      assert_equal(:Fs, n.pitch_class)
      assert_equal(6, n.interval)
      assert_equal(102, n.midi_note)
    end

    def test_init_error_sam
      assert_raise Note::InvalidNoteError do
        Note.new(:sam)
      end
    end

    def test_init_error_KF_4
      assert_raise Note::InvalidNoteError do
        Note.new(:KF, 4)
      end
    end

    def test_init_error_Ebb2
      assert_raise Note::InvalidNoteError do
        Note.new(:Ebb2!)
      end
    end

    def test_init_invalid_octave
      assert_raise Note::InvalidOctaveError do
        Note.new(:Eb, :foo)
      end

      assert_raise Note::InvalidOctaveError do
        Note.new(:Eb, 3.5)
      end

      assert_raise Note::InvalidOctaveError do
        Note.new(:Eb, 3.0)
      end
    end

    def test_c_flat_is_octave_lower
      cb = Note.new(:Cb4)
      assert_equal(4, cb.octave)
      assert_equal(:Cb, cb.pitch_class)
      assert_equal(-1, cb.interval)
      assert_equal(59, cb.midi_note)
    end

  end


end
