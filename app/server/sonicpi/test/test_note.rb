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

end
end
