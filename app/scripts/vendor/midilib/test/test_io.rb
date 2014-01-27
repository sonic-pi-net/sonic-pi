# Start looking for MIDI classes in the directory above this one.
# This forces us to use the local copy of MIDI, even if there is
# a previously installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')
# Add current directory so we can find event_equality
$LOAD_PATH[0, 0] = File.dirname(__FILE__)

require 'test/unit'
require 'midilib'
require 'event_equality'

class IOTester < Test::Unit::TestCase

  SEQ_TEST_FILE = File.join(File.dirname(__FILE__), 'test.mid')
  OUTPUT_FILE = 'testout.mid'

  def compare_tracks(t0, t1)
    assert_equal(t0.name, t1.name, 'track names differ')
    assert_equal(t0.events.length, t1.events.length,
                 'number of track events differ')
    t0.each_with_index { |ev0, i| assert_equal(ev0, t1.events[i], 'events differ') }
    assert_equal(t0.instrument, t1.instrument)
  end

  def compare_sequences(s0, s1)
    assert_equal(s0.name, s1.name, 'sequence names differ')
    assert_equal(s0.tracks.length, s1.tracks.length,
                 'number of tracks differ')
    s0.each_with_index { |track0, i| compare_tracks(track0, s1.tracks[i]) }
  end

  def test_read_and_write
    seq0 = MIDI::Sequence.new()
    File.open(SEQ_TEST_FILE, 'rb') { |f| seq0.read(f) }
    File.open(OUTPUT_FILE, 'wb') { |f| seq0.write(f) }
    seq1 = MIDI::Sequence.new()
    File.open(OUTPUT_FILE, 'rb') { |f| seq1.read(f) }
    compare_sequences(seq0, seq1)
  ensure
    File.delete(OUTPUT_FILE) if File.exist?(OUTPUT_FILE)
  end

  def test_read_strings
    seq = MIDI::Sequence.new
    File.open(SEQ_TEST_FILE, 'rb') { |f| seq.read(f) }
    assert_equal('Sequence Name', seq.tracks[0].name)
    assert_equal(MIDI::GM_PATCH_NAMES[0], seq.tracks[1].instrument)
  end

end
