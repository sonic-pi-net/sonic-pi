# Start looking for MIDI classes in the directory above this one.
# This forces us to use the local copy of MIDI, even if there is
# a previously installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'test/unit'
require 'midilib'

class EventTester < Test::Unit::TestCase

  def test_note_on
    e = MIDI::NoteOn.new
    assert_equal(MIDI::NOTE_ON, e.status)
    assert_equal(0, e.channel)
    assert_equal(64, e.note)
    assert_equal(64, e.velocity)
    assert_equal(0, e.delta_time)
    assert_equal(0, e.time_from_start)
  end

  def test_to_s
    e = MIDI::NoteOn.new
    assert_equal("0: ch 00 on 40 40", e.to_s)
    e.print_decimal_numbers = true
    assert_equal("0: ch 0 on 64 64", e.to_s)
    e.print_note_names = true
    assert_equal("0: ch 0 on E4 64", e.to_s)
    e.print_decimal_numbers = false
    assert_equal("0: ch 00 on E4 40", e.to_s)
  end

  def test_pitch_bend
    e = MIDI::PitchBend.new(0, 128)
    b = e.data_as_bytes
    assert_equal(0, b[1])	# lsb, 7 bits
    assert_equal(1, b[2])	# msb, 7 bits

    e.value = (3 << 7) + 42
    b = e.data_as_bytes
    assert_equal(42, b[1])	# lsb, 7 bits
    assert_equal(3, b[2])	# msb, 7 bits
  end

  def test_quantize_1
    e = MIDI::NoteOn.new
    e.quantize_to(4)
    assert_equal(0, e.time_from_start)

    # Each value in this array is the expected quantized value of
    # its index in the array.

    # Test with quantize_to(4)
    [0, 0, 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12, 16].each_with_index do |after, before|
      e.time_from_start = before
      e.quantize_to(4)
      assert_equal(after, e.time_from_start)
    end

    # Test with quantize_to(6)
    [0, 0, 0, 6, 6, 6, 6, 6, 6, 12, 12, 12, 12, 12, 12,
      18, 18, 18, 18, 18, 18, 24].each_with_index do |after, before|
      e.time_from_start = before
      e.quantize_to(6)
      assert_equal(after, e.time_from_start)
    end
  end

  def test_quantize_2
    e = MIDI::NoteOn.new(0, 64, 64, 0)
    e.quantize_to(80)
    assert_equal(0, e.time_from_start)

    e.time_from_start = 1
    e.quantize_to(80)
    assert_equal(0, e.time_from_start)

    e.time_from_start = 70
    e.quantize_to(80)
    assert_equal(80, e.time_from_start)

    e.time_from_start = 100
    e.quantize_to(80)
    assert_equal(80, e.time_from_start)

    e.time_from_start = 398
    e.quantize_to(80)
    assert_equal(400, e.time_from_start)

    e.time_from_start = 405
    e.quantize_to(80)
    assert_equal(400, e.time_from_start)

    e.time_from_start = 439
    e.quantize_to(80)
    assert_equal(400, e.time_from_start)

    e.time_from_start = 440
    e.quantize_to(80)
    assert_equal(480, e.time_from_start)

    e.time_from_start = 441
    e.quantize_to(80)
    assert_equal(480, e.time_from_start)
  end

  def test_meta_strings
    e = MIDI::MetaEvent.new(MIDI::META_TEXT, [97, 98, 99])
    assert_equal([97, 98, 99], e.data)
    assert_equal('abc', e.data_as_str)

    assert_equal([MIDI::META_EVENT, MIDI::META_TEXT, 3, 97, 98, 99], e.data_as_bytes)
  end

  def test_meta_event_string_in_ctor
    e = MIDI::MetaEvent.new(MIDI::META_TEXT, 'abc')
    assert_equal([97, 98, 99], e.data)
    assert_equal('abc', e.data_as_str)
    assert_equal([MIDI::META_EVENT, MIDI::META_TEXT, 3, 97, 98, 99], e.data_as_bytes)
  end

  def test_meta_event_data_assignment
    foobar_as_array = [102, 111, 111, 98, 97, 114]

    e = MIDI::MetaEvent.new(MIDI::META_TEXT, [97, 98, 99])
    e.data = 'foobar'
    assert_equal('foobar', e.data_as_str)
    assert_equal(foobar_as_array, e.data)

    e.data = nil
    e.data = foobar_as_array
    assert_equal('foobar', e.data_as_str)
    assert_equal(foobar_as_array, e.data)
  end

end
