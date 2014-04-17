# Start looking for MIDI classes in the directory above this one.
# This forces us to use the local copy of MIDI, even if there is
# a previously installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'test/unit'
require 'midilib'

class TrackTester < Test::Unit::TestCase

  def setup
    @seq = MIDI::Sequence.new
    @track = MIDI::Track.new(@seq)
    @seq.tracks << @track
    3.times { @track.events << MIDI::NoteOn.new(0, 64, 64, 100) }
    @track.recalc_times
  end

  def test_basics
    assert_equal(3, @track.events.length)
    3.times do |i|
      assert_equal(100, @track.events[i].delta_time)
      assert_equal((i+1) * 100, @track.events[i].time_from_start)
    end
    assert_equal(MIDI::Track::UNNAMED, @track.name)
  end

  def test_append_event
    @track.events << MIDI::NoteOn.new(0, 64, 64, 100)
    @track.recalc_times
    assert_equal(4, @track.events.length)
    4.times do |i|
      assert_equal((i+1) * 100, @track.events[i].time_from_start)
    end
  end

  def test_append_list
    @track.events +=
      (1..12).collect { |i| MIDI::NoteOn.new(0, 64, 64, 3) }
    @track.recalc_times

    3.times do |i|
      assert_equal(100, @track.events[i].delta_time)
      assert_equal((i+1) * 100, @track.events[i].time_from_start)
    end
    12.times do |i|
      assert_equal(3, @track.events[3 + i].delta_time)
      assert_equal(300 + ((i+1) * 3),
                   @track.events[3 + i].time_from_start)
    end
  end

  def test_insert
    @track.events[1,0] = MIDI::NoteOn.new(0, 64, 64, 3)
    @track.recalc_times
    assert_equal(100, @track.events[0].time_from_start)
    assert_equal(103, @track.events[1].time_from_start)
    assert_equal(203, @track.events[2].time_from_start)
    assert_equal(303, @track.events[3].time_from_start)
  end

  def test_merge
    list = (1..12).collect { |i| MIDI::NoteOn.new(0, 64, 64, 10) }
    @track.merge(list)
    assert_equal(15, @track.events.length)
    assert_equal(10, @track.events[0].time_from_start)
    assert_equal(10, @track.events[0].delta_time)
    assert_equal(20, @track.events[1].time_from_start)
    assert_equal(10, @track.events[1].delta_time)
    assert_equal(30, @track.events[2].time_from_start)
    assert_equal(40, @track.events[3].time_from_start)
    assert_equal(50, @track.events[4].time_from_start)
    assert_equal(60, @track.events[5].time_from_start)
    assert_equal(70, @track.events[6].time_from_start)
    assert_equal(80, @track.events[7].time_from_start)
    assert_equal(90, @track.events[8].time_from_start)
    assert_equal(100, @track.events[9].time_from_start)
    assert_equal(100, @track.events[10].time_from_start)
    assert_equal(110, @track.events[11].time_from_start)
    assert_equal(120, @track.events[12].time_from_start)
    assert_equal(200, @track.events[13].time_from_start)
    assert_equal(300, @track.events[14].time_from_start)
  end

  def test_recalc_delta_from_times
    @track.each { |event| event.delta_time = 0 }
    @track.recalc_delta_from_times
    @track.each { |event| assert_equal(100, event.delta_time) }
  end

  def test_recalc_delta_from_times_unsorted
    @track.events[0].time_from_start = 100
    @track.events[1].time_from_start = 50
    @track.events[2].time_from_start = 150
    @track.recalc_delta_from_times
    prev_start_time = 0
    @track.each do |event|
      assert(prev_start_time <= event.time_from_start)
      assert(event.delta_time > 0)
      prev_start_time = event.time_from_start
    end
  end

  def test_sort
    e = @track.events[0]
    e.time_from_start = 300
    e = @track.events[1]
    e.time_from_start = 100
    e = @track.events[2]
    e.time_from_start = 200

    @track.sort

    assert_equal(100, @track.events[0].time_from_start)
    assert_equal(100, @track.events[0].delta_time)

    assert_equal(200, @track.events[1].time_from_start)
    assert_equal(100, @track.events[1].delta_time)

    assert_equal(300, @track.events[2].time_from_start)
    assert_equal(100, @track.events[2].delta_time)
  end

  def test_quantize
    @seq.ppqn = 80

    @track.quantize(1)	# Quantize to a quarter note
    assert_equal(80, @track.events[0].time_from_start) # was 100
    assert_equal(240, @track.events[1].time_from_start) # was 200
    assert_equal(320, @track.events[2].time_from_start) # was 300
  end

  def test_instrument
    @track.instrument = 'foo'
    assert_equal('foo', @track.instrument)
  end

  def test_old_note_class_names
    x = MIDI::NoteOn.new(0, 64, 64, 10)
    assert(x.kind_of?(MIDI::NoteOnEvent))  # old name
    x = MIDI::NoteOff.new(0, 64, 64, 10)
    assert(x.kind_of?(MIDI::NoteOffEvent)) # old name
  end

  def test_mergesort
    @track.events = []

    # Two events with later start times but earlier in the event list
    e2 = MIDI::NoteOff.new(0, 64, 64, 100)
    e2.time_from_start = 100
    @track.events << e2

    e3 = MIDI::NoteOn.new(0, 64, 64, 100)
    e3.time_from_start = 100
    @track.events << e3

    # Earliest start time, latest in the list of events
    e1 = MIDI::NoteOn.new(0, 64, 64, 100)
    e1.time_from_start = 0
    @track.events << e1

    # Recalc sorts. Make sure note off/note on pair at t 100 are in the
    # correct order.
    @track.recalc_delta_from_times
    
    # These tests would fail before we moved to mergesort.
    assert_equal(e1, @track.events[0])
    assert_equal(e2, @track.events[1])
    assert_equal(e3, @track.events[2])

  end
end
