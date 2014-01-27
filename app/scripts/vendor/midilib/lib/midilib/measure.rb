require 'midilib/consts'

module MIDI

  # The Measure class contains information about a measure from the sequence.
  # The measure data is based on the time signature information from the sequence
  # and is not stored in the sequence itself
  class Measure
    # The numerator (top digit) for the measure's time signature
    attr_reader :numerator
    # The denominator for the measure's time signature
    attr_reader :denominator
    # Start clock tick for the measure
    attr_reader :start
    # End clock tick for the measure (inclusive)
    attr_reader :end
    # The measure number (1-based)
    attr_reader :measure_number
    # The metronome tick for the measure
    attr_reader :metronome_ticks

    # Constructor
    def initialize(meas_no, start_time, duration, numer, denom, met_ticks)
      @measure_number = meas_no
      @start = start_time
      @end = start_time + duration - 1
      @numerator = numer
      @denominator = denom
      @metronome_ticks = met_ticks
    end

    # Returns a detailed string with information about the measure
    def to_s
      t = "#{@numerator}/#{2**@denominator}"
      m = @metronome_ticks.to_f / 24
      "measure #{@measure_number}  #{@start}-#{@end}  #{t}   #{m} qs metronome"
    end

    # Returns +true+ if the event is in the measure
    def contains_event?(e)
      (e.time_from_start >= @start) && (e.time_from_start <= @end)
    end
  end

  # A specialized container for MIDI::Measure objects, which can be use to map
  # event times to measure numbers. Please note that this object has to be remade
  # when events are deleted/added in the sequence.
  class Measures < Array
    # The highest event time in the sequence (at the time when the
    # object was created)
    attr_reader :max_time

    # The ppqd from the sequence
    attr_reader :ppqd

    # Constructor
    def initialize(max_time, ppqd)
      super(0)
      @max_time = max_time
      @ppqd = ppqd
    end

    # Returns the MIDI::Measure object where the event is located.
    # Returns +nil+ if the event isn't found in the container (should
    # never happen if the MIDI::Measures object is up to date).
    def measure_for_event(e)
      detect { |m| m.contains_event?(e) }
    end

    # Returns the event's time as a formatted MBT string (Measure:Beat:Ticks)
    # as found in MIDI sequencers.
    def to_mbt(e)
      m = measure_for_event(e)
      b = (e.time_from_start.to_f - m.start.to_f) / @ppqd
      b *= 24 / m.metronome_ticks
      sprintf("%d:%02d:%03d", m.measure_number, b.to_i + 1, (b - b.to_i) * @ppqd)
    end
  end

end
