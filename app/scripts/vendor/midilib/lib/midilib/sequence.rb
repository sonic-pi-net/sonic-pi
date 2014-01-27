require 'midilib/io/seqreader'
require 'midilib/io/seqwriter'
require 'midilib/measure.rb'

module MIDI

  # A MIDI::Sequence contains MIDI::Track objects.
  class Sequence

    include Enumerable

    UNNAMED = 'Unnamed Sequence'
    DEFAULT_TEMPO = 120

    NOTE_TO_LENGTH = {
      'whole' => 4.0,
      'half' => 2.0,
      'quarter' => 1.0,
      'eighth' => 0.5,
      '8th' => 0.5,
      'sixteenth' => 0.25,
      '16th' => 0.25,
      'thirty second' => 0.125,
      'thirtysecond' => 0.125,
      '32nd' => 0.125,
      'sixty fourth' => 0.0625,
      'sixtyfourth' => 0.0625,
      '64th' => 0.0625
    }

    # Array with all tracks for the sequence
    attr_accessor :tracks
    # Pulses (i.e. clocks) Per Quarter Note resolution for the sequence
    attr_accessor :ppqn
    # The MIDI file format (0, 1, or 2)
    attr_accessor :format
    attr_accessor :numer, :denom, :clocks, :qnotes
    # The class to use for reading MIDI from a stream. The default is
    # MIDI::IO::SeqReader. You can change this at any time.
    attr_accessor :reader_class
    # The class to use for writeing MIDI from a stream. The default is
    # MIDI::IO::SeqWriter. You can change this at any time.
    attr_accessor :writer_class

    def initialize
      @tracks = Array.new()
      @ppqn = 480

      # Time signature
      @numer = 4		# Numer + denom = 4/4 time default
      @denom = 2
      @clocks = 24    # Bug fix  Nov 11, 2007 - this is not the same as ppqn!
      @qnotes = 8

      @reader_class = IO::SeqReader
      @writer_class = IO::SeqWriter
    end

    # Sets the time signature.
    def time_signature(numer, denom, clocks, qnotes)
      @numer = numer
      @denom = denom
      @clocks = clocks
      @qnotes = qnotes
    end

    # Returns the song tempo in beats per minute.
    def beats_per_minute
      return DEFAULT_TEMPO if @tracks.nil? || @tracks.empty?
      event = @tracks.first.events.detect { |e| e.kind_of?(MIDI::Tempo) }
      return event ? (Tempo.mpq_to_bpm(event.tempo)) : DEFAULT_TEMPO
    end
    alias_method :bpm, :beats_per_minute
    alias_method :tempo, :beats_per_minute

    # Pulses (also called ticks) are the units of delta times and event
    # time_from_start values. This method converts a number of pulses to a
    # float value that is a time in seconds.
    def pulses_to_seconds(pulses)
      (pulses.to_f / @ppqn.to_f / beats_per_minute()) * 60.0
    end

    # Given a note length name like "whole", "dotted quarter", or "8th
    # triplet", return the length of that note in quarter notes as a delta
    # time.
    def note_to_delta(name)
      return length_to_delta(note_to_length(name))
    end

    # Given a note length name like "whole", "dotted quarter", or "8th
    # triplet", return the length of that note in quarter notes as a
    # floating-point number, suitable for use as an argument to
    # length_to_delta.
    #
    # Legal names are any value in NOTE_TO_LENGTH, optionally prefixed by
    # "dotted_" and/or suffixed by "_triplet". So, for example,
    # "dotted_quarter_triplet" returns the length of a dotted quarter-note
    # triplet and "32nd" returns 1/32.
    def note_to_length(name)
      name.strip!
      name =~ /^(dotted)?(.*?)(triplet)?$/
      dotted, note_name, triplet = $1, $2, $3
      note_name.strip!
      mult = 1.0
      mult = 1.5 if dotted
      mult /= 3.0 if triplet
      len = NOTE_TO_LENGTH[note_name]
      raise "Sequence.note_to_length: \"#{note_name}\" not understood in \"#{name}\"" unless len
      return len * mult
    end

    # Translates +length+ (a multiple of a quarter note) into a delta time.
    # For example, 1 is a quarter note, 1.0/32.0 is a 32nd note, 1.5 is a
    # dotted quarter, etc. Be aware when using division; 1/32 is zero due to
    # integer mathematics and rounding. Use floating-point numbers like 1.0
    # and 32.0. This method always returns an integer.
    #
    # See also note_to_delta and note_to_length.
    def length_to_delta(length)
      return (@ppqn * length).to_i
    end

    # Returns the name of the first track (track zero). If there are no
    # tracks, returns UNNAMED.
    def name
      return UNNAMED if @tracks.empty?
      return @tracks.first.name()
    end

    # Hands the name to the first track. Does nothing if there are no tracks.
    def name=(name)
      return if @tracks.empty?
      @tracks.first.name = name
    end

    # Reads a MIDI stream.
    def read(io, proc = nil)	# :yields: track, num_tracks, index
      reader = @reader_class.new(self, block_given?() ? Proc.new() : proc)
      reader.read_from(io)
    end

    # Writes to a MIDI stream.
    def write(io, proc = nil)	# :yields: track, num_tracks, index
      writer = @writer_class.new(self, block_given?() ? Proc.new() : proc)
      writer.write_to(io)
    end

    # Iterates over the tracks.
    def each			# :yields: track
      @tracks.each { |track| yield track }
    end

    # Returns a Measures object, which is an array container for all measures
    # in the sequence
    def get_measures
      # Collect time sig events and scan for last event time
      time_sigs = []
      max_pos = 0
      @tracks.each  do |t|
        t.each do |e|
          time_sigs << e if e.kind_of?(MIDI::TimeSig)
          max_pos = e.time_from_start if e.time_from_start > max_pos
        end
      end
      time_sigs.sort { |x,y| x.time_from_start <=> y.time_from_start }

      # Add a "fake" time sig event at the very last position of the sequence,
      # just to make sure the whole sequence is calculated.
      t = MIDI::TimeSig.new(4, 2, 24, 8, 0)
      t.time_from_start = max_pos
      time_sigs << t

      # Default to 4/4
      measure_length = @ppqn * 4
      oldnumer, olddenom, oldbeats = 4, 2, 24

      measures = MIDI::Measures.new(max_pos, @ppqn)
      curr_pos = 0
      curr_meas_no = 1
      time_sigs.each do |te|
        meas_count = (te.time_from_start - curr_pos) / measure_length
        meas_count += 1 if (te.time_from_start - curr_pos) % measure_length > 0
        1.upto(meas_count) do |i|
          measures << MIDI::Measure.new(curr_meas_no, curr_pos, measure_length,
                                        oldnumer, olddenom, oldbeats)
          curr_meas_no += 1
          curr_pos += measure_length
        end
        oldnumer, olddenom, oldbeats = te.numerator, te.denominator, te.metronome_ticks
        measure_length = te.measure_duration(@ppqn)
      end
      measures
    end

  end
end
