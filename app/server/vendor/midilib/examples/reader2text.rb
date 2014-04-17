#! /usr/bin/env ruby
#
# usage: reader2text.rb [midi_file]
#
# This script translates a MIDI file into text. It subclasses MIDI::MIDIFile
# and overrides the methods that are called when various events are seen
# in the file.
#
# For a simpler way to do the same thing, see seq2text.rb.
#

# Start looking for MIDI module classes in the directory above this one.
# This forces us to use the local copy, even if there is a previously
# installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'midilib/sequence'
require 'midilib/io/midifile'

DEFAULT_MIDI_TEST_FILE = 'NoFences.mid'

class TextTranslator < MIDI::IO::MIDIFile

  def initialize(seq, proc = nil)
    super()
    @seq = seq
    @track = nil
    @update_block = block_given?() ? Proc.new() : proc
  end

  # Generate a unique number for a channel/note combination. This is used
  # to remember pending note on events.
  def note_hash(chan, note)
    return (chan << 8) + note
  end

  # Print a delta time.
  def pdelta
    print "#{@curr_ticks}: "
  end

  # The remaining methods are overrides of methods in MIDI::IO::MIDIFile.

  def header(format, ntrks, division)
    puts "header: format = #{format}, ntrks = #{ntrks}," +
      " division = #{division}"

    @ntrks = ntrks
    @update_block.call(nil, @ntrks, 0) if @update_block
  end

  def start_track()
    pdelta()
    puts "track start"

    @pending = []
    @chan_mask = 0
  end

  def end_track()
    pdelta()
    puts "track end; chans used bitmask = #{@chan_mask}"
    # Write message for any pending note on messages
    @pending.each_with_index do |num, chan|
      puts "pending note off missing for chan #{num >> 8}," +
      " note #{num & 0xff}" if note_obj
    end
    @pending = nil

    # call update block
    @update_block.call(@track, @ntrks, @seq.tracks.length) if @update_block
  end

  def note_on(chan, note, vel)
    pdelta()
    if vel == 0
      print "(note on, vel 0) "
      note_off(chan, note, 64)
      return
    end

    puts "note on chan #{chan}, note #{note}, vel #{vel}"
    @pending << note_hash(chan, note)
    track_uses_channel(chan)
  end

  def note_off(chan, note, vel)
    pdelta()
    # Find note on, create note off, connect the two, and remove
    # note on from pending list.
    pnum = note_hash(chan, note)
    @pending.each_with_index do |num, i|
      if pnum == num
        puts "note off chan #{chan}, note #{note}, vel #{vel}"
        @pending.delete_at(i)
        return
      end
    end
    puts "note off with no earlier note on (ch #{chan}, note" +
      " #{note}, vel #{vel})"
  end

  def pressure(chan, note, press)
    pdelta()
    puts "pressure chan #{chan}, note #{note}, press #{press}"
    track_uses_channel(chan)
  end

  def controller(chan, control, value)
    pdelta()
    puts "controller chan #{chan}, control #{control}, value #{value}"
    track_uses_channel(chan)
  end

  def pitch_bend(chan, msb, lsb)
    pdelta()
    puts "pitch bend chan #{chan}, msb #{msb}, lsb #{lsb}"
    track_uses_channel(chan)
  end

  def program(chan, program)
    pdelta()
    puts "program chan #{chan}, program #{program}"
    track_uses_channel(chan)
  end

  def chan_pressure(chan, press)
    pdelta()
    puts "chan press chan #{chan}, press #{press}"
    track_uses_channel(chan)
  end

  def sysex(msg)
    pdelta()
    puts "sysex size #{msg.length}"
  end

  def meta_misc(type, msg)
    pdelta()
    puts "meta misc type #{type}, length #{msg.length}"
  end

  def sequencer_specific(type, msg)
    pdelta()
    puts "sequencer specific type #{type}, msg #{msg.length}"
  end

  def sequence_number(num)
    pdelta()
    puts "sequence number #{num}"
  end

  def text(type, msg)
    pdelta()
    msg = MIDI::MetaEvent.bytes_as_str(msg)
    case type
    when MIDI::META_SEQ_NAME
      puts "seq or track name #{msg}"
    when MIDI::META_INSTRUMENT
      puts "instrument name #{msg}"
    when MIDI::META_MARKER
      puts "marker #{msg}"
    else
      puts "text = #{msg}, type = #{type}"
    end
  end

  def eot()
    pdelta()
    puts "end of track event"
  end

  def time_signature(numer, denom, clocks, qnotes)
    pdelta()
    puts "time sig numer #{numer}, denom #{denom}, clocks #{clocks}," +
      " qnotes #{qnotes}"
  end

  def smpte(hour, min, sec, frame, fract)
    pdelta()
    puts "smpte #{hour}:#{min}.#{sec}, frame #{frame}, fract #{fract}"
  end

  def tempo(microsecs)
    pdelta()
    bpm = 1.0 / microsecs	# quarter notes per microsecond
    bpm *= 1000000.0	# quarter notes per second
    bpm *= 60.0		# quarter notes per minute
    puts "tempo microsecs pqn = #{microsecs} (#{bpm} bpm)"
  end

  def key_signature(sharpflat, is_minor)
    pdelta()
    puts "key sig sharpflat #{sharpflat}, is_minor #{is_minor}"
  end

  def arbitrary(msg)
    pdelta()
    puts "arbitrary length = #{msg.length}"
  end

  def track_uses_channel(chan)
    @chan_mask = @chan_mask | (1 << chan)
  end

end

# ================================================================

seq = MIDI::Sequence.new()

# Specify what class to use when reading the MIDI file.
seq.reader_class = TextTranslator

File.open(ARGV[0] || DEFAULT_MIDI_TEST_FILE, 'rb') do | file |
  # The block we pass in to Sequence.read is called at the end of every
  # track read. It is optional, but is useful for progress reports.
  seq.read(file) do |track, num_tracks, i|
    puts "read track #{track ? track.name : ''} (#{i} of #{num_tracks})"
  end
end
