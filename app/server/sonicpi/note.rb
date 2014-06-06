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
module SonicPi
  class Note

    NOTES_TO_INTERVALS =
      {c:  0,
       cs: 1,  df: 1,  db: 1,
       d:  2,
       eb: 3,  ef: 3,  ds: 3,
       e:  4,  fb: 4,  ff: 4,
       f:  5,  es: 5,
       fs: 6,  gb: 6,  gf: 6,
       g:  7,
       gs: 8,  ab: 8,  af: 8,
       a:  9,
       bb: 10, bf: 10, as: 10,
       b:  11, cf: 11, cb: 11,
       bs: 12}

    INTERVALS_TO_NOTES = {
      0  => :C,
      1  => :Cs,
      2  => :D,
      3  => :Eb,
      4  => :E,
      5  => :F,
      6  => :Fs,
      7  => :G,
      8  => :Ab,
      9  => :A,
      10 => :Bb,
      11 => :B}

    DEFAULT_OCTAVE = 4

    MIDI_NOTE_RE = /([a-gA-G][sSbBfF]?)([-]?[0-9]*)/

    attr_reader :pitch_class, :octave, :interval, :midi_note, :midi_string

    @@notes_cache = {}

    def self.resolve_midi_note_without_octave(n)
      return @@notes_cache[n] if @@notes_cache[n]
      return n if n.is_a? Fixnum
      note = case n
             when Symbol, String
               self.new(n).midi_note
             when NilClass
               nil
             end
      @@notes_cache[n] = note
      note
    end

    def self.resolve_midi_note(n, o=nil)
      n = NOTES_TO_INTERVALS[n.to_sym]
      raise "Invalid note without octave #{n.inspect}, expected something of the form :c, :Bb, Fs" unless n
      o = o.to_i * 12
      n + o + 12
    end

    def self.resolve_note_name(n, o=nil)
      note = resolve_midi_note(n, o)
      note = note % 12
      INTERVALS_TO_NOTES[note]
    end

    def initialize(n, o=nil)
      n = n.to_s

      m = MIDI_NOTE_RE.match n

      raise "Invalid note: #{n}" unless m
      @pitch_class = m[1].capitalize

      if o
        @octave = o.to_i
      else
        @octave = m[2].empty? ? DEFAULT_OCTAVE : m[2].to_i
      end

      @interval = NOTES_TO_INTERVALS[m[1].downcase.to_sym]

      raise "Invalid note: #{n}" unless @interval

      @midi_note = (@octave * 12) + @interval + 12
      @midi_string = "#{@pitch_class.capitalize}#{@interval}"
    end

    def to_s
      @midi_string
    end
  end
end
