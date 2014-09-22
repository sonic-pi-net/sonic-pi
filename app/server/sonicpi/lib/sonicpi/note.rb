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

    class InvalidNoteError < ArgumentError ; end ;
    class InvalidOctaveError < ArgumentError ; end ;

    NOTES_TO_INTERVALS =
      {cf: -1, CF: -1, Cf: -1, cF: -1,
       cb: -1, CB: -1, Cb: -1, cB: -1,
       c:  0,  C:  0,
       cs: 1,  CS: 1, cS: 1, Cs: 1,
       df: 1,  DF: 1, Df: 1, dF: 1,
       db: 1,  DB: 1, Db: 1, dB: 1,
       d:  2,  D:  2,
       eb: 3,  EB: 3, Eb: 3, eB: 3,
       ef: 3,  EF: 3, Ef: 3, eF: 3,
       ds: 3,  DS: 3, Ds: 3, dS: 3,
       e:  4,  E:  4,
       fb: 4,  FB: 4, Fb: 4, fB: 4,
       ff: 4,  FF: 4, Ff: 4, fF: 4,
       f:  5,  F:  5,
       es: 5,  ES: 5, Es: 5, eS: 5,
       fs: 6,  FS: 6, Fs: 6, fS: 6,
       gb: 6,  GB: 6, Gb: 6, gB: 6,
       gf: 6,  GF: 6, Gf: 6, gF: 6,
       g:  7,  G:  7,
       gs: 8,  GS: 8, Gs: 8, gS: 8,
       ab: 8,  AB: 8, Ab: 8, aB: 8,
       af: 8,  AF: 8, Af: 8, aF: 8,
       a:  9,  A:  9,
       bb: 10, BB: 10, Bb: 10, bB: 10,
       bf: 10, BF: 10, Bf: 10, bF: 10,
       as: 10, AS: 10, As: 10, aS: 10,
       b:  11, B: 11,
       bs: 12, BS: 12, Bs: 12, bS: 12}

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

    MIDI_NOTE_RE = /\A(([a-gA-G])([sSbBfF]?))([-]?[0-9]*)\Z/

    @@notes_cache = {}

    def self.resolve_midi_note_without_octave(n)
      return @@notes_cache[n] if @@notes_cache[n]
      return n if n.is_a? Numeric
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
      return resolve_midi_note_without_octave(n) unless o
      n = resolve_midi_note_without_octave(n)
      raise InvalidOctaveError, "Invalid octave: #{o.inspect}, expecting a number" unless o.is_a? Numeric
      n = n % 12
      o = o.to_i * 12
      n + o + 12
    end

    def self.resolve_note_name(n, o=nil)
      note = resolve_midi_note(n, o)
      note = note % 12
      INTERVALS_TO_NOTES[note]
    end

    attr_reader :pitch_class, :octave, :interval, :midi_note, :midi_string

    def initialize(n, o=nil)
      n = n.to_s

      m = MIDI_NOTE_RE.match n
      raise InvalidNoteError, "Invalid note: #{n}" unless m

      @pitch_class = "#{m[2].capitalize}#{unify_sharp_flat_modifier(m[3])}".to_sym

      if o
        raise InvalidOctaveError, "Invalid octave: #{o.inspect}, expecting a whole number such as 3 or 4!" unless o.is_a? Integer
        @octave = o.to_i
      else
        @octave = m[4].empty? ? DEFAULT_OCTAVE : m[4].to_i
      end

      @interval = NOTES_TO_INTERVALS[m[1].downcase.to_sym]
      raise InvalidNoteError, "Invalid note: #{n}" unless @interval

      @midi_note = (@octave * 12) + @interval + 12
      @midi_string = "#{@pitch_class.capitalize}#{@octave}"
    end

    def to_s
      "#<SonicPi::Note :#{@midi_string}>"
    end

    def inspect
      to_s
    end

    private

    def unify_sharp_flat_modifier(mod)
      m = mod.downcase
      return "b" if m == "f"
      m
    end
  end
end
