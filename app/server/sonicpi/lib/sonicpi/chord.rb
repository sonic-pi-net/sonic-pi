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
  class Chord
    # Ported from Overtone: https://github.com/overtone/overtone/blob/master/src/overtone/music/pitch.clj


    include Enumerable
    include Comparable

     CHORD = lambda{
      major  = [0, 4, 7]
      minor  = [0, 3, 7]
      major7 = [0, 4, 7, 11]
      dom7   = [0, 4, 7, 10]
      minor7 = [0, 3, 7, 10]
      aug    = [0, 4, 8]
      dim    = [0, 3, 6]
      dim7   = [0, 3, 6, 9]
      {
        "1"           => [0],
        "5"           => [0, 7],
        "+5"          => [0, 4, 8],
        "m+5"         => [0, 3, 8],
        "sus2"        => [0, 2, 7],
        "sus4"        => [0, 5, 7],
        "6"           => [0, 4, 7, 9],
        "m6"          => [0, 3, 7, 9],
        "7sus2"       => [0, 2, 7, 10],
        "7sus4"       => [0, 5, 7, 10],
        "7-5"         => [0, 4, 6, 10],
        "m7-5"        => [0, 3, 6, 10],
        "7+5"         => [0, 4, 8, 10],
        "m7+5"        => [0, 3, 8, 10],
        "9"           => [0, 4, 7, 10, 14],
        "m9"          => [0, 3, 7, 10, 14],
        "m7+9"        => [0, 3, 7, 10, 14],
        "maj9"        => [0, 4, 7, 11, 14],
        "9sus4"       => [0, 5, 7, 10, 14],
        "6*9"         => [0, 4, 7, 9, 14],
        "m6*9"        => [0, 3, 9, 7, 14],
        "7-9"         => [0, 4, 7, 10, 13],
        "m7-9"        => [0, 3, 7, 10, 13],
        "7-10"        => [0, 4, 7, 10, 15],
        "9+5"         => [0, 10, 13],
        "m9+5"        => [0, 10, 14],
        "7+5-9"       => [0, 4, 8, 10, 13],
        "m7+5-9"      => [0, 3, 8, 10, 13],
        "11"          => [0, 4, 7, 10, 14, 17],
        "m11"         => [0, 3, 7, 10, 14, 17],
        "maj11"       => [0, 4, 7, 11, 14, 17],
        "11+"         => [0, 4, 7, 10, 14, 18],
        "m11+"        => [0, 3, 7, 10, 14, 18],
        "13"          => [0, 4, 7, 10, 14, 17, 21],
        "m13"         => [0, 3, 7, 10, 14, 17, 21],
        "major"       => major,
        "M"           => major,
        "minor"       => minor,
        "m"           => minor,
        "major7"      => major7,
        "dom7"        => dom7,
        "7"           => dom7,
        "M7"          => major7,
        "minor7"      => minor7,
        "m7"          => minor7,
        "augmented"   => aug,
        "a"           => aug,
        "diminished"  => dim,
        "dim"         => dim,
        "i"           => dim,
        "diminished7" => dim7,
        "dim7"        => dim7,
        "i7"          => dim7}}.call

    attr_reader :name, :tonic, :notes

    def initialize(tonic, name)
      name = name.to_s
      intervals = CHORD[name]
      raise "Unknown chord name: #{name.inspect}" unless intervals

      tonic = Note.resolve_midi_note_without_octave(tonic)
      res = []
      intervals.each do |i|
        res << tonic + i
      end


      @name = name
      @tonic = tonic
      @notes = res

    end

    def to_s
      "#<SonicPi::Chord :#{Note.resolve_note_name(@tonic)} :#{@name} #{@notes}>"
    end

    def inspect
      to_s
    end

    def to_a
      @notes
    end

    def each &block
      @notes.each(&block)
    end

    def <=> other
      @notes <=> other.to_a
    end

  end
end
