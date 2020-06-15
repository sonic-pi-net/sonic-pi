#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative 'wrappingarray'

module SonicPi
  class Chord < WrappingArray
    # Ported from Overtone: https://github.com/overtone/overtone/blob/master/src/overtone/music/pitch.clj

    CHORD, CHORD_LOOKUP, CHORD_NAMES = lambda do
      major   = [0, 4, 7]
      minor   = [0, 3, 7]
      major7  = [0, 4, 7, 11]
      dom7    = [0, 4, 7, 10]
      minor7  = [0, 3, 7, 10]
      aug     = [0, 4, 8]
      dim     = [0, 3, 6]
      dim7    = [0, 3, 6, 9]
      halfdim = [0, 3, 6, 10]
      all_chords = {
        "1"              => [0],
        "5"              => [0, 7],
        "+5"             => [0, 4, 8],
        "m+5"            => [0, 3, 8],
        :sus2            => [0, 2, 7],
        :sus4            => [0, 5, 7],
        "6"              => [0, 4, 7, 9],
        :m6              => [0, 3, 7, 9],
        "7sus2"          => [0, 2, 7, 10],
        "7sus4"          => [0, 5, 7, 10],
        "7-5"            => [0, 4, 6, 10],
        :halfdiminished  => halfdim,
        "7+5"            => [0, 4, 8, 10],
        "m7+5"           => [0, 3, 8, 10],
        "9"              => [0, 4, 7, 10, 14],
        :m9              => [0, 3, 7, 10, 14],
        "m7+9"           => [0, 3, 7, 10, 14],
        :maj9            => [0, 4, 7, 11, 14],
        "9sus4"          => [0, 5, 7, 10, 14],
        "6*9"            => [0, 4, 7, 9, 14],
        "m6*9"           => [0, 3, 9, 7, 14],
        "7-9"            => [0, 4, 7, 10, 13],
        "m7-9"           => [0, 3, 7, 10, 13],
        "7-10"           => [0, 4, 7, 10, 15],
        "7-11"           => [0, 4, 7, 10, 16],
        "7-13"           => [0, 4, 7, 10, 20],
        "9+5"            => [0, 10, 13],
        "m9+5"           => [0, 10, 14],
        "7+5-9"          => [0, 4, 8, 10, 13],
        "m7+5-9"         => [0, 3, 8, 10, 13],
        "11"             => [0, 4, 7, 10, 14, 17],
        :m11             => [0, 3, 7, 10, 14, 17],
        :maj11           => [0, 4, 7, 11, 14, 17],
        "11+"            => [0, 4, 7, 10, 14, 18],
        "m11+"           => [0, 3, 7, 10, 14, 18],
        "13"             => [0, 4, 7, 10, 14, 17, 21],
        :m13             => [0, 3, 7, 10, 14, 17, 21],
        :add2            => [0, 2, 4, 7],
        :add4            => [0, 4, 5, 7],
        :add9            => [0, 4, 7, 14],
        :add11           => [0, 4, 7, 17],
        :add13           => [0, 4, 7, 21],
        :madd2           => [0, 2, 3, 7],
        :madd4           => [0, 3, 5, 7],
        :madd9           => [0, 3, 7, 14],
        :madd11          => [0, 3, 7, 17],
        :madd13          => [0, 3, 7, 21],
        :major           => major,
        :maj             => major,
        :M               => major,
        :minor           => minor,
        :min             => minor,
        :m               => minor,
        :major7          => major7,
        :dom7            => dom7,
        "7"              => dom7,
        :M7              => major7,
        :minor7          => minor7,
        :m7              => minor7,
        :augmented       => aug,
        :a               => aug,
        :diminished      => dim,
        :dim             => dim,
        :i               => dim,
        :diminished7     => dim7,
        :dim7            => dim7,
        :i7              => dim7,
        :halfdim         => halfdim,
        "m7b5"           => halfdim,
        "m7-5" => halfdim
      }

      all_chords_lookup = all_chords.inject({}) do |res, chord_intervals|
        k, v = *chord_intervals
        res[k.to_sym] = v
        res
      end

      all_chords_names = all_chords.inject([]) do |res, chord_intervals|
        k, _v = *chord_intervals
        res << k.to_s
        res
      end

      return all_chords, all_chords_lookup, all_chords_names.sort
    end.call

    attr_reader :tonic, :notes, :num_octaves

    def self.resolve_degree(degree, tonic, name, no_of_notes)
      name = name.to_s
      degree_int = Scale.resolve_degree_index(degree)
      scale = Scale.resolve_scale(tonic, name, 2)
      scale.notes.drop(degree_int).select.with_index{|_, i| i % 2 == 0}.take(no_of_notes)
    end

    def initialize(tonic, name, num_octaves=1)
      num_octaves = 1 unless num_octaves
      name = name.to_sym
      intervals = CHORD_LOOKUP[name]
      raise "Unknown chord name: #{name.inspect}" unless intervals

      tonic = Note.resolve_midi_note_without_octave(tonic)
      res = []

      num_octaves.times do |o|
        intervals.each do |i|
          res << tonic + i + (o * 12)
        end
      end

      @name = name
      @tonic = tonic
      @notes = res
      @num_octaves = num_octaves
      super(res)
    end

    def name
      @name.to_s
    end

    def to_s
      "#<SonicPi::Chord :#{Note.resolve_note_name(@tonic)} :#{@name} #{@notes}>"
    end

    def inspect
      to_s
    end
  end
end
