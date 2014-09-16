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
  class Scale
    # Ported from Overtone: https://github.com/overtone/overtone/blob/master/src/overtone/music/pitch.clj

    class InvalidScaleError < ArgumentError; end ;
    class InvalidDegreeError < ArgumentError; end ;

    include Enumerable
    include Comparable

    SCALE = lambda{
      ionian_sequence     = [2, 2, 1, 2, 2, 2, 1]
      hex_sequence        = [2, 2, 1, 2, 2, 3]
      pentatonic_sequence = [3, 2, 2, 3, 2]
      {
           diatonic:           ionian_sequence,
           ionian:             ionian_sequence,
           major:              ionian_sequence,
           dorian:             ionian_sequence.rotate(1),
           phrygian:           ionian_sequence.rotate(2),
           lydian:             ionian_sequence.rotate(3),
           mixolydian:         ionian_sequence.rotate(4),
           aeolian:            ionian_sequence.rotate(5),
           minor:              ionian_sequence.rotate(5),
           locrian:            ionian_sequence.rotate(6),
           hex_major6:         hex_sequence,
           hex_dorian:         hex_sequence.rotate(1),
           hex_phrygian:       hex_sequence.rotate(2),
           hex_major7:         hex_sequence.rotate(3),
           hex_sus:            hex_sequence.rotate(4),
           hex_aeolian:        hex_sequence.rotate(5),
           minor_pentatonic:   pentatonic_sequence,
           yu:                 pentatonic_sequence,
           major_pentatonic:   pentatonic_sequence.rotate(1),
           gong:               pentatonic_sequence.rotate(1),
           egyptian:           pentatonic_sequence.rotate(2),
           shang:              pentatonic_sequence.rotate(2),
           jiao:               pentatonic_sequence.rotate(3),
           zhi:                pentatonic_sequence.rotate(4),
           ritusen:            pentatonic_sequence.rotate(4),
           whole_tone:         [2, 2, 2, 2, 2, 2],
           whole:              [2, 2, 2, 2, 2, 2],
           chromatic:          [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
           harmonic_minor:     [2, 1, 2, 2, 1, 3, 1],
           melodic_minor_asc:  [2, 1, 2, 2, 2, 2, 1],
           hungarian_minor:    [2, 1, 3, 1, 1, 3, 1],
           octatonic:          [2, 1, 2, 1, 2, 1, 2, 1],
           messiaen1:          [2, 2, 2, 2, 2, 2],
           messiaen2:          [1, 2, 1, 2, 1, 2, 1, 2],
           messiaen3:          [2, 1, 1, 2, 1, 1, 2, 1, 1],
           messiaen4:          [1, 1, 3, 1, 1, 1, 3, 1],
           messiaen5:          [1, 4, 1, 1, 4, 1],
           messiaen6:          [2, 2, 1, 1, 2, 2, 1, 1],
           messiaen7:          [1, 1, 1, 2, 1, 1, 1, 1, 2, 1],
           super_locrian:      [1, 2, 1, 2, 2, 2, 2],
           hirajoshi:          [2, 1, 4, 1, 4],
           kumoi:              [2, 1, 4, 2, 3],
           neapolitan_major:   [1, 2, 2, 2, 2, 2, 1],
           bartok:             [2, 2, 1, 2, 1, 2, 2],
           bhairav:            [1, 3, 1, 2, 1, 3, 1],
           locrian_major:      [2, 2, 1, 1, 2, 2, 2],
           ahirbhairav:        [1, 3, 1, 2, 2, 1, 2],
           enigmatic:          [1, 3, 2, 2, 2, 1, 1],
           neapolitan_minor:   [1, 2, 2, 2, 1, 3, 1],
           pelog:              [1, 2, 4, 1, 4],
           augmented2:         [1, 3, 1, 3, 1, 3],
           scriabin:           [1, 3, 3, 2, 3],
           harmonic_major:     [2, 2, 1, 2, 1, 3, 1],
           melodic_minor_desc: [2, 1, 2, 2, 1, 2, 2],
           romanian_minor:     [2, 1, 3, 1, 2, 1, 2],
           hindu:              [2, 2, 1, 2, 1, 2, 2],
           iwato:              [1, 4, 1, 4, 2],
           melodic_minor:      [2, 1, 2, 2, 2, 2, 1],
           diminished2:        [2, 1, 2, 1, 2, 1, 2, 1],
           marva:              [1, 3, 2, 1, 2, 2, 1],
           melodic_major:      [2, 2, 1, 2, 1, 2, 2],
           indian:             [4, 1, 2, 3, 2],
           spanish:            [1, 3, 1, 2, 1, 2, 2],
           prometheus:         [2, 2, 2, 5, 1],
           diminished:         [1, 2, 1, 2, 1, 2, 1, 2],
           todi:               [1, 2, 3, 1, 1, 3, 1],
           leading_whole:      [2, 2, 2, 2, 2, 1, 1],
           augmented:          [3, 1, 3, 1, 3, 1],
           purvi:              [1, 3, 2, 1, 1, 3, 1],
           chinese:            [4, 2, 1, 4, 1],
           lydian_minor:       [2, 2, 2, 1, 1, 2, 2]}}.call

    # Zero indexed for CS compatibility
    DEGREES = {:i    => 0,
               :ii   => 1,
               :iii  => 2,
               :iv   => 3,
               :v    => 4,
               :vi   => 5,
               :vii  => 6,
               :viii => 7,
               :ix   => 8,
               :x    => 9,
               :xi   => 10,
               :xii  => 11}

    def self.resolve_degree_index(degree)
      if idx = DEGREES[degree]
        return idx
      elsif degree.is_a? Numeric
        return degree - 1
      else
        raise InvalidDegreeError, "Invalid scale degree #{degree.inspect}, expecting #{DEGREES.keys.join ','} or a number"
      end
    end


    def self.resolve_degree(degree, tonic, scale)
      scale = Scale.new(tonic, scale)
      index = resolve_degree_index(degree)
      scale.notes[index]
    end


    attr_reader :name, :tonic, :num_octaves, :notes

    def initialize(tonic, name, num_octaves=1)
      name = name.to_sym
      intervals = SCALE[name]
      raise InvalidScaleError, "Unknown scale name: #{name.inspect}" unless intervals
      intervals = intervals * num_octaves
      current = Note.resolve_midi_note(tonic)
      res = [current]
      intervals.each do |i|
        current += i
        res << current
      end

      @name = name
      @tonic = tonic
      @num_octaves = num_octaves
      @notes = res
    end

    def to_s
      "#<SonicPi::Scale :#{Note.resolve_note_name(@tonic)} :#{@name} #{@notes}>"
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
