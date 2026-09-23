# SPDX-License-Identifier: AGPL-3.0-or-later
# Chords and scales as rings of MIDI notes, from the interval tables the
# oracle carries (runtime/data/theory.rb).
module SonicPi
  module Theory
    def self.chord(tonic, name, num_octaves = nil)
      num_octaves = 1 unless num_octaves
      name = name.to_sym
      intervals = SonicPi::Data::CHORDS[name] or raise "Unknown chord name: #{name.inspect}"
      tonic = Note.midi(tonic)
      res = []
      num_octaves.times do |o|
        intervals.each { |i| res << tonic + i + (o * 12) }
      end
      res
    end

    def self.scale(tonic, name, num_octaves = 1)
      name = name.to_sym
      intervals = SonicPi::Data::SCALES[name] or raise InvalidScaleError, "Unknown scale name: #{name.inspect}"
      intervals = intervals * num_octaves
      current = Note.midi(tonic)
      res = [current]
      intervals.each do |i|
        current += i
        res << current
      end
      res
    end
  end
end
