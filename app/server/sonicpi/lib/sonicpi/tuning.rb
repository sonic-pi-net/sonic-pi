#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative 'note'

module SonicPi
  class Tuning

    class InvalidTuningError < ArgumentError ; end ;

    attr_accessor :tunings

    def initialize
      # http://en.wikipedia.org/wiki/Musical_tuning#Systems_for_the_twelve-note_chromatic_scale
      @tunings = {
        :just =>
        [
          1, #c
          16.0/15.0, #cs
          9.0/8.0, #d
          6.0/5.0, #ds
          5.0/4.0, #e
          4.0/3.0, #f
          45.0/32.0, #fs
          3.0/2.0, #g
          8.0/5.0, #gs
          5.0/3.0, #a
          9.0/5.0, #as
          15.0/8.0],

        :pythagorean =>
        [
          1.0/1, #unison
          2187.0/2048, #apotome
          9.0/8, #major whole tone
          19683.0/16384, #Pythagorean augmented second
          81.0/64, #Pythagorean major third
          4.0/3, #perfect fourth
          729.0/512, #Pythagorean tritone
          3.0/2, #perfect fifth
          6561.0/4096, #Pythagorean augmented fifth
          27.0/16, #Pythagorean major sixth
          59049.0/32768, #Pythagorean augmented sixth
          243.0/128],

        :meantone =>
        [
          1.00000,
          1.0449,
          1.1180,
          1.1963,
          1.2500,
          1.3375,
          1.3975,
          1.4953,
          1.5625,
          1.6719,
          1.7889,
          1.8692]}

      # create a cache for tuned notes
      @tuned_notes_cache = {}
    end

    def resolve_tuning(note, tuning_system, fundamental)
      return note if tuning_system == :equal

      fundamental = Note.resolve_midi_note_without_octave(fundamental)
      note = Note.resolve_midi_note(note)
      # midi note 0 is a :c
      # by getting a fundamental without an octave
      # we know it's in the middle register between
      # 60 and 72
      fundamental = fundamental - 60

      #select the tuned note for a given midi note
      if note.is_a? Float
        note_above = tuned_notes(tuning_system, fundamental)[note.ceil]
        note_below = tuned_notes(tuning_system, fundamental)[note.floor]
        n_diff = note - note.floor
        return hz_to_midi(midi_to_hz(note_below) + ((midi_to_hz(note_above) - midi_to_hz(note_below)) * n_diff))
      else
        return tuned_notes(tuning_system, fundamental)[note]
      end
    end

    def midi_to_hz(n)
      (440.0 * (2 ** ((n - 69) / 12.0))).round(9)
    end

    def hz_to_midi(freq)
      ((12 * (Math.log(freq * 0.0022727272727) / Math.log(2))) + 69).round(9)
    end

    private

    def tuned_notes(tuning_system, fundamental_offset)

      notes = @tuned_notes_cache[[tuning_system, fundamental_offset]]

      return notes if notes

      notes = (0..150).to_a.map do |midi_note|
        # vary the fundamental here
        note_idx = (midi_note - fundamental_offset) % 12
        # tuning systems are ratios of the fundamental note
        tuned_note = midi_to_hz(midi_note - note_idx) * @tunings[tuning_system][note_idx]
        hz_to_midi(tuned_note)
      end

      @tuned_notes_cache[[tuning_system, fundamental_offset]] = notes
      notes
    end
  end
end
