## encoding: utf-8
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
require_relative 'support/docsystem'
require_relative "../version"
require_relative "../util"

module SonicPi
  module Lang
    module Pattern


      include SonicPi::Util
      include SonicPi::Lang::Support::DocSystem

      def nested_beat(el, beat_length=nil)
        if el.kind_of? Hash and el.keys == [:over, :val]
          # We have a triple syntax hash
          no_of_beats = calculate_number_of_beats(el[:val])
          if beat_length.nil?
            # Top level beats are always 1 in length
            beat_length = 1.0
          else
            # This is different for triplet rhythms
            beat_length = (beat_length*el[:over].to_f)/no_of_beats
          end

          el[:val].map {|x| nested_beat(x, beat_length)}
        elsif el.kind_of? Array
          no_of_beats = calculate_number_of_beats(el)
          if beat_length.nil?
            # Top level beats are always 1 in length
            beat_length = 1.0
          else
            beat_length = beat_length/no_of_beats
          end

          el.map {|x| nested_beat(x, beat_length) }
        else
          {level: beat_length, step: el}
        end
      end

      def calculate_number_of_beats(arr)
        # Calculate the number of beats a pattern
        # takes up, accounting for the size of the
        # triplet hash :over param
        arr.reduce(0.0) {|acc,x|
          if(x.kind_of?(Hash) and x.keys == [:over, :val])
            acc + x[:over]
          else
            acc + 1
          end
        }
      end

      def get_nested_beat(pattern)
        nested_beat(pattern).flatten.map do |step|
          [(1.0/step[:level]), step[:step]]
        end
      end

      def play_nested_pattern(pattern, *args)
        args_h = resolve_synth_opts_hash_or_array(args)
        args_h[:mode] = :notes unless args_h[:mode]
        args_h[:beat_length] = 1 unless args_h[:beat_length]

        if [:notes, :samples, :lambdas].none? {|x| x == args_h[:mode] }
          raise "Unrecognized :mode for play_nested_pattern. Must be one of [:notes, :samples, :lambdas]"
        end

        get_nested_beat(pattern).each do |mul, val|
          with_bpm_mul mul do
            case args_h[:mode]
            when :notes
              play val
            when :samples
              sample val
            when :lambdas
              val.call
            end
            sleep args_h[:beat_length] # this is scaled by the bpm mul
          end
        end
      end
      doc name:          :play_nested_pattern,
      introduced:    Version.new(2,8,0),
      summary:       "Play a nested pattern of notes, samples or lambdas",
      doc:           "Using a nested array to represent rhythm, you can use this method to structure melodies, beats and other rhythmic patterns.

  A nested array is just an array that contains other arrays inside it e.g. `[1, [2, 2], 1, 1]`. Here we can see three elements at the top level of the array (the ones) and two elements which are nested two arrays deep (the twos).

  We can use this nesting to play through the contents of the array at faster and faster rates. Things nested at the second level will play twice as fast, things nested at the third level will play four times as fast and so on.

  It might help to think about music notation - if the level 1 is a crotchet/quarter note, then level two is like a quaver/eighth note and so on, all the way down to hemidemisemiquavers and beyond.

  If you want to use a triplet rhythm you can use a special notation to spread across multiple beats e.g. `[:d5, :cs5, {over: 2, val: [:c5,:c5,:c5]}, :b4, :bb4, :a4]`. This spaces the three `:c5` notes over the space of two normal notes which gives you a quaver triplet rhythm. You might recognize this from Bizet's opera Carmen.",
      args:          [[:pattern]],
      opts:          {beat_length: "Length of a single (top level) beat - defaults to 1",
        mode: "One of `:notes`, `:samples` or `:lambdas` depending on what is in your nested pattern. See examples below."},
      accepts_block: false,
      examples:      ["
      play_nested_pattern [:c, [:d, :f], :e, :c]
                                # Same as:
                                #   play :c
                                #   sleep 1
                                #   play :d
                                #   sleep 0.5
                                #   play :f
                                #   sleep 0.5
                                #   play :e
                                #   sleep 1
                                #   play :c
                                #   sleep 1
      ","
      # We can also use sample names with `:samples` as the mode option
      rock_you_pattern =
        [:bd_haus, :elec_snare, [:bd_haus, :bd_haus], :elec_snare]

      play_nested_pattern(rock_you_pattern, mode: :samples) # Same as:
                                #   sample :bd_haus
                                #   sleep 1
                                #   sample :elec_snare
                                #   sleep 1
                                #   sample :bd_haus
                                #   sleep 0.5
                                #   sample :bd_haus
                                #   sleep 0.5
                                #   sample :elec_snare
                                #   sleep 1
      ","
      # We can also use lambdas with `:lambdas` as the mode option
      rand_notes = lambda {
        notes = scale(:c3, :minor_pentatonic, num_octaves: 3)
        play_pattern_timed(notes.shuffle.take(3), 0.33)
      }
      random_pattern = [rand_notes, rand_notes, [rand_notes, rand_notes], rand_notes]
      loop do
        # because we already sleep inside play_pattern_timed,
        # we set the :beat_length to zero
        play_nested_pattern(random_pattern, mode: :lambdas, beat_length: 0)
      end
      ","
      # Triplets and cross rhythms
      # By using a hash with :over and :val keys, we can spread, for example, three notes over the space of two
      # This creates the equivalent of triplets in music
      carmen_pattern = [:d5, :cs5, {over: 2, val: [:c5,:c5,:c5]}, :b4, :bb4, :a4]
      # This spaces the three `:c5` notes (our `:val`) over the space of two (our `:over`) normal notes which gives you a quaver triplet rhythm.
      # You might recognize this from Bizet's opera Carmen.
      play_nested_pattern(carmen_pattern, beat_length: 0.5)
      ",
        "
      # Advanced version - a randomised drum beat
      live_loop :beatz do
        use_bpm 120

        bd = lambda { sample :bd_haus, rate: 2}
        sn = lambda { sample :drum_snare_hard, rate: 4 }
        hh = lambda { sample :drum_cymbal_closed, rate: rrand(3,4)}
        rest = lambda { nil }

        drumbeat = [bd, sn, [bd, [bd,bd]], [sn, hh]]
        drumbreak = [[bd,bd,bd,bd], {over: 2, val: [sn,sn,sn]}, [rest,hh,rest,hh]]

        3.times do
          play_nested_pattern(drumbeat, mode: :lambdas)
        end
        # We use .shuffle to get a different break each time
        play_nested_pattern(drumbreak.shuffle, mode: :lambdas)
      end"]

    end
  end
end
