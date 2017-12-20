## encoding: utf-8
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016, 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative 'support/docsystem'
require_relative 'core'
require_relative "../version"
require_relative "../util"


module SonicPi
  module Core
    class PatternVector < RingVector
      def tick(*args)
        super(*args)[:val]
      end

      def look(*args)
        super(*args)[:val]
      end

      def tatum(*args)
        idx = SonicPi::Core::ThreadLocalCounter.look(*args)
        self[idx][:beats_to_sleep_for]
      end

      # TODO: implement .play_each, .sample_each and .call_each
      # The three obvious use cases are patterns made of notes,
      # samples and lambdas. It would be nice to define a shorthand
      # for playing each event in a loop like so:
      #
      #    def play_each(*args)
      #      self.each do |step|
      #        SonicPiLang.play step[:val], *args
      #        SonicPiLang.sleep step[:beats_to_sleep_for]
      #      end
      #    end
      #
      # however this is difficult as play, sample and sleep aren't
      # in scope here.

      def ___sp_vector_name
        "pattern"
      end
    end
  end
end

module SonicPi
  module Lang
    module Pattern

      include SonicPi::Util
      include SonicPi::Lang::Support::DocSystem

      def pattern(pattern, *args)
        args_h = resolve_synth_opts_hash_or_array(args)
        args_h[:beat_length] = 1 unless args_h[:beat_length]

        SonicPi::Core::PatternVector.new(nested_beat(pattern,
                                                     calculate_number_of_beats(pattern)).flatten)
      end
      doc name:          :pattern,
      introduced:    Version.new(3,1,0),
      summary:       "Create patterns of values",
      doc:           "Using a nested array to represent rhythm, you can use this method to structure melodies, beats and other rhythmic patterns.

  Patterns are a special kind of ring - values are accessed like a normal ring with tick and look. The sleep timings are accessed with a special method called tatum.

  A nested array is just an array that contains other arrays inside it e.g. `[1, [2, 2], 1, 1]`. Here we can see three elements at the top level of the array (the ones) and two elements which are nested two arrays deep (the twos).

  We can use this nesting to play through the contents of the array at faster and faster rates. Things nested at the second level will play twice as fast, things nested at the third level will play four times as fast and so on.

  It might help to think about music notation - if the level 1 is a crotchet/quarter note, then level two is like a quaver/eighth note and so on, all the way down to hemidemisemiquavers and beyond.

  If you want to use a triplet rhythm you can use a special notation to spread across multiple beats e.g. `[:d5, :cs5, {over: 2, val: [:c5,:c5,:c5]}, :b4, :bb4, :a4]`. This spaces the three `:c5` notes over the space of two normal notes which gives you a quaver triplet rhythm. You might recognize this from Bizet's opera Carmen.

  For advanced usage, you can iterate over the events in a pattern. Each event yields a hash containing a `:val` key and a `:beats_to_sleep_for` key. See the advanced example below for more information on how to use this.",
      args:          [[:pattern]],
      opts:          {num_beats: "Number of beats in the pattern - defaults to 4"},
      accepts_block: false,
      examples:      ["
      pat = pattern([:c, [:d, :f], :e, :c])
      loop do
        play pat.tick
        sleep pat.tatum
      end
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
        pattern([:bd_haus, :elec_snare, [:bd_haus, :bd_haus], :elec_snare])

      loop do
        sample rock_you_pattern.tick
        sleep rock_you_pattern.tatum
      end # Same as:
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
      # Triplets and cross rhythms
      # By using a hash with :over and :val keys, we can spread, for example, three notes over the space of two
      # This creates the equivalent of triplets in music
      carmen_pattern = pattern([:d5, :cs5, {over: 2, val: [:c5,:c5,:c5]}, :b4, :bb4, :a4, nil])
      # This spaces the three `:c5` notes (our `:val`) over the space of two (our `:over`) normal notes which gives you a quaver triplet rhythm.
      # You might recognize this from Bizet's opera Carmen.
      loop do
        play carmen_pattern.tick
        sleep carmen_pattern.tatum
      end
      ",
        "
      # Advanced version - a randomised drum beat
      live_loop :beatz do
        use_bpm 120

        bd = lambda { sample :bd_haus, rate: 2}
        sn = lambda { sample :drum_snare_hard, rate: 4 }
        hh = lambda { sample :drum_cymbal_closed, rate: rrand(3,4)}
        rest = lambda { nil }

        drumbeat = pattern([bd, sn, [bd, [bd,bd]], [sn, hh]])
        drumbreak = pattern([[bd,bd,bd,bd], {over: 2, val: [sn,sn,sn]}, [rest,hh,rest,hh]])

        3.times do
          drumbeat.each do |step|
            step[:val].call
            sleep step[:beats_to_sleep_for]
          end
        end
        # We use .shuffle to get a different break each time
        drumbreak.shuffle.each do |step|
          step[:val].call
          sleep step[:beats_to_sleep_for]
        end
      end"]

      private
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
          {beats_to_sleep_for: beat_length, val: el}
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

    end
  end
end
