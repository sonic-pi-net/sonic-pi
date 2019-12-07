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

require_relative "../../setup_test"
require 'mocha/setup'

require_relative "../../../lib/sonicpi/util"
require_relative "../../../lib/sonicpi/lang/sound"
require_relative "../../../lib/sonicpi/lang/core"
require_relative "../../../lib/sonicpi/lang/pattern"

module SonicPi
  class PlayNestedPatternTester < Minitest::Test
    def setup
      @mock_sound = Object.new
      @mock_sound.extend(Lang::Sound)
      @mock_sound.extend(Lang::Core)
      @mock_sound.extend(Lang::Pattern)
      @mock_sound.stubs(:sleep) # avoid loading Spider class
    end

    def test_nested_pattern_basic
      # We're testing that the maths for
      # nested patterns works, so we can
      # ignore sleep and play for now
      @mock_sound.stubs(:play)

      test_pattern = [:c, [:d, :f], :e, :c]
      bpm_muls     = [ 1,   2,  2,   1,  1]

      @mock_sound.expects(:with_bpm_mul).at_most(5).with do |bpm_mul|
        bpm_mul == bpm_muls.shift
      end
      @mock_sound.play_nested_pattern(test_pattern)
    end

    def test_nested_pattern_triplet
      # We're testing that the maths for
      # nested patterns works, so we can
      # ignore sleep and play for now
      @mock_sound.stubs(:play)

      test_pattern = [:c, {over: 2, val: [:d,  :f,  :a]}, :c]
      bpm_muls     = [ 1,                  1.5, 1.5, 1.5,  1]

      @mock_sound.expects(:with_bpm_mul).times(5).with do |bpm_mul|
        bpm_mul == bpm_muls.shift
      end
      @mock_sound.play_nested_pattern(test_pattern)
    end

    def test_nested_pattern_modes
      @mock_sound.expects(:play).with(:c).times(4)
      @mock_sound.play_nested_pattern([:c, :c, :c, :c], mode: :notes)

      @mock_sound.expects(:sample).with(:loop_amen).times(2)
      @mock_sound.play_nested_pattern([:loop_amen, [:loop_amen]], mode: :samples)

      test_lambda = lambda { "Doing stuff" }
      test_lambda.expects(:call).times(2)
      @mock_sound.play_nested_pattern([test_lambda, [test_lambda]], mode: :lambdas)
    end

    def test_nested_pattern_beat_length
      @mock_sound.expects(:play).with(:c).times(4)
      @mock_sound.expects(:sleep).with(0.5).times(4)
      @mock_sound.play_nested_pattern([:c, [:c, :c], :c], beat_length: 0.5)
    end

    def test_nested_pattern_edge_cases
      @mock_sound.expects(:play).with(:c).never
      @mock_sound.play_nested_pattern([])
    end

    def test_nested_pattern_with_hashes
      test_hash = {note: :c, release: 0.1}
      @mock_sound.expects(:play).with(test_hash).times(4)
      @mock_sound.play_nested_pattern([test_hash, [test_hash, test_hash], test_hash])
    end

    def test_nested_pattern_crazy_nesting
      # We're testing that the maths for
      # nested patterns works, so we can
      # ignore sleep and play for now
      @mock_sound.stubs(:play)

      # This is a 4 beat bar
      # because there are 4 top level beats
      # NB the over: 2 counts as two beats
      test_pattern = [:c, {over: 2, val: [:d,  :f,  :a]}, [:c, [:c, :c, [:c, :c, :c, {over: 2, val: [:c, :c, :c]}]]]]
      bpm_muls     = [ 1,                  1.5, 1.5, 1.5,   2,   6,  6,  30, 30, 30,                 45, 45, 45]

      # To prove test is correct
      assert_equal(test_pattern.length + 1, 4) # account for the :over beat
      # The bpm muls add up to 4 beats
      assert_equal(bpm_muls.reduce(0) {|acc, el| acc + (1.0/el) }.round(6), 4)

      bpm_muls.each do |mul|
        @mock_sound.expects(:with_bpm_mul).with(mul)
      end

      @mock_sound.play_nested_pattern(test_pattern)
    end
  end
end
