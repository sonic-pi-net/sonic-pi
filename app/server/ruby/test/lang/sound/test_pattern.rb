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

require_relative "../../setup_test"
require 'mocha/setup'

require_relative "../../../lib/sonicpi/lang/pattern"

module SonicPi
  class PlayNestedPatternTester < Minitest::Test
    include SonicPi::Lang::Pattern

    def test_pattern_basic
      test_pattern = pattern([:c, [:d, :f], :e, :c])
      exp_lengths  =         [ 1, 0.5, 0.5,  1,  1]

      # patterns are flattened internally so length
      # becomes a count of the number of elements in the pattern
      assert_equal(test_pattern.length, 5)

      # The beat_lengths add up to 4 beats
      assert_equal(test_pattern.to_a.reduce(0) {|acc, el| acc + el[:beats_to_sleep_for] }, 4)
    end

    def test_nested_pattern_triplet
      test_pattern = pattern([:c, {over: 2, val: [:d,  :f,  :a]}, :c])
      exp_lengths  = [ 1,                 1.3333, 1.3333, 1.3333,  1]

      assert_equal(test_pattern.to_a.reduce(0) {|acc, el| acc + el[:beats_to_sleep_for] }.round(2), 4.0)
    end

    def test_pattern_edge_cases
      assert_equal pattern([]).to_a, []
    end

    def test_nested_pattern_crazy_nesting
      # This is a 4 beat bar
      # because there are 4 top level beats
      # NB the over: 2 counts as two beats
      test_pattern = [:c, {over: 2, val: [:d,  :f,  :a]}, [:c, [:c, :c, [:c, :c, :c, {over: 2, val: [:c, :c, :c]}]]]]
      bpm_muls     = [ 1,                  1.5, 1.5, 1.5,   2,   6,  6,  30, 30, 30,                 45, 45, 45]

      # To prove test is correct
      assert_equal(test_pattern.length + 1, 4) # account for the :over beat
      # The bpm muls add up to 4 beats
      assert_equal(bpm_muls.reduce(0) {|acc, el| acc + (1.0/el) }.round(6), 4)

      assert_equal(pattern(test_pattern).to_a.reduce(0) {|acc, e|
        acc + e[:beats_to_sleep_for]
      }.round(2), 4.0)
    end
  end
end
