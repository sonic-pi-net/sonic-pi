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

require 'test/unit'
require_relative "../lib/sonicpi/chord"

module SonicPi
  class ChordTester < Test::Unit::TestCase

    def test_resolution_of_basic_major
      assert_equal(Chord.new(:C4, :major), [60, 64, 67])
      assert_equal(Chord.new(60, :major), [60, 64, 67])
    end
  end
end
