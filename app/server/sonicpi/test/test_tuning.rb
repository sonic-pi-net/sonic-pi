#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'test/unit'
require_relative "../../core"
require_relative "../lib/sonicpi/note"
require_relative "../lib/sonicpi/tuning"


module SonicPi
  class TuningTester < Test::Unit::TestCase

    def test_just
      assert_equal(63.86313713844061, Tuning.new.resolve_tuning(:e4, :just, :c))
      assert_equal(63.86313713844061, Tuning.new.resolve_tuning(64, :just, :c))

      # slightly different value due to midi -> hz -> conversions
      assert_equal(63.86313713823286, Tuning.new.resolve_tuning(64.0, :just, :c))
    end
  end
end
