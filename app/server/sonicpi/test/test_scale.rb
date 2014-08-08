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
require_relative "../lib/sonicpi/scale"

module SonicPi
  class NoteTester < Test::Unit::TestCase

    def test_tonic_capitalisation_should_make_no_difference
      assert_equal(Scale.new(:fs,:major,2), Scale.new(:Fs, :major, 2), Scale.new(:FS, :major, 2))
    end
  end
end
