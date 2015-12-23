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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi
  class ChanceTester < Minitest::Test
    include SonicPi::Lang::Core

    def test_one_in_out_of_bounds
      500.times do
        assert_equal(false, one_in(0))
        assert_equal(false, one_in(-1))
      end
    end
  end
end
