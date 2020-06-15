#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"
require 'mocha/setup'

module SonicPi

  class SleepTester < Minitest::Test
    def setup
      @lang = SonicPi::MockLang.new
    end

    def test_basic_sleep
      @lang.run  do
        sleep 2
        assert_equal(__current_local_run_time, 2.0)
        sleep 1
        assert_equal(__current_local_run_time, 3.0)
      end
    end

    def test_bpm_scaled_sleep
      @lang.run do
        use_bpm 120

        sleep 2
        assert_equal(__current_local_run_time, 1.0)
        sleep 1
        assert_equal(__current_local_run_time, 1.5)
      end
    end

  end
end
