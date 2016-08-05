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
require_relative "../../../lib/sonicpi/runtime"
require_relative "../../../lib/sonicpi/util"
require 'mocha/setup'

module SonicPi

  class SleepTester < Minitest::Test
    include SonicPi::Lang::Core
    include SonicPi::RuntimeMethods
    include SonicPi::Util

    def setup
      Kernel.stubs(:sleep)

      @mod_sound_studio = mock()
      @mod_sound_studio.expects(:sched_ahead_time).at_least_once.returns(0.5)

      @start_time = Time.now.freeze

      __system_thread_locals.set(:sonic_pi_spider_time, @start_time)
      __system_thread_locals.set(:sonic_pi_spider_start_time, @start_time)

      SleepTester.any_instance.stubs(:__schedule_delayed_blocks_and_messages!).returns(true)

      use_bpm 60
    end

    def test_basic_sleep
      sleep 2
      assert_equal(__current_local_run_time, 2.0)
      sleep 1
      assert_equal(__current_local_run_time, 3.0)
    end

    def test_bpm_scaled_sleep
      use_bpm 120

      sleep 2
      assert_equal(__current_local_run_time, 1.0)
      sleep 1
      assert_equal(__current_local_run_time, 1.5)
    end

  end
end
