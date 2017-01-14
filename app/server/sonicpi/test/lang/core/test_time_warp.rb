#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi
  class SonicPiMiniTest < MiniTest::Test

    class MockSonicPiLang
      include SonicPi::RuntimeMethods
      include SonicPi::Lang::Core
      include SonicPi::Lang::Sound

      def initialize
        @mod_sound_studio = Object.new
        @mod_sound_studio.stubs(:sched_ahead_time).returns(0.5)

        __set_default_user_thread_locals!
        now = Time.now.freeze
        __system_thread_locals.set :sonic_pi_spider_time, now
        __system_thread_locals.set :sonic_pi_spider_start_time, now
        __system_thread_locals.set :sonic_pi_spider_beat, 0
      end

      def __enqueue_multi_message(*args)
        # do nothing
      end
    end

    def setup
      @lang = MockSonicPiLang.new
    end

    def test_no_warp
      @lang.instance_eval do
        assert_equal(vt, 0)
        sleep 0.5
        assert_equal(vt, 0.5)
        time_warp do
          use_synth(:saw)
          assert_equal(vt, 0.5)
        end
        assert_equal(:saw, current_synth)
      end
    end

    def test_no_warp_again
      @lang.instance_eval do
        assert_equal(vt, 0)
        sleep -0.1
        assert_equal(vt, -0.1)
        time_warp do
          use_synth(:tri)
          assert_equal(vt, -0.1)
        end
        assert_equal(:tri, current_synth)
      end
    end

    def test_warp
      @lang.instance_eval do
        assert_equal(vt, 0)
        sleep -0.1
        assert_equal(vt, -0.1)
        time_warp 0.1 do
          use_synth(:tri)
          assert_equal(vt, 0)
        end
        assert_equal(:tri, current_synth)
      end
    end

    def test_multi_warp
      @lang.instance_eval do
        assert_equal(vt, 0)
        sleep -0.1
        assert_equal(vt, -0.1)
        time_warp 0.1 do
          assert_equal(vt, 0)
          time_warp 0.1 do
            assert_similar(vt, 0.1)
            time_warp -0.15 do
              assert_similar(vt, -0.05)
              time_warp 3 do
                time_warp -2 do
                  assert_similar(vt, 0.95)
                  use_synth(:tri)
                end
              end
            end
          end
        end
        assert_equal(:tri, current_synth)
      end
    end
  end
end
