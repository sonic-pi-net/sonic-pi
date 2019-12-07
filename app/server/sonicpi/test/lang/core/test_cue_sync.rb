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
require_relative "../../../lib/sonicpi/lang/core"


module SonicPi
  class CueSyncTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end


    def test_time_increment
      @lang.run do

        assert_equal 0, vt
        sleep 0.05
        assert_equal 0.05, vt

        in_thread do
          sleep 0.1
          cue :foo
        end

        sync :foo

        assert_equal 0.15,  vt
        sleep 0.1
        assert_equal 0.25,  vt

        in_thread do
          sleep 0.1
          cue :foo
        end

        sync :foo

        assert_equal 0.35,  vt

        in_thread do
          sleep 0.02
          cue :foo
        end

        sync :foo

        assert_equal 0.37,  vt
      end
    end

    # syncing should return the 'next' event from the current
    # time.

    def test_time_increment_w_faster_bpm
      @lang.run do
        use_bpm 120
        assert_equal 0, vt
        sleep 0.1
        assert_equal 0.1 / 2, vt

        in_thread do
          sleep 0.05
          cue :foo
        end

        sync :foo

        assert_equal 0.15 / 2,  vt
        sleep 0.02
        assert_equal 0.17 / 2,  vt
        in_thread do
          sleep 0.04
          cue :foo
        end

        sync :foo

        assert_equal 0.21 / 2,  vt

        in_thread do
          sleep 0.05
          cue :foo, 2
        end

        in_thread do
          sleep 0.05
          cue :foo, 3
        end

        v = sync :foo

        assert_equal 0.26 / 2, vt
        assert_equal 2, v[0]
      end
    end

  end
end
