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
require 'mocha/setup'

module SonicPi
  class SonicPiTimeWarpTest < MiniTest::Test
    class TestError < StandardError ; end

    def setup
      @lang = SonicPi::MockLang.new
    end

    def test_no_warp
      @lang.run do
        assert_equal(vt, 0)
        sleep 0.1
        assert_equal(vt, 0.1)
        time_warp do
          use_synth(:saw)
          assert_equal(vt, 0.1)
        end
        assert_equal(:saw, current_synth)
      end
    end

    def test_no_warp_again
      @lang.run do
        assert_equal(vt, 0)
        sleep(-0.1)
        assert_equal(vt, -0.1)
        time_warp do
          use_synth(:tri)
          assert_equal(vt, -0.1)
        end
        assert_equal(:tri, current_synth)
      end
    end

    def test_warp
      @lang.run do
        assert_equal(vt, 0)
        sleep(-0.1)
        assert_equal(vt, -0.1)
        time_warp 0.1 do
          use_synth(:tri)
          assert_equal(vt, 0)
        end
        assert_equal(:tri, current_synth)
      end
    end

    def test_multi_warp
      @lang.run do
        assert_equal(vt, 0)
        sleep(-0.1)
        assert_equal(vt, -0.1)
        time_warp 0.1 do
          assert_equal(vt, 0)
          time_warp 0.1 do
            assert_similar(vt, 0.1)
            time_warp(-0.15) do
              assert_similar(vt, -0.05)
              time_warp 3 do
                time_warp(-2) do
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

    def test_exception_handling
      @lang.run do
        assert_error TestError do
          time_warp 0.1 do
            raise TestError
          end
        end
      end
    end
  end
end
