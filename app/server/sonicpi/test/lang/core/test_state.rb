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
require_relative "../../../lib/sonicpi/state"


module SonicPi

  class StateTester < Minitest::Test
    def setup
      @lang = SonicPi::MockLang.new
    end

    def test_get_with_no_set
      @lang.instance_eval do
        assert_equal nil, get(:foodbarbaz)
      end
    end

    def test_get_defaults
      @lang.run do
        assert_equal :bar, get(:foo, :bar)
      end
    end

    def test_basic_set
      @lang.run do
        set(:foo, 1)
        get(:foo)
        assert_equal 1, get(:foo, 999)
        set(:foo, 300)
        get(:foo)
        assert_equal 300, get(:foo, 9998)
      end
    end

    def test_time_warp_future_setting
      @lang.run do
        assert_equal nil, get(:intensity)
        20.times do
          t2 = in_thread do
            assert_equal get(:intensity), 100
            sleep 0.03
            assert_equal get(:intensity), 102
          end


          t1 = in_thread do
            set(:intensity, 100)
            sleep 0.03
            set(:intensity, 102)

          end

          t2.join
          t1.join

          sleep 0.05
        end
      end
    end


    def test_time_warp_future_setting_further
      @lang.run do
        assert_equal nil, get(:intensity)

        10.times do
          sleep 0.1
          time_warp [0.005, 0.01] do |v|
            set(:intensity, (ring 100, 900).tick)
          end

          sleep 0.005
          assert_equal get(:intensity), 100
          sleep 0.005
          assert_equal get(:intensity), 900
          sleep 0.005
          assert_equal get(:intensity), 900
          sleep -0.01
          assert_equal get(:intensity), 100
          sleep 0.005
          assert_equal get(:intensity), 900
        end
      end
    end

    def test_get_inside_future_time_warp_throws_an_error
      @lang.run do
        time_warp 0.1 do
          set(:amp, 0.7)
          assert_error SonicPi::Lang::Core::TimingError do
            get(:amp)
          end
        end
      end
    end

    def test_get_inside_past_time_warp
      @lang.run do
        sleep 0.05
        assert_equal get(:amp, :default), :default
        set :amp, 0.4
        assert_equal get(:amp, :default), 0.4
        sleep 0.05
        set :amp, 0.8
        assert_equal get(:amp), 0.8
        time_warp -0.1 do
          v = get(:amp)
#          raise "yoohoo"
          #assert_equal v, 0.5
        end
      end
    end

    def test_multi_sets
      @lang.run do
        set(:foo, :bar)
        set(:foo, :baz)
        assert_equal :baz, get(:foo, :default)
      end
        end

    def test_osc_sync_no_prior_vals
      @lang.run do
        p = Promise.new
        in_thread do
          res = sync_osc "/barbazquuux"
          p.deliver! res
        end
        assert_error do
          p.get(1)
        end
      end
    end

    def test_osc_sync
      @lang.run do
        p1 = Promise.new
        p2 = Promise.new

        in_thread do
          res = sync_osc "/foo"
          p1.deliver! res
        end

        in_thread do
          res = sync_osc "/foo"
          p2.deliver! res
        end
        sleep 0.01
        time_warp 0.1 do
          @osc_state.set current_time, 0, "/foo", [2, 4, 6]
        end
        time_warp 0.2 do
          @osc_state.set current_time, 0, "/foo", [1, 2, 3]
        end

        assert_equal p1.get(1), [2, 4, 6]
        assert_equal p2.get(1), [2, 4, 6]
      end
    end


  end
end
