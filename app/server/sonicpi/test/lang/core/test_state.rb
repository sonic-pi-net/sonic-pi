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
require 'mocha/setup'

module SonicPi

  class StateTester < Minitest::Test
    def setup
      @state = SonicPi::State.new
      @time = Time.now
    end

    def test_basic_get
      @state.reset!
      assert_equal nil, @state.get(:foo, @time + 0)
      assert_equal :bar, @state.get(:foo, @time + 0, :bar)
      assert_equal :bar, @state.get(:foo, @time + 10, :bar)
      assert_equal :bar, @state.get(:foo, @time - 10, :bar)
    end

    def test_basic_set
      @state.reset!
      @state.set(:foo, @time, :bar)
      assert_equal :bar, @state.get(:foo, @time, :quux)
    end

    def test_multi_set
      @state.reset!
      @state.set(:foo, @time + 1, :bar)
      @state.set(:foo, @time + 3, :quux)
      assert_equal nil, @state.get(:foo, @time + 0)
      assert_equal :bar, @state.get(:foo, @time + 1)
      assert_equal :bar, @state.get(:foo, @time + 2)
      assert_equal :quux, @state.get(:foo,@time + 3)
      assert_equal :quux, @state.get(:foo,@time + 4)
    end

    def test_pruning
      @state.reset!
      @state.set(:foo, @time - 20000, :bar, @time)
      @state.set(:foo, @time - 10000, :quux, @time)
      assert_equal nil, @state.get(:foo, @time - 150000)
    end
  end
end
