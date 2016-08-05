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

require_relative "./setup_test"

module SonicPi
  class VersionTester < Minitest::Test

    def test_basic_setting_and_getting
      t = SonicPi::Core::ThreadLocal.new
      assert_equal(t.get(:foo), nil)
      t.set(:foo, 1)
      assert_equal(t.get(:foo), 1)
      t.set(:foo, 3)
      assert_equal(t.get(:foo), 3)
    end

    def test_local_setting
      t = SonicPi::Core::ThreadLocal.new
      assert_equal(t.get(:foo), nil)
      t.set_local(:foo, 1)
      assert_equal(t.get(:foo), 1)
      t.set(:foo, 3)
      assert_equal(t.get(:foo), 3)
    end

    def test_inheritance
      t = SonicPi::Core::ThreadLocal.new
      t.set(:foo, 1)
      assert_equal(t.get(:foo), 1)
      t2 = SonicPi::Core::ThreadLocal.new(t)
      assert_equal(t2.get(:foo), 1)
      t.set_local(:foo, 10)
      assert_equal(t.get(:foo), 10)
      assert_equal(t2.get(:foo), 1)
    end

    def test_local_setting_is_not_inherited
      t = SonicPi::Core::ThreadLocal.new
      t.set_local(:foo, 1)
      assert_equal(t.get(:foo), 1)
      t2 = SonicPi::Core::ThreadLocal.new(t)
      t2.set(:bar, 3)
      assert_equal(t2.get(:bar), 3)
      assert_equal(t2.get(:foo), nil)
    end
  end
end
