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
require_relative "../lib/sonicpi/version"

module SonicPi
  class VersionTester < Minitest::Test

    def test_version_init
      v = Version.new(2, 1, 0)
      assert_equal(2, v.major)
      assert_equal(1, v.minor)
      assert_equal(0, v.patch)
    end

    def test_version_init_with_dev
      v = Version.new(2, 1, 0, "RC12")
      assert_equal(2, v.major)
      assert_equal(1, v.minor)
      assert_equal(0, v.patch)
      assert_equal("RC12", v.dev)
    end

    def test_equality
      v1 = Version.new(2, 1, 0, "RC12")
      v2 = Version.new(2, 1, 0, "RC12")
      assert_equal(v1, v2)
    end

    def test_inequality
      v1 = Version.new(2, 1, 0, "RC12")
      v2 = Version.new(2, 1, 0, "RC11")
      v3 = Version.new(2, 1, 1, "RC12")
      v4 = Version.new(1, 1, 0, "RC12")
      refute_equal(v1, v2)
      refute_equal(v1, v3)
      refute_equal(v1, v4)
    end

    def test_less_than
      v1 = Version.new(2, 1, 1)
      v2 = Version.new(2, 1, 0)
      assert_equal(true, v2 < v1)
    end

    def test_to_i
      v3 = Version.new(2, 11, 1)
      v4 = Version.new(3, 0, 0, "beta")
      assert_equal(true, v3 < v4)
      assert_equal(true, v3.to_i < v4.to_i)
    end

    def test_greater_than
      v1 = Version.new(2, 1, 1)
      v2 = Version.new(2, 1, 0)
      assert_equal(true, v1 > v2)
    end

    def test_greater_than_with_dev
      v1 = Version.new(2, 1, 1)
      v2 = Version.new(2, 1, 1, "dev")
      assert_equal(true, v1 > v2)
    end

    def test_less_than_with_dev
      v1 = Version.new(2, 1, 1, "dev")
      v2 = Version.new(2, 1, 1)
      assert_equal(true, v1 < v2)
    end

    def test_less_than_with_both_dev
      v1 = Version.new(2, 1, 1, "a")
      v2 = Version.new(2, 1, 1, "b")
      assert_equal(true, v1 < v2)
    end

    def test_greater_than_with_both_dev
      v1 = Version.new(2, 1, 1, "c")
      v2 = Version.new(2, 1, 1, "a")
      assert_equal(true, v1 > v2)
    end

    def test_less_than_or_equal_equality
      v1 = Version.new(2, 1, 0, "RC12")
      v2 = Version.new(2, 1, 0, "RC12")
      assert_equal(true, v1 <= v2)
      assert_equal(true, v2 <= v1)
    end

    def test_string_init
      v1 = Version.new(2, 1, 0, "RC12")
      v2 = Version.init_from_string("v2.1.0-RC12")
      assert_equal(v1, v2)

      v1 = Version.new(2, 1, 0)
      v2 = Version.init_from_string("v2.1.0")
      assert_equal(v1, v2)

      v1 = Version.new(2, 1)
      v2 = Version.init_from_string("v2.1")
      assert_equal(v1, v2)

      v1 = Version.new(2)
      v2 = Version.init_from_string("v2")
      assert_equal(v1, v2)
    end

  end
end
