#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/thread_id"

module SonicPi
  class ThreadIdTester < Minitest::Test
    def test_init
      v = ThreadId.new(3)
      assert_equal [3], v.ids
      assert v.ids.frozen?
    end

    def test_insert
      v = ThreadId.new(3, 5, 7)
      assert_equal [3, 5, 7], v.ids

      v2 = v << 9
      assert_equal [3, 5, 7, 9], v2.ids
      assert_equal [3, 5, 7], v.ids
    end

    def test_ordering
      v1 = ThreadId.new(3, 5, 7)
      v2 = ThreadId.new(3, 5, 8)
      v3 = ThreadId.new(3, 5, 6)
      v4 = ThreadId.new(3, 5)
      v5 = ThreadId.new(3, 5, 6, 1)
      v6 = ThreadId.new(3, 5, 6, 1, 0)

      assert !(v2 < v1)
      assert (v1 < v2)
      assert v5 > v3
      assert v5 < v6

      orig    = [v1, v2, v3, v4, v5, v6]
      ordered = [v4, v3, v5, v6, v1, v2]
      assert_equal ordered, orig.sort
    end

    def test_equality
      v1 = ThreadId.new(3, 5, 7)
      v2 = ThreadId.new(3, 5, 7)
      v3 = ThreadId.new(3, 5, 8)

      assert_equal v1, v2
      assert !(v1 == v3)
    end

    def test_serialization
      v1 = ThreadId.new(3, 5, 7)
      v1s = v1.serialize
      assert_equal "::-::thread-id::-::3_5_7", v1s

      v2 = ThreadId.new_from_serialized(v1s)

      assert_equal v1, v2
    end

  end
end
