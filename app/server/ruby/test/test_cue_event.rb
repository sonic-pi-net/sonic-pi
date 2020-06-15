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
require_relative "../lib/sonicpi/cueevent"

module SonicPi
  class CueEventTester < Minitest::Test
    def test_init
      t = Time.now
      p = 0
      i = 0
      d = 0
      b = 0
      m = 60
      v = [:a, :b, :c]
      n = "/foo/bar"
      c = CueEvent.new(t, p, i, d, b, m, n, v)
      assert_equal t, c.time
      assert_equal i, c.thread_id
      assert_equal d.to_i, c.delta
      assert_equal b.to_i, c.beat
      assert_equal v.ring, c.val.ring
      assert_equal Array, c.val.class
    end

    def test_non_thread_safe_init
      t = Time.now
      p = 0
      i = 0
      d = 0
      b = 0
      m = 60
      n = "/foo/bar"
      a = [:a, :b, Object.new]
      assert_raises SonicPi::Core::NotThreadSafeError do
        CueEvent.new(t, p, i, d, b, m, n, a)
      end
    end

    def test_ordering
      t1 = Time.now
      p = 0
      sleep 0.001
      t2 = Time.now
      sleep 0.001
      t3 = Time.now
      i = 0
      d1 = 0
      d2 = 1
      m = 60
      a = [:a, :b, :c]
      n = "/foo/bar"
      n2 = "/z/foo/bar"
      c1 = CueEvent.new(t1, p, i, d1, 0, m, n, a )
      c1clone = CueEvent.new(t1, p, i, d1, 0, m, n, a )
      c1diff = CueEvent.new(t1, p, i, d1, 0, m, n, [:foo])
      c2 = CueEvent.new(t2, p, i, d1, 0, m, n, a)
      c3 = CueEvent.new(t3, p, i, d1, 0, m, n, a)
      c3p2 = CueEvent.new(t3, p, i, d2, 0, m, n2, a)
      assert c1 < c2
      assert c1 < c3
      assert c2 < c3
      assert c1 == c1
      assert c1 == c1clone
      assert c1 != c1diff
      assert c3 < c3p2
    end

    def test_path_splitting
      t = Time.now
      p = 0
      i = 0
      d = 0
      m = 60
      a = [:a, :b, :c]
      n = "/foo/bar"
      c = CueEvent.new(t, p, i, d, 0, m, n, a)
      assert_equal ["foo", "bar"], c.split_path
    end

    def test_path_segment
      t = Time.now
      p = 0
      i = 0
      d = 0
      m = 60
      a = [:a, :b, :c]
      n = "/foo/bar/baz"
      c = CueEvent.new(t, p, i, d, 0, m, n, a)
      assert_equal "foo", c.path_segment(0)
      assert_equal "bar", c.path_segment(1)
      assert_equal "baz", c.path_segment(2)
      assert_equal nil, c.path_segment(3)
    end

  end

end
