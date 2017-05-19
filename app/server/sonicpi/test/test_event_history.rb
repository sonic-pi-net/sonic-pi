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


require_relative "./setup_test"
require_relative "../lib/sonicpi/cueevent"
require_relative "../lib/sonicpi/event_history"

module SonicPi
  class CueEventTester < Minitest::Test


    def test_basic_set_get_w_sym
      history = EventHistory.new
      t = 1495180483.9735098
      p = :foobar
      history.set(0, 0, 0, 0, p, :foo, {})
      v = history.get(t, 0, 0, 0, :foobar)
      assert_equal :foo, v.val
    end

    def test_basic_set_get_with_arg_matcher
      history = EventHistory.new
      t = 0
      p = "/foo/bar"
      m = lambda do |args|
        args.size == 2 && args.first != :foo
      end

      history.set(t, 0, 0, 0, p, [:baz, :bar], {})
      history.set(t, 0, 1, 0, p, [:foo, :bar], {})
      history.set(t, 0, 2, 0, p, [:baz, :bar, :foo], {})
      history.set(t, 0, 3, 0, p, [:bar], {})

      v = history.get(t, 0, 5, 0, p, m)
      assert_equal [:baz, :bar], v.val
    end

    def test_deltas
      history = EventHistory.new
      t = 0
      p = "/foo/bar"
      history.set(t, 0, 0, 0, p, [:foo], {})
      history.set(t, 0, 1, 0, p, [:bar], {})
      v = history.get(t, 0, 0, 0, p)
      v2 = history.get(t, 0, 1, 0, p)
      v3 = history.get(t, 0, 0, 0, p)
      assert_equal [:foo], v.val
      assert_equal [:bar], v2.val
      assert_equal [:foo], v3.val
    end

    def test_multi_set_paths_get
      history = EventHistory.new
      t = 0
      p = "/foo/bar"
      p2 = "/foo/quux"
      history.set(t, 0, 0, 0, p, [:foo], {})
      history.set(t, 0, 0, 0, p2, [:bar], {})

      v = history.get(t, 0, 0, 0, p)
      v2 = history.get(t, 0, 0, 0, p2)

      assert_equal [:foo], v.val
      assert_equal [:bar], v2.val
    end

    def test_multi_set_paths_get_inverse_set_order
      history = EventHistory.new
      t = 0
      p = "/foo/bar"
      p2 = "/foo/quux"

      history.set(t, 0, 0, 0, p2, [:bar], {})
      history.set(t, 0, 0, 0, p, [:foo], {})

      v = history.get(t, 0, 0, 0, p)
      v2 = history.get(t, 0, 0, 0, p2)

      assert_equal [:foo], v.val
      assert_equal [:bar], v2.val
    end

    def test_star_matchers
      history = EventHistory.new
      t = 0
      p = "/foo/bar/baz"
      p2 = "/foo/bar/boz"
      p3 = "/foo/quux"

      history.set(t, 0, 0, 0, p, [:foo], {})
      history.set(t + 1, 0,0, 0, p2, [:bar], {})
      history.set(t, 0, 0, 0, p3, [:baz], {})

      v = history.get(t, 0, 0, 0, "/foo/bar/*")
      assert_equal [:foo], v.val
      v2 = history.get(t, 0, 0, 0, "/foo/*/quux")
      assert_nil  v2
      v3 = history.get(t, 0, 0, 0,"/*/*/boz")
      assert_nil  v3
      v4 = history.get(t + 1, 0, 0, 0, "/*/*/boz")
      assert_equal [:bar], v4.val
    end

    def test_newer_history_is_ignored
      # Get should return last seen version
      # (at or before the current time)
      history = EventHistory.new
      t = 0
      p = "/foo/bar/baz"

      history.set(t, 0, 0, 0, p, [:foo], {})
      history.set(t + 1, 0, 0, 0, p, [:bar], {})

      v = history.get(t, 0, 0, 0, p)
      v2 = history.get(t + 0.5, 0, 0, 0, p)
      v3 = history.get(t + 1, 0, 0, 0, p)

      assert_equal [:foo], v.val
      assert_equal [:foo], v2.val
      assert_equal [:bar], v3.val

    end

    def test_double_star_matcher_at_end
      history = EventHistory.new

      t = 0
      #t = 1
      p = "/foo/bar/baz"

      # t = 0
      p2 = "/foo/bar/boz"
      p3 = "/foo/quux"
      p4 = "/foo/quux/eggs"
      p5 = "/a"
      p6 = "/z"
      p7 = "/foo/quux/beans/milk"

      history.set(t, 0, 1, 0, p, [:baz], {})
      history.set(t, 0, 0, 0, p2, [:boz], {})
      history.set(t, 0, 2, 0, p3, [:quux], {})
      history.set(t, 0, 3, 0, p4, [:eggs], {})
      history.set(t, 0, 5, 0, p5, [:a], {})
      history.set(t, 0, 4, 0, p6, [:z], {})
      history.set(t, 0, 6, 0, p7, [:milk], {})

      v = history.get(t, 0, 0, 0, "/**")
      assert_equal [:boz], v.val

      v = history.get(t, 0, 0, 0, "/foo/**")
      assert_equal [:boz], v.val

      v = history.get(t, 1, 0, 0, "/foo/*/beans/**")
      assert_equal [:milk], v.val
    end

    def test_double_star_matcher_not_at_end
      history = EventHistory.new


      p1  = "/foo/bar/baz"
      p2 = "/foo/bar/boz"
      p3 = "/foo/quux"
      p4 = "/foo/quux/eggs"
      p5 = "/a"
      p6 = "/z"
      p7 = "/foo/quux/beans/milk"
      p8 = "/foo/cheese/beans/quorn"
      p9 = "/foo/cheese/bleans/quorn"

      history.set(0, 0, 0, 0, p1, [:baz], {})
      history.set(0, 0, 1, 0, p1, [:baz1], {})
      history.set(0, 0, 2, 0, p1, [:baz2], {})
      history.set(1, 0, 3, 0, p1, [:baz3], {})
      history.set(0, 0, 4, 0, p2, [:boz], {})
      history.set(0, 0, 5, 0, p3, [:quux], {})
      history.set(0, 0, 6, 0, p4, [:eggs], {})
      history.set(0, 0, 7, 0, p5, [:a], {})
      history.set(0, 0, 8, 0, p6, [:z], {})
      history.set(0, 0, 9, 0, p7, [:milk], {})
      history.set(0, 0, 10, 0, p8, [:quorn], {})
      history.set(1, 0, 11, 0, p9, [:quorn2], {})

      assert_nil history.get(0, 0, 0, 0, "/**/quux")
      assert_equal [:quux],   history.get(2, 0, 0, 0, "/**/**/**/quux").val
      assert_equal [:quux],   history.get(2, 0, 0, 0, "/**/quux").val
      assert_equal [:quux],   history.get(2, 0, 0, 0, "/**/*/quux").val
      assert_equal [:quux],   history.get(2, 0, 0, 0, "/*/**/quux").val
      assert_equal [:milk],   history.get(2, 0, 0, 0, "/*/**/quux/**").val
      assert_equal [:quorn],  history.get(2, 0, 0, 0, "/foo/*/beans/**").val
      assert_equal [:quorn],  history.get(2, 0, 0, 0, "/foo/**/beans/**").val
      assert_equal [:baz],    history.get(0, 0, 0, 0, "/**").val
      assert_equal [:baz],    history.get(0, 0, 0, 0, "/**/").val
      assert_equal [:baz],    history.get(0, 0, 0, 0, "**").val
      assert_equal [:baz],    history.get(0, 0, 0, 0, "**").val
      assert_equal [:baz1],   history.get(0, 0, 1, 0, "/**/").val
      assert_equal [:quorn2], history.get(2, 0, 0, 0, "/**/").val
      assert_equal [:baz],    history.get(0, 0, 0, 0, "/foo/**/").val
      assert_equal [:baz1],   history.get(0, 0, 1, 0, "/foo/**/").val
      assert_equal [:baz],    history.get(0, 0, 0, 0, "/foo/bar/baz").val
      assert_equal [:baz1],   history.get(0, 0, 1, 0, "/foo/bar/baz").val
      assert_equal [:baz2],   history.get(1, 0, 2, 0, "/foo/bar/baz").val
      assert_equal [:quorn2], history.get(2, 0, 0, 0, "/f*/*/b*/quorn").val
      assert_equal [:quorn2], history.get(2, 0, 0, 0, "/f*/*se/b*/quorn").val
    end

    def test_more_globs
      history = EventHistory.new


      p1 = "/foo/bar/baz"

      history.set(0, 0, 0, 0, p1, [:baz], {})
      history.set(0, 0, 1, 0, p1, [:baz1], {})

      assert_equal [:baz],  history.get(0, 0, 0, 0, p1).val
      assert_equal [:baz1], history.get(0, 0, 1, 0, p1).val
      assert_equal [:baz1], history.get(0, 0, 1, 0, "/f*/bar/*").val
      assert_equal [:baz],  history.get(0, 0, 0, 0, "/foo/bar/*").val
      assert_equal [:baz],  history.get(0, 0, 0, 0, "/fo*o/b*/*").val
      assert_equal [:baz1], history.get(0, 0, 1, 0, "/*/*/*").val
      assert_equal [:baz],  history.get(0, 0, 0, 0, "/*/*/*").val
      assert_equal [:baz],  history.get(0, 0, 0, 0, "/**").val
      assert_equal [:baz1], history.get(0, 0, 1, 0, "/**").val
      assert_equal [:baz1], history.get(0, 0, 1, 0, "/**/**").val
      assert_equal [:baz1], history.get(0, 0, 1, 0, "/foo/**/*").val
    end

    def test_basic_get_next
      history = EventHistory.new
      p1 = "/foo/bar/baz"
      history.set(0, 0, 0, 0, p1, [:baz0], {})
      history.set(0, 0, 1, 0, p1, [:baz1], {})
      history.set(1, 0, 0, 0, p1, [:baz2], {})
      assert_equal [:baz1], history.get_next(0, 0, 0, 0, p1).val
    end

    def test_get_next
      history = EventHistory.new

      p1 = "/foo/bar/baz"
      history.set(0, 0, 0, 0, p1, [:baz0], {})
      history.set(0, 0, 1, 0, p1, [:baz01], {})
      history.set(1, 0, 0, 0, p1, [:baz1], {})

      history.set(2, 0, 1, 0, p1, [:baz2], {})
      history.set(5, 0, 3, 0, p1, [:baz3], {})
      history.set(5, 0, 2, 0, "/foo/bar/beans/baaaz", [:baaaz2], {})
      history.set(5, 0, 3, 0, "/foo/bar/beans/baaaz", [:baaaz3], {})
      history.set(5, 0, 3, 0, "/foo/qux/beans/baaaz", [:baaaz31], {})
      history.set(5, 0, 5, 0, "/foo/qux/beans/baaaz", [:baaaz32], {})
      history.set(5, 0, 4, 0, "/foo/bar/beans/baaaz", [:baaaz4], {})


      assert_equal [:baz01],   history.get(0, 0, 1, 0, p1).val
      assert_equal [:baz01],   history.get_next(0, 0, 0, 0, p1).val
      assert_equal [:baz1],    history.get_next(0, 0, 1, 0, p1).val
      assert_equal [:baz1],    history.get_next(0, 0, 1, 0, "/**/").val
      assert_equal [:baz1],    history.get_next(0, 0, 1, 0, "/foo/*/*").val
      assert_equal [:baz1],    history.get_next(0, 0, 2, 0, "/foo/*/*").val
      assert_equal [:baz3],    history.get_next(0 + 3, 0, 3, 0, "/foo/*/*").val
      assert_equal [:baaaz2],  history.get_next(0 + 3, 0, 3, 0, "/**/*/*").val
      assert_equal [:baaaz2],  history.get_next(0 + 3, 0, 3, 0, "/**/*").val
      assert_equal [:baaaz3], history.get_next(5, 0, 2, 0, "/foo/*/*/baaaz/").val
      assert_equal [:baaaz32], history.get(5, 0, 6, 0, "/foo/*/*/baaaz/").val
      assert_equal [:baaaz4],  history.get(5, 0, 6, 0, "/foo/bar/*/baaaz/").val

      assert_equal [:baaaz32], history.get(5, 0, 6, 0, "/**/*/baaaz/").val

      assert_equal [:baz3],    history.get_next(3, 0, 3, 0, "/**/bar/*").val
      assert_equal [:baaaz2],  history.get_next(0 + 3, 0, 3, 0, "/foo/bar/**/").val
    end


    def test_event_order
      history = EventHistory.new

      history.set(0, 0, 2, 0, "/foo/bar/baz", [:baz3], {})
      history.set(0, 0, 1, 0, "/foo/bar/baz", [:baz2], {})
      history.set(0, 0, 0, 0, "/foo/bar/baz", [:baz1], {})

      assert_equal [:baz1], history.get(0, 0, 0, 0, "/foo/bar/baz").val
      assert_equal [:baz2], history.get(0, 0, 1, 0, "/foo/bar/baz").val
      assert_equal [:baz3], history.get(0, 0, 2, 0, "/foo/bar/baz").val
    end

    def test_event_matcher
      m = EventMatcher.new("/foo/bar/baz")
      assert  m.match("/foo/bar/baz")
      assert  m.match("/foo/bar/baz/")
      assert  m.match("foo/bar/baz")
      assert_nil m.match("/foo/bar/bazz")
    end


    def test_event_matcher_star
      m = EventMatcher.new("/foo*/*/*baz")
      assert  m.match("/foo/bar/baz")
      assert  m.match("/foo/bar/baz/")
      assert  m.match("foo/bar/baz")
      assert  m.match("foo/bar/eggsbaz")
      assert  m.match("foo333/bar/eggsbaz")
      assert_nil  m.match("foo333//bar/eggsbaz")
      assert_nil  m.match("foo333/beans/bar/eggsbaz")
      assert_nil m.match("/foo/bar/bazz")
    end

    def test_event_matcher_glob_star
      m = EventMatcher.new("/foo/**/baz")
      assert  m.match("/foo/bar/baz")
      assert  m.match("/foo/bar/baz/")
      assert  m.match("/foo/bar/beans/quux/baz/")
      assert  m.match("foo/bar/baz")
      assert_nil  m.match("/foo/bar/beans/quux/bazz/")
      assert_nil  m.match("foo333//bar/eggsbaz")
      assert_nil  m.match("foo333/beans/bar/eggsbaz")
      assert_nil m.match("/foo/bar/bazz")
    end

    def test_sync_with_existing_event
      history = EventHistory.new
      p1 = "/foo/bar/baz"
      history.set(0, 0, 1, 0, p1, [:baz1], {})
      assert_equal [:baz1], history.sync(0, 0, 0, 0, p1).val
    end

    def test_basic_sync
      history = EventHistory.new
      p1 = "/foo/bar/baz"
      Thread.new do
        Kernel.sleep 0.01
        history.set(0, 0, 1, 0, p1, [:baz1], {})
      end
      assert_equal [:baz1], history.sync(0, 0, 0, 0, p1).val
    end
  end
end
