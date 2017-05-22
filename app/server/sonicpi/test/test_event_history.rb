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
      i = ThreadId.new(5)
      t = 1495180483.9735098
      n = :foobar
      history.set(0, 0, i, 0, 0, n, :foo, {})
      v = history.get(t, 0, i, 0, 0, :foobar)
      assert_equal :foo, v.val
    end

    def test_basic_set_get_with_arg_matcher
      history = EventHistory.new
      t = 0
      n = "/foo/bar"

      i = ThreadId.new(5)

      tm = lambda { |_| true }

      m = lambda do |args|
        args.size == 2 && args.first != :foo
      end

      m2 = lambda do |args|
        args.size == 3 && args.first == :baz
      end

      history.set(t, 0, i, 0, 0, n, [:baz, :bar], {})
      history.set(t, 0, i, 1, 0, n, [:foo, :bar], {})
      history.set(t, 0, i, 2, 0, n, [:boz, :boz, :boz], {})
      history.set(t, 0, i, 3, 0, n, [:boz, :boz], {})
      history.set(t, 0, i, 3, 0, n, [:baz, :boz], {})
      history.set(t, 0, i, 4, 0, n, [:baz, :bar, :foo], {})
      history.set(t, 0, i, 5, 0, n, [:bar], {})
###sdlkfjsdlfjdslkfj
      v = history.get(t, 0, i, 6, 0, n, m)
      assert_equal [:baz, :boz], v.val

      v = history.get_next(t, 0, i, 0, 0, n, tm)
      assert_equal [:foo, :bar], v.val

      v = history.get_next(t, 0, i, 0, 0, n, m)
      assert_equal [:boz, :boz], v.val

      v = history.get_next(t, 0, i, 0, 0, n, m2)
      assert_equal [:baz, :bar, :foo], v.val
    end




    def test_deltas
      history = EventHistory.new
      t = 0
      i = ThreadId.new(5)
      n = "/foo/bar"
      history.set(t, 0, i, 0, 0, n, [:foo], {})
      history.set(t, 0, i, 1, 0, n, [:bar], {})
      v = history.get(t, 0, i, 0, 0, n)
      v2 = history.get(t, 0, i, 1, 0, n)
      v3 = history.get(t, 0, i, 0, 0, n)
      assert_equal [:foo], v.val
      assert_equal [:bar], v2.val
      assert_equal [:foo], v3.val
    end

    def test_multi_set_paths_get
      history = EventHistory.new
      t = 0
      i = ThreadId.new(5)
      n = "/foo/bar"
      n2 = "/foo/quux"
      history.set(t, 0, i, 0, 0, n, [:foo], {})
      history.set(t, 0, i, 0, 0, n2, [:bar], {})

      v = history.get(t, 0, i, 0, 0, n)
      v2 = history.get(t, 0, i, 0, 0, n2)

      assert_equal [:foo], v.val
      assert_equal [:bar], v2.val
    end

    def test_multi_set_paths_get_inverse_set_order
      history = EventHistory.new
      t = 0
      i = ThreadId.new(5)
      n = "/foo/bar"
      n2 = "/foo/quux"

      history.set(t, 0, i, 0, 0, n2, [:bar], {})
      history.set(t, 0, i, 0, 0, n, [:foo], {})

      v = history.get(t, 0, i, 0, 0, n)
      v2 = history.get(t, 0, i, 0, 0, n2)

      assert_equal [:foo], v.val
      assert_equal [:bar], v2.val
    end

    def test_star_matchers
      history = EventHistory.new
      i = ThreadId.new(5)
      t = 0
      n = "/foo/bar/baz"
      n2 = "/foo/bar/boz"
      n3 = "/foo/quux"

      history.set(t, 0, i, 0, 0, n, [:foo], {})
      history.set(t + 1, 0, i, 0, 0, n2, [:bar], {})
      history.set(t, 0, i, 0, 0, n3, [:baz], {})

      v = history.get(t, 0, i, 0, 0, "/foo/bar/*")
      assert_equal [:foo], v.val
      v2 = history.get(t, 0, i, 0, 0, "/foo/*/quux")
      assert_nil  v2
      v3 = history.get(t, 0, i, 0, 0,"/*/*/boz")
      assert_nil  v3
      v4 = history.get(t + 1, 0, i, 0, 0, "/*/*/boz")
      assert_equal [:bar], v4.val
    end

    def test_newer_history_is_ignored
      # Get should return last seen version
      # (at or before the current time)
      history = EventHistory.new
      i = ThreadId.new(5)
      t = 0
      n = "/foo/bar/baz"

      history.set(t, 0, i, 0, 0, n, [:foo], {})
      history.set(t + 1, 0, i, 0, 0, n, [:bar], {})

      v = history.get(t, 0, i, 0, 0, n)
      v2 = history.get(t + 0.5, 0, i, 0, 0, n)
      v3 = history.get(t + 1, 0, i, 0, 0, n)

      assert_equal [:foo], v.val
      assert_equal [:foo], v2.val
      assert_equal [:bar], v3.val

    end

    def test_double_star_matcher_at_end
      history = EventHistory.new
      i = ThreadId.new(5)
      t = 0
      #t = 1
      n = "/foo/bar/baz"

      # t = 0
      n2 = "/foo/bar/boz"
      n3 = "/foo/quux"
      n4 = "/foo/quux/eggs"
      n5 = "/a"
      n6 = "/z"
      n7 = "/foo/quux/beans/milk"

      history.set(t, 0, i, 1, 0, n, [:baz], {})
      history.set(t, 0, i, 0, 0, n2, [:boz], {})
      history.set(t, 0, i, 2, 0, n3, [:quux], {})
      history.set(t, 0, i, 3, 0, n4, [:eggs], {})
      history.set(t, 0, i, 5, 0, n5, [:a], {})
      history.set(t, 0, i, 4, 0, n6, [:z], {})
      history.set(t, 0, i, 6, 0, n7, [:milk], {})

      v = history.get(t, 0, i, 0, 0, "/**")
      assert_equal [:boz], v.val

      v = history.get(t, 0, i, 0, 0, "/foo/**")
      assert_equal [:boz], v.val

      v = history.get(t, 1, i, 0, 0, "/foo/*/beans/**")
      assert_equal [:milk], v.val
    end

    def test_double_star_matcher_not_at_end
      history = EventHistory.new
      i = ThreadId.new(5)

      n1 = "/foo/bar/baz"
      n2 = "/foo/bar/boz"
      n3 = "/foo/quux"
      n4 = "/foo/quux/eggs"
      n5 = "/a"
      n6 = "/z"
      n7 = "/foo/quux/beans/milk"
      n8 = "/foo/cheese/beans/quorn"
      n9 = "/foo/cheese/bleans/quorn"

      history.set(0, 0, i, 0,  0, n1, [:baz], {})
      history.set(0, 0, i, 1,  0, n1, [:baz1], {})
      history.set(0, 0, i, 2,  0, n1, [:baz2], {})
      history.set(1, 0, i, 3,  0, n1, [:baz3], {})
      history.set(0, 0, i, 4,  0, n2, [:boz], {})
      history.set(0, 0, i, 5,  0, n3, [:quux], {})
      history.set(0, 0, i, 6,  0, n4, [:eggs], {})
      history.set(0, 0, i, 7,  0, n5, [:a], {})
      history.set(0, 0, i, 8,  0, n6, [:z], {})
      history.set(0, 0, i, 9,  0, n7, [:milk], {})
      history.set(0, 0, i, 10, 0, n8, [:quorn], {})
      history.set(1, 0, i, 11, 0, n9, [:quorn2], {})

      assert_nil history.get(0, 0, i, 0, 0, "/**/quux")
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, "/**/**/**/quux").val
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, "/**/quux").val
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, "/**/*/quux").val
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, "/*/**/quux").val
      assert_equal [:milk],   history.get(2, 0, i, 0, 0, "/*/**/quux/**").val
      assert_equal [:quorn],  history.get(2, 0, i, 0, 0, "/foo/*/beans/**").val
      assert_equal [:quorn],  history.get(2, 0, i, 0, 0, "/foo/**/beans/**").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, "/**").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, "/**/").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, "**").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, "**").val
      assert_equal [:baz1],   history.get(0, 0, i, 1, 0, "/**/").val
      assert_equal [:quorn2], history.get(2, 0, i, 0, 0, "/**/").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, "/foo/**/").val
      assert_equal [:baz1],   history.get(0, 0, i, 1, 0, "/foo/**/").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, "/foo/bar/baz").val
      assert_equal [:baz1],   history.get(0, 0, i, 1, 0, "/foo/bar/baz").val
      assert_equal [:baz2],   history.get(1, 0, i, 2, 0, "/foo/bar/baz").val
      assert_equal [:quorn2], history.get(2, 0, i, 0, 0, "/f*/*/b*/quorn").val
      assert_equal [:quorn2], history.get(2, 0, i, 0, 0, "/f*/*se/b*/quorn").val
    end

    def test_more_globs
      history = EventHistory.new

      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"

      history.set(0, 0, i, 0, 0, n1, [:baz], {})
      history.set(0, 0, i, 1, 0, n1, [:baz1], {})

      assert_equal [:baz],  history.get(0, 0, i, 0, 0, n1).val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, n1).val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, "/f*/bar/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, "/foo/bar/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, "/fo*o/b*/*").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, "/*/*/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, "/*/*/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, "/**").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, "/**").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, "/**/**").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, "/foo/**/*").val
    end

    def test_basic_get_next
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      history.set(0, 0, i, 0, 0, n1, [:baz0], {})
      history.set(0, 0, i, 1, 0, n1, [:baz1], {})
      history.set(1, 0, i, 0, 0, n1, [:baz2], {})
      assert_equal [:baz1], history.get_next(0, 0, i, 0, 0, n1).val
    end

    def test_get_next
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      history.set(0, 0, i, 0, 0, n1, [:baz0], {})
      history.set(0, 0, i, 1, 0, n1, [:baz01], {})
      history.set(1, 0, i, 0, 0, n1, [:baz1], {})

      history.set(2, 0, i, 1, 0, n1, [:baz2], {})
      history.set(5, 0, i, 3, 0, n1, [:baz3], {})
      history.set(5, 0, i, 2, 0, "/foo/bar/beans/baaaz", [:baaaz2], {})
      history.set(5, 0, i, 3, 0, "/foo/bar/beans/baaaz", [:baaaz3], {})
      history.set(5, 0, i, 3, 0, "/foo/qux/beans/baaaz", [:baaaz31], {})
      history.set(5, 0, i, 5, 0, "/foo/qux/beans/baaaz", [:baaaz32], {})
      history.set(5, 0, i, 4, 0, "/foo/bar/beans/baaaz", [:baaaz4], {})

      assert_equal [:baz01],   history.get(0, 0, i, 1, 0, n1).val
      assert_equal [:baz01],   history.get_next(0, 0, i, 0, 0, n1).val
      assert_equal [:baz1],    history.get_next(0, 0, i, 1, 0, n1).val
      assert_equal [:baz1],    history.get_next(0, 0, i, 1, 0, "/**/").val
      assert_equal [:baz1],    history.get_next(0, 0, i, 1, 0, "/foo/*/*").val
      assert_equal [:baz1],    history.get_next(0, 0, i, 2, 0, "/foo/*/*").val
      assert_equal [:baz3],    history.get_next(3, 0, i, 3, 0, "/foo/*/*").val
      assert_equal [:baaaz2],  history.get_next(3, 0, i, 3, 0, "/**/*/*").val
      assert_equal [:baaaz2],  history.get_next(3, 0, i, 3, 0, "/**/*").val
      assert_equal [:baaaz3],  history.get_next(5, 0, i, 2, 0, "/foo/*/*/baaaz/").val
      assert_equal [:baaaz32], history.get(5, 0, i, 6, 0, "/foo/*/*/baaaz/").val
      assert_equal [:baaaz4],  history.get(5, 0, i, 6, 0, "/foo/bar/*/baaaz/").val

      assert_equal [:baaaz32], history.get(5, 0, i, 6, 0, "/**/*/baaaz/").val

      assert_equal [:baz3],    history.get_next(3, 0, i, 3, 0, "/**/bar/*").val
      assert_equal [:baaaz2],  history.get_next(3, 0, i, 3, 0, "/foo/bar/**/").val
    end


    def test_event_order
      history = EventHistory.new
      i = ThreadId.new(5)

      history.set(0, 0, i, 2, 0, "/foo/bar/baz", [:baz3], {})
      history.set(0, 0, i, 1, 0, "/foo/bar/baz", [:baz2], {})
      history.set(0, 0, i, 0, 0, "/foo/bar/baz", [:baz1], {})

      assert_equal [:baz1], history.get(0, 0, i, 0, 0, "/foo/bar/baz").val
      assert_equal [:baz2], history.get(0, 0, i, 1, 0, "/foo/bar/baz").val
      assert_equal [:baz3], history.get(0, 0, i, 2, 0, "/foo/bar/baz").val
    end

    def test_event_matcher
      m = EventMatcher.new("/foo/bar/baz")
      assert  m.match("/foo/bar/baz", nil)
      assert  m.match("/foo/bar/baz/", nil)
      assert  m.match("foo/bar/baz", nil)
      assert_nil m.match("/foo/bar/bazz", nil)
    end


    def test_event_matcher_star
      m = EventMatcher.new("/foo*/*/*baz")
      assert  m.match("/foo/bar/baz", nil)
      assert  m.match("/foo/bar/baz/", nil)
      assert  m.match("foo/bar/baz", nil)
      assert  m.match("foo/bar/eggsbaz", nil)
      assert  m.match("foo333/bar/eggsbaz", nil)
      assert_nil  m.match("foo333//bar/eggsbaz", nil)
      assert_nil  m.match("foo333/beans/bar/eggsbaz", nil)
      assert_nil m.match("/foo/bar/bazz", nil)
    end

    def test_event_matcher_glob_star
      m = EventMatcher.new("/foo/**/baz")
      assert  m.match("/foo/bar/baz", nil)
      assert  m.match("/foo/bar/baz/", nil)
      assert  m.match("/foo/bar/beans/quux/baz/", nil)
      assert  m.match("foo/bar/baz", nil)
      assert_nil  m.match("/foo/bar/beans/quux/bazz/", nil)
      assert_nil  m.match("foo333//bar/eggsbaz", nil)
      assert_nil  m.match("foo333/beans/bar/eggsbaz", nil)
      assert_nil m.match("/foo/bar/bazz", nil)
    end

    def test_sync_with_existing_event
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      history.set(0, 0, i, 1, 0, n1, [:baz1], {})
      assert_equal [:baz1], history.sync(0, 0, i, 0, 0, n1).val
    end

    def test_basic_sync
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      Thread.new do
        Kernel.sleep 0.01
        history.set(0, 0, i, 1, 0, n1, [:baz1], {})
      end
      assert_equal [:baz1], history.sync(0, 0, i, 0, 0, n1).val
    end
  end
end
