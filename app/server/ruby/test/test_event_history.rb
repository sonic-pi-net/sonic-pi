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
    include SonicPi::Util

    def make_cue_event(path)
      t = Time.now
      p = 0
      i = 0
      d = 0
      b = 0
      m = 60
      v = [:a, :b, :c]
      n = path
      c = CueEvent.new(t, p, i, d, b, m, n, v)
    end

    def test_get_and_set_w_diff_thread_ids
      history = EventHistory.new()

      i1 = ThreadId.new(18, 0, 0)
      i2 = ThreadId.new(18, 0, 1)
      t = 1495752780.305016

      history.set(t, 0, i1, 0, 0, 60, "/cue/bar", [:hello])
      v = history.get(t, 0, i2, 0, 0, 60, "/cue/bar")
      assert_equal [:hello], v.val

    end

    def test_basic_set_get_w_sym
      history = EventHistory.new
      i = ThreadId.new(5)
      t = 1495180483.9735098
      n = :foobar
      m = 60

      history.set(0, 0, i, 0, 0, m, n, :foo)
      v = history.get(t, 0, i, 0, 0, m, :foobar)
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

      history.set(t, 0, i, 0, 0, 60, n, [:baz, :bar])
      history.set(t, 0, i, 1, 0, 60, n, [:foo, :bar])
      history.set(t, 0, i, 2, 0, 60, n, [:boz, :boz, :boz])
      history.set(t, 0, i, 3, 0, 60, n, [:boz, :boz])
      history.set(t, 0, i, 3, 0, 60, n, [:baz, :boz])
      history.set(t, 0, i, 4, 0, 60, n, [:baz, :bar, :foo])
      history.set(t, 0, i, 5, 0, 60, n, [:bar])
###sdlkfjsdlfjdslkfj
      v = history.get(t, 0, i, 6, 0, 60, n, m)
      assert_equal [:baz, :boz], v.val

      v = history.get_next(t, 0, i, 0, 0, 60, n, tm)
      assert_equal [:foo, :bar], v.val

      v = history.get_next(t, 0, i, 0, 0, 60, n, m)
      assert_equal [:boz, :boz], v.val

      v = history.get_next(t, 0, i, 0, 0, 60, n, m2)
      assert_equal [:baz, :bar, :foo], v.val
    end




    def test_deltas
      history = EventHistory.new
      t = 0
      i = ThreadId.new(5)
      n = "/foo/bar"
      m = 60
      history.set(t, 0, i, 0, 0, m, n, [:foo])
      history.set(t, 0, i, 1, 0, m, n, [:bar])
      v = history.get(t, 0, i, 0, 0, m, n)
      v2 = history.get(t, 0, i, 1, 0, m, n)
      v3 = history.get(t, 0, i, 0, 0, m, n)
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
      m = 60

      history.set(t, 0, i, 0, 0, m, n, [:foo])
      history.set(t, 0, i, 0, 0, m, n2, [:bar])

      v = history.get(t, 0, i, 0, 0, m, n)
      v2 = history.get(t, 0, i, 0, 0, m, n2)

      assert_equal [:foo], v.val
      assert_equal [:bar], v2.val
    end

    def test_multi_set_paths_get_inverse_set_order
      history = EventHistory.new
      t = 0
      i = ThreadId.new(5)
      n = "/foo/bar"
      n2 = "/foo/quux"
      m = 60

      history.set(t, 0, i, 0, 0, m, n2, [:bar])
      history.set(t, 0, i, 0, 0, m, n, [:foo])

      v = history.get(t, 0, i, 0, 0, m, n)
      v2 = history.get(t, 0, i, 0, 0, m, n2)

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
      m = 60

      history.set(t, 0, i, 0, 0, m, n, [:foo])
      history.set(t + 1, 0, i, 0, 0, m, n2, [:bar])
      history.set(t, 0, i, 0, 0, m, n3, [:baz])

      v = history.get(t, 0, i, 0, 0, m, "/foo/bar/*")
      assert_equal [:foo], v.val
      v2 = history.get(t, 0, i, 0, 0, m, "/foo/*/quux")
      assert_nil  v2
      v3 = history.get(t, 0, i, 0, 0, m, "/*/*/boz")
      assert_nil  v3
      v4 = history.get(t + 1, 0, i, 0, 0, m, "/*/*/boz")
      assert_equal [:bar], v4.val
    end

    def test_newer_history_is_ignored
      # Get should return last seen version
      # (at or before the current time)
      history = EventHistory.new
      i = ThreadId.new(5)
      t = 0
      n = "/foo/bar/baz"
      m = 60

      history.set(t, 0, i, 0, 0, m, n, [:foo])
      history.set(t + 1, 0, i, 0, 0, m, n, [:bar])

      v = history.get(t, 0, i, 0, 0, m, n)
      v2 = history.get(t + 0.5, 0, i, 0, 0, m, n)
      v3 = history.get(t + 1, 0, i, 0, 0, m, n)

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
      m = 60

      # t = 0
      n2 = "/foo/bar/boz"
      n3 = "/foo/quux"
      n4 = "/foo/quux/eggs"
      n5 = "/a"
      n6 = "/z"
      n7 = "/foo/quux/beans/milk"

      history.set(t, 0, i, 1, 0, m, n, [:baz])
      history.set(t, 0, i, 0, 0, m, n2, [:boz])
      history.set(t, 0, i, 2, 0, m, n3, [:quux])
      history.set(t, 0, i, 3, 0, m, n4, [:eggs])
      history.set(t, 0, i, 5, 0, m, n5, [:a])
      history.set(t, 0, i, 4, 0, m, n6, [:z])
      history.set(t, 0, i, 6, 0, m, n7, [:milk])

      v = history.get(t, 0, i, 0, 0, m, "/**")
      assert_equal [:boz], v.val

      v = history.get(t, 0, i, 0, 0, m, "/foo/**")
      assert_equal [:boz], v.val

      v = history.get(t, 1, i, 0, 0, m, "/foo/*/beans/**")
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
      m = 60

      history.set(0, 0, i, 0,  0, m, n1, [:baz])
      history.set(0, 0, i, 1,  0, m, n1, [:baz1])
      history.set(0, 0, i, 2,  0, m, n1, [:baz2])
      history.set(1, 0, i, 3,  0, m, n1, [:baz3])
      history.set(0, 0, i, 4,  0, m, n2, [:boz])
      history.set(0, 0, i, 5,  0, m, n3, [:quux])
      history.set(0, 0, i, 6,  0, m, n4, [:eggs])
      history.set(0, 0, i, 7,  0, m, n5, [:a])
      history.set(0, 0, i, 8,  0, m, n6, [:z])
      history.set(0, 0, i, 9,  0, m, n7, [:milk])
      history.set(0, 0, i, 10, 0, m, n8, [:quorn])
      history.set(1, 0, i, 11, 0, m, n9, [:quorn2])

      assert_nil history.get(0, 0, i, 0, 0, m, "/**/quux")
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, m, "/**/**/**/quux").val
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, m, "/**/quux").val
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, m, "/**/*/quux").val
      assert_equal [:quux],   history.get(2, 0, i, 0, 0, m, "/*/**/quux").val
      assert_equal [:milk],   history.get(2, 0, i, 0, 0, m, "/*/**/quux/**").val
      assert_equal [:quorn],  history.get(2, 0, i, 0, 0, m, "/foo/*/beans/**").val
      assert_equal [:quorn],  history.get(2, 0, i, 0, 0, m, "/foo/**/beans/**").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, m, "/**").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, m, "/**/").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, m, "**").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, m, "**").val
      assert_equal [:baz1],   history.get(0, 0, i, 1, 0, m, "/**/").val
      assert_equal [:quorn2], history.get(2, 0, i, 0, 0, m, "/**/").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, m, "/foo/**/").val
      assert_equal [:baz1],   history.get(0, 0, i, 1, 0, m, "/foo/**/").val
      assert_equal [:baz],    history.get(0, 0, i, 0, 0, m, "/foo/bar/baz").val
      assert_equal [:baz1],   history.get(0, 0, i, 1, 0, m, "/foo/bar/baz").val
      assert_equal [:baz2],   history.get(1, 0, i, 2, 0, m, "/foo/bar/baz").val
      assert_equal [:quorn2], history.get(2, 0, i, 0, 0, m, "/f*/*/b*/quorn").val
      assert_equal [:quorn2], history.get(2, 0, i, 0, 0, m, "/f*/*se/b*/quorn").val
    end

    def test_more_globs
      history = EventHistory.new

      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      m = 60

      history.set(0, 0, i, 0, 0, m, n1, [:baz])
      history.set(0, 0, i, 1, 0, m, n1, [:baz1])

      assert_equal [:baz],  history.get(0, 0, i, 0, 0, m, n1).val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, m, n1).val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, m, "/f*/bar/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, m, "/foo/bar/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, m, "/fo*o/b*/*").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, m, "/*/*/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, m, "/*/*/*").val
      assert_equal [:baz],  history.get(0, 0, i, 0, 0, m, "/**").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, m, "/**").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, m, "/**/**").val
      assert_equal [:baz1], history.get(0, 0, i, 1, 0, m, "/foo/**/*").val
    end

    def test_basic_get_next
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      m = 60

      history.set(0, 0, i, 0, 0, m, n1, [:baz0])
      history.set(0, 0, i, 1, 0, m, n1, [:baz1])
      history.set(1, 0, i, 0, 0, m, n1, [:baz2])
      assert_equal [:baz1], history.get_next(0, 0, i, 0, 0, m, n1).val
    end

    def test_get_next
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      m = 60

      history.set(0, 0, i, 0, 0, m, n1, [:baz0])
      history.set(0, 0, i, 1, 0, m, n1, [:baz01])
      history.set(1, 0, i, 0, 0, m, n1, [:baz1])

      history.set(2, 0, i, 1, 0, m, n1, [:baz2])
      history.set(5, 0, i, 3, 0, m, n1, [:baz3])
      history.set(5, 0, i, 2, 0, m, "/foo/bar/beans/baaaz", [:baaaz2])
      history.set(5, 0, i, 3, 0, m, "/foo/bar/beans/baaaz", [:baaaz3])
      history.set(5, 0, i, 3, 0, m, "/foo/qux/beans/baaaz", [:baaaz31])
      history.set(5, 0, i, 5, 0, m, "/foo/qux/beans/baaaz", [:baaaz32])
      history.set(5, 0, i, 4, 0, m, "/foo/bar/beans/baaaz", [:baaaz4])

      assert_equal [:baz01],   history.get(0, 0, i, 1, 0, m, n1).val
      assert_equal [:baz01],   history.get_next(0, 0, i, 0, 0, m, n1).val
      assert_equal [:baz1],    history.get_next(0, 0, i, 1, 0, m, n1).val
      assert_equal [:baz1],    history.get_next(0, 0, i, 1, 0, m, "/**/").val
      assert_equal [:baz1],    history.get_next(0, 0, i, 1, 0, m, "/foo/*/*").val
      assert_equal [:baz1],    history.get_next(0, 0, i, 2, 0, m, "/foo/*/*").val
      assert_equal [:baz3],    history.get_next(3, 0, i, 3, 0, m, "/foo/*/*").val
      assert_equal [:baaaz2],  history.get_next(3, 0, i, 3, 0, m, "/**/*/*").val
      assert_equal [:baaaz2],  history.get_next(3, 0, i, 3, 0, m, "/**/*").val
      assert_equal [:baaaz3],  history.get_next(5, 0, i, 2, 0, m, "/foo/*/*/baaaz/").val
      assert_equal [:baaaz32], history.get(5, 0, i, 6, 0, m, "/foo/*/*/baaaz/").val
      assert_equal [:baaaz4],  history.get(5, 0, i, 6, 0, m, "/foo/bar/*/baaaz/").val

      assert_equal [:baaaz32], history.get(5, 0, i, 6, 0, m, "/**/*/baaaz/").val

      assert_equal [:baz3],    history.get_next(3, 0, i, 3, 0, m, "/**/bar/*").val
      assert_equal [:baaaz2],  history.get_next(3, 0, i, 3, 0, m, "/foo/bar/**/").val
      assert_equal [:baaaz2],  history.get_next(3, 0, i, 3, 0, m, "/foo/bar/**").val
    end


    def test_event_order
      history = EventHistory.new
      i = ThreadId.new(5)
      m = 60

      history.set(0, 0, i, 2, 0, m, "/foo/bar/baz", [:baz3])
      history.set(0, 0, i, 1, 0, m, "/foo/bar/baz", [:baz2])
      history.set(0, 0, i, 0, 0, m, "/foo/bar/baz", [:baz1])

      assert_equal [:baz1], history.get(0, 0, i, 0, 0, m, "/foo/bar/baz").val
      assert_equal [:baz2], history.get(0, 0, i, 1, 0, m, "/foo/bar/baz").val
      assert_equal [:baz3], history.get(0, 0, i, 2, 0, m, "/foo/bar/baz").val
    end

    def test_event_matcher
      m = EventMatcher.new(make_cue_event("/foo/bar/baz"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/foo/bar/baz", nil)
      assert  m.path_match("/foo/bar/baz/", nil)
      assert  m.path_match("foo/bar/baz", nil)
      assert_nil m.path_match("/foo/bar/bazz", nil)
    end


    def test_event_matcher_star
      m = EventMatcher.new(make_cue_event("/foo*/*/*baz"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/foo/bar/baz", nil)
      assert  m.path_match("/foo/bar/baz/", nil)
      assert  m.path_match("foo/bar/baz", nil)
      assert  m.path_match("foo/bar/eggsbaz", nil)
      assert  m.path_match("foo333/bar/eggsbaz", nil)
      assert_nil  m.path_match("foo333//bar/eggsbaz", nil)
      assert_nil  m.path_match("foo333/beans/bar/eggsbaz", nil)
      assert_nil m.path_match("/foo/bar/bazz", nil)
    end

    def test_event_matcher_glob_star
      m = EventMatcher.new(make_cue_event("/foo/**/baz"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/foo/bar/baz", nil)
      assert  m.path_match("/foo/bar/baz/", nil)
      assert  m.path_match("/foo/bar/beans/quux/baz/", nil)
      assert  m.path_match("foo/bar/baz", nil)
      assert_nil  m.path_match("/foo/bar/beans/quux/bazz/", nil)
      assert_nil  m.path_match("foo333//bar/eggsbaz", nil)
      assert_nil  m.path_match("foo333/beans/bar/eggsbaz", nil)
      assert_nil m.path_match("/foo/bar/bazz", nil)
    end

    def test_event_matcher_glob_star_at_end
      m = EventMatcher.new(make_cue_event("/foo/**"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/foo/bar/baz", nil)
      assert  m.path_match("/foo/bar/baz/", nil)
      assert  m.path_match("/foo/bar/beans/quux/baz/", nil)
      assert  m.path_match("foo/bar/baz", nil)
      assert  m.path_match("/foo/bar/beans/quux/bazz/", nil)
      assert_nil  m.path_match("foo333//bar/eggsbaz", nil)
      assert_nil  m.path_match("foo333/beans/bar/eggsbaz", nil)
      assert m.path_match("/foo/bar/bazz", nil)
    end

    def test_sync_with_existing_event
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      m = 60
      history.set(0, 0, i, 1, 0, m, n1, [:baz0])
      history.set(1, 0, i, 1, 0, m, n1, [:baz1])
      history.set(2, 0, i, 1, 0, m, n1, [:baz2])
      assert_equal [:baz1], history.sync(0, 0, i, 1, 0, m, n1).val
    end

    def test_sync_w_matchers
      history = EventHistory.new
      i = ThreadId.new(5)
      n1 = "/foo/bar/baz"
      m = 60

     t = Thread.new do

       history.set(0, 0, i, 1, 0, m, n1, [:baz1])
     end
     assert_equal [:baz1], history.sync(0, 0, i, 0, 0, m, "/foo/**").val
     t.join
    end


    def test_simultaneous_sync_set
      50.times do |i|
        history = EventHistory.new
        i = ThreadId.new(5)
        n1 = "/foo/bar/baz"
        m = 60

        t1 = Thread.new do
          history.set(0, 0, i, 1, 0, m, n1, [:baz1])
        end
        t2 = Thread.new do
          assert_equal [:baz1], history.sync(0, -100, i, 0, 0, m, n1).val
        end

        t1.join
        t2.join
      end
    end

    def test_event_matcher_pruning
      history = EventHistory.new
      i = ThreadId.new(5)
      i2 = ThreadId.new(5, 1)
      m = 60

      t = Thread.new do
        history.sync 0, 0, i, 0, 0, m, "/foo/bar"
      end

      t2 = Thread.new do
        history.sync 0, 0, i2, 0, 0, m, "/foo/bar/baz"
      end
      Kernel.sleep 0.1
      assert_equal 2, history.event_matchers.matchers.size
      history.event_matchers.prune(i)
      assert_equal 1, history.event_matchers.matchers.size
      history.event_matchers.prune(i2)
      assert_equal 0, history.event_matchers.matchers.size
      t.kill
      t2.kill
    end

    def test_get_matchers

      history = EventHistory.new
      i1 = ThreadId.new(18, 0, 0)
      i2 = ThreadId.new(18, 0, 1)
      t = 1495752780.305016
      t2 = 1495752780.305016 - 1
      t3 = 1495752780.305016 - 2
      t4 = 1495752780.305016 + 2

      history.set(t, 0, i1, 0, 0, 60, "/cue/bar", [:hello])
      history.set(t2, 0, i1, 0, 0, 60, "/set/bar", [:there])
      history.set(t3, 0, i1, 0, 0, 60, "/set/yo3", [:yo])
      history.set(t4, 0, i1, 0, 0, 60, "/set/howdy30", [:howdy])
      v = history.get(t, 0, i2, 0, 0, 60, "/cue/bar")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/set/bar")
      assert_equal [:there], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/*/bar")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/{cue,set}/bar")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/{cu?,set}/bar")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/{ce?,set}/bar")
      assert_equal [:there], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/cu/bar")
      assert_equal nil, v
      v = history.get(t, 0, i2, 0, 0, 60, "/cu?/bar")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/{cu?/bar")
      assert_equal nil, v
      v = history.get(t, 0, i2, 0, 0, 60, "/???/ba?")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/[cd]ue/ba?")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/[a-d]ue/ba?")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/[!d]ue/ba?")
      assert_equal [:hello], v.val
      v = history.get(t, 0, i2, 0, 0, 60, "/[!c]ue/ba?")
      assert_equal nil, v

      v = history.get(t4, 0, i2, 0, 0, 60, "/set/howdy30")
      assert_equal [:howdy], v.val

      v = history.get(t4, 0, i2, 0, 0, 60, "/set/howdy[3]0")
      assert_equal [:howdy], v.val

      v = history.get(t4, 0, i2, 0, 0, 60, "/set/howdy[3]")
      assert_equal nil, v

      v = history.get(t, 0, i2, 0, 0, 60, "/set/yo?")
      assert_equal [:yo], v.val
    end

    def test_event_matcher_star

      m = EventMatcher.new(make_cue_event("/{cue,set}/[a-c]az"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/cue/baz", nil)
      assert  m.path_match("/set/aaz/", nil)
      assert  m.path_match("/set/baz/", nil)
      assert  m.path_match("/set/caz/", nil)
      assert  m.path_match("/cue/caz/", nil)
      assert_nil  m.path_match("/cue/daz/", nil)
    end

    def test_event_matcher_multi_squares
      m = EventMatcher.new(make_cue_event("/[a-c]ue/[d-g]az/[!w]uux"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/cue/daz/quux/", nil)
      assert_nil  m.path_match("/cue/daz/wuux/", nil)
    end

    def test_event_matcher_square_neg_range
      m = EventMatcher.new(make_cue_event("/[a-c]ue/[d-g]az/[!a-w]uux"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/cue/daz/zuux/", nil)
      assert_nil  m.path_match("/cue/daz/wuux/", nil)
    end


    def test_event_matcher_broken_squares
      m = EventMatcher.new(make_cue_event("/[cue/caz]"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/[cue/caz]/", nil)
    end

    def test_event_matcher_broken_squiggles
      m = EventMatcher.new(make_cue_event("/{cue/caz}"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/{cue/caz}/", nil)
    end

    def test_event_matcher_multi_squiggles
      m = EventMatcher.new(make_cue_event("/{a,c}ue/{baz,boz}/[!w]uux"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/cue/baz/quux/", nil)
      assert  m.path_match("/cue/boz/quux/", nil)
      assert_nil  m.path_match("/due/baz/quux/", nil)
    end

    def test_event_matcher_with_single_char
      m = EventMatcher.new(make_cue_event("/?ue/{baz,boz}/[!w]uux"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/cue/baz/quux/", nil)
      assert  m.path_match("/cue/boz/quux/", nil)
      assert  m.path_match("/due/baz/quux/", nil)
    end

    def test_event_matcher_with_single_char_at_end
      m = EventMatcher.new(make_cue_event("/?ue/{baz,boz}/quux[!12]"), nil, ThreadId.new(5), Promise.new)
      assert  m.path_match("/cue/baz/quux3/", nil)
      assert_nil  m.path_match("/cue/baz/quux34/", nil)
    end

    def test_event_matcher_with_single_char_at_end2
      m = EventMatcher.new(make_cue_event("/push[!1]"), nil, ThreadId.new(5), Promise.new)
      assert_nil  m.path_match("/push40", nil)
    end

    def test_foo
      history = EventHistory.new
      i1 = ThreadId.new(0, 0)
      i2 = ThreadId.new(0, 0, 0)
      i3 = ThreadId.new(0, 0, 1)
      n = "/cue/foo"
      m = 60

      history.set(1496358140.495527, 0, i1, 0, 0, m, n, 1)
      history.set(1496358140.495527, 0, i1, 1, 0, m, n, 2)
      history.set(1496358140.595527, 0, i2, 0, 0.1, m, n, 3)
      history.get(1496358140.595527, -100, i1, 0, 0.1, m, n)
      history.set(1496358140.595527,  0, i3, 0, 0.1, m, n, 5)
      history.get(1496358140.595527, -100, i1, 0, 0.1, m, n)
      v = history.get(1496358140.6955268, -100, i1, 0, 0.2, m, n)
      assert_equal 5, v.val
    end
  end
end
