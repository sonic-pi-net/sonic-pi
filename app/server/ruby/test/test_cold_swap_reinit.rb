#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/cold_swap_reinit"

module SonicPi
  # Regression guarded against: after a cold swap the server waited a second
  # of quiet before stopping its jobs, so running live_loops — and bundles
  # already scheduled ahead in the engine — played into the rebuilt, empty
  # world ("SynthDef not found", "Group not found").
  class ColdSwapReinitTester < Minitest::Test
    QUIET = 0.4

    def setup
      @events = Queue.new
      @rebuild_results = []
    end

    def reinit(**opts)
      ColdSwapReinit.new(halt:    -> { @events << [:halt, now] },
                         rebuild: -> { @events << [:rebuild, now]; @rebuild_results.empty? || @rebuild_results.shift },
                         quiet: QUIET, retry_delay: 0.05, **opts)
    end

    def now
      Process.clock_gettime(Process::CLOCK_MONOTONIC)
    end

    def next_event(timeout = 3)
      deadline = now + timeout
      loop do
        return @events.pop(true) rescue ThreadError
        flunk "no event within #{timeout} s" if now > deadline
        sleep 0.005
      end
    end

    # The quiet is an hour, so a halt that waited for it would never come: the
    # halt arrives with the rebuild still an hour off, however slow the
    # machine. (The worker is left waiting out that hour; the process ends
    # first.)
    def test_a_setup_halts_at_once
      r = reinit(quiet: 3600)
      r.setup!(2)
      kind, _ = next_event(30)
      assert_equal :halt, kind
      assert @events.empty?, "nothing but the halt before the quiet has passed"
    end

    def test_the_rebuild_comes_once_the_quiet_has_passed
      r = reinit
      at = now
      r.setup!(2)
      kind, _ = next_event
      assert_equal :halt, kind
      kind, t = next_event
      assert_equal :rebuild, kind
      assert_operator t - at, :>=, QUIET
      r.wait_idle
    end

    def test_a_burst_of_setups_is_rebuilt_for_once_after_it_settles
      r = reinit
      3.times { |i| r.setup!(2 + i); sleep QUIET / 4 }
      r.wait_idle
      kinds = []
      kinds << @events.pop until @events.empty?
      assert_equal 1, kinds.count { |k, _| k == :rebuild }
      assert_equal :halt, kinds.first.first
    end

    def test_a_rebuild_that_falls_short_is_retried
      @rebuild_results = [false, true]
      r = reinit
      r.setup!(2)
      r.wait_idle
      kinds = []
      kinds << @events.pop until @events.empty?
      assert_equal 2, kinds.count { |k, _| k == :rebuild }
    end

    def test_a_setup_during_a_rebuild_gets_a_pass_of_its_own
      r = nil
      first = true
      r = ColdSwapReinit.new(halt:    -> { @events << [:halt, now] },
                             rebuild: -> {
                               @events << [:rebuild, now]
                               if first
                                 first = false
                                 r.setup!(3)   # the device churned while rebuilding
                               end
                               true
                             },
                             quiet: QUIET, retry_delay: 0.05)
      r.setup!(2)
      r.wait_idle
      kinds = []
      kinds << @events.pop until @events.empty?
      assert_equal 2, kinds.count { |k, _| k == :rebuild }
      assert_equal 2, kinds.count { |k, _| k == :halt }
    end
  end
end
