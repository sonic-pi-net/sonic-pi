#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/cuestamper"
require_relative "../lib/sonicpi/cueevent"
require_relative "../lib/sonicpi/event_history"
require_relative "../lib/sonicpi/thread_id"

module SonicPi
  class CueStamperTester < Minitest::Test

    def test_distinct_clock_reads_pass_through
      stamper = CueStamper.new
      t1 = Time.at(100)
      t2 = Time.at(101)
      assert_equal t1.to_r, stamper.stamp(t1).to_r
      assert_equal t2.to_r, stamper.stamp(t2).to_r
    end

    def test_identical_clock_reads_stamp_strictly_increasing
      # A burst of cues drained faster than the clock ticks (GH #1839 -
      # coarse Windows clocks stamped whole bursts identically and sync
      # then dropped all but one).
      stamper = CueStamper.new
      t = Time.at(100)
      stamps = 5.times.map { stamper.stamp(t).to_r }
      assert_equal stamps.sort, stamps
      assert_equal stamps.uniq, stamps
    end

    def test_clock_stepping_backwards_still_stamps_increasing
      stamper = CueStamper.new
      stamps = [Time.at(100), Time.at(99), Time.at(98)].map { |t| stamper.stamp(t).to_r }
      assert_equal stamps.sort, stamps
      assert_equal stamps.uniq, stamps
    end

    def test_concurrent_stamps_are_unique
      # The stamper is shared by the OSC, Link, MIDI and gamepad APIs, each
      # dispatching cues from its own listener thread.
      stamper = CueStamper.new
      t = Time.at(100)
      stamps = 8.times.map do
        Thread.new { 250.times.map { stamper.stamp(t).to_r } }
      end.flat_map(&:value)
      assert_equal 2000, stamps.uniq.length
    end

    def test_burst_cues_all_reach_a_syncing_loop
      # The end-to-end shape of GH #1839: three OSC cues arrive in one
      # burst with the same clock read, a live_loop syncs on the path and
      # walks time forward from each received cue. Every cue must arrive.
      stamper = CueStamper.new
      history = EventHistory.new
      sys = ThreadId.new(-1)
      user = ThreadId.new(10, 2)
      clock = Time.at(100.5)
      [[1], [2], [3]].each do |val|
        history.set(stamper.stamp(clock), 0, sys, 0, 0, 60, "/osc/test", val)
      end

      received = []
      t = Time.at(100)
      while (e = history.get_next(t, 0, user, 0, 0, 60, "/osc/test"))
        received << e.val
        t = e.time
      end
      assert_equal [[1], [2], [3]], received
    end
  end
end
