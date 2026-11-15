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
require_relative "../lib/sonicpi/stall_monitor"

# Spider has long had a heartbeat that writes a line whenever the whole
# process stops being scheduled. The daemon had none, so when the GUI went
# quiet for 50 s on 2026-11-13 nothing recorded whether the daemon itself had
# been running. One monitor now serves both; these cases drive its heartbeat
# with a clock of the test's own, so a stall is a number, not a wait.
module SonicPi
  class StallMonitorTester < Minitest::Test

    def setup
      @now = 1000.0
      @lines = []
      @monitor = StallMonitor.new("Daemon", out: ->(l) { @lines << l },
                                  interval: 0.05, stall_ms: 150, telemetry_every: 60.0,
                                  clock: -> { @now })
    end

    def tick_after(seconds)
      @now += seconds
      @monitor.tick
    end

    def test_a_heartbeat_on_time_writes_nothing
      tick_after(0.05)
      tick_after(0.06)
      assert_empty @lines
    end

    def test_a_late_heartbeat_names_the_stall_and_who_stalled
      stall = tick_after(0.05 + 0.4)
      assert_equal 400, stall
      assert_equal 1, @lines.size
      assert_match(/\ADaemon - PROCESS STALL: ~400ms \(GC time in window: \d+ms, GC count: \d+\)\z/, @lines[0])
    end

    def test_lateness_under_the_threshold_is_not_a_stall
      assert_nil tick_after(0.05 + 0.1)
      assert_empty @lines
    end

    def test_heap_telemetry_comes_once_a_minute
      tick_after(0.05)
      assert_empty @lines
      tick_after(60.0)
      heap = @lines.select { |l| l.include?("HEAP") }
      assert_equal 1, heap.size
      assert_match(/\ADaemon - HEAP: live=\d+ alloc\/min=\d+ majorGC\/min=[\d.]+ gc_time_total=\d+ms\z/, heap[0])
    end
  end
end
