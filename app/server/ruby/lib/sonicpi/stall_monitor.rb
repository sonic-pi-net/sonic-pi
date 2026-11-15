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

module SonicPi
  # A heartbeat that notices when this whole Ruby process stops being
  # scheduled - a major GC, a page-in, the OS not running it - for long enough
  # to matter. In Spider a ~0.5 s stall pushes every scheduled thread past its
  # sched-ahead window and shows up engine-side only as an unexplained "LATE";
  # in the daemon it is the difference between the GUI going quiet and the
  # daemon not being there to hear it. One line per stall, with the GC time in
  # the window so the log says whether GC caused it. Heap telemetry once a
  # minute, because allocation churn is the usual cause of whole-process stalls.
  #
  # `out` takes each line. `clock` and `tick` are the seams the tests use: a
  # stall is then a number, not a wait.
  class StallMonitor
    def initialize(label, out:, interval: 0.05, stall_ms: 150, telemetry_every: 60.0,
                   clock: -> { Process.clock_gettime(Process::CLOCK_MONOTONIC) })
      @label           = label
      @out             = out
      @interval        = interval
      @stall_ms        = stall_ms
      @telemetry_every = telemetry_every
      @clock           = clock

      GC.measure_total_time = true if GC.respond_to?(:measure_total_time=)
      @gc_buf = {}   # reused so the heartbeat itself allocates ~nothing per tick
      GC.stat(@gc_buf)
      @gc_time_last   = @gc_buf[:time] || 0
      @alloc_last     = @gc_buf[:total_allocated_objects] || 0
      @major_last     = @gc_buf[:major_gc_count] || 0
      @last           = @clock.call
      @last_telemetry = @last
      @thread         = nil
    end

    # Runs the heartbeat on a thread of its own, above the work it watches.
    def start
      @thread = Thread.new do
        Thread.current.priority = 20
        loop do
          sleep @interval
          tick
        end
      end
      self
    end

    def stop
      @thread&.kill
    end

    # One heartbeat. Returns the stall it reported, in ms, or nil.
    def tick
      now = @clock.call
      stall_ms = ((now - @last - @interval) * 1000).round
      GC.stat(@gc_buf)
      reported = nil
      if stall_ms > @stall_ms
        gc_time_now = @gc_buf[:time] || 0
        @out.call("#{@label} - PROCESS STALL: ~#{stall_ms}ms " \
                  "(GC time in window: #{gc_time_now - @gc_time_last}ms, " \
                  "GC count: #{GC.count})")
        reported = stall_ms
      end
      @gc_time_last = @gc_buf[:time] || 0
      if now - @last_telemetry >= @telemetry_every
        alloc_now = @gc_buf[:total_allocated_objects] || 0
        major_now = @gc_buf[:major_gc_count] || 0
        mins = (now - @last_telemetry) / 60.0
        @out.call("#{@label} - HEAP: live=#{@gc_buf[:heap_live_slots]} " \
                  "alloc/min=#{((alloc_now - @alloc_last) / mins).round} " \
                  "majorGC/min=#{format('%.1f', (major_now - @major_last) / mins)} " \
                  "gc_time_total=#{@gc_buf[:time] || 0}ms")
        @last_telemetry = now
        @alloc_last = alloc_now
        @major_last = major_now
      end
      @last = now
      reported
    end
  end
end
