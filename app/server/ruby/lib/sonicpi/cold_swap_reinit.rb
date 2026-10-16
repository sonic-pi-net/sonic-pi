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
  # What the server does when the engine rebuilds its world (a cold swap,
  # announced by /clockwork/setup). The world is empty from that moment, so
  # the server halts at once — its jobs stop and the engine's schedule is
  # cleared — and sends nothing more into it. The rebuild waits for the
  # swaps to settle (a device churning sends a burst), runs once for the
  # burst, and is tried again when it falls short. A setup that arrives
  # during a rebuild gets a pass of its own.
  class ColdSwapReinit
    def initialize(halt:, rebuild:, quiet: 1.0, retries: 4, retry_delay: 2.0,
                   log: ->(msg) { STDOUT.puts "Spider - #{msg}"; STDOUT.flush })
      @halt = halt
      @rebuild = rebuild
      @quiet = quiet
      @retries = retries
      @retry_delay = retry_delay
      @log = log
      @mutex = Mutex.new
      @cv = ConditionVariable.new
      @last_setup = nil
      @halt_due = false
      @worker = nil
    end

    # From the thread the setup arrives on; the work is the worker's.
    def setup!(generation)
      @log.call("received /clockwork/setup gen #{generation}")
      @mutex.synchronize do
        @generation = generation
        @last_setup = now
        @halt_due = true
        @cv.broadcast
        @worker = Thread.new { work } unless @worker&.alive?
      end
    end

    # The engine refused something we sent, saying it was between instances
    # (/clockwork/error with "rebuilding", "no instance" or "stalled",
    # docs/RESTARTS.md). A pass under way could not reach the engine, so it
    # is not a failed pass of this generation: it spends no retry, and the
    # next pass waits for the setup the engine will send.
    def refused!(reason)
      @mutex.synchronize { @refused = reason }
    end

    # Until the passes asked for so far have run (for tests).
    def wait_idle
      worker = @mutex.synchronize { @worker }
      worker&.join
    end

    private

    def now
      Process.clock_gettime(Process::CLOCK_MONOTONIC)
    end

    def work
      # The retry budget is one generation's (docs/RESTARTS.md, "What a
      # client does"): a setup that arrives mid-pass opens a new one, and
      # the passes against it start with a budget of their own.
      retries = 0
      budget_for = nil
      loop do
        halt_if_due
        wait_for_quiet
        halt_if_due
        started = now
        generation = @mutex.synchronize { @refused = nil; @generation }
        if generation != budget_for
          budget_for = generation
          retries = 0
        end
        @log.call("setup settled, reinitialising...")
        ok = begin
          @rebuild.call
        rescue Exception => e
          @log.call("cold swap reinit error: #{e.message}\n#{e.backtrace.first(5).join("\n")}")
          false
        end
        refused = @mutex.synchronize { @refused }
        if ok
          retries = 0
        elsif refused
          # The engine was between instances while this pass ran, and said
          # so: nothing of this pass reached it. Not a failed pass of this
          # generation, so no retry is spent and none is scheduled — the
          # engine's setup is what comes next.
          @log.call("reinit could not reach the engine (#{refused}) - waiting for /clockwork/setup")
        elsif (retries += 1) <= @retries
          # The world has settled, so no further setup is coming to retry
          # for us: queue our own pass.
          @log.call("reinit incomplete, scheduling retry #{retries}/#{@retries}")
          Kernel.sleep @retry_delay
          @mutex.synchronize { @last_setup = now }
        else
          @log.call("reinit still incomplete after #{@retries} retries - waiting for next device event")
        end
        again = @mutex.synchronize do
          (@last_setup > started).tap { |more| @worker = nil unless more }
        end
        break unless again
      end
    end

    def halt_if_due
      due = @mutex.synchronize { @halt_due.tap { @halt_due = false } }
      return unless due
      begin
        @halt.call
      rescue Exception => e
        @log.call("cold swap halt error: #{e.message}")
      end
    end

    def wait_for_quiet
      @mutex.synchronize do
        loop do
          remaining = @quiet - (now - @last_setup)
          break if remaining <= 0
          @cv.wait(@mutex, remaining)
        end
      end
    end
  end
end
