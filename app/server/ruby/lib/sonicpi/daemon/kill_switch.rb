#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2021, 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../promise"

module SonicPi
  module Daemon
    # Takes the session down when the process that launched the daemon - the
    # GUI - has gone, so SuperSonic and Spider are never left running on their
    # own. Two signs of the GUI are watched:
    #
    # The GUI's end of the daemon's stdin pipe (`gui_pipe`). The GUI launches
    # the daemon with its stdin as a pipe it alone holds, so the pipe closes
    # exactly when the GUI's process ends, however it ends: quit, crash, force
    # quit. That is the fact, on every platform, and it fires the switch at
    # once. A daemon launched without such a pipe (a terminal, another client)
    # passes nil here.
    #
    # Keep-alives (`keep_alive!`), which the GUI sends every few seconds. With
    # no pipe to watch, silence for longer than `timeout` is all there is to
    # go on and it fires the switch, as it did from 2022. With a pipe, silence
    # is recorded and nothing more: on 2026-11-13 the GUI stalled for 50 s
    # with its window open, and the silence was read as the GUI having gone.
    # Every gap of a window or more is logged, and so is its end, so the next
    # stall leaves its length in the daemon log.
    class KillSwitch
      def initialize(safe_exit, log:, gui_pipe: nil, warmup: 40, window: 10, silent_windows: 4,
                     clock: -> { Process.clock_gettime(Process::CLOCK_MONOTONIC) },
                     sleeper: Kernel.method(:sleep))
        @safe_exit = safe_exit
        @log       = log
        @gui_pipe  = gui_pipe
        @window    = window
        @timeout   = (silent_windows + 1) * window
        @clock     = clock

        @fired            = Promise.new
        @fire_mut         = Mutex.new
        @last_seen        = clock.call
        @silence_reported = false   # a line went out for the gap under way
        @stay_up_reported = false   # ... and one for staying up through it

        activate

        @timer_thread = Thread.new do
          sleeper.call warmup
          loop do
            sleeper.call window
            check_silence
          end
        end

        @pipe_thread = gui_pipe && Thread.new do
          begin
            watch(gui_pipe)   # returns only once the GUI's end has closed
          rescue EOFError, IOError, SystemCallError
          end
          fire("The GUI's end of the daemon's stdin pipe closed: the GUI has gone.")
        end
      end

      def keep_alive!
        now = @clock.call
        if @silence_reported
          @log.call("Keep-alives from the GUI resumed after ~#{(now - @last_seen).round(1)}s without one.")
          @silence_reported = false
          @stay_up_reported = false
        end
        @last_seen = now
      end

      def fired?
        @fired.delivered?
      end

      def wait
        @fired.get
      end

      def activate
        return if @armed_thread
        @armed_thread = Thread.new do
          wait
          @log.call("[EXIT] Daemon kill switch triggered. Exiting...")
          [@timer_thread, @pipe_thread].compact.each(&:kill)
          @safe_exit.exit
        end
      end

      # Ends the watching without firing.
      def stop
        [@armed_thread, @timer_thread, @pipe_thread].compact.each(&:kill)
      end

      private

      # Never waits in a read. On Windows a read waiting on the daemon's stdin
      # held up every process the daemon started until the read ended, so
      # SuperSonic did not start until the GUI gave up and closed the pipe
      # (#3614). IO.select asks whether the pipe can be read (on Windows it
      # looks without reading), and only then is it read: what the GUI wrote,
      # which is nothing, or EOFError once its end has closed.
      def watch(pipe)
        loop { pipe.readpartial(4096) if IO.select([pipe], nil, nil, 1) }
      end

      def check_silence
        gap = @clock.call - @last_seen
        return if gap < @window
        @log.call("No keep-alive from the GUI for ~#{gap.round(1)}s.")
        @silence_reported = true
        return if gap < @timeout
        if @gui_pipe
          return if @stay_up_reported
          @log.call("The GUI's pipe is still open, so it is still running: staying up.")
          @stay_up_reported = true
        else
          fire("Kill switch timed out...")
        end
      end

      def fire(reason)
        @fire_mut.synchronize do
          return if @fired.delivered?
          @log.call(reason)
          @fired.deliver!(true)
        end
      end
    end
  end
end
