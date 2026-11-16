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
require_relative "../lib/sonicpi/daemon/kill_switch"

# On 2026-11-13 the GUI sent no keep-alive for 50 s - it was running, its
# window open, but stalled - and the daemon read the silence as the GUI having
# gone: it killed SuperSonic and Spider, and the GUI sat on with nothing behind
# it. Silence is not absence. The daemon has a fact to go on instead: its stdin
# is a pipe whose other end only the GUI holds, so the pipe closes exactly when
# the GUI's process ends, however it ends, on every platform. These cases hold
# the kill switch to that: a silent GUI whose pipe is open stays up; a pipe
# that closes takes the session down at once; and a daemon launched without
# such a pipe keeps the old timeout, since silence is all it has.
module SonicPi
  class DaemonKillSwitchTester < Minitest::Test

    # The daemon's SafeExit, minus the exiting.
    class FakeExit
      attr_reader :exits
      def initialize; @exits = 0; end
      def exit; @exits += 1; end
    end

    # Timings a test can wait out: the daemon's are 40 s, 10 s and 4 windows.
    WARMUP = 0.1
    WINDOW = 0.1
    SILENT_WINDOWS = 2
    TIMEOUT = (SILENT_WINDOWS + 1) * WINDOW   # silence that would have killed the session

    def setup
      @lines = []
      @exit = FakeExit.new
      @switches = []
    end

    def teardown
      @switches.each(&:stop)
    end

    def kill_switch(gui_pipe: nil, silent_windows: SILENT_WINDOWS)
      ks = Daemon::KillSwitch.new(@exit, log: ->(m) { @lines << m }, gui_pipe: gui_pipe,
                                  warmup: WARMUP, window: WINDOW, silent_windows: silent_windows)
      @switches << ks
      ks
    end

    def test_without_a_pipe_silence_still_takes_the_session_down
      ks = kill_switch
      assert wait_until(WARMUP + TIMEOUT * 3) { @exit.exits > 0 },
             "silence with no pipe to watch must fire the kill switch, as it always has"
      assert ks.fired?
      assert @lines.any? { |l| l.include?("Kill switch timed out") }, @lines.inspect
    end

    def test_a_silent_gui_whose_pipe_is_open_is_left_running
      r, w = IO.pipe
      ks = kill_switch(gui_pipe: r)
      sleep WARMUP + TIMEOUT * 3
      refute ks.fired?, "a GUI that has gone quiet is not a GUI that has gone"
      assert_equal 0, @exit.exits
      assert @lines.any? { |l| l =~ /No keep-alive from the GUI for ~[\d.]+s/ }, @lines.inspect
      assert @lines.any? { |l| l.include?("still open") && l.include?("staying up") }, @lines.inspect
    ensure
      w.close
    end

    def test_the_pipe_closing_takes_the_session_down_at_once
      r, w = IO.pipe
      ks = kill_switch(gui_pipe: r)
      ks.keep_alive!
      w.close
      assert wait_until(1.0) { @exit.exits > 0 }, "the GUI's pipe closed: nothing to wait for"
      assert ks.fired?
      assert @lines.any? { |l| l.include?("pipe") && l.include?("closed") }, @lines.inspect
    end

    # The log's lines are about how long it has been since a keep-alive, so
    # this case keeps the time itself: the clock is a number it moves, and the
    # switch's wait for a window ends when it says so. Real sleeps here made
    # the case fail on a busy machine, where a feeder's short sleep overran a
    # window and the switch rightly logged the gap.
    def test_a_steady_gui_fires_nothing_and_a_gap_is_recorded_when_it_ends
      now = 0.0
      waiting, go = Queue.new, Queue.new
      ks = Daemon::KillSwitch.new(@exit, log: ->(m) { @lines << m },
                                  warmup: WARMUP, window: WINDOW, silent_windows: 10,
                                  clock: -> { now },
                                  sleeper: ->(_) { waiting << true; go.pop })
      @switches << ks
      # One window passes and the switch has looked at it.
      window_passes = -> { now += WINDOW; go << true; waiting.pop }
      waiting.pop                 # the switch is in its warm-up
      go << true; waiting.pop     # ... and now in its first window

      10.times do                 # a keep-alive in every window
        now += WINDOW / 2
        ks.keep_alive!
        now -= WINDOW / 2
        window_passes.call
      end
      refute ks.fired?
      refute @lines.any? { |l| l.include?("No keep-alive") }, @lines.inspect

      3.times { window_passes.call }   # a gap three windows long
      ks.keep_alive!
      assert @lines.any? { |l| l.include?("No keep-alive from the GUI") }, @lines.inspect
      assert @lines.any? { |l| l =~ /resumed after ~[\d.]+s/ }, @lines.inspect
      refute ks.fired?
    end

    # The real thing: a process standing in for the GUI holds the pipe's write
    # end. Stopped (SIGSTOP), it sends nothing and is still there - the daemon
    # stays up. Killed, the kernel closes its end - the daemon goes.
    def test_a_stopped_gui_process_keeps_the_session_and_a_killed_one_ends_it
      skip "SIGSTOP and fd mapping are POSIX" if RUBY_PLATFORM =~ /mingw|mswin/
      r, w = IO.pipe
      pid = Process.spawn(RbConfig.ruby, "-e", "sleep", w => w)
      w.close                     # only the stand-in holds the write end now
      ks = kill_switch(gui_pipe: r)

      Process.kill(:STOP, pid)
      sleep WARMUP + TIMEOUT * 3
      refute ks.fired?, "a stopped GUI is still a GUI"
      assert_equal 0, @exit.exits

      Process.kill(:CONT, pid)
      Process.kill(:KILL, pid)
      Process.wait(pid)
      assert wait_until(1.0) { @exit.exits > 0 }, "the GUI's process ended: its pipe closed"
      assert ks.fired?
    end
  end
end
