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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

require "timeout"

module SonicPi
  # A live loop run again, from the same buffer or card or another, takes the
  # new code without missing a beat: the loop's thread goes on, running the
  # code of the run that last defined it (the Hotswap deck's cards all play
  # :groove, each taking it over from the last).
  #
  # Runs go through __spider_eval as the server's do. The loops wait on gates
  # the test opens, not on time, so what each run does is in the test's hands.
  class LiveLoopTakeoverTester < Minitest::Test
    WAIT = 5 # seconds, only ever spent when a test fails

    # No sound here: a live loop's scope tap just runs its block, and an error
    # is kept for the test to report rather than raised in the run's thread.
    class TakeoverLang < MockLang
      attr_reader :errors

      def initialize
        super
        @errors = Queue.new
      end

      def with_fx(*_args, &blk)
        blk.call
      end

      def __error(e, m = nil)
        @errors << e
      end
    end

    def setup
      @lang = TakeoverLang.new
      @log = Queue.new
      @gates = Hash.new { |h, k| h[k] = Queue.new }
      log = @log
      gates = @gates
      # what the runs' code calls to tell the test what it did, and to wait for it
      @lang.define_singleton_method(:__test_log) { |x| log << x }
      @lang.define_singleton_method(:__test_gate) { |name| stop if gates[name].pop == :stop }
      # where a sound played here would flash, as play and sample ask (Runtime#__caller_workspace_line)
      @lang.define_singleton_method(:__test_where) { __caller_workspace_line }
    end

    def teardown
      errs = errors_so_far
      @lang.instance_variable_get(:@user_jobs).each_id { |id| @lang.__stop_job(id) }
      assert_empty errs, "the runs raised nothing"
    end

    # A run from a workspace: a buffer's (workspace_one) or a card's
    # (sonic-pi-quickstart-0-1), as the server's /save-and-run-buffer and
    # /run-code send them.
    def run_from(workspace, code)
      @lang.__spider_eval(code, {workspace: workspace})
    end

    def next_logged(what)
      Timeout.timeout(WAIT) { @log.pop }
    rescue Timeout::Error
      flunk "waited for #{what}#{errors_so_far}"
    end

    def errors_so_far
      errs = []
      errs << @lang.errors.pop until @lang.errors.empty?
      errs.empty? ? "" : " - the runs raised: #{errs.map { |e| "#{e.class}: #{e.message}" }.join('; ')}"
    end

    # (a loop must sleep each time round, or Sonic Pi stops it)
    def loop_logging(name, what, gate)
      "live_loop #{name.inspect} do\n  __test_log #{what.inspect}\n  __test_gate #{gate.inspect}\n  sleep 0.01\nend\n"
    end

    def test_a_run_again_takes_over_its_own_live_loop
      run_from "workspace_one", loop_logging(:groove, :first, :groove)
      assert_equal :first, next_logged("the first run's loop")
      run_from "workspace_one", loop_logging(:groove, :second, :groove)
      @gates[:groove] << :go
      assert_equal :second, next_logged("the loop to go round with the second run's code")
    end

    def test_a_card_takes_over_another_cards_live_loop
      run_from "sonic-pi-quickstart-0-1", loop_logging(:groove, :first, :groove)
      assert_equal :first, next_logged("the first card's loop")
      run_from "sonic-pi-quickstart-0-2", loop_logging(:groove, :second, :groove)
      @gates[:groove] << :go
      assert_equal :second, next_logged("the loop to go round with the second card's code")
    end

    # A loop taken over runs on in the thread the first run started, but its
    # code is the run's that took it over: its sounds flash there, as its
    # header and end do (the Hotswap deck: the second card's lines light).
    def test_a_loop_another_card_takes_over_flashes_that_cards_lines
      run_from "sonic-pi-quickstart-0-1", "live_loop :groove do\n  __test_log __test_where\n  __test_gate :groove\n  sleep 0.01\nend\n"
      assert_equal ["sonic-pi-quickstart-0-1", 2], next_logged("the first card's sound")
      run_from "sonic-pi-quickstart-0-2", "live_loop :groove do\n  sleep 0.01\n  __test_log __test_where\n  __test_gate :groove\nend\n"
      @gates[:groove] << :go
      assert_equal ["sonic-pi-quickstart-0-2", 3], next_logged("the second card's sound")
    end

    def test_a_loop_another_buffer_takes_over_flashes_that_buffers_lines
      run_from "workspace_one", "live_loop :groove do\n  __test_log __test_where\n  __test_gate :groove\n  sleep 0.01\nend\n"
      assert_equal ["workspace_one", 2], next_logged("the first buffer's sound")
      run_from "workspace_two", "live_loop :groove do\n  sleep 0.01\n  __test_log __test_where\n  __test_gate :groove\nend\n"
      @gates[:groove] << :go
      assert_equal ["workspace_two", 3], next_logged("the second buffer's sound")
    end
  end
end
