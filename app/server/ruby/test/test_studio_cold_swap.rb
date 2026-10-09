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
require_relative "../lib/sonicpi/studio"
require_relative "./lib/headless_engine"

module SonicPi
  # Regression guarded against: changing audio device while code ran left
  # Sonic Pi silent until the next device event. The halt stops the jobs, and
  # each job's teardown ends in the idle pause, which froze the root group of
  # the engine's new world while the rebuild was loading synthdefs. The
  # server-info probe that came next was made inside the frozen root, never
  # answered, and was never freed, so the rebuild got no mixer and every retry
  # collided with it ("Duplicate node ID: 1").
  #
  # A real Studio against a headless engine. Skipped when the engine is not
  # built, as the mix tests are.
  class StudioColdSwapTester < Minitest::Test
    def setup
      skip "engine not built at #{MixEngine::ENGINE}" unless HeadlessEngine.available?
      @engine = HeadlessEngine.new("cold-swap-test")
      @studio = Studio.new({scsynth_port: @engine.port, scsynth_send_port: @engine.port}, Queue.new,
                           HeadlessEngine::State.new, ->(*) {}, -> { nil })
      @server = @studio.server
    end

    def teardown
      # A failure here is the engine's story as much as the studio's, and on
      # CI its log is otherwise left behind on the runner.
      puts "\n--- #{name}: the engine's log, #{@engine.log} ---", @engine.log_tail unless passed? || @engine.nil?
      @server&.shutdown rescue nil
      @engine&.stop
    end

    def test_a_rebuild_comes_through_a_pause_that_reached_the_new_world_first
      @studio.pause
      @studio.cold_swap_reinit!
      refute_nil @studio.mixer_group, "the rebuild ended with no mixer; see #{@engine.log}"
      refute root_runs?, "nothing is playing, so the rebuilt graph should rest"
    end

    def test_a_pause_that_comes_during_a_rebuild_waits_for_it
      pause = nil
      just_before_the_rebuild_probes do
        pause = Thread.new { @studio.pause }
        Timeout.timeout(5) { Thread.pass until !pause.alive? || waiting_on_the_studio_gate?(pause) }
      end
      @studio.cold_swap_reinit!
      refute_nil @studio.mixer_group, "the rebuild ended with no mixer; see #{@engine.log}"
      assert pause.join(5), "the pause never went ahead after the rebuild"
      refute root_runs?, "the pause that waited for the rebuild should still rest the graph"
    end

    # /synced says the commands before it are done, not that the notifications
    # they caused have arrived: an old node's /n_end can come after it. A new
    # node with the old one's number would take that end as its own.
    def test_a_rebuild_never_gives_a_new_node_an_old_ones_number
      before = @studio.mixer_group.to_i
      @studio.cold_swap_reinit!
      refute_nil @studio.mixer_group, "the rebuild ended with no mixer; see #{@engine.log}"
      assert_operator @studio.mixer_group.to_i, :>, before
    end

    def test_a_probe_that_got_no_answer_leaves_nothing_behind
      @server.node_pause(0, true)
      assert_raises(StandardError) { @server.fetch_scsynth_info!(1) }
      refute node_exists?(1), "a probe left in the graph collides with the next one"
    end

    private

    # The server-info probe answers only from a running graph.
    def root_runs?
      @server.fetch_scsynth_info!(1)
      true
    rescue StandardError
      false
    end

    def node_exists?(id)
      answer = Promise.new
      @server.add_event_oneshot_handler("/n_info") { |payload| answer.deliver!(true) if payload.to_a[0] == id }
      @server.add_event_oneshot_handler("/fail") { |payload| answer.deliver!(false) if payload.to_a[0] == "/n_query" }
      @server.osc("/n_query", id)
      answer.get(5)
    end

    # The last moment a pause can do harm: the probe is made, and answers,
    # only in a running graph.
    def just_before_the_rebuild_probes(&during)
      probe = @server.method(:fetch_scsynth_info!)
      server = @server
      @server.define_singleton_method(:fetch_scsynth_info!) do |*args|
        server.singleton_class.send(:remove_method, :fetch_scsynth_info!)
        during.call
        probe.call(*args)
      end
    end

    def waiting_on_the_studio_gate?(thread)
      thread.status == "sleep" && (thread.backtrace || []).any? { |frame| frame.include?("studio_ready_gate.rb") }
    end
  end
end
