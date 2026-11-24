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

# The reader against the REAL engine: the shipped SuperSonic, headless, on a
# free port. This is the test that says the Ruby mirror of clockwork's
# layout is right — every answer the reader computes from the arena is
# compared with the engine's own RPC answer from the same clock. Skips when
# the engine is not built, like the mix tests.

require_relative "../setup_test"
require_relative "../lib/headless_engine"
require_relative "../../lib/sonicpi/link_api"
require_relative "../../lib/sonicpi/clockwork_clock_reader"

module SonicPi
  class ClockworkClockReaderEngineTester < Minitest::Test
    @@engine = nil
    @@unavailable = nil

    def self.engine
      return @@engine if @@engine
      unless HeadlessEngine.available?
        @@unavailable = "engine not built at #{MixEngine::ENGINE}"
        return nil
      end
      @@engine = EngineProcess.new.start
      Minitest.after_run { @@engine.stop }
      @@engine
    end

    # The headless engine plus a LinkAPI on its command port (TCP, the
    # transport Spider uses); the attach endpoint derives from the same port.
    class EngineProcess
      attr_reader :port, :api

      def start
        @engine = HeadlessEngine.new("clock-reader")
        @port = @engine.port
        @log = @engine.log
        deadline = Time.now + 20
        begin
          @api = LinkAPI.new("127.0.0.1", @port, {}, clock_reader: nil)
          @api.link_system_start!
        rescue StandardError => e
          raise "engine did not accept a connection on #{@port}: #{e} (see #{@log})" if Time.now > deadline
          sleep 0.2
          retry
        end
        until @api.link_current_time != 0
          raise "engine never answered a clock query (see #{@log})" if Time.now > deadline
          sleep 0.1
        end
        # From here the engine is up, and a question it does not answer is a failure. Left as it is, the connection
        # gives up after a second and LinkAPI reads that as 0, which a test would then compare as if it were an answer.
        @api.instance_variable_get(:@link_comms).extend(AnswersOrFails)
        self
      end

      module AnswersOrFails
        def rpc(req_pattern, *args, expect:, timeout: 1.0)
          super(req_pattern, *args, expect: expect, timeout: 30) || raise("the engine did not answer #{req_pattern} in 30 seconds")
        end
      end

      def stop
        @engine&.stop
        @engine = nil
        EngineConnection.reset!
      end
    end

    def setup
      @engine = self.class.engine
      skip @@unavailable unless @engine
      @rpc = @engine.api                                  # answers from the engine
      @reader = ClockworkClockReader.new(port: @engine.port, secret: HeadlessEngine::SECRET)
      assert @reader.attach!, "attach failed: #{@reader.last_error}"
    end

    def teardown
      @reader&.detach!
    end

    BEATS = [0, 1, 1.5, 100, 4096.25, 1_000_000, -3].freeze

    def test_the_arena_is_where_the_mirror_says_it_is
      s = @reader.snapshot
      refute_nil s
      assert s.bpm >= 1.0
      assert_equal 4, s.meter_num
    end

    def test_the_engine_publishes_the_clock_state_for_clients
      clock = @reader.instance_variable_get(:@arena).region(ClockworkArena::REGION_CLOCK_STATE)
      assert_equal ClockworkArena::AREA_CLOCKWORK_STATE, clock[:area],
                   "the clock state must lie in clockwork's state area, which clients read"
      assert_equal ClockworkArena::AUDIENCE_PUBLIC, clock[:audience] & ClockworkArena::AUDIENCE_PUBLIC,
                   "the engine's table must mark the clock state public (got #{clock[:audience]})"
    end

    # The engine asked a question between two reads of the grid, again until the grid stood still around it: the
    # grid that was read, and what the block gave back. The engine answers from whichever grid is published when it
    # is asked, and it publishes a tempo change twice: at once, as it was asked for, and again when Link has taken
    # it (LinkSession::applyTempoChange, on Link's thread), with an origin that can be microseconds from the first.
    # A test before this one may have left such a change still landing.
    def asked_of_a_still_grid
      40.times do
        before = @reader.snapshot
        answer = yield before
        after = @reader.snapshot
        return [before, answer] if [before.bpm, before.beat_origin_ntp] == [after.bpm, after.beat_origin_ntp]
      end
      flunk "the grid moved under every one of 40 questions"
    end

    def test_time_at_beat_agrees_with_the_engine_to_the_microsecond
      BEATS.each do |b|
        s, engine = asked_of_a_still_grid { @rpc.link_get_time_at_beat(b) }
        assert_in_delta engine, (s.time_at_beat(b) * 1e6).round, 1, "beat #{b}"
      end
    end

    def test_beat_at_time_agrees_with_the_engine
      now = @rpc.link_current_time
      [now, now + 1_000_000, now - 60_000_000].each do |t|
        s, engine = asked_of_a_still_grid { @rpc.link_get_beat_at_time(t) }
        assert_in_delta engine, s.beat_at_time(t / 1e6), 1e-6, "time #{t}"
      end
    end

    def test_a_tempo_change_moves_the_grid_in_shared_memory_and_keeps_the_beat
      s0 = @reader.snapshot
      now_ntp = Time.now.to_f + LinkAPI::NTP_EPOCH_OFFSET
      beat_before = s0.beat_at_time(now_ntp)
      new_bpm = s0.bpm == 100.0 ? 110.0 : 100.0
      @rpc.link_set_bpm!(new_bpm)
      s1 = nil
      deadline = Time.now + 30
      loop do
        s1 = @reader.snapshot
        break if (s1 && s1.bpm == new_bpm) || Time.now > deadline
        sleep 0.05
      end
      assert_equal new_bpm, s1.bpm, "shared memory never showed the new tempo"
      refute_equal s0.beat_origin_ntp, s1.beat_origin_ntp, "a retempo re-anchors the origin"
      # The beat playing at the moment of the change is where it was (retempo).
      assert_in_delta beat_before, s1.beat_at_time(now_ntp), 0.5
      # And the engine agrees with the new grid.
      s2, engine = asked_of_a_still_grid { @rpc.link_get_time_at_beat(1000) }
      assert_in_delta engine, (s2.time_at_beat(1000) * 1e6).round, 1
    ensure
      @rpc.link_set_bpm!(s0.bpm) if s0
    end

    def test_the_local_link_api_matches_the_rpc_link_api
      local = LinkAPI.new("127.0.0.1", @engine.port, {}, clock_reader: @reader)
      BEATS.each do |b|
        _, (engine, mine) = asked_of_a_still_grid { [@rpc.link_get_time_at_beat(b), local.link_get_time_at_beat(b)] }
        assert_in_delta engine, mine, 1, "beat #{b}"
      end
      # The two are asked for now one after the other, however long apart that turns out to be: the beat and the
      # phase the second gives are the first's, moved on by the time between them.
      s, ((t_rpc, b_rpc, p_rpc), (t_loc, b_loc, p_loc)) =
        asked_of_a_still_grid { [@rpc.link_get_now_beat_and_phase(4), local.link_get_now_beat_and_phase(4)] }
      passed = (t_loc - t_rpc) / 1e6 * s.bpm / 60.0
      assert_in_delta b_rpc + passed, b_loc, 1e-3
      off = (p_loc - p_rpc - passed) % 4
      assert [off, 4 - off].min < 1e-3, "phase #{p_rpc} then #{p_loc}, #{passed} beats later"
    end
  end
end
