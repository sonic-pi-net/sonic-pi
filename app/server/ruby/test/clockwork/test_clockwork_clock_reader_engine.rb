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
require_relative "../mix/lib/mix_engine"
require_relative "../../lib/sonicpi/link_api"
require_relative "../../lib/sonicpi/clockwork_clock_reader"

module SonicPi
  class ClockworkClockReaderEngineTester < Minitest::Test
    ENGINE = MixEngine::ENGINE

    @@engine = nil
    @@unavailable = nil

    def self.engine
      return @@engine if @@engine
      unless File.executable?(ENGINE)
        @@unavailable = "engine not built at #{ENGINE}"
        return nil
      end
      @@engine = EngineProcess.new.start
      Minitest.after_run { @@engine.stop }
      @@engine
    end

    class EngineProcess
      attr_reader :port, :api

      def start
        @port = MixEngine.free_port_pair
        @log = File.join(Dir.tmpdir, "sonic-pi-clock-reader-#{@port}.log")
        # -u and --tcp on one port, as the daemon starts it: the command plane
        # is TCP (LinkAPI's transport), the attach endpoint derives from -u.
        @pid = Process.spawn(ENGINE, "--headless", "-u", @port.to_s, "--tcp", @port.to_s,
                             "-o", "2", "-i", "0", out: @log, err: [:child, :out])
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
        self
      end

      def stop
        return unless @pid
        Process.kill("TERM", @pid) rescue nil
        begin
          Timeout.timeout(3) { Process.wait(@pid) }
        rescue StandardError
          Process.kill("KILL", @pid) rescue nil
          Process.wait(@pid) rescue nil
        end
        @pid = nil
      end
    end

    def setup
      @engine = self.class.engine
      skip @@unavailable unless @engine
      @rpc = @engine.api                                  # answers from the engine
      @reader = ClockworkClockReader.new(port: @engine.port)
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
      arena = @reader.instance_variable_get(:@arena)
      # An engine from before the audience words says nothing about who a
      # region is for, and is not judged. One that does say must say this.
      skip "engine's table predates audience words (clockwork < 0.81)" unless arena.states_audiences?
      audience = arena.audience(ClockworkArena::REGION_CLOCK_STATE)
      assert_equal ClockworkArena::AUDIENCE_PUBLISHED, audience & ClockworkArena::AUDIENCE_PUBLISHED,
                   "the engine's table must mark the clock state as a published contract (got #{audience})"
      block_end, = arena.published_ends
      assert arena.clock_state_offset - 64 + ClockworkArena::CLOCK_STATE_BYTES <= block_end + 64,
             "the clock state must lie inside the block's published run"
    end

    def test_time_at_beat_agrees_with_the_engine_to_the_microsecond
      s = @reader.snapshot
      BEATS.each do |b|
        engine = @rpc.link_get_time_at_beat(b)
        local  = (s.time_at_beat(b) * 1e6).round
        assert_in_delta engine, local, 1, "beat #{b}"
      end
    end

    def test_beat_at_time_agrees_with_the_engine
      s = @reader.snapshot
      now = @rpc.link_current_time
      [now, now + 1_000_000, now - 60_000_000].each do |t|
        engine = @rpc.link_get_beat_at_time(t)
        local  = s.beat_at_time(t / 1e6)
        assert_in_delta engine, local, 1e-6, "time #{t}"
      end
    end

    def test_a_tempo_change_moves_the_grid_in_shared_memory_and_keeps_the_beat
      s0 = @reader.snapshot
      now_ntp = Time.now.to_f + LinkAPI::NTP_EPOCH_OFFSET
      beat_before = s0.beat_at_time(now_ntp)
      new_bpm = s0.bpm == 100.0 ? 110.0 : 100.0
      @rpc.link_set_bpm!(new_bpm)
      s1 = nil
      20.times do
        s1 = @reader.snapshot
        break if s1 && s1.bpm == new_bpm
        sleep 0.05
      end
      assert_equal new_bpm, s1.bpm, "shared memory never showed the new tempo"
      refute_equal s0.beat_origin_ntp, s1.beat_origin_ntp, "a retempo re-anchors the origin"
      # The beat playing at the moment of the change is where it was (retempo).
      assert_in_delta beat_before, s1.beat_at_time(now_ntp), 0.5
      # And the engine agrees with the new grid.
      assert_in_delta @rpc.link_get_time_at_beat(1000), (s1.time_at_beat(1000) * 1e6).round, 1
    ensure
      @rpc.link_set_bpm!(s0.bpm) if s0
    end

    def test_the_local_link_api_matches_the_rpc_link_api
      local = LinkAPI.new("127.0.0.1", @engine.port, {}, clock_reader: @reader)
      BEATS.each do |b|
        assert_in_delta @rpc.link_get_time_at_beat(b), local.link_get_time_at_beat(b), 1, "beat #{b}"
      end
      t_rpc, b_rpc, p_rpc = @rpc.link_get_now_beat_and_phase(4)
      t_loc, b_loc, p_loc = local.link_get_now_beat_and_phase(4)
      assert_in_delta t_rpc, t_loc, 50_000            # two clocks read 50 ms apart at most
      assert_in_delta b_rpc, b_loc, 0.2
      assert_in_delta p_rpc, p_loc, 0.2
    end
  end
end
