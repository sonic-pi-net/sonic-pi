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

# LinkAPI answers the Link timeline's beat <-> time questions from the clock
# snapshot in shared memory, and asks the engine nothing. The RPC stays for
# the midi timelines (not in the arena) and as the fallback when there is no
# snapshot. No engine: the comms are a fake that records every rpc.

require_relative "../setup_test"
require_relative "../../lib/sonicpi/link_api"
require_relative "../../lib/sonicpi/clockwork_arena"

module SonicPi
  class LinkAPILocalClockTester < Minitest::Test
    NTP = LinkAPI::NTP_EPOCH_OFFSET

    class FakeComms
      attr_reader :rpcs, :sends
      def initialize(replies = {})
        @replies, @rpcs, @sends, @handlers = replies, [], [], {}
      end
      def rpc(pattern, *args, expect:, timeout: 1.0)
        @rpcs << [pattern, args, expect]
        r = @replies[expect]
        r.respond_to?(:call) ? r.call(args) : r
      end
      def send(pattern, *args)
        @sends << [pattern, args]
      end
      def add_method(addr, &blk)
        @handlers[addr] = blk
      end
      def subscribe_to_notifications!; end
      def push(addr, *args)
        @handlers.fetch(addr).call(args)
      end
    end

    class FakeReader
      attr_accessor :snap, :attaches
      def initialize(snap) ; @snap = snap; @attaches = 0; end
      def attach!  ; @attaches += 1; !@snap.nil?; end
      def reattach!; attach!; end
      def attached?; !@snap.nil?; end
      def snapshot ; @snap; end
      def detach!  ; @snap = nil; end
      def last_error; @snap ? nil : "fake reader: nothing served"; end
    end

    def snapshot(bpm: 120.0, origin_ntp: 3_900_000_000.0, playing: true, at: 3_900_000_000.0)
      ClockworkArena::ClockState.new(bpm: bpm, beat_origin_ntp: origin_ntp,
                                     is_playing: playing, is_playing_at_ntp: at,
                                     flags: 0, meter_num: 4, meter_den: 4)
    end

    def api(snap = snapshot, replies = {})
      @comms = FakeComms.new(replies)
      @reader = FakeReader.new(snap)
      LinkAPI.new("127.0.0.1", 4556, {}, comms: @comms, clock_reader: @reader)
    end

    # ── the Link timeline is answered locally ─────────────────────────────

    def test_time_at_beat_is_local_and_exact
      a = api(snapshot(bpm: 120.0, origin_ntp: 3_900_000_000.0))
      micros = a.link_get_time_at_beat(8)
      assert_equal((3_900_000_000.0 + 8 * 60.0 / 120.0) * 1e6, micros.to_f)
      assert_kind_of Integer, micros
      assert_empty @comms.rpcs
    end

    def test_time_at_beat_rounds_to_the_nearest_microsecond_like_the_engine
      # engine: llround(seconds * 1e6). origin + 1/3 beat at 90 bpm = origin + 0.2222...
      a = api(snapshot(bpm: 90.0, origin_ntp: 3_900_000_000.0))
      expected = (3_900_000_000.0 + (1.0 / 3.0) * 60.0 / 90.0) * 1e6
      assert_equal expected.round, a.link_get_time_at_beat(1.0 / 3.0)
    end

    def test_clock_time_at_beat_is_unix_seconds
      a = api(snapshot(bpm: 60.0, origin_ntp: NTP + 1_700_000_000.0))
      assert_in_delta 1_700_000_000.0 + 10.0, a.link_get_clock_time_at_beat(10), 1e-6
      assert_empty @comms.rpcs
    end

    def test_beat_and_phase_at_time_are_local
      a = api(snapshot(bpm: 120.0, origin_ntp: 1000.0))
      t = ((1000.0 + 2.75) * 1e6).round          # 5.5 beats in
      assert_in_delta 5.5, a.link_get_beat_at_time(t, 4), 1e-9
      assert_in_delta 1.5, a.link_get_phase_at_time(t, 4), 1e-9
      assert_empty @comms.rpcs
    end

    def test_beat_and_phase_at_clock_time_is_local
      a = api(snapshot(bpm: 120.0, origin_ntp: 1000.0))
      beat, phase = a.link_get_beat_and_phase_at_clock_time(1000.0 + 2.75 - NTP, 4)
      assert_in_delta 5.5, beat, 1e-9
      assert_in_delta 1.5, phase, 1e-9
      assert_empty @comms.rpcs
    end

    def test_now_beat_and_phase_reads_the_wall_clock
      origin = Time.now.to_f + NTP - 10.0        # beat 0 was ten seconds ago
      a = api(snapshot(bpm: 60.0, origin_ntp: origin))
      before = ((Time.now.to_f + NTP) * 1e6).round
      t, beat, phase = a.link_get_now_beat_and_phase(4)
      after = ((Time.now.to_f + NTP) * 1e6).round
      assert t.between?(before, after), "now #{t} not within [#{before}, #{after}]"
      assert_in_delta 10.0, beat, 0.05
      assert_in_delta 2.0, phase, 0.05
      assert_empty @comms.rpcs
    end

    def test_current_time_is_local
      a = api
      before = ((Time.now.to_f + NTP) * 1e6).round
      t = a.link_current_time
      assert t >= before
      assert_empty @comms.rpcs
    end

    def test_current_time_and_beat_quantises_locally
      a = api(snapshot(bpm: 60.0, origin_ntp: Time.now.to_f + NTP - 10.4))
      clock_time, beat = a.link_current_time_and_beat
      assert_equal 11, beat
      assert_in_delta Time.now.to_f + 0.6, clock_time, 0.05
      assert_empty @comms.rpcs
    end

    def test_tempo_comes_from_the_snapshot
      a = api(snapshot(bpm: 133.0))
      assert_equal 133.0, a.link_tempo
      assert_empty @comms.rpcs
    end

    def test_transport_state_is_local
      a = api(snapshot(playing: true, at: NTP + 1_700_000_000.0))
      s = a.link_transport_state
      assert_equal true, s[:playing]
      assert_equal true, s[:anchored]
      assert_in_delta 1_700_000_000.0, s[:at], 1e-6
      assert_empty @comms.rpcs
      assert a.link_is_playing?
    end

    def test_next_beat_at_phase_needs_no_rpc
      a = api(snapshot(bpm: 120.0, origin_ntp: Time.now.to_f + NTP))
      beat, time = a.link_get_next_beat_and_clock_time_at_phase(0, 4, 0.5)
      assert_equal 0, beat % 4
      assert time > Time.now.to_f + 0.4
      assert_empty @comms.rpcs
    end

    # ── what still goes to the engine ─────────────────────────────────────

    def test_midi_timelines_still_use_the_rpc
      a = api(snapshot, "/clockwork/clock/midi/rpc/time_at_beat.reply" => [123_456])
      assert_equal 123_456, a.link_get_time_at_beat(4, 4, tl: "midi")
      assert_equal 1, @comms.rpcs.size
      assert_equal "/clockwork/clock/midi/rpc/time_at_beat", @comms.rpcs[0][0]
    end

    def test_without_a_snapshot_the_rpc_is_the_fallback
      a = api(nil, "/clockwork/clock/rpc/time_at_beat.reply" => [777])
      assert_equal 777, a.link_get_time_at_beat(1)
      assert_equal 1, @comms.rpcs.size
    end

    def test_a_snapshot_that_disappears_mid_session_falls_back
      a = api(snapshot, "/clockwork/clock/rpc/time_at_beat.reply" => [777])
      a.link_get_time_at_beat(1)
      assert_empty @comms.rpcs
      @reader.snap = nil                          # engine re-laying out, or gone
      assert_equal 777, a.link_get_time_at_beat(1)
      assert_equal 1, @comms.rpcs.size
    end

    def test_a_missing_reader_attempts_to_attach_at_start
      a = api(nil)
      a.link_system_start!
      assert_equal 1, @reader.attaches
    end

    def test_reattach_reaches_the_reader
      a = api(snapshot)
      a.reattach_clock_reader!
      assert_equal 1, @reader.attaches
    end

    def test_the_default_reader_is_the_shared_memory_one
      a = LinkAPI.new("127.0.0.1", 4556, {}, comms: FakeComms.new)
      assert_kind_of ClockworkClockReader, a.instance_variable_get(:@clock_reader)
    end

    def test_the_environment_can_force_the_rpc_path
      saved = ENV["SONIC_PI_LINK_CLOCK_RPC"]
      ENV["SONIC_PI_LINK_CLOCK_RPC"] = "1"
      comms = FakeComms.new("/clockwork/clock/rpc/time_at_beat.reply" => [5])
      a = LinkAPI.new("127.0.0.1", 4556, {}, comms: comms)
      assert_nil a.instance_variable_get(:@clock_reader)
      assert_equal 5, a.link_get_time_at_beat(1)
      assert_equal 1, comms.rpcs.size
    ensure
      saved.nil? ? ENV.delete("SONIC_PI_LINK_CLOCK_RPC") : ENV["SONIC_PI_LINK_CLOCK_RPC"] = saved
    end

    def test_set_bpm_still_asks_the_engine
      # Writing the grid is the engine's; only reading moved.
      a = api(snapshot)
      Thread.new { sleep 0.02; @comms.push("/clockwork/clock/notify/tempo", 100.0) }
      a.link_set_bpm!(100.0)
      assert_equal ["/clockwork/clock/tempo/set", [100.0]], @comms.sends.last
    end

    def test_a_tempo_notify_wakes_a_link_sleep
      a = api(snapshot)
      woken = false
      Thread.new { sleep 0.02; @comms.push("/clockwork/clock/notify/tempo", 100.0) }
      a.link_sleep(1.0) { woken = true }
      assert woken
    end
  end
end
