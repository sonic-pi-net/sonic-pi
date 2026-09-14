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

# The reader the spider holds: attach to an endpoint, map the segment, hand
# out clock snapshots. Against the fake endpoint end to end.

require_relative "../setup_test"
require_relative "fake_segment"
require_relative "../../lib/sonicpi/clockwork_clock_reader"

module SonicPi
  class ClockworkClockReaderTester < Minitest::Test
    F = FakeClockworkSegment

    def setup
      skip "Unix sockets only" if ClockworkShmAttach.windows?
    end

    def teardown
      @reader&.detach!
      @served&.close
    end

    def serve(**fields)
      @served = F::Served.new(F.build(**fields))
      @reader = ClockworkClockReader.new(endpoint: @served.path)
    end

    def test_is_detached_until_attached
      serve
      refute @reader.attached?
      assert_nil @reader.snapshot
    end

    def test_attaches_and_snapshots_the_clock
      serve(bpm: 130.0, origin_ntp: 3_900_000_000.5, playing: true, playing_at_ntp: 3_900_000_001.0)
      assert @reader.attach!
      assert @reader.attached?
      s = @reader.snapshot
      assert_equal 130.0, s.bpm
      assert_equal 3_900_000_000.5, s.beat_origin_ntp
      assert_equal true, s.is_playing
      assert_equal 3_900_000_001.0, s.is_playing_at_ntp
      assert_equal 4, s.meter_num
    end

    def test_snapshots_are_live_reads_of_the_engines_memory
      serve(bpm: 100.0, origin_ntp: 10.0)
      @reader.attach!
      assert_equal 100.0, @reader.snapshot.bpm
      @served.set_clock(bpm: 90.0, origin_ntp: 20.0)      # the engine re-anchors
      s = @reader.snapshot
      assert_equal 90.0, s.bpm
      assert_equal 20.0, s.beat_origin_ntp
    end

    def test_attach_failure_is_reported_not_raised
      @reader = ClockworkClockReader.new(endpoint: "/nonexistent/clockwork-shm-9.sock")
      refute @reader.attach!
      refute @reader.attached?
      assert_match(/connect/, @reader.last_error)
      assert_nil @reader.snapshot
    end

    def test_a_segment_that_is_not_an_arena_is_an_attach_failure
      @served = F::Served.new(F.build(arena_magic: 0x0BADF00D))
      @reader = ClockworkClockReader.new(endpoint: @served.path)
      refute @reader.attach!
      assert_match(/not a clockwork arena/, @reader.last_error)
    end

    def test_an_arena_unpublished_at_snapshot_time_yields_no_snapshot
      # Published at attach, then the engine re-lays it out: the header's
      # state says so, and the reader answers nothing rather than a torn grid.
      serve
      @reader.attach!
      refute_nil @reader.snapshot
      state_off = 64 + 10 * 4      # arena header word 10: state
      @served.file.seek(state_off); @served.file.write([0].pack("L<")); @served.file.flush
      assert_nil @reader.snapshot
      @served.file.seek(state_off); @served.file.write([1].pack("L<")); @served.file.flush
      refute_nil @reader.snapshot
    end

    def test_detach_releases_the_mapping_and_reattach_works
      serve(bpm: 111.0)
      @reader.attach!
      @reader.detach!
      refute @reader.attached?
      assert_nil @reader.snapshot
      assert @reader.attach!
      assert_equal 111.0, @reader.snapshot.bpm
      assert_equal 2, @served.endpoint.connections
    end

    def test_reattach_is_attach_after_detach
      serve(bpm: 111.0)
      @reader.attach!
      assert @reader.reattach!
      assert_equal 2, @served.endpoint.connections
      assert_equal 111.0, @reader.snapshot.bpm
    end

    def test_endpoint_defaults_from_the_engine_port
      r = ClockworkClockReader.new(port: 4556)
      assert_equal ClockworkShmAttach.default_endpoint(4556), r.endpoint
    end

    def test_snapshot_math_is_the_engines
      serve(bpm: 120.0, origin_ntp: 1000.0)
      @reader.attach!
      s = @reader.snapshot
      assert_equal 1004.0, s.time_at_beat(8)
      assert_equal 8.0, s.beat_at_time(1004.0)
      assert_equal 0.5, s.phase_at_time(1000.0 + 4.5 * 0.5, 4.0)
    end

    def test_snapshot_is_a_value_not_a_view
      serve(bpm: 100.0)
      @reader.attach!
      s = @reader.snapshot
      @served.set_clock(bpm: 50.0, origin_ntp: 0.0)
      assert_equal 100.0, s.bpm        # the snapshot taken earlier does not move
    end
  end
end
