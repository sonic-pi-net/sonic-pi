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

# The arena parser: from a byte buffer laid out as clockwork publishes it,
# find the clock state and read it. Pure — no engine, no mmap, no sockets.

require_relative "../setup_test"
require_relative "fake_segment"
require_relative "../../lib/sonicpi/clockwork_arena"

module SonicPi
  class ClockworkArenaTester < Minitest::Test
    F = FakeClockworkSegment

    def mem(bytes)
      ClockworkArena::StringMemory.new(bytes)
    end

    # ── layout pins: the numbers mirrored from clockwork_arena.h ──────────
    # If clockwork changes any of these, this test is where it should surface
    # (and the integration test against a real engine will fail with it).

    def test_layout_constants_mirror_clockwork
      assert_equal 0x5C09E00C, ClockworkArena::SEGMENT_MAGIC
      assert_equal 0x43574152, ClockworkArena::ARENA_MAGIC       # 'CWAR'
      assert_equal 1,          ClockworkArena::ARENA_VERSION
      assert_equal 4096,       ClockworkArena::ARENA_HEADER_BYTES
      assert_equal 48,         ClockworkArena::ARENA_MAX_ENTRIES
      assert_equal 64,         ClockworkArena::ARENA_ENTRY_BYTES
      assert_equal 4,          ClockworkArena::REGION_CLOCK_STATE
      assert_equal 40,         ClockworkArena::CLOCK_STATE_BYTES
    end

    # ── finding the clock state ───────────────────────────────────────────

    def test_locates_the_clock_state_through_both_headers
      seg = F.build(blob_offset: 128, clock_offset: 8192)
      arena = ClockworkArena.parse(mem(seg))
      assert_equal 128 + 8192, arena.clock_state_offset
      assert_equal 40, arena.region(ClockworkArena::REGION_CLOCK_STATE)[:bytes]
    end

    def test_finds_the_region_by_id_not_by_position
      # Two other regions in the table beside the clock state, at other ids.
      others = [[1, 4096 + 40, 16, 1, F::AUDIENCE_TRANSPORT], [2, 4096 + 56, 8, 1, 0]]
      seg = F.build(extra_entries: others)
      arena = ClockworkArena.parse(mem(seg))
      assert_equal 64 + 4096, arena.clock_state_offset
      assert_equal 16, arena.region(1)[:bytes]
      assert_nil arena.region(3)
    end

    def test_rejects_a_segment_that_is_not_clockworks
      seg = F.build(segment_magic: 0x12345678)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/segment magic/, err.message)
    end

    def test_rejects_an_arena_that_is_not_clockworks
      seg = F.build(arena_magic: 0x0BADF00D)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/not a clockwork arena/, err.message)
    end

    def test_rejects_an_arena_version_it_does_not_know
      seg = F.build(version: 2)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/version/, err.message)
    end

    def test_rejects_an_unpublished_arena
      # LAYING_OUT: the table may not be trusted yet.
      seg = F.build(published: false)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/not yet published/, err.message)
    end

    def test_rejects_an_entry_shape_it_does_not_know
      seg = F.build(entry_bytes: 32)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/entry shape/, err.message)
    end

    def test_rejects_an_arena_without_a_clock_state
      seg = F.build(clock_region: 99)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/no clock state/, err.message)
    end

    def test_rejects_a_clock_state_region_of_the_wrong_size
      seg = F.build(extra_entries: [])
      # Patch the entry's byte count in place: 40 -> 24.
      entry_at = 64 + 16 * 4
      seg[entry_at + 8, 4] = [24].pack("L<")
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/clock state region/, err.message)
    end

    def test_rejects_a_buffer_too_short_for_the_headers
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem("\0" * 8)) }
      assert_match(/too small/, err.message)
    end

    # ── audiences: the table says who a region is for ─────────────────────

    def test_reads_the_audience_word_of_every_region
      seg = F.build(extra_entries: [[9, 4096 + 40, 16, 3, F::AUDIENCE_TRANSPORT]])
      arena = ClockworkArena.parse(mem(seg))
      assert_equal ClockworkArena::AUDIENCE_PUBLISHED, arena.audience(ClockworkArena::REGION_CLOCK_STATE)
      assert_equal ClockworkArena::AUDIENCE_TRANSPORT, arena.audience(9)
      assert_equal 0, arena.audience(42)                      # absent: nobody's
    end

    def test_refuses_a_clock_state_the_table_does_not_publish
      # A future layout that moved the clock into the transport run would be
      # read as garbage by a reader that ignored the word. This one refuses.
      seg = F.build(clock_audience: F::AUDIENCE_TRANSPORT)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/not published for clients/, err.message)
    end

    def test_a_writer_from_before_the_audience_word_is_still_read
      # audience 0: the engine that wrote this table never said. Not a refusal.
      seg = F.build(clock_audience: 0)
      arena = ClockworkArena.parse(mem(seg))
      assert_equal 0, arena.audience(ClockworkArena::REGION_CLOCK_STATE)
      assert_equal 120.0, arena.read_clock_state.bpm
    end

    def test_the_header_states_where_the_published_runs_end
      seg = F.build
      arena = ClockworkArena.parse(mem(seg))
      assert_equal [4096 + 40, 4096 + 40], arena.published_ends
      assert arena.states_audiences?
      # A writer from before: both words zero, nothing stated.
      seg[64 + 11 * 4, 8] = [0, 0].pack("L<2")
      arena = ClockworkArena.parse(mem(seg))
      assert_equal [0, 0], arena.published_ends
      refute arena.states_audiences?
    end

    def test_audience_constants_mirror_clockwork
      assert_equal 11, ClockworkArena::GEOM_AUDIENCE       # the last of 12 geometry words
      assert_equal 1,  ClockworkArena::AUDIENCE_PUBLISHED
      assert_equal 8,  ClockworkArena::AUDIENCE_TRANSPORT
    end

    # ── reading the clock state ───────────────────────────────────────────

    def test_reads_every_field_exactly
      seg = F.build(bpm: 123.456, origin_ntp: 3_900_000_000.123456789,
                    playing: true, playing_at_ntp: 3_900_000_100.5,
                    flags: 0b101, meter: [7, 8])
      arena = ClockworkArena.parse(mem(seg))
      s = arena.read_clock_state
      assert_equal 123.456, s.bpm                       # bit-exact double
      assert_equal 3_900_000_000.123456789, s.beat_origin_ntp
      assert_equal true, s.is_playing
      assert_equal 3_900_000_100.5, s.is_playing_at_ntp
      assert_equal 0b101, s.flags
      assert_equal 7, s.meter_num
      assert_equal 8, s.meter_den
    end

    def test_a_stopped_transport_reads_as_not_playing
      seg = F.build(playing: false)
      s = ClockworkArena.parse(mem(seg)).read_clock_state
      assert_equal false, s.is_playing
    end

    def test_reads_are_live_not_cached
      seg = F.build(bpm: 100.0)
      m = mem(seg)
      arena = ClockworkArena.parse(m)
      assert_equal 100.0, arena.read_clock_state.bpm
      # The engine writes a new grid: origin first, then bpm.
      off = arena.clock_state_offset
      m.bytes_replace(off + 8, [42.0].pack("E"))
      m.bytes_replace(off, [140.0].pack("E"))
      s = arena.read_clock_state
      assert_equal 140.0, s.bpm
      assert_equal 42.0, s.beat_origin_ntp
    end

    # The write protocol: origin first, then bpm with release. A reader that
    # loads bpm, then origin, then bpm AGAIN and retries when the two bpm
    # loads differ can never pair a new origin with an old tempo.
    def test_a_torn_read_across_a_tempo_change_is_retried
      seg = F.build(bpm: 100.0, origin_ntp: 1000.0)
      m = mem(seg)
      arena = ClockworkArena.parse(m)
      off = arena.clock_state_offset
      # Script the memory: the first bpm load sees 100, then the engine
      # publishes (origin 2000, bpm 200) before the origin load.
      loads = 0
      m.on_read = lambda do |o, _len|
        loads += 1
        if loads == 1                     # after the first bpm load...
          m.bytes_replace(off + 8, [2000.0].pack("E"))
          m.bytes_replace(off,     [200.0].pack("E"))
        end
      end
      s = arena.read_clock_state
      assert_equal 200.0,  s.bpm
      assert_equal 2000.0, s.beat_origin_ntp
      assert loads >= 4, "expected a retry (bpm, origin, bpm, then again); got #{loads} loads"
    end

    def test_gives_up_after_too_many_torn_reads
      seg = F.build(bpm: 100.0)
      m = mem(seg)
      arena = ClockworkArena.parse(m)
      off = arena.clock_state_offset
      tick = 0
      m.on_read = lambda do |_o, _len|
        tick += 1
        m.bytes_replace(off, [100.0 + tick].pack("E"))   # bpm never settles
      end
      assert_raises(ClockworkArena::TornRead) { arena.read_clock_state }
    end

    # ── the snapshot's arithmetic mirrors clockwork's clock_math.h ────────

    def test_time_at_beat_and_beat_at_time_are_the_engines_formulae
      s = ClockworkArena::ClockState.new(bpm: 120.0, beat_origin_ntp: 1000.0,
                                         is_playing: true, is_playing_at_ntp: 0.0,
                                         flags: 0, meter_num: 4, meter_den: 4)
      assert_equal 1000.0 + 8 * 60.0 / 120.0, s.time_at_beat(8)          # origin + beat*60/bpm
      assert_equal (1004.0 - 1000.0) * 120.0 / 60.0, s.beat_at_time(1004.0)
      assert_in_delta 3.5, s.beat_at_time(s.time_at_beat(3.5)), 1e-9
    end

    def test_phase_wraps_like_wrap_phase
      s = ClockworkArena::ClockState.new(bpm: 60.0, beat_origin_ntp: 0.0,
                                         is_playing: false, is_playing_at_ntp: 0.0,
                                         flags: 0, meter_num: 4, meter_den: 4)
      assert_equal 1.5, s.phase_at_time(5.5, 4.0)      # beat 5.5 in a 4-beat quantum
      assert_equal 2.5, s.phase_at_time(-1.5, 4.0)     # negative beats wrap up, never down
      assert_equal 0.0, s.phase_at_time(5.5, 0.0)      # no quantum: no phase
      assert_equal 0.0, s.phase_at_time(5.5, -1.0)
    end
  end
end
