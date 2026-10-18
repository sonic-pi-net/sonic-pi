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

    # ── the layout is clockwork's, as its headers in this tree say ────────
    # The reader's numbers and field lists against clockwork_arena.h and
    # clockwork_clock_state.h themselves, so a change there fails here, with
    # no engine built (the integration test needs one).

    CLOCKWORK_SRC = File.expand_path("../../../../external/supersonic/clockwork/src", __dir__)

    def clockwork_header(name)
      file = File.join(CLOCKWORK_SRC, name)
      skip "clockwork's sources are not in this tree (#{file})" unless File.exist?(file)
      File.read(file)
    end

    # Every #define and enum value a header gives a number.
    def c_numbers(src)
      defines = src.scan(/^#define\s+(CLOCKWORK_\w+)\s+(0x\h+|\d+)u?\b/)
      enums   = src.scan(/\b(CLOCKWORK_\w+)\s*=\s*(\d+)\b/)
      (defines + enums).to_h { |name, v| [name, Integer(v)] }
    end

    # A C struct's members in order: [type, name, count], count 1 for a scalar.
    def c_members(src, struct, numbers)
      body = src[/typedef struct #{struct} \{(.*?)\} #{struct};/m, 1]
      flunk "no struct #{struct} in clockwork_arena.h" unless body
      body.scan(/^\s*(\w+)\s+(\w+)(?:\[(\w+)\])?;/).map do |type, name, n|
        [type, name.to_sym, n ? numbers.fetch(n) { Integer(n) } : 1]
      end
    end

    # The leading u32 fields as a reader lists them: an array once per element.
    def u32_fields(members)
      members.take_while { |type, _, _| type == "uint32_t" }.flat_map { |_, name, n| [name] * n }
    end

    def test_the_numbers_are_clockwork_arena_h_s
      n = c_numbers(clockwork_header("clockwork_arena.h"))
      a = ClockworkArena
      assert_equal n.fetch("CLOCKWORK_ARENA_MAGIC"),        a::ARENA_MAGIC
      assert_equal n.fetch("CLOCKWORK_ARENA_VERSION"),      a::ARENA_VERSION
      assert_equal n.fetch("CLOCKWORK_ARENA_HEADER_BYTES"), a::ARENA_HEADER_BYTES
      assert_equal n.fetch("CLOCKWORK_ARENA_PUBLISHED"),    a::ARENA_PUBLISHED
      assert_equal n.fetch("CLOCKWORK_ARENA_MAX_AREAS"),    a::ARENA_MAX_AREAS
      assert_equal n.fetch("CLOCKWORK_ARENA_MAX_ENTRIES"),  a::ARENA_MAX_ENTRIES
      assert_equal n.fetch("CLOCKWORK_ARENA_GEOM_WORDS"),   a::ARENA_GEOM_WORDS
      assert_equal n.fetch("CLOCKWORK_ARENA_CLOCKWORK_CLOCK_STATE"), a::REGION_CLOCK_STATE
      assert_equal n.fetch("CLOCKWORK_AREA_CLOCKWORK_STATE"),     a::AREA_CLOCKWORK_STATE
      assert_equal n.fetch("CLOCKWORK_AREA_CLOCKWORK_AUDIO"),     a::AREA_CLOCKWORK_AUDIO
      assert_equal n.fetch("CLOCKWORK_AREA_CLOCKWORK_TRANSPORT"), a::AREA_CLOCKWORK_TRANSPORT
      assert_equal n.fetch("CLOCKWORK_AREA_GUEST_PUBLIC"),        a::AREA_GUEST_PUBLIC
      assert_equal n.fetch("CLOCKWORK_AREA_GUEST_PRIVATE"),       a::AREA_GUEST_PRIVATE
      assert_equal n.fetch("CLOCKWORK_SPACE_ARENA"),        a::SPACE_ARENA
      assert_equal n.fetch("CLOCKWORK_AUDIENCE_PUBLIC"),    a::AUDIENCE_PUBLIC
      assert_equal n.fetch("CLOCKWORK_AUDIENCE_TRANSPORT"), a::AUDIENCE_TRANSPORT
      # The five facts, as the ranges clockwork_arena_check allows.
      assert_equal n.fetch("CLOCKWORK_OWNER_CLOCKWORK")..n.fetch("CLOCKWORK_OWNER_HOST"), a::FACTS[:owner]
      audiences = %w[PUBLIC GUEST HOST TRANSPORT].sum { |w| n.fetch("CLOCKWORK_AUDIENCE_#{w}") }
      assert_equal 1..audiences, a::FACTS[:audience]
      assert_equal n.fetch("CLOCKWORK_CADENCE_BLOCK")..n.fetch("CLOCKWORK_CADENCE_BOOT"), a::FACTS[:cadence]
      assert_equal n.fetch("CLOCKWORK_TIER_FAST")..n.fetch("CLOCKWORK_TIER_BULK"), a::FACTS[:tier]
      assert_equal n.fetch("CLOCKWORK_LIFETIME_ARENA")..n.fetch("CLOCKWORK_LIFETIME_REBUILD"), a::FACTS[:lifetime]
    end

    def test_the_tables_are_clockwork_arena_h_s_structs_field_for_field
      src = clockwork_header("clockwork_arena.h")
      n = c_numbers(src)
      a = ClockworkArena
      header = c_members(src, "ClockworkArenaHeader", n)
      assert_equal a::HEADER_FIELDS, u32_fields(header)
      assert_equal [["ClockworkArenaAreaEntry", :areas, a::ARENA_MAX_AREAS],
                    ["ClockworkArenaEntry", :entries, a::ARENA_MAX_ENTRIES]],
                   header.drop_while { |type, _, _| type == "uint32_t" }
      area = c_members(src, "ClockworkArenaAreaEntry", n)
      assert_equal a::AREA_FIELDS, u32_fields(area)
      assert_equal area.sum { |_, _, c| c }, a::AREA_FIELDS.size     # nothing but u32s
      entry = c_members(src, "ClockworkArenaEntry", n)
      assert_equal a::ENTRY_FIELDS, u32_fields(entry)
      assert_equal entry.sum { |_, _, c| c }, a::ENTRY_FIELDS.size
    end

    def test_the_clock_state_is_clockwork_clock_state_h_s
      src = clockwork_header("clockwork_clock_state.h")
      body = src[/^struct alignas\(8\) ClockworkClockState \{\n(.*?)^\};/m, 1]
      flunk "no struct ClockworkClockState in clockwork_clock_state.h" unless body
      at = 0
      fields = body.scan(/^\s*std::atomic<uint(64|32)_t>\s+(\w+)\{/).to_h do |bits, name|
        field = [name.to_sym, at]
        at += Integer(bits) / 8
        field
      end
      assert_equal ClockworkArena::CLOCK_STATE_FIELDS, fields
      assert_equal ClockworkArena::CLOCK_STATE_BYTES, at
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
      others = [{ id: 1, offset: F.state_spare, bytes: 16 },
                { id: 9, offset: F.transport_base, bytes: 8, audience: F::AUDIENCE_TRANSPORT }]
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
      [1, 3].each do |version|      # the one before, and one to come
        seg = F.build(version: version)
        err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
        assert_match(/arena version #{version} this reader does not know/, err.message)
      end
    end

    def test_rejects_an_unpublished_arena
      # LAYING_OUT: the table may not be trusted yet.
      seg = F.build(published: false)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/not yet published/, err.message)
    end

    def test_rejects_an_area_shape_it_does_not_know
      seg = F.build(area_bytes: 16)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/area shape/, err.message)
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
      seg = F.build
      # Patch the entry's byte count in place: 40 -> 24.
      seg[F.entry_at(0) + 8, 4] = [24].pack("L<")
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/clock state region/, err.message)
    end

    def test_rejects_a_buffer_too_short_for_the_headers
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem("\0" * 8)) }
      assert_match(/too small/, err.message)
    end

    # ── areas and facts: the table says what each region is ───────────────

    def test_reads_where_every_region_lies_and_who_it_is_for
      seg = F.build(extra_entries: [{ id: 9, offset: F.transport_base, bytes: 16, owner: 3,
                                      audience: F::AUDIENCE_TRANSPORT }])
      arena = ClockworkArena.parse(mem(seg))
      clock = arena.region(ClockworkArena::REGION_CLOCK_STATE)
      assert_equal ClockworkArena::AREA_CLOCKWORK_STATE, clock[:area]
      assert_equal ClockworkArena::AUDIENCE_PUBLIC, clock[:audience]
      assert_equal ClockworkArena::AREA_CLOCKWORK_TRANSPORT, arena.region(9)[:area]
      assert_equal ClockworkArena::AUDIENCE_TRANSPORT, arena.region(9)[:audience]
      assert_equal 3, arena.region(9)[:owner]
    end

    def test_refuses_a_clock_state_that_is_not_public
      # A layout that made the clock transport would be read as garbage by a
      # reader that ignored the word. This one refuses.
      seg = F.build(clock_audience: F::AUDIENCE_TRANSPORT)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/region 4 is in a public area but not public/, err.message)
    end

    def test_refuses_a_clock_state_outside_clockworks_state_area
      # Public, so the table is sound, but in the audio area: not the clock.
      areas = [[F::AREA_CLOCKWORK_AUDIO, F::SPACE_ARENA, 4096, 256]]
      seg = F.build(areas: areas)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/clock state lies in area 2/, err.message)
    end

    def test_refuses_a_region_that_does_not_state_its_five_facts
      %i[owner audience cadence tier lifetime].each do |fact|
        seg = F.build(clock_facts: { fact => 0 })
        err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
        assert_match(/region 4 has no #{fact}/, err.message)
      end
    end

    def test_refuses_a_region_in_no_area
      # Across the line between the state and the transport areas.
      seg = F.build(extra_entries: [{ id: 1, offset: F.transport_base - 8, bytes: 16 }])
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/region 1 is in no area/, err.message)
    end

    def test_refuses_a_public_region_outside_the_public_areas
      seg = F.build(extra_entries: [{ id: 9, offset: F.transport_base, bytes: 8,
                                      audience: F::AUDIENCE_PUBLIC | F::AUDIENCE_TRANSPORT }])
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/region 9 is public but outside the public areas/, err.message)
    end

    def test_refuses_a_transport_area_region_that_is_not_transport
      seg = F.build(extra_entries: [{ id: 9, offset: F.transport_base, bytes: 8, audience: 4 }])
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/region 9 is in the transport area but not transport/, err.message)
    end

    def test_refuses_areas_that_overlap
      areas = F.areas
      areas << [F::AREA_CLOCKWORK_AUDIO, F::SPACE_ARENA, areas[0][2] + 8, 64]
      seg = F.build(areas: areas)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/two areas overlap/, err.message)
    end

    def test_refuses_an_area_in_the_wrong_half
      # A guest area inside clockwork's half (the fake's guest half is empty).
      areas = F.areas + [[ClockworkArena::AREA_GUEST_PUBLIC, F::SPACE_ARENA, 4096 + 1024, 64]]
      seg = F.build(areas: areas)
      err = assert_raises(ClockworkArena::LayoutError) { ClockworkArena.parse(mem(seg)) }
      assert_match(/an area in the wrong half/, err.message)
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
