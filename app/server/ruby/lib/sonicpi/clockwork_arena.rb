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

module SonicPi
  # The engine's shared-memory arena, as a reader in Ruby sees it.
  #
  # clockwork publishes its clock in a segment it hands to any process of the
  # same user (see ClockworkShmAttach). The segment starts with a small header
  # naming where the arena blob is; the arena starts with a header that
  # describes it: a table of the areas it is divided into and a table of its
  # regions, each keyed by a stable id. Region 4 is the 40-byte
  # ClockworkClockState: the Link timeline as two numbers, tempo and the NTP
  # time of beat 0, plus the transport. Every beat <-> time question the
  # spider asks is arithmetic on those two numbers, so reading them here
  # replaces a round trip to the engine per `sleep`.
  #
  # The constants and field lists mirror clockwork's shm_segment.hpp,
  # clockwork_arena.h and clockwork_clock_state.h. test_clockwork_arena.rb
  # compares them with those headers in this tree, and
  # test_clockwork_clock_reader_engine.rb with the real engine: if clockwork
  # moves, those fail before a user hears a wrong beat.
  module ClockworkArena
    SEGMENT_MAGIC        = 0x5C09E00C   # shm_segment_header::MAGIC
    SEGMENT_HEADER_BYTES = 12           # magic, blob_offset, blob_size (the rest is not ours)

    ARENA_MAGIC          = 0x43574152   # 'CWAR'
    ARENA_VERSION        = 2
    ARENA_HEADER_BYTES   = 4096         # where the first region may start
    ARENA_PUBLISHED      = 1            # header.state once the table may be trusted
    ARENA_MAX_AREAS      = 16
    ARENA_MAX_ENTRIES    = 48
    ARENA_GEOM_WORDS     = 8

    # The u32 fields of ClockworkArenaHeader (before its two tables),
    # ClockworkArenaAreaEntry and ClockworkArenaEntry, in order.
    HEADER_NAMED  = %i[magic version header_bytes instance_id arena_bytes clockwork_bytes
                       guest_offset guest_bytes area_count area_bytes entry_count entry_bytes
                       state generation].freeze
    HEADER_FIELDS = (HEADER_NAMED + %i[reserved reserved]).freeze
    AREA_FIELDS   = %i[area space base_lo base_hi bytes_lo bytes_hi].freeze
    ENTRY_NAMED   = %i[id offset bytes owner audience cadence tier lifetime].freeze
    ENTRY_FIELDS  = (ENTRY_NAMED + [:geom] * ARENA_GEOM_WORDS).freeze

    AREA_BYTES    = AREA_FIELDS.size * 4
    ENTRY_BYTES   = ENTRY_FIELDS.size * 4
    AREAS_AT      = HEADER_FIELDS.size * 4                        # from the arena base
    ENTRIES_AT    = AREAS_AT + ARENA_MAX_AREAS * AREA_BYTES
    HEADER_STRUCT_BYTES = ENTRIES_AT + ARENA_MAX_ENTRIES * ENTRY_BYTES   # sizeof(ClockworkArenaHeader)
    STATE_AT      = HEADER_NAMED.index(:state) * 4

    # The five areas inside the arena, and the space their bases are in.
    AREA_CLOCKWORK_STATE     = 1
    AREA_CLOCKWORK_AUDIO     = 2
    AREA_CLOCKWORK_TRANSPORT = 3
    AREA_GUEST_PUBLIC        = 4
    AREA_GUEST_PRIVATE       = 5
    PUBLIC_AREAS = [AREA_CLOCKWORK_STATE, AREA_CLOCKWORK_AUDIO, AREA_GUEST_PUBLIC].freeze
    SPACE_ARENA  = 1

    # Who a region is for, a bit set. A region this reader parses by hand
    # must be PUBLIC: a data contract.
    AUDIENCE_PUBLIC    = 1
    AUDIENCE_TRANSPORT = 8

    # The five facts every region states, and the values each may take.
    FACTS = { owner: 1..4, audience: 1..15, cadence: 1..3, tier: 1..2, lifetime: 1..3 }.freeze

    REGION_CLOCK_STATE = 4              # CLOCKWORK_ARENA_CLOCKWORK_CLOCK_STATE
    # ClockworkClockState's fields and where each starts.
    CLOCK_STATE_FIELDS = { bpm: 0, beat_origin_ntp: 8, is_playing_at_ntp: 16,
                           is_playing: 24, flags: 28, meter: 32, generation: 36 }.freeze
    CLOCK_STATE_BYTES  = 40

    # A tempo change is two stores, origin then bpm; a read that straddles
    # them is retried. This many retries means the memory is not a clock.
    MAX_TORN_RETRIES     = 8

    class LayoutError < StandardError; end
    class TornRead     < StandardError; end

    # One coherent reading of the clock: clockwork's ClockworkClockSnapshot,
    # with clock_math.h's three functions on it.
    ClockState = Struct.new(:bpm, :beat_origin_ntp, :is_playing, :is_playing_at_ntp,
                            :flags, :meter_num, :meter_den, keyword_init: true) do
      # clockwork::timeAtBeat — origin + beat * 60 / bpm, in NTP seconds.
      def time_at_beat(beat)
        beat_origin_ntp + beat * 60.0 / bpm
      end

      # clockwork::beatAt — (t - origin) * bpm / 60.
      def beat_at_time(ntp_seconds)
        (ntp_seconds - beat_origin_ntp) * bpm / 60.0
      end

      # clockwork::wrapPhase — the non-negative phase of the beat at `t`
      # within `quantum`; 0 when there is no quantum.
      def phase_at_time(ntp_seconds, quantum)
        ClockState.wrap_phase(beat_at_time(ntp_seconds), quantum)
      end

      def self.wrap_phase(beat, quantum)
        return 0.0 if quantum <= 0.0
        p = beat % quantum          # Ruby's % takes the divisor's sign: never negative
        p += quantum if p < 0.0
        p
      end
    end

    # Memory over a String — the tests' segments, and a pattern for the
    # mapped one (ClockworkShmAttach::MappedMemory) to match: `bytes` and
    # `size`. `on_read` lets a test change the bytes between two loads.
    class StringMemory
      attr_accessor :on_read

      def initialize(str)
        @str = str.b
      end

      def size
        @str.bytesize
      end

      def bytes(offset, len)
        @on_read&.call(offset, len)
        raise LayoutError, "segment too small: #{offset + len} > #{size}" if offset + len > size
        @str.byteslice(offset, len)
      end

      def bytes_replace(offset, str)
        @str[offset, str.bytesize] = str.b
      end
    end

    # Parse the headers once; the result reads the clock on demand.
    def self.parse(memory)
      Arena.new(memory)
    end

    class Arena
      attr_reader :memory, :blob_offset, :clock_state_offset

      def initialize(memory)
        @memory = memory
        if memory.size < SEGMENT_HEADER_BYTES
          raise LayoutError, "segment too small for its header (#{memory.size} bytes)"
        end
        magic, @blob_offset, blob_size = memory.bytes(0, SEGMENT_HEADER_BYTES).unpack("L<3")
        unless magic == SEGMENT_MAGIC
          raise LayoutError, format("segment magic 0x%08X is not clockwork's 0x%08X", magic, SEGMENT_MAGIC)
        end
        if @blob_offset + HEADER_STRUCT_BYTES > memory.size
          raise LayoutError, "segment too small for an arena header"
        end
        @regions = parse_tables(blob_size)
        clock = @regions[REGION_CLOCK_STATE]
        raise LayoutError, "arena has no clock state (region #{REGION_CLOCK_STATE})" unless clock
        unless clock[:bytes] == CLOCK_STATE_BYTES
          raise LayoutError, "clock state region is #{clock[:bytes]} bytes, not #{CLOCK_STATE_BYTES}"
        end
        unless clock[:area] == AREA_CLOCKWORK_STATE
          raise LayoutError, "clock state lies in area #{clock[:area]}, not clockwork's state area"
        end
        @clock_state_offset = @blob_offset + clock[:offset]
        if @clock_state_offset + CLOCK_STATE_BYTES > memory.size
          raise LayoutError, "clock state runs past the segment"
        end
      end

      def region(id)
        @regions[id]
      end

      # The engine sets state to PUBLISHED once the table may be trusted. The
      # segment's creator writes the table and the engine writes it again as
      # it takes the arena at boot, clearing state while it does: read it
      # live, every time.
      def published?
        @memory.bytes(@blob_offset + STATE_AT, 4).unpack1("L<") == ARENA_PUBLISHED
      end

      # The clock, coherently. The writer stores the origin first and then the
      # tempo (with release); a reader loads tempo, origin, tempo again, and
      # retries when the two tempo loads differ, so a new origin can never be
      # paired with an old tempo. The rest of the record is read after.
      def read_clock_state
        off = @clock_state_offset
        f = CLOCK_STATE_FIELDS
        bpm_bits = origin_bits = nil
        tries = 0
        loop do
          first  = @memory.bytes(off + f[:bpm], 8)
          origin = @memory.bytes(off + f[:beat_origin_ntp], 8)
          second = @memory.bytes(off + f[:bpm], 8)
          if first == second
            bpm_bits, origin_bits = first, origin
            break
          end
          tries += 1
          raise TornRead, "clock state kept changing under #{tries} reads" if tries > MAX_TORN_RETRIES
        end
        playing_at = @memory.bytes(off + f[:is_playing_at_ntp], 8).unpack1("E")
        is_playing, flags, meter = @memory.bytes(off + f[:is_playing], 12).unpack("L<3")
        ClockState.new(bpm: bpm_bits.unpack1("E"),
                       beat_origin_ntp: origin_bits.unpack1("E"),
                       is_playing: is_playing != 0,
                       is_playing_at_ntp: playing_at,
                       flags: flags,
                       meter_num: meter >> 16,
                       meter_den: meter & 0xFFFF)
      end

      private

      # Both tables, refused as clockwork_arena_check refuses them, in its words.
      def parse_tables(blob_size)
        h = HEADER_NAMED.zip(@memory.bytes(@blob_offset, HEADER_NAMED.size * 4)
                                   .unpack("L<#{HEADER_NAMED.size}")).to_h
        raise LayoutError, "not a clockwork arena (magic 0x#{h[:magic].to_s(16)})" unless h[:magic] == ARENA_MAGIC
        raise LayoutError, "arena version #{h[:version]} this reader does not know" unless h[:version] == ARENA_VERSION
        raise LayoutError, "arena not yet published" unless h[:state] == ARENA_PUBLISHED
        raise LayoutError, "header reservation smaller than the header" if h[:header_bytes] < HEADER_STRUCT_BYTES
        raise LayoutError, "area shape this reader does not know (#{h[:area_bytes]} bytes)" unless h[:area_bytes] == AREA_BYTES
        raise LayoutError, "entry shape this reader does not know (#{h[:entry_bytes]} bytes)" unless h[:entry_bytes] == ENTRY_BYTES
        raise LayoutError, "more areas than the table holds (#{h[:area_count]})" if h[:area_count] > ARENA_MAX_AREAS
        raise LayoutError, "more entries than the table holds (#{h[:entry_count]})" if h[:entry_count] > ARENA_MAX_ENTRIES
        raise LayoutError, "arena (#{h[:arena_bytes]} bytes) larger than its blob (#{blob_size})" if h[:arena_bytes] > blob_size
        raise LayoutError, "clockwork's half larger than the arena" if h[:clockwork_bytes] > h[:arena_bytes]
        raise LayoutError, "the guest's half does not start where clockwork's ends" unless h[:guest_offset] == h[:clockwork_bytes]
        raise LayoutError, "the two halves do not cover the arena" unless h[:guest_bytes] == h[:arena_bytes] - h[:guest_offset]
        parse_regions(h, parse_areas(h))
      end

      # The areas by id; those inside the arena carry an offset from its base.
      def parse_areas(h)
        areas = {}
        h[:area_count].times do |i|
          a = AREA_FIELDS.zip(@memory.bytes(@blob_offset + AREAS_AT + i * AREA_BYTES, AREA_BYTES)
                                     .unpack("L<#{AREA_FIELDS.size}")).to_h
          raise LayoutError, "an empty area inside the count" if a[:area] == 0
          raise LayoutError, "an area listed twice" if areas.key?(a[:area])
          if a[:space] == SPACE_ARENA
            base, last = a[:base_lo], a[:base_lo] + a[:bytes_lo]
            raise LayoutError, "an area in the arena with a 64-bit base" if a[:base_hi] != 0 || a[:bytes_hi] != 0
            raise LayoutError, "an area outside the arena" if base < h[:header_bytes] || last > h[:arena_bytes]
            guests = a[:area] == AREA_GUEST_PUBLIC || a[:area] == AREA_GUEST_PRIVATE
            raise LayoutError, "an area in the wrong half" if guests ? base < h[:guest_offset] : last > h[:clockwork_bytes]
            areas.each_value do |b|
              next unless b[:space] == SPACE_ARENA
              raise LayoutError, "two areas overlap" if base < b[:base_lo] + b[:bytes_lo] && b[:base_lo] < last
            end
          elsif a[:area] <= AREA_GUEST_PRIVATE
            raise LayoutError, "an area of the arena placed outside it"
          end
          areas[a[:area]] = a
        end
        areas
      end

      # The regions by id, each with the area it lies in.
      def parse_regions(h, areas)
        regions = {}
        h[:entry_count].times do |i|
          e = ENTRY_NAMED.zip(@memory.bytes(@blob_offset + ENTRIES_AT + i * ENTRY_BYTES, ENTRY_NAMED.size * 4)
                                     .unpack("L<#{ENTRY_NAMED.size}")).to_h
          id = e[:id]
          raise LayoutError, "an empty entry inside the count" if id == 0
          raise LayoutError, "a region overlaps the arena header" if e[:offset] < h[:header_bytes]
          raise LayoutError, "a region runs past the arena" if e[:offset] + e[:bytes] > h[:arena_bytes]
          FACTS.each do |fact, allowed|
            raise LayoutError, "region #{id} has no #{fact}" unless allowed.cover?(e[fact])
          end
          area = areas.each_value.find do |a|
            a[:space] == SPACE_ARENA && e[:offset] >= a[:base_lo] && e[:offset] + e[:bytes] <= a[:base_lo] + a[:bytes_lo]
          end&.fetch(:area)
          raise LayoutError, "region #{id} is in no area" unless area
          public = (e[:audience] & AUDIENCE_PUBLIC) != 0
          if PUBLIC_AREAS.include?(area)
            raise LayoutError, "region #{id} is in a public area but not public" unless public
          elsif public
            raise LayoutError, "region #{id} is public but outside the public areas"
          end
          if area == AREA_CLOCKWORK_TRANSPORT && (e[:audience] & AUDIENCE_TRANSPORT) == 0
            raise LayoutError, "region #{id} is in the transport area but not transport"
          end
          regions[id] = { offset: e[:offset], bytes: e[:bytes], owner: e[:owner],
                          audience: e[:audience], area: area }
        end
        regions
      end
    end
  end
end
