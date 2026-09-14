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
  # naming where the arena blob is; the arena starts with a self-describing
  # table of regions keyed by a stable id; and region 4 is the 40-byte
  # ClockworkClockState: the Link timeline as two numbers, tempo and the NTP
  # time of beat 0, plus the transport. Every beat <-> time question the
  # spider asks is arithmetic on those two numbers, so reading them here
  # replaces a round trip to the engine per `sleep`.
  #
  # The constants mirror clockwork's shm_segment.hpp, clockwork_arena.h and
  # shared_memory.h. They are pinned by test_clockwork_arena.rb and, against
  # the real engine, by test_clockwork_clock_reader_engine.rb: if clockwork
  # moves, those fail before a user hears a wrong beat.
  module ClockworkArena
    SEGMENT_MAGIC        = 0x5C09E00C   # shm_segment_header::MAGIC
    SEGMENT_HEADER_BYTES = 12           # magic, blob_offset, blob_size (the rest is not ours)

    ARENA_MAGIC          = 0x43574152   # 'CWAR'
    ARENA_VERSION        = 1
    ARENA_HEADER_BYTES   = 4096         # where the first region may start
    ARENA_MAX_ENTRIES    = 48
    ARENA_ENTRY_BYTES    = 64           # id, offset, bytes, owner, 12 geometry words
    ARENA_HEADER_WORDS   = 16           # 11 named u32 + 5 reserved, before the entries
    ARENA_PUBLISHED      = 1            # header.state once the table may be trusted

    REGION_CLOCK_STATE   = 4            # CLOCKWORK_ARENA_CLOCK_STATE
    CLOCK_STATE_BYTES    = 40

    # Who a region is FOR: the last geometry word of every entry (a bit set).
    # A region this reader parses by hand must be PUBLISHED — a data contract
    # — and the engine's table says so. 0 is a writer from before the word
    # existed, which is not a refusal.
    GEOM_AUDIENCE        = 11
    AUDIENCE_PUBLISHED   = 1
    AUDIENCE_TRANSPORT   = 8

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
        if @blob_offset + ARENA_HEADER_WORDS * 4 > memory.size
          raise LayoutError, "segment too small for an arena header"
        end
        @regions = parse_table(blob_size)
        clock = @regions[REGION_CLOCK_STATE]
        raise LayoutError, "arena has no clock state (region #{REGION_CLOCK_STATE})" unless clock
        unless clock[:bytes] == CLOCK_STATE_BYTES
          raise LayoutError, "clock state region is #{clock[:bytes]} bytes, not #{CLOCK_STATE_BYTES}"
        end
        if clock[:audience] != 0 && (clock[:audience] & AUDIENCE_PUBLISHED) == 0
          raise LayoutError, "clock state region is not published for clients (audience #{clock[:audience]})"
        end
        @clock_state_offset = @blob_offset + clock[:offset]
        if @clock_state_offset + CLOCK_STATE_BYTES > memory.size
          raise LayoutError, "clock state runs past the segment"
        end
      end

      def region(id)
        @regions[id]
      end

      # The engine sets state to PUBLISHED once the table may be trusted, and
      # clears it while laying the arena out again. Read live, every time.
      def published?
        @memory.bytes(@blob_offset + 10 * 4, 4).unpack1("L<") == ARENA_PUBLISHED
      end

      # Who region `id` is for (its audience bits); 0 when the writer never said.
      def audience(id)
        r = @regions[id]
        r ? r[:audience] : 0
      end

      # Where each half's published run ends, as the header states it
      # ([block_end, guest_end]); both 0 from a writer that never said.
      def published_ends
        @memory.bytes(@blob_offset + 11 * 4, 8).unpack("L<2")
      end

      # True when the writer said who its regions are for at all. An engine
      # from before the audience words is not wrong, only silent.
      def states_audiences?
        published_ends.any? { |e| e != 0 }
      end

      # The clock, coherently. The writer stores the origin first and then the
      # tempo (with release); a reader loads tempo, origin, tempo again, and
      # retries when the two tempo loads differ, so a new origin can never be
      # paired with an old tempo. The rest of the record is read after.
      def read_clock_state
        off = @clock_state_offset
        bpm_bits = origin_bits = nil
        tries = 0
        loop do
          first  = @memory.bytes(off, 8)
          origin = @memory.bytes(off + 8, 8)
          second = @memory.bytes(off, 8)
          if first == second
            bpm_bits, origin_bits = first, origin
            break
          end
          tries += 1
          raise TornRead, "clock state kept changing under #{tries} reads" if tries > MAX_TORN_RETRIES
        end
        rest = @memory.bytes(off + 16, CLOCK_STATE_BYTES - 16)
        playing_at = rest.byteslice(0, 8).unpack1("E")
        is_playing, flags, meter = rest.byteslice(8, 12).unpack("L<3")
        ClockState.new(bpm: bpm_bits.unpack1("E"),
                       beat_origin_ntp: origin_bits.unpack1("E"),
                       is_playing: is_playing != 0,
                       is_playing_at_ntp: playing_at,
                       flags: flags,
                       meter_num: meter >> 16,
                       meter_den: meter & 0xFFFF)
      end

      private

      def parse_table(blob_size)
        words = @memory.bytes(@blob_offset, ARENA_HEADER_WORDS * 4).unpack("L<#{ARENA_HEADER_WORDS}")
        magic, version, header_bytes, _instance, arena_bytes, _block, _guest_off, _guest_bytes,
          entry_count, entry_bytes, state = words
        raise LayoutError, "not a clockwork arena (magic 0x#{magic.to_s(16)})" unless magic == ARENA_MAGIC
        raise LayoutError, "arena version #{version} this reader does not know" unless version == ARENA_VERSION
        raise LayoutError, "arena not yet published" unless state == ARENA_PUBLISHED
        raise LayoutError, "entry shape this reader does not know (#{entry_bytes} bytes)" unless entry_bytes == ARENA_ENTRY_BYTES
        raise LayoutError, "more entries than the table holds (#{entry_count})" if entry_count > ARENA_MAX_ENTRIES
        raise LayoutError, "arena (#{arena_bytes} bytes) larger than its blob (#{blob_size})" if arena_bytes > blob_size
        table_start = @blob_offset + ARENA_HEADER_WORDS * 4
        table_bytes = entry_count * ARENA_ENTRY_BYTES
        raise LayoutError, "segment too small for the arena table" if table_start + table_bytes > @memory.size
        regions = {}
        entry_count.times do |i|
          entry = @memory.bytes(table_start + i * ARENA_ENTRY_BYTES, ARENA_ENTRY_BYTES)
          id, offset, bytes, owner = entry.unpack("L<4")
          raise LayoutError, "an empty entry inside the count" if id == 0
          raise LayoutError, "a region overlaps the arena header" if offset < header_bytes
          raise LayoutError, "a region runs past the arena" if offset > arena_bytes || bytes > arena_bytes - offset
          audience = entry.byteslice(16 + GEOM_AUDIENCE * 4, 4).unpack1("L<")
          regions[id] = { offset: offset, bytes: bytes, owner: owner, audience: audience }
        end
        regions
      end
    end
  end
end
