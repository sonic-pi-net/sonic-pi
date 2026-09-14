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

require 'socket'
require 'tempfile'
require 'tmpdir'

module SonicPi
  # A clockwork shared-memory segment, built in Ruby, byte for byte the shape
  # the engine publishes (clockwork's shm_segment.hpp + clockwork_arena.h +
  # shared_memory.h). The reader under test parses THIS; the integration test
  # parses the real thing. If the two ever disagree, the integration test is
  # the one that is right and this builder is what to fix.
  module FakeClockworkSegment
    SEGMENT_MAGIC = 0x5C09E00C
    ARENA_MAGIC   = 0x43574152   # 'CWAR'
    ARENA_VERSION = 1
    HEADER_BYTES  = 4096
    ENTRY_BYTES   = 64
    MAX_ENTRIES   = 48
    CLOCK_STATE_REGION = 4
    PUBLISHED     = 1
    CLOCK_STATE_BYTES = 40

    # Options mirror the fields; the defaults are a plausible live engine.
    AUDIENCE_PUBLISHED = 1
    AUDIENCE_TRANSPORT = 8
    GEOM_AUDIENCE      = 11

    # Entries are [id, offset, bytes, owner, audience]; a four-element entry
    # is a writer from before the audience word (0). The clock state is
    # published unless `clock_audience` says otherwise.
    def self.build(bpm: 120.0, origin_ntp: 3_900_000_000.25, playing: false,
                   playing_at_ntp: 0.0, flags: 0, meter: [4, 4],
                   blob_offset: 64, published: true, version: ARENA_VERSION,
                   segment_magic: SEGMENT_MAGIC, arena_magic: ARENA_MAGIC,
                   entry_bytes: ENTRY_BYTES, clock_region: CLOCK_STATE_REGION,
                   clock_offset: HEADER_BYTES, clock_audience: AUDIENCE_PUBLISHED,
                   extra_entries: [])
      arena_bytes = clock_offset + CLOCK_STATE_BYTES + 64
      entries = [[clock_region, clock_offset, CLOCK_STATE_BYTES, 1, clock_audience]] + extra_entries
      raise ArgumentError, "too many entries" if entries.size > MAX_ENTRIES
      # The published run of the (single-half) fake: everything up to the end
      # of the clock state; stated in the header like the engine states it.
      published_end = clock_offset + CLOCK_STATE_BYTES

      arena = [arena_magic, version, HEADER_BYTES, 0, arena_bytes, arena_bytes,
               arena_bytes, 0, entries.size, entry_bytes,
               published ? PUBLISHED : 0, published_end, published_end].pack("L<13") + ("\0" * 4 * 3)
      entries.each do |id, off, bytes, owner, audience|
        geom = [0] * 12
        geom[GEOM_AUDIENCE] = audience || 0
        arena << [id, off, bytes, owner].pack("L<4") << geom.pack("L<12")
      end
      arena << "\0" * (clock_offset - arena.bytesize)
      arena << clock_state_bytes(bpm: bpm, origin_ntp: origin_ntp, playing: playing,
                                 playing_at_ntp: playing_at_ntp, flags: flags, meter: meter)
      arena << "\0" * (arena_bytes - arena.bytesize)

      seg = [segment_magic, blob_offset, arena_bytes].pack("L<3")
      seg << "\0" * (blob_offset - seg.bytesize)
      seg << arena
      seg.force_encoding(Encoding::BINARY)
    end

    # The 40-byte ClockworkClockState: two doubles as their bit patterns, the
    # transport timestamp, then four u32 words.
    def self.clock_state_bytes(bpm:, origin_ntp:, playing: false, playing_at_ntp: 0.0,
                               flags: 0, meter: [4, 4])
      packed_meter = (meter[0] << 16) | (meter[1] & 0xFFFF)
      [bpm, origin_ntp, playing_at_ntp].pack("E3") +
        [playing ? 1 : 0, flags, packed_meter, 0].pack("L<4")
    end

    # Where the clock state sits in a segment built with the defaults.
    def self.clock_state_offset(blob_offset: 64, clock_offset: HEADER_BYTES)
      blob_offset + clock_offset
    end

    # A fake attach endpoint: a Unix socket that, per connection, sends the
    # 24-byte hello and passes the descriptor of `file` — exactly what the
    # engine's shm_attach::server does. Serves until stopped.
    class Endpoint
      HELLO_MAGIC   = 0x43574154   # 'CWAT'
      HELLO_VERSION = 1

      attr_reader :path, :connections

      def initialize(file, dir: Dir.mktmpdir("cw-attach"), magic: HELLO_MAGIC,
                     version: HELLO_VERSION, size: nil, silent: false, short: false)
        @file = file
        @path = File.join(dir, "clockwork-shm-test.sock")
        @server = UNIXServer.new(@path)
        @magic, @version = magic, version
        @size = size || File.size(file.path)
        @silent, @short = silent, short
        @connections = 0
        @thread = Thread.new { serve }
      end

      def stop
        @server.close rescue nil
        @thread.join(1)
        File.delete(@path) rescue nil
      end

      private

      def serve
        loop do
          client = @server.accept
          @connections += 1
          begin
            if @silent
              sleep 0.5
            elsif @short
              # A truncated hello with no descriptor: what a reader must refuse.
              client.sendmsg([@magic, @version].pack("L<2") + "\0\0")
            else
              # One sendmsg: the 24-byte hello with the descriptor riding
              # alongside as SCM_RIGHTS, exactly as shm_attach::server sends it.
              hello = [@magic, @version, @size, 0].pack("L<2Q<2")
              client.sendmsg(hello, 0, nil, Socket::AncillaryData.unix_rights(@file.to_io))
            end
          ensure
            client.close rescue nil
          end
        end
      rescue IOError, Errno::EBADF, Errno::ENOTCONN
        nil
      end
    end

    # A segment on disk, mappable by the reader, plus its endpoint.
    class Served
      attr_reader :file, :endpoint

      def initialize(bytes, **endpoint_opts)
        @file = Tempfile.new("cw-segment")
        @file.binmode
        write(bytes)
        @endpoint = Endpoint.new(@file, **endpoint_opts)
      end

      def write(bytes)
        @file.rewind
        @file.write(bytes)
        @file.flush
        @file.truncate(bytes.bytesize)
      end

      # Overwrite the clock state in place, as the engine does on a tempo
      # change: the reader's mapping sees it on its next read.
      def set_clock(**fields)
        @file.seek(FakeClockworkSegment.clock_state_offset)
        @file.write(FakeClockworkSegment.clock_state_bytes(**fields))
        @file.flush
      end

      def path
        @endpoint.path
      end

      def close
        @endpoint.stop
        @file.close!
      end
    end
  end
end
