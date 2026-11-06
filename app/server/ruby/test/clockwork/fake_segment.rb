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
require_relative "../../lib/sonicpi/engine_secret"

module SonicPi
  # A clockwork shared-memory segment, built in Ruby, byte for byte the shape
  # the engine publishes (clockwork's shm_segment.hpp + clockwork_arena.h +
  # shared_memory.h). The reader under test parses THIS; the integration test
  # parses the real thing. If the two ever disagree, the integration test is
  # the one that is right and this builder is what to fix.
  module FakeClockworkSegment
    SEGMENT_MAGIC = 0x5C09E00D
    ARENA_MAGIC   = 0x43574152   # 'CWAR'
    ARENA_VERSION = 2
    HEADER_BYTES  = 4096
    HEADER_WORDS  = 16
    MAX_AREAS     = 16
    AREA_BYTES    = 24
    MAX_ENTRIES   = 48
    ENTRY_BYTES   = 64
    ENTRIES_AT    = HEADER_WORDS * 4 + MAX_AREAS * AREA_BYTES   # from the arena base
    CLOCK_STATE_REGION = 4
    PUBLISHED     = 1
    CLOCK_STATE_BYTES = 40

    AREA_CLOCKWORK_STATE     = 1
    AREA_CLOCKWORK_AUDIO     = 2
    AREA_CLOCKWORK_TRANSPORT = 3
    SPACE_ARENA        = 1
    AUDIENCE_PUBLIC    = 1
    AUDIENCE_TRANSPORT = 8
    # What a region states besides where it is: written by clockwork, read
    # by clients, touched on a change, fast RAM, kept for the arena's life.
    FACTS = { owner: 1, audience: AUDIENCE_PUBLIC, cadence: 2, tier: 1, lifetime: 1 }.freeze

    AREA_SPAN = 256   # each of the fake's areas

    # The fake's areas, clockwork's half only: its state area from the end of
    # the header to past the clock state, then a transport area. Each is
    # [area, space, base, bytes].
    def self.areas(clock_offset: HEADER_BYTES)
      state_end = clock_offset + AREA_SPAN
      [[AREA_CLOCKWORK_STATE, SPACE_ARENA, HEADER_BYTES, state_end - HEADER_BYTES],
       [AREA_CLOCKWORK_TRANSPORT, SPACE_ARENA, state_end, AREA_SPAN]]
    end

    # Room in the state area beside the clock state, and the transport area's
    # base: where a test puts the regions it adds.
    def self.state_spare(clock_offset: HEADER_BYTES)
      clock_offset + CLOCK_STATE_BYTES
    end

    def self.transport_base(clock_offset: HEADER_BYTES)
      clock_offset + AREA_SPAN
    end

    # Where entry `i` of the region table is in a segment built with the defaults.
    def self.entry_at(i, blob_offset: 64)
      blob_offset + ENTRIES_AT + i * ENTRY_BYTES
    end

    # Options mirror the fields; the defaults are a plausible live engine.
    # A region is a hash of id, offset, bytes and any of the five facts that
    # differ from FACTS. The clock state is public unless `clock_audience`
    # says otherwise, and `clock_facts` overrides any of its facts.
    def self.build(bpm: 120.0, origin_ntp: 3_900_000_000.25, playing: false,
                   playing_at_ntp: 0.0, flags: 0, meter: [4, 4],
                   blob_offset: 64, published: true, version: ARENA_VERSION,
                   segment_magic: SEGMENT_MAGIC, arena_magic: ARENA_MAGIC,
                   area_bytes: AREA_BYTES, entry_bytes: ENTRY_BYTES,
                   clock_region: CLOCK_STATE_REGION, clock_offset: HEADER_BYTES,
                   clock_audience: AUDIENCE_PUBLIC, clock_facts: {},
                   areas: self.areas(clock_offset: clock_offset), extra_entries: [])
      arena_bytes = areas.map { |_, _, base, bytes| base + bytes }.max
      clock = { id: clock_region, offset: clock_offset, bytes: CLOCK_STATE_BYTES,
                audience: clock_audience }.merge(clock_facts)
      entries = [clock] + extra_entries
      raise ArgumentError, "too many entries" if entries.size > MAX_ENTRIES
      raise ArgumentError, "too many areas" if areas.size > MAX_AREAS

      # Clockwork's half is the whole of the fake; the guest's is empty.
      arena = [arena_magic, version, HEADER_BYTES, 0, arena_bytes, arena_bytes,
               arena_bytes, 0, areas.size, area_bytes, entries.size, entry_bytes,
               published ? PUBLISHED : 0, 1, 0, 0].pack("L<#{HEADER_WORDS}")
      areas.each { |area, space, base, bytes| arena << [area, space, base, 0, bytes, 0].pack("L<6") }
      arena << "\0" * ((MAX_AREAS - areas.size) * AREA_BYTES)
      entries.each do |e|
        f = FACTS.merge(e)
        arena << [f[:id], f[:offset], f[:bytes], f[:owner], f[:audience], f[:cadence],
                  f[:tier], f[:lifetime]].pack("L<8") << ([0] * 8).pack("L<8")
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

    # The secret every fake endpoint asks for unless given another.
    SECRET = SonicPi::EngineSecret.generate

    # A fake attach endpoint: a Unix socket that, per connection, reads the
    # client's request (the auth frame, then the role it asks for) and, if
    # the frame carries `secret`, sends the 40-byte hello and passes the
    # descriptor of `file` — exactly what the engine's shm_attach::server
    # does. A refused connection is closed without a hello and counted. A
    # role other than the reader's is answered with STATUS_UNKNOWN_ROLE and no
    # descriptor; `status:` forces the status, `granted_role:` the role the
    # hello names. Serves until stopped.
    class Endpoint
      HELLO_MAGIC   = 0x43574154   # 'CWAT'
      HELLO_VERSION = 4
      AUTH_FRAME_BYTES = 60
      REQUEST_BYTES = AUTH_FRAME_BYTES + 4
      ROLE_READER = 1
      STATUS_OK = 0
      STATUS_UNKNOWN_ROLE = 3

      attr_reader :path, :connections, :refused, :roles

      def initialize(file, dir: Dir.mktmpdir("cw-attach"), magic: HELLO_MAGIC,
                     version: HELLO_VERSION, size: nil, silent: false, short: false,
                     secret: SECRET, status: nil, granted_role: nil)
        @file = file
        @secret = secret
        @refused = 0
        @roles = []
        @path = File.join(dir, "clockwork-shm-test.sock")
        @server = UNIXServer.new(@path)
        @magic, @version = magic, version
        @size = size || File.size(file.path)
        @silent, @short = silent, short
        @status, @granted_role = status, granted_role
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
            role = admitted_role(client)
            unless role
              @refused += 1
              next
            end
            @roles << role
            status = @status || (role == ROLE_READER ? STATUS_OK : STATUS_UNKNOWN_ROLE)
            granted = @granted_role || role
            if @silent
              sleep 0.5
            elsif @short
              # A truncated hello with no descriptor: what a reader must refuse.
              client.sendmsg([@magic, @version].pack("L<2") + "\0\0")
            elsif status != STATUS_OK
              # The role was declined: the hello says why and hands over nothing.
              client.sendmsg(hello(0, granted, status))
            else
              # One sendmsg: the 40-byte hello with the descriptor riding
              # alongside as SCM_RIGHTS, exactly as shm_attach::server sends it.
              client.sendmsg(hello(@size, granted, status), 0, nil,
                             Socket::AncillaryData.unix_rights(@file.to_io))
            end
          ensure
            client.close rescue nil
          end
        end
      rescue IOError, Errno::EBADF, Errno::ENOTCONN
        nil
      end

      # magic, version, u64 size, u64 handle (0: the descriptor rides
      # alongside), u32 role, u32 status, u64 wake event (0 off Windows).
      def hello(size, role, status)
        [@magic, @version, size, 0, role, status, 0].pack("L<2Q<2L<2Q<")
      end

      # The client's request, within 2 s: /clockwork/auth with our secret,
      # then the role it asks for. Nil when the secret is not ours.
      def admitted_role(client)
        return nil unless IO.select([client], nil, nil, 2)
        request = client.read(REQUEST_BYTES)
        return nil unless request && request[0, AUTH_FRAME_BYTES] == expected_auth_frame
        request[AUTH_FRAME_BYTES, 4].unpack1("N")
      rescue IOError, SystemCallError
        nil
      end

      # Built here, not borrowed from the reader: a framed OSC /clockwork/auth
      # with the secret as a 32-byte blob, as shm_attach::auth_frame lays it out.
      def expected_auth_frame
        [56].pack("N") + "/clockwork/auth\0".b + ",b\0\0".b + [32].pack("N") + @secret.b
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
