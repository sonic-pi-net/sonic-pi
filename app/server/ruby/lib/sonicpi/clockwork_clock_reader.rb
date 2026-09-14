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

require_relative "clockwork_shm_attach"
require_relative "clockwork_arena"

module SonicPi
  # The spider's window onto the engine's clock: attach to the engine's
  # shared-memory segment, and hand out coherent snapshots of the Link
  # timeline on demand. A snapshot is two numbers (tempo, origin) plus the
  # transport; LinkAPI turns every beat <-> time question into arithmetic on
  # it, where it used to ask the engine.
  #
  # The segment lives for the engine's life, so one attach serves a session;
  # a swap re-lays the arena out under the same mapping and the header's
  # published flag says when the table may be trusted again. A new engine
  # process is a new segment: re-attach (reattach!) when the engine comes
  # back. Every failure is reported, never raised — the caller's answer to
  # "no snapshot" is the RPC it always had.
  class ClockworkClockReader
    attr_reader :endpoint, :last_error

    def initialize(port: nil, endpoint: nil)
      raise ArgumentError, "port or endpoint" unless port || endpoint
      @endpoint = endpoint || ClockworkShmAttach.default_endpoint(port)
      @mutex = Mutex.new
      @memory = nil
      @arena = nil
      @last_error = nil
    end

    def attached?
      !@arena.nil?
    end

    # Connect, map, parse. False (with last_error) on any failure.
    def attach!
      memory = nil
      begin
        io, size = ClockworkShmAttach.receive(@endpoint)
        begin
          memory = ClockworkShmAttach.map(io, size)
        ensure
          io.close if io.respond_to?(:close)
        end
        arena = ClockworkArena.parse(memory)
      rescue ClockworkShmAttach::AttachError, ClockworkArena::LayoutError => e
        memory&.unmap
        @last_error = e.message
        return false
      end
      @mutex.synchronize do
        old = @memory
        @memory, @arena = memory, arena
        old&.unmap
      end
      @last_error = nil
      true
    end

    # Attach again — after an engine restart, the old mapping is of a dead
    # segment. On failure the old mapping is kept: a reader that was working
    # is not thrown away for a hand-off that did not happen.
    def reattach!
      attach!
    end

    def detach!
      @mutex.synchronize do
        @memory&.unmap
        @memory = @arena = nil
      end
    end

    # A coherent ClockworkArena::ClockState, or nil: not attached, the arena
    # being re-laid-out, or a read that would not settle.
    def snapshot
      arena = @arena
      return nil unless arena
      return nil unless arena.published?
      arena.read_clock_state
    rescue ClockworkArena::TornRead, ClockworkArena::LayoutError, ClockworkShmAttach::AttachError => e
      @last_error = e.message
      nil
    end
  end
end
