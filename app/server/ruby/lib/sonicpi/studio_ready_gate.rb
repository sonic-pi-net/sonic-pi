#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# concurrent-ruby's internal requires expect 'concurrent/...' to resolve.
# Sonic Pi's vendor loader (core.rb) puts vendor/concurrent-ruby-1.3.5/lib/
# on $LOAD_PATH but the gem's own internal requires use 'concurrent/...'
# which only resolves if vendor/concurrent-ruby-1.3.5/lib/concurrent-ruby
# is also on the path. Add it.
_concurrent_inner = File.expand_path(
  "../../vendor/concurrent-ruby-1.3.5/lib/concurrent-ruby",
  __dir__)
$LOAD_PATH.unshift(_concurrent_inner) unless $LOAD_PATH.include?(_concurrent_inner)
require 'concurrent/atomic/reentrant_read_write_lock'

module SonicPi
  # Reader-writer protocol for studio operations.
  #
  #   - with_studio_ready(op) — readers (trigger_synth, new_group,
  #     allocate_buffer, etc.). Many concurrent. If a writer
  #     (cold_swap_reinit) holds the lock, blocks until ready.
  #     Reentrant per-thread so trigger_fx → trigger_synth on the
  #     same thread doesn't self-deadlock.
  #
  #   - with_studio_writer — cold_swap_reinit. Waits for in-flight
  #     readers to drain, then owns the studio for its phases.
  #     Reentrant on the same thread (the phase code calls studio
  #     methods that take the read lock).
  #
  #   - mark_permanently_broken!(reason) — supersonic has died and
  #     waiting won't help. Subsequent with_studio_ready calls raise
  #     immediately rather than blocking forever.
  class StudioReadyGate
    def initialize
      @lock = Concurrent::ReentrantReadWriteLock.new
      @permanently_broken = false
      @broken_reason = nil
    end

    def with_studio_ready(op_name, &block)
      if @permanently_broken
        raise StudioCurrentlyRebootingError,
              "studio permanently broken: #{@broken_reason} (called from #{op_name})"
      end
      @lock.with_read_lock(&block)
    end

    def with_studio_writer(&block)
      @lock.with_write_lock(&block)
    end

    def mark_permanently_broken!(reason)
      @permanently_broken = true
      @broken_reason = reason
    end
  end

  # Raised when studio is unavailable. Aliased from studio.rb so
  # existing `rescue Studio::StudioCurrentlyRebootingError` clauses
  # keep working.
  class StudioCurrentlyRebootingError < StandardError; end
end
