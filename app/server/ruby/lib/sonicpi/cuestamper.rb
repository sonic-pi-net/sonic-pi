#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi

  # Wall-clock stamps for system-ingressed cue events (external OSC, MIDI).
  #
  # These cues all share the same priority, thread id and delta, so events
  # stamped with an identical time are fully equal in CueEvent order and a
  # syncing thread can only ever be woken by one of them - the rest are
  # unreachable (GH #1839: coarse clocks stamped whole bursts identically
  # and all but one cue was lost). Arrival here is serialised, so arrival
  # order is a true order: when the clock hasn't advanced past the previous
  # stamp, nudge forward by a microsecond to keep stamps strictly
  # increasing.
  class CueStamper
    EPSILON = Rational(1, 1_000_000)

    def initialize
      @mut = Mutex.new
      @last = nil
    end

    def stamp(t = Time.now)
      @mut.synchronize do
        tr = t.to_r
        tr = @last + EPSILON if @last && tr <= @last
        @last = tr
        Time.at(tr)
      end
    end
  end
end
