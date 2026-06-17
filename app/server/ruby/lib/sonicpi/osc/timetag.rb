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
  module OSC
    # NTP epoch (seconds 1900→1970). Matches OscEncode#time_encoded and the
    # engine's ntp_to_osc_timetag, so an event timetagged here lands on the same
    # clock as a scsynth bundle scheduled for the same time. Shared by the OSC
    # and MIDI APIs so the encoding can't drift between them.
    NTP_EPOCH_OFFSET = 2208988800

    # Seconds (spider/SuperClock domain) → 64-bit OSC timetag.
    def self.osc_timetag(t)
      secs, frac = (t.to_f + NTP_EPOCH_OFFSET).divmod(1)
      (secs.to_i << 32) | (frac * 4294967296.0).to_i
    end
  end
end
