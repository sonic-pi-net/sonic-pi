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
  # Clockwork's C and C++ headers in this tree, for the tests that hold the
  # Ruby readers to them: a change there fails here, with no engine built.
  module ClockworkHeaders
    SRC = File.expand_path("../../../../external/supersonic/clockwork/src", __dir__)

    # The header's text, or nil when clockwork's sources are not in the tree.
    def self.read(name)
      file = File.join(SRC, name)
      return nil unless File.exist?(file)
      File.read(file, encoding: "UTF-8")   # not the locale's: a C locale reads US-ASCII, and the headers' prose isn't
    end

    # `NAME = <number>` wherever the header writes it: a #define, an enum
    # value or a static constexpr.
    def self.number(src, name)
      m = src.match(/\b#{name}\b\s*=?\s*(0x\h+|\d+)u?\b/)
      m && Integer(m[1])
    end
  end
end
