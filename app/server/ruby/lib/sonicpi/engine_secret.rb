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

require 'securerandom'

module SonicPi
  # The per-boot secret SuperSonic asks of every client. The daemon makes it
  # when it boots the engine, writes it to the engine's stdin as 64 hex
  # digits and a newline, and hands it on to the GUI and Spider. A client's
  # first packet on any connection is /clockwork/auth with the 32 bytes as a
  # blob (SuperSonic's docs/NATIVE.md, "The secret").
  module EngineSecret
    BYTES = 32
    AUTH_ADDRESS = "/clockwork/auth"

    def self.generate
      SecureRandom.random_bytes(BYTES)
    end

    def self.hex(secret)
      secret.unpack1("H*")
    end

    def self.stdin_line(secret)
      hex(secret) + "\n"
    end

    def self.from_hex(hex)
      raise ArgumentError, "a secret is #{BYTES * 2} hex digits" unless hex.match?(/\A[0-9a-fA-F]{#{BYTES * 2}}\z/)
      [hex].pack("H*")
    end
  end
end
