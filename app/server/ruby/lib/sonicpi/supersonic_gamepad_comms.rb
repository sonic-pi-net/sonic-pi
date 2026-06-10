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

require_relative "supersonic_comms"

module SonicPi
  # OSC client for SuperSonic's gamepad subsystem, over the /gamepad/* address
  # space: subscribe_to_notifications! registers this client for
  # /gamepad/in/* + /gamepad/devices pushes.
  class SupersonicGamepadComms < SupersonicComms
    def initialize(supersonic_host, supersonic_port)
      super(supersonic_host, supersonic_port,
            address_space: "/gamepad",
            name: "SuperSonic Gamepad Comms")
    end
  end
end
