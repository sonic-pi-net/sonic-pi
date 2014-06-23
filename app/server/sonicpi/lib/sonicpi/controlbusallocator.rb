#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "busallocator"
require_relative "controlbus"

module SonicPi
  class ControlBusAllocator < BusAllocator
    def bus_class
      ControlBus
    end

    def to_s
      "<#SonicPi::ControlBusAllocator>"
    end
  end
end
