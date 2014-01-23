require_relative "busallocator"
require_relative "controlbus"

module SonicPi
  class ControlBusAllocator < BusAllocator
    def bus_class
      ControlBus
    end
  end
end
