require_relative "busallocator"
require_relative "audiobus"

module SonicPi
  class AudioBusAllocator < BusAllocator
    def bus_class
      AudioBus
    end
  end
end
